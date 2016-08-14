/*C**********************************************************************
* FILENAME :        cosim_clash.c        
*
* DESCRIPTION :
*       Simulation -start, -step, -info-exchanges, which will be loaded in CLaSH
*                needed for co-simulation between CLaSH and a simulator...
*
* 
* AUTHOR  :    John Verheij        DATE :    July 6, 2016
*
*
*C*/

/* Include */
#include "./comm/cosim_comm.c"
#include "./process/cosim_commands.c"
#include "./process/cosim_process.c"
#include "./traversing/cosim_traversing.c"
#include "./types/cosim_types.c"

#include <sys/stat.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>

/* settings in simStart */
#define SIM     0
#define HDL     1
#define PERIOD  2
#define RST     3
#define STDOUT  4
#define NOFILES 5
#define LEN     6


struct coSimState *disposeSettings(int **settings, char **topEntity, char ***sourcePaths, struct coSimState **state, struct coSimFiles **files)
{
    if (*topEntity != NULL)
    {   
        free(*topEntity);
        *topEntity = NULL;
    }
    if (settings != NULL)
    {   
        disposeStringPointers(sourcePaths, (*settings)[NOFILES]);
        free(*settings);
        *settings = NULL;
    }
    disposeFileNames(files);
    disposeCoSimState(state); 
    
    return NULL;
}

/* function to start co-simulation */
struct coSimState *simStart(int *settings, char *topEntity, char **sourcePaths)
{
    // variables
    char*** commandC = NULL;        // command for compiling
    char** commandE = NULL;         // command for execution
    char* ext = NULL;
    int i, noCommands, numberOfFiles  = 0;
    struct coSimFiles *files = NULL;
    struct coSimState *retVal = NULL, *nullState = NULL;
    
    // create new state
    retVal = createCoSimState();
    if (retVal == NULL)
        return disposeSettings(&settings, &topEntity, &sourcePaths, &retVal, &files);
    retVal->reset = settings[RST];
    
    // check if source files exist
    if(sourcePaths == NULL || settings[NOFILES] <= 0) 
    {
        printf("No source or file given\n");
        return disposeSettings(&settings, &topEntity, &sourcePaths, &retVal, &files);
    }
    if(topEntity == NULL)
    {
        printf("No top-entity given\n");
        return disposeSettings(&settings, &topEntity, &sourcePaths, &retVal, &files);
    }
    
    // get all the files
    ext = hdlExtension(settings[HDL]);
    numberOfFiles = getAllFiles(&files, settings[NOFILES], sourcePaths, ext);
    if (ext != NULL) free(ext);
    if (numberOfFiles <= 0) return disposeSettings(&settings, &topEntity, &sourcePaths, &retVal, &files);
    
    // set created files in struct, they will be deleted afterwards
    retVal->noFiles = 2;
    retVal->fileNames = malloc (retVal->noFiles * sizeof(char*));
    retVal->fileNames[0] = NULL;
    retVal->fileNames[1] = (char*) malloc((strlen(sourcePaths[0])+1) * sizeof(char));
    strcpy(retVal->fileNames[1], sourcePaths[0]);
    
    // create tempory name 
    if(tempName(&(retVal->fileNames[0])) < 0) return disposeSettings(&settings, &topEntity, &sourcePaths, &retVal, &files);
    
    // build command for compiling
    noCommands = commandCompile(settings[SIM], retVal->fileNames[0], topEntity, files, numberOfFiles, &commandC);
    if (noCommands < 0 || commandC == NULL) return disposeSettings(&settings, &topEntity, &sourcePaths, &retVal, &files);
    
    // execute commands
    for (i = 0; i<noCommands; i++)
    {   
        if (startProcess(commandC[i], NULL, settings[STDOUT], 1) < 0)
            return disposeSettings(&settings, &topEntity, &sourcePaths, &retVal, &files);
    }
    
    // build command for execution
    commandE = commandExecute(settings[SIM], retVal->fileNames[0], topEntity);
    if (commandE == NULL) return disposeSettings(&settings, &topEntity, &sourcePaths, &retVal, &files);
        
    // start simulator    
    if (startProcess(commandE, &(retVal->comm), settings[STDOUT], 0) < 0)
        return disposeSettings(&settings, &topEntity, &sourcePaths, &retVal, &files);
    
    // send period
    sprintf(retVal->strR, "%d", settings[PERIOD]);
    if (writeMessage(retVal->strR,retVal->comm, 1) < 0)
        return disposeSettings(&settings, &topEntity, &sourcePaths, &retVal, &files);
    
    // send reset
    sprintf(retVal->strR, "%d", settings[RST]);
    if (writeMessage(retVal->strR,retVal->comm, 1) < 0)
        return disposeSettings(&settings, &topEntity, &sourcePaths, &retVal, &files);
        
    // exchange input port-names
    retVal->noInput = exchangePortNames(retVal);
    if (retVal->noInput < 0)
        return disposeSettings(&settings, &topEntity, &sourcePaths, &retVal, &files);
        
    if (retVal->noInput != settings[LEN])
    {
        printf("Simulator expects %d input ports, but %d given\n", retVal->noInput, settings[LEN]);
        return disposeSettings(&settings, &topEntity, &sourcePaths, &retVal, &files);
    }

    // exchange port-bits
    retVal->inputSizes = (int *) malloc(retVal->noInput * sizeof(int));
    if (exchangePortSizes(retVal, 1) < 0)
        return disposeSettings(&settings, &topEntity, &sourcePaths, &retVal, &files);
            
    // malloc inputs
    retVal->input = malloc (retVal->noInput * sizeof (int*));
    for(i = 0; i<retVal->noInput; i++)
        retVal->input[i] = (int *) malloc(retVal->inputSizes[i] * sizeof(int));
        
    // exchange output port-names
    retVal->noOutput = exchangePortNames(retVal);
    if (retVal->noOutput < 0)
        return disposeSettings(&settings, &topEntity, &sourcePaths, &retVal, &files);
        
    // exchange port-bits
    retVal->outputSizes = (int *) malloc(retVal->noOutput * sizeof(int));
    if (exchangePortSizes(retVal, 0) < 0)
        return disposeSettings(&settings, &topEntity, &sourcePaths, &retVal, &files);
        
    // malloc output
    retVal->output = malloc (retVal->noInput * sizeof (int*));
    for(i = 0; i<retVal->noOutput; i++)
        retVal->output[i] = (int *) malloc(retVal->outputSizes[i] * sizeof(int));
    
    // dispose
    disposeSettings(&settings, &topEntity, &sourcePaths, &nullState, &files);
    
    // return struct
    return retVal;
}

/* perform single simulation step */
int simStep(struct coSimState *state)
{
    // variables
    int i, j;

    // check null-pointer
    if (state == NULL) return -1;
    
    // write 'next', part of protocol
    if (writeMessage("next",state->comm, 1) < 0) return -1;
    
    if (state->reset > 0) state->reset--;
    else
    {
        // write 'input', part of protocol
        if (writeMessage("input",state->comm, 1) < 0) return -1;
    
        // loop all input-signals
        for (i = 0; i<state->noInput; i++)
        {
            // loop all vector-parts
            for (j = state->inputSizes[i]-1; j>=0; j--)
            {
                // send vector to simulator
                sprintf(state->strR, "%d", state->input[i][j]);
                if (writeMessage(state->strR,state->comm, 1) < 0) return -1; 
            }
        }
    }
    
    // write 'output', part of protocol
    writeMessage("output",state->comm, 0);
    
    // loop all input-signals
    for (i = 0; i<state->noOutput; i++)
    {
        // loop all vector-parts
        for (j = state->outputSizes[i]-1; j>=0; j--)
        {
            // get vector from simulator
            if (readMessage(state->strR, state->comm, 1) < 0) return -1;
            state->output[i][j] = atoi(state->strR);
        }
    }

    return (getAccept(state->comm) <= 0 ? -1 : 0);
}

/* simulation end -> dispose struct and finish simulation */
void simEnd(struct coSimState *state)
{   
    // check null-pointer
    if(state == NULL)
    {
        printf("Simulation finish error\n");
    }
    else
    {
        // write 'finish', part of protocol
        writeMessage("finish",state->comm, 0);
        
        // dispose struct
        disposeCoSimState(&state);
    }
}
