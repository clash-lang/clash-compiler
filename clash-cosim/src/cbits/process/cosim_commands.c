/*C**********************************************************************
* FILENAME :        cosim_commands.c     
*
* DESCRIPTION :
*       Commands for using the simulators
*                needed for co-simulation between CLaSH and a Verilog simulator
*
* 
* AUTHOR  :    John Verheij        DATE :    August 14, 2016
*
*
*C*/

/* Include */
#include "cosim_commands.h"

char* hdlExtension(int hdl)
{
    char* ext = NULL;
    
    switch (hdl)
    {
        case VERILOG :
            ext = malloc ((strlen(".v")+1) * sizeof(char));
            strcpy(ext, ".v");
            break;
        
        default :
            printf ("Not supported HDL provided\n");
            break;
    }
    
    return ext;
}

/* Define the command to compile the sources for a specific simulator */
int commandCompile(int simulator, char *fileName, char *topEntity, struct coSimFiles *files, int noFiles, char ****commands)
{
    // variables
    int i;
    struct coSimFiles *current = files;
    
    // command has to NULL
    if (*commands != NULL)
    {
        printf("The argument 'commands' must be null\n");
        return -1;
    }
    
    // check for sufficient input-files
    if (noFiles == 0 || files == NULL)
    {
        printf ("Expected at least one source-file for defining command.\n");
        return -1;
    }
    
    // switch simulator
    switch (simulator)
    {
        case ICARUS :
        
            if(topEntity == NULL)
            {
                printf ("Top-Entity is needed for defining command.\n");
                return -1;
            }
            else if (fileName == NULL)
            {
                printf ("File-Name is needed for defining command.\n");
                return -1;
            }
            else
            {
                // malloc command
                *commands = (char ***) malloc (sizeof(char**));
                (*commands)[0] = (char **) malloc ((7 + noFiles) * sizeof(char*));
                
                // program '/usr/local/bin/iverilog'
                ((*commands)[0])[0] = (char*) malloc((strlen("iverilog")+1) * sizeof(char));
                strcpy(((*commands)[0])[0], "iverilog");
                
                // '-s'
                ((*commands)[0])[1] = (char*) malloc((strlen("-s")+1) * sizeof(char));
                strcpy(((*commands)[0])[1], "-s");
                
                // top-entity
                ((*commands)[0])[2] = (char*) malloc((strlen(topEntity)+1) * sizeof(char));
                strcpy(((*commands)[0])[2], topEntity);
                
                // '-o'
                ((*commands)[0])[3] = (char*) malloc((strlen("-o")+1) * sizeof(char));
                strcpy(((*commands)[0])[3], "-o");
                
                // fileName
                ((*commands)[0])[4] = (char*) malloc((strlen(fileName)+1) * sizeof(char));
                strcpy(((*commands)[0])[4], fileName);
                
                ((*commands)[0])[5] = (char*) malloc((strlen("-g2005")+1) * sizeof(char));
                strcpy(((*commands)[0])[5], "-g2005");
            
                // loop & add all the file-names
                for(i = 0; i<noFiles; i++)
                {
                    
                    ((*commands)[0])[i+6] = (char*) malloc((strlen(current->fileName)+1) * sizeof(char));
                    strcpy(((*commands)[0])[i+6], current->fileName);
                    current = current->next;
                }
                
                // NULL
                ((*commands)[0])[6+noFiles] = NULL;
            }
            return 1;
        
        case GHDL :
            
            // see https://github.com/tgingold/ghdl/issues/101
            
            printf("GHDL is not supported yet.\n");
            return -1;
            
        case MODELSIM :
            
            if(topEntity == NULL)
            {
                printf ("Top-Entity is needed for defining command.\n");
                return -1;
            }
            else
            {
            
                // malloc command
                *commands = (char ***) malloc (2 * sizeof(char**));
                (*commands)[0] = (char **) malloc (sizeof(char*) * 3);
                (*commands)[1] = (char **) malloc (sizeof(char*) * (3 + noFiles));

                // program 'vlib'
                ((*commands)[0])[0] = (char*) malloc((strlen("vlib")+1) * sizeof(char));
                strcpy(((*commands)[0])[0], "vlib");
                
                // work
                ((*commands)[0])[1] = (char*) malloc((strlen("work")+1) * sizeof(char));
                strcpy(((*commands)[0])[1], "work");
                
                // NULL
                ((*commands)[0])[2] = NULL;
                
                // program 'vlog'
                ((*commands)[1])[0] = (char*) malloc((strlen("vlog")+1) * sizeof(char));
                strcpy(((*commands)[1])[0], "vlog");
                
                ((*commands)[1])[1] = (char*) malloc((strlen("-quiet")+1) * sizeof(char));
                strcpy(((*commands)[1])[1], "-quiet");
        
                // loop & add all the file-names
                for(i = 0; i<noFiles; i++)
                {
                    ((*commands)[1])[i+2] = (char*) malloc((strlen(current->fileName)+1) * sizeof(char));
                    strcpy(((*commands)[1])[i+1], current->fileName);
                    current = current->next;
                }
                
                // NULL
                ((*commands)[1])[1 + noFiles] = NULL;
            }
            return 2;
            
        default :
        
            printf ("Not supported simulator provided.%d\n", simulator);
            return -1;
    }
    
    // return
    return -1;
}

/* Define the command to execute a specific simulator */
char** commandExecute(int simulator,  char* fileName, char* topEntity)
{
    // variables
    char **command;
    
    // init
    command = NULL;
    
    // switch simulator
    switch (simulator)
    {
        case ICARUS :
            
            if (fileName == NULL)
            {
                printf ("File-Name is needed for defining command.\n");
            }
            else
            {
                // malloc command
                command = (char **) malloc (6 * sizeof(char*));
            
                // program '/usr/local/bin/vvp'
                command[0] = (char*) malloc((strlen("vvp")+1) * sizeof(char));
                strcpy(command[0], "vvp");
            
                // Non-interactive ($stop = $finish) with exit-code is 1
                command[1] = (char*) malloc((strlen("-N")+1) * sizeof(char));
                strcpy(command[1], "-N");
            
                // current dir interface
                command[2] = (char*) malloc((strlen("-M.")+1) * sizeof(char));
                strcpy(command[2], "-M.");
            
                // name vpi interface
                command[3] = (char*) malloc((strlen("-mcosim_vpi")+1) * sizeof(char));
                strcpy(command[3], "-mcosim_vpi");
            
                // file-name
                command[4] = (char*) malloc((strlen(fileName)+1) * sizeof(char));
                strcpy(command[4], fileName);
            
                // NULL
                command[5] = NULL;
            }
            
            break;
        
        case GHDL :
            
            // see https://github.com/tgingold/ghdl/issues/101
            
            printf("GHDL is not supported yet.\n");
            return NULL;

        case MODELSIM :
            
            if(topEntity == NULL)
            {
                printf ("Top-Entity is needed for defining command.\n");
            }
            else
            {
                // malloc command
                command = (char **) malloc (9 * sizeof(char*));
            
                // program
                command[0] = (char*) malloc((strlen("vsim")+1) * sizeof(char));
                strcpy(command[0], "vsim");
            
                // current dir interface
                command[1] = (char*) malloc((strlen("-c")+1) * sizeof(char));
                strcpy(command[1], "-c");
            
                //-onfinish
                command[2] = (char*) malloc((strlen("-quiet")+1) * sizeof(char));
                strcpy(command[2], "-quiet");
            
                // name vpi interface
                command[3] = (char*) malloc((strlen("-do")+1) * sizeof(char));
                strcpy(command[3], "-do");
            
                // file-name
                command[4] = (char*) malloc((strlen("onfinish exit; run -all")+1) * sizeof(char));
                strcpy(command[4], "onfinish exit; run -all");
                
                // top-entity
                command[5] = (char*) malloc((strlen(topEntity)+1) * sizeof(char));
                strcpy(command[5], topEntity);
            
                // enable vpi
                command[6] = (char*) malloc((strlen("-pli")+1) * sizeof(char));
                strcpy(command[6], "-pli");
            
                // name vpi interface
                command[7] = (char*) malloc((strlen("cosim_vpi.sl")+1) * sizeof(char));
                strcpy(command[7], "cosim_vpi.sl");
            
                // NULL
                command[8] = NULL;
            }
            
            break;
        default :
        
            break;
    }
    return command;
}