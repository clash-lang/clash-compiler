/*C**********************************************************************
* FILENAME :        cosim_types.c       
*
* DESCRIPTION :
*       Types and defines 
*                needed for co-simulation between CLaSH and a (Verilog) simulator
*
* 
* AUTHOR  :    John Verheij        DATE :    August 14, 2016
*
*
*C*/

/* Include */
#include "cosim_types.h"

/* get number of input values */
int getInputLength(struct coSimState *state)
{   
    return (state == NULL) ? -1 : state->noInput;
}

/* get number of output values */
int getOutputLength(struct coSimState *state)
{
    return (state == NULL) ? -1 : state->noOutput;
}

/* get number of bit for every input value */
int* getInputSizes(struct coSimState *state)
{
    return (state == NULL) ? NULL : state->inputSizes;
}

/* get number of bit for every output value */
int* getOutputSizes(struct coSimState *state)
{
    return (state == NULL) ? NULL : state->outputSizes;
}

/* get pointer to input-values-array */
int** getInputPtr(struct coSimState *state)
{
    return (state == NULL) ? NULL : state->input;
}

/* get pointer to output-values-array */
int** getOutputPtr(struct coSimState *state)
{
    return (state == NULL) ? NULL : state->output;
}

struct coSimState *createCoSimState(void)
{
    // create new state
    struct coSimState *retVal   = malloc (sizeof (struct coSimState));
    
    // set state to default values
    retVal->reset               = 0;
    retVal->noInput             = -1;
    retVal->noOutput            = -1;
    retVal->noFiles             = -1;
    retVal->inputSizes          = NULL;
    retVal->outputSizes         = NULL;
    retVal->input               = NULL;
    retVal->output              = NULL;
    retVal->fileNames           = NULL;
    retVal->comm                = NULL;
    
    return retVal;
}

/* dispose an array of int-pointers */
void disposeIntPointers(int ***array, int length)
{
    // variables
    int i;
    
    // check if input is a correct pointer
	if(*array != NULL)
	{
        // loop all the pointers
		for (i = 0; i < length; i++)
		{
            // free pointer if not NULL
			if ((*array)[i] != NULL)
			{
				free((*array)[i]);
                (*array)[i] = NULL;
			}
		}
        
        // free input
		free(*array);
		*array = NULL;
	}
}

/* dispose an array of string-pointers */
void disposeStringPointers(char ***array, int length)
{
    // variables
    int i;
    
    // check if input is a correct pointer
	if(*array != NULL)
	{
        // loop all the pointers
		for (i = 0; i < length; i++)
		{
            // free pointer if not NULL
			if ((*array)[i] != NULL)
			{
				free((*array)[i]);
                (*array)[i] = NULL;
			}
		}
        
        // free input
		free(*array);
		*array = NULL;
	}
}

void disposeFileNames(struct coSimFiles **files)
{
    struct coSimFiles *current = *files, *next = NULL;
    
    if (*files != NULL)
    {
        while (current != NULL)
        {
            next = current->next;
            free(current->fileName);
            free(current);
            current = next;
        }
        *files = NULL;
    }
}

/* dispose an array of string-pointers */
void disposeStringPointers_(char ***array)
{
    // variables
    int i;
    
    // check if input is a correct pointer
	if(*array != NULL)
	{
        // loop all the pointers
		for (i = 0;; i++)
		{
            // free pointer if not NULL
			if ((*array)[i] != NULL)
			{
				free((*array)[i]);
                (*array)[i] = NULL;
			}
            else
            {
                // free input
                free(*array);
                *array = NULL;
                return;
            }
		}
	}
}

/* dispose the struct "CoSim_State" */
void disposeCoSimState(struct coSimState **state)
{   
    // variables
    int i;

    // check if input is a correct pointer
    if (*state != NULL)
    {
        // dispose input-bits 
        if((*state)->inputSizes != NULL)
		{
            free((*state)->inputSizes);
		}
        
        // dispose output-bits 
        if((*state)->outputSizes != NULL)
		{
            free((*state)->outputSizes);
		}
		
        // dispose inputs 
		disposeIntPointers(&(*state)->input, (*state)->noInput);
    
        // dispose outputs 
		disposeIntPointers(&(*state)->output, (*state)->noOutput);
        
        
        // dispose file-names
        if((*state)->fileNames != NULL)
        {
            // loop all file-names 
            for (i = 0; i<(*state)->noFiles; i++)
            {
                // null-pointer check
                if ((*state)->fileNames[i] != NULL)
                {
                    // check if file exist
                    if (access((*state)->fileNames[i], F_OK ) != -1 ) 
                    {
                        // remove file
                        if(remove((*state)->fileNames[i]) != 0)
                        {   
                            printf ("Unable to delete generate file '%s'\nPlease delete manually...\n", (*state)->fileNames[i]);           
                        }
                    }
                    free((*state)->fileNames[i]);
                }
            }
            free((*state)->fileNames);
        }
        
        // close pipes
        disposeComm(&((*state)->comm));
        
        // dispose struct 
        free(*state);
        *state = NULL;
    }
}