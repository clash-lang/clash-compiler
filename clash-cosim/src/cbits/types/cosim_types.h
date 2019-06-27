/*H**********************************************************************
* FILENAME :        cosim_types.h        
*
* DESCRIPTION :
*       Types and defines 
*                needed for co-simulation between CLaSH and a (Verilog) simulator
*
* 
* AUTHOR  :    John Verheij        DATE :    August 14, 2016
*
*
*H*/

/* Guard */
#ifndef COSIM_TYPES_H_  
#define COSIM_TYPES_H_

/* Include */
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include "../comm/cosim_comm.h"

struct coSimFiles
{
    char *fileName;
    struct coSimFiles *next;
};

/* Struct used for every co-simulation step */
struct coSimState {
    
    /* communication */
    struct CoSimComm *comm;
    
    /* for receiving messages from simulator */
    char strR[MAX_BUF+1];          
    
    /* no of reset cycles often 0 or 1 */
    int reset;
    
    /* number of input and output ports */
    int noInput;
    int noOutput;
    
    /* bit-sizes */
    int *inputSizes;
    int *outputSizes;
    
    /* input and outputs */
    int **input;
    int **output;
    
    /* file-names */
    int noFiles;
    char **fileNames;
};

/* get number of input values */
int getInputLength(struct coSimState *state);

/* get number of output values */
int getOutputLength(struct coSimState *state);

/* get number of bit for every input value */
int* getInputSizes(struct coSimState *state);

/* get number of bit for every output value */
int* getOutputSizes(struct coSimState *state);

/* get pointer to input-values-array */
int** getInputPtr(struct coSimState *state);

/* get pointer to output-values-array */
int** getOutputPtr(struct coSimState *state);

struct coSimState *createCoSimState(void);

/* dispose an array of int-pointers */
void disposeIntPointers(int ***array, int length);

/* dispose an array of string-pointers */
void disposeStringPointers(char ***array, int length);

/* dispose an array of string-pointers */
void disposeStringPointers_(char ***array);

void disposeFileNames(struct coSimFiles **files);

/* dispose the struct "CoSim_State" */
void disposeCoSimState(struct coSimState **state);

#endif 