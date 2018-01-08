/*H**********************************************************************
* FILENAME :        cosim_commands.h      
*
* DESCRIPTION :
*       Commands for using the simulators
*                needed for co-simulation between CLaSH and a Verilog simulator
*
* 
* AUTHOR  :    John Verheij        DATE :    August 14, 2016
*
*
*H*/

/* Guard */
#ifndef COSIM_COMMANDS_H_  
#define COSIM_COMMANDS_H_

/* Include */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../types/cosim_types.h"

/* Define */
#define MAX_COMMAND 512

#define ICARUS          1
#define MODELSIM        2
#define GHDL            3

#define VERILOG         1
#define SYSTEMVERILOG   2
#define VHDL            3

char* hdlExtension(int hdl);

/* Define the command to compile the sources for a specific simulator */
int commandCompile(int simulator, char *fileName, char *topEntity, struct coSimFiles *files, int noFiles, char ****commands);

/* Define the command to execute a specific simulator */
char** commandExecute(int simulator,  char* fileName, char* topEntity);

#endif 