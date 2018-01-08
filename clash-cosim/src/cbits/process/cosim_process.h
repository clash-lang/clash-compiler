/*H**********************************************************************
* FILENAME :        cosim_process.h        
*
* DESCRIPTION :
*       Executing a process and writing to files
*                needed for co-simulation between CLaSH and a Verilog simulator
*
* 
* AUTHOR  :    John Verheij        DATE :    August 14, 2016
*
*
*H*/

/* Guard */
#ifndef COSIM_PROCESS_H_  
#define COSIM_PROCESS_H_

/* Include */
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <dirent.h>
#include "../types/cosim_types.h"

int tempName(char **name);

int isDir(const char *path);

int hasExtension(char const *name, char const *ext);

int getAllFiles(struct coSimFiles  **files, int noFiles, char **sourcePaths, char *extension);

int getFiles(char *dir, struct coSimFiles **files, char const *ext, int *noFiles);

/* Write data to file */
char *writeToFile(char *data);

/* start a process */
int startProcess(char **arguments, struct CoSimComm **comm, int stdOut, int waitProcess);

#endif