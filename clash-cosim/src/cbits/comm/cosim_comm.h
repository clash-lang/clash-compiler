/*H**********************************************************************
* FILENAME :        cosim_comm.h        
*
* DESCRIPTION :
*       Communication functions (Linux -> pipes)
*                needed for co-simulation between CLaSH and a Verilog simulator
*
* 
* AUTHOR  :    John Verheij        DATE :    August 14, 2016
*
*
*H*/

/* Guard */
#ifndef COSIM_COMM_H_   
#define COSIM_COMM_H_

/* Includes */
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

/* Define */
#define MAX_BUF 64
#define ENVREAD "fd_in"
#define ENVWRITE "fd_out"

struct CoSimComm
{
    int fdRead;
    int fdWrite;
};

int createComm(struct CoSimComm **comm1, struct CoSimComm **comm2);

int writeMessage(char *message, struct CoSimComm *comm, int accept);

int readMessage(char *message, struct CoSimComm *comm, int accept);

void disposeComm(struct CoSimComm **comm);

int shareComm(struct CoSimComm **comm);

int getSharedComm(struct CoSimComm **comm);

void disposeSharedComm(void);

int getAccept(struct CoSimComm *comm);

int sendAccept(struct CoSimComm *comm);

#endif 