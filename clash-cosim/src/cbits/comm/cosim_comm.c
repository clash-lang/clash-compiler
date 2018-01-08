/*C**********************************************************************
* FILENAME :        cosim_comm.c        
*
* DESCRIPTION :
*       Communication functions (Linux -> pipes)
*                needed for co-simulation between CLaSH and a Verilog simulator
*
* 
* AUTHOR  :    John Verheij        DATE :    August 14, 2016
*
*
*C*/

/* Include */
#include "cosim_comm.h"

int createComm(struct CoSimComm **comm1, struct CoSimComm **comm2)
{
    // variables
    int fd1[2], fd2[2];

    // check input
    if ((*comm1) != NULL || (*comm2) != NULL)
    {
        printf("Structs already set by creating communication \n");
        return -1;
    }

    // create pipe
    if (pipe(fd1) < 0)
    { 
        printf("Unable to create pipe\n");  
        return -1;
    }
    
    // create pipe
    if (pipe(fd2) < 0)
    { 
        // close pipes
        close (fd1[0]);
        close (fd1[1]);
    
        printf("Unable to create pipe\n");  
        return -1;
    }
    
    // malloc
    (*comm1) = malloc(sizeof(struct CoSimComm));
    (*comm2) = malloc(sizeof(struct CoSimComm));
    
    // set structs
    (*comm1)->fdRead = fd1[0];
    (*comm1)->fdWrite = fd2[1];
    (*comm2)->fdRead = fd2[0];
    (*comm2)->fdWrite = fd1[1];
    
    return 0;
}

int writeMessage(char *message, struct CoSimComm *comm, int accept)
{
    // variables
    int res;
    char str[MAX_BUF];
    
    // init
    errno = 0;
    
    // copy message
    snprintf(str, MAX_BUF-1, "%s", message);
    
    // write message
    res =  write(comm->fdWrite,str, MAX_BUF);
    if (res < 0)
    {
        //fprintf(stderr, "Communication error [%s]\n", strerror( errno ));
        return res;
    }
    
    return (accept ? getAccept(comm) : 0);
}

int readMessage(char *message, struct CoSimComm *comm, int accept)
{
    // variables
    int res;
    
    // init
    errno = 0;

    // read message
    res = read(comm->fdRead, message, MAX_BUF);
    if (res < 0)
    {
        //fprintf(stderr, "Communication error [%s]\n", strerror( errno ));
        return res;
    }
    
    return (accept ? sendAccept(comm) : 0);
}

void disposeComm(struct CoSimComm **comm)
{
    if (*comm != NULL)
    {
        // close fifos
        close((*comm)->fdRead);
        close((*comm)->fdWrite);
        
        // free
        free(*comm);
        (*comm) = NULL;
    }
}

int getSharedComm(struct CoSimComm **comm)
{
    // variables
    int fdR, fdW;
    
    // init
    fdR = -1;
    fdW = -1;

    // check argument
    if ((*comm) != NULL)
    {
        fprintf(stderr,"Struct already set by getting communication \n");
    } 
    
    // get fd from environment
    if (getenv(ENVREAD) != NULL) 
        fdR = atoi(getenv(ENVREAD));
    if (getenv(ENVWRITE) != NULL) 
        fdW = atoi(getenv(ENVWRITE));
    
    if (fdR < 3 || fdW < 3)
    {
        fprintf(stderr,"Incorrect file-handlers [%d] [%d]\n", fdR, fdW);
        return -1;
    }
    
    // malloc
    (*comm) = malloc(sizeof(struct CoSimComm));
    
    // set structs
    (*comm)->fdRead  = fdR;
    (*comm)->fdWrite = fdW;
    
    return 0;
}

void disposeSharedComm(void)
{
    // close fifos
    if (getenv(ENVREAD) != NULL) 
        close(atoi(getenv(ENVREAD)));
    
    if (getenv(ENVWRITE) != NULL) 
        close(atoi(getenv(ENVWRITE)));
}

int shareComm(struct CoSimComm **comm)
{
    if (*comm == NULL)
    {
        printf("Nullpointer in shareComm\n");
        return -1;
    }

    // variables
    char fdR[MAX_BUF];        
    char fdW[MAX_BUF];  
    
    // copy environment
    snprintf(fdR, MAX_BUF-1, "%d",  (*comm)->fdRead);
    snprintf(fdW, MAX_BUF-1, "%d", (*comm)->fdWrite);
    
    // set environment
    setenv(ENVREAD, fdR,1);
    setenv(ENVWRITE,fdW,1);
    
    // free
    free(*comm);
    (*comm) = NULL;
    
    return 0;
}

/* protocol, after receiving sometimes needed to send 'accept' */
int sendAccept(struct CoSimComm *comm)
{
    // write value "1"
    return writeMessage("1", comm, 0);
}

/* protocol, after sending sometimes needed to get 'accept' */
int getAccept(struct CoSimComm *comm)
{
    // variables
    char str[MAX_BUF+1];
    
    // check if got the value "1"
    return readMessage(str, comm, 0) < 0 ? -1 : (atoi(str) == 1 ? 1 : -1);
}
