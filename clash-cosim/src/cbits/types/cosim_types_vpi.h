/*H**********************************************************************
* FILENAME :        cosim_types_vpi.h        
*
* DESCRIPTION :
*       Types and defines needed for VPI
*                needed for co-simulation between CLaSH and a Verilog simulator
*
* 
* AUTHOR  :    John Verheij        DATE :    August 14, 2016
*
*
*H*/

/* Guard */
#ifndef COSIM_TYPES_VPI_H_  
#define COSIM_TYPES_VPI_H_

/* Include */
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include "../include/vpi_user.h"
#include "../comm/cosim_comm.h"

struct vpiState
{
    /* communication */
    struct CoSimComm *comm;
    
    /* for receiving messages from CLaSH */
    char strR[MAX_BUF+1]; 
    int period;
    int reset;
    
    /* number of input and output ports */
    int noInput;
    int noOutput;
    
    /* port specific information */
    struct port *inputPorts;
    struct port *outputPorts;
    
    /* allocation for writing into simulator */
    s_vpi_time time;
    s_vpi_value vector;
};

/* struct for every port in a module */
struct port {
  int type;
  char* name;
  char* fullName;
  int width;
  struct port *next;
  s_vpi_vecval *vector;
  vpiHandle handle;
};

struct vpiState *createState(void);

void disposeState(struct vpiState **state);

/* dispose the struct port */
void disposePorts(struct port **ports);

#endif 