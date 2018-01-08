/*H**********************************************************************
* FILENAME :        cosim_control.h        
*
* DESCRIPTION :
*       Control functions needed for VPI
*                needed for co-simulation between CLaSH and a Verilog simulator
*
* 
* AUTHOR  :    John Verheij        DATE :    August 14, 2016
*
*
*H*/

/* Guard */
#ifndef COSIM_CONTROL_VPI_H_  
#define COSIM_CONTROL_VPI_H_

#include "../types/cosim_types_vpi.h"

int abortSim(void);
int abortSimM(struct vpiState *state);

// a typedef creates a fake type, in this
// case for a function pointer
typedef PLI_INT32 (*f_cb)(p_cb_data cb_data);

extern PLI_INT32 startOfSim(p_cb_data cb_data);

void registerCB(struct vpiState *state, f_cb f, PLI_INT32 reason, PLI_INT32 time);

void registerCallbacks(void);

#endif