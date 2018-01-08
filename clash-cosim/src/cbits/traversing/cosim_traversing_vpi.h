/*H**********************************************************************
* FILENAME :        CoSimTypesVPI.h        
*
* DESCRIPTION :
*       Traversing functions needed for VPI
*                needed for co-simulation between CLaSH and a Verilog simulator
*
* 
* AUTHOR  :    John Verheij        DATE :    August 14, 2016
*
*
*H*/

/* Guard */
#ifndef COSIM_TRAVERSING_VPI_H_  
#define COSIM_TRAVERSING_VPI_H_

#include "../types/cosim_types_vpi.h"

int exchangeSeq(struct vpiState *state);

int exchangePortSpecs(struct vpiState *state, int input);

int getPortSpecs(vpiHandle handle, struct vpiState *state, char *moduleName);

int getModuleSpecs(vpiHandle handle, struct vpiState *state);

#endif