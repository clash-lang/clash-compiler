/*H**********************************************************************
* FILENAME :        cosim_traversing.h        
*
* DESCRIPTION :
*       Traversing functions
*                needed for co-simulation between CLaSH and a Verilog simulator
*
* 
* AUTHOR  :    John Verheij        DATE :    August 14, 2016
*
*
*H*/

/* Guard */
#ifndef COSIM_TRAVERSING_H_  
#define COSIM_TRAVERSING_H_

#include "../types/cosim_types.h"

int exchangePortNames(struct coSimState *state);

int exchangePortBits(struct coSimState *state, int input);

#endif