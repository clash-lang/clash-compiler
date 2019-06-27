/*C**********************************************************************
* FILENAME :        cosim_traversing.c       
*
* DESCRIPTION :
*       Traversing functions
*                needed for co-simulation between CLaSH and a Verilog simulator
*
* 
* AUTHOR  :    John Verheij        DATE :    August 14, 2016
*
*
*C*/

/* Include */
#include "cosim_traversing.h"

/* Exchange port-names between CLaSH and simulator */
int exchangePortNames(struct coSimState *state)
{ 
    // check null pointer
    if (state == NULL) return -1;
    
    // get # ports
    if (writeMessage("ports",state->comm, 0) < 0) return -1;
    if (readMessage(state->strR, state->comm, 0) < 0) return -1;
    return atoi(state->strR);
}

/* Exchange the bit-size of every port between CLaSH and simulator */
int exchangePortSizes(struct coSimState *state, int isInput)
{
    // variables
    int i, noPorts, *bits;
    
    // init
    noPorts = isInput ? state->noInput : state->noOutput;
    bits = isInput ? state->inputSizes : state->outputSizes;
    
    // null check
    if (state == NULL || bits == NULL)
    {
        printf("Null-pointer in 'exchangePortBits' function\n");
        return -1;
    }
    
    // write 'bits' to simulator, part of protocol
    writeMessage("bits",state->comm, 0);
    
    // loop every port
    for(i = 0; i<noPorts; i++)
    {
        // get # bits
        if (readMessage(state->strR, state->comm, 1) < 0) return -1;
        bits[i] = atoi(state->strR);
    }
    
    // get accept, part of protocol 
    return (getAccept(state->comm) <= 0) ? -1 : 1;
}