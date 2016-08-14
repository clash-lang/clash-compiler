/*C**********************************************************************
* FILENAME :        cosim_types_vpi.c        
*
* DESCRIPTION :
*       Types and defines needed for VPI
*                needed for co-simulation between CLaSH and a Verilog simulator
*
* 
* AUTHOR  :    John Verheij        DATE :    August 14, 2016
*
*
*C*/

/* Include */
#include "cosim_types_vpi.h"

struct vpiState *createState(void)
{
    struct vpiState *state;
    state = malloc(sizeof(struct vpiState));
    state->period = -1;
    state->reset = 0;
    state->noInput = 0;
    state->noOutput = 0;
    state->comm = NULL;
    state->inputPorts = NULL;
    state->outputPorts = NULL;
    
    // define time for writing values
    state->time.type = vpiSimTime;
    state->time.low = 0;
    state->time.high = 0;
    
    // define vector for writing values
    state->vector.format = vpiVectorVal;
    return state;
}

void disposeState(struct vpiState **state)
{
    if (*state != NULL)
    {
        disposeComm(&(*state)->comm);
        disposePorts(&(*state)->inputPorts);
        disposePorts(&(*state)->outputPorts);
        free(*state);
        *state = NULL;
    }
}

/* dispose the struct port */
void disposePorts(struct port **ports)
{
    struct port *current = *ports, *tmp = NULL;
  
    if (*ports != NULL)
    {
        while(current != NULL)
        {
            tmp = current->next;
            if (current -> name != NULL)
                {
                free(current->name);
                current->name = NULL;
            }
            if (current -> fullName != NULL)
            {
                free(current->fullName);
                current->fullName = NULL;
            }
            if (current -> vector != NULL)
            {
                free(current->vector);
                current->vector = NULL;
            }
            free(current);
            current = tmp;
        }
        *ports = NULL;
    }
}