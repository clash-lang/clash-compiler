/*C**********************************************************************
* FILENAME :        cosim_vpi.c        
*
* DESCRIPTION :
*       Simulation callbacks and info-exchanges, which will be loaded in the Verilog simulator
*                needed for co-simulation between CLaSH and a Verilog simulator
*
* 
* AUTHOR  :    John Verheij        DATE :    August 14, 2016
*
*
*C*/

/* Include */
#include "./comm/cosim_comm.c"
#include "./include/vpi_user.h"
#include "./types/cosim_types_vpi.c"
#include "./control/cosim_control_vpi.c"
#include "./traversing/cosim_traversing_vpi.c"

PLI_INT32 readWriteSynch(p_cb_data cb_data);
PLI_INT32 readOnlySynch(p_cb_data cb_data);
PLI_INT32 registerRD(p_cb_data cb_data);
PLI_INT32 synchStep(p_cb_data cb_data);
PLI_INT32 endOfSim(p_cb_data cb_data);
PLI_INT32 startOfSim(p_cb_data __attribute__((__unused__)) cb_data);

/**********************************************
 ** ReadWrite Sync CallBack
 *********************************************/
PLI_INT32 readWriteSynch(p_cb_data cb_data)
{
    // variables
    struct vpiState *state;
    struct port *port;
    int i;
    
    // init
    state = (struct vpiState*) cb_data->user_data;
    port = state->inputPorts;
    
    // read 'input', part of protocol
    if (readMessage(state->strR, state->comm, 1) < 0) return abortSim();

    // loop all the input-ports
    while (port != NULL)
    {
        // null-pointer check
        if (port->handle == NULL)
        {
            vpi_printf("Handle for output port is null \n");
            return abortSim();
        }
        else
        {
            // memory is already allocated, so null-pointer should be impossible
            if (port->vector == NULL)
            {
                vpi_printf("Input vector is null \n");
                return abortSim();
            }
            else
            {   
                for (i = 0; i<port->width; i++)
                {
                    // read a value from CLaSH
                    if (readMessage(state->strR, state->comm, 1) < 0) return abortSim();
                    
                    // set value in the vector
                    port->vector[i].aval = atoi(state->strR);
                    port->vector[i].bval = 0; 
                }
                
                // write vector into simulator
                state->vector.value.vector = port->vector;
                vpi_put_value(port->handle, &state->vector, &state->time, vpiInertialDelay); 
            }
        }
        port = port->next;
    }
    
    registerCB(state, registerRD, cbAfterDelay, state->period-1);
    
    return 0;
}
/**********************************************
 ** ReadOnly Sync CallBack
 *********************************************/
PLI_INT32 readOnlySynch(p_cb_data cb_data)
{
    // variables
    struct vpiState *state;
    struct port *port;
    int i, val;  
    
    // init
    state = (struct vpiState*) cb_data->user_data;
    port = state->outputPorts;
    
    // read 'output', part of protocol
    if (readMessage(state->strR, state->comm, 0) < 0) return abortSim();
    
    // loop all output-ports
    while (port != NULL)
    {
        // null-pointer check
        if (port->handle == NULL)
        {
            vpi_printf("Handle for output port is null \n");
            return abortSim();
        }
        else
        {
            // get vector from simulator
            vpi_get_value(port->handle, &state->vector);
            
            // null-pointer check
            if (state->vector.value.vector == NULL)
            {
                vpi_printf("Output vector is null \n");
                return abortSim();
            }
            else
            {
                // loop all the vector-parts
                for (i = 0; i<port->width; i++)
                {
                    // convert vector[i] to '0' or '1'
                    val = state->vector.value.vector[i].aval;
                    val &= ~state->vector.value.vector[i].bval;
                    
                    // send value to CLaSH
                    sprintf(state->strR,"%d",val);
                    if (writeMessage(state->strR, state->comm, 1) < 0) return abortSim();
                }
            }
        }
        port = port->next;
    }
    
    // send accept, part of protocol
    if (sendAccept(state->comm) < 0) return abortSim();
    
    registerCB(state, synchStep, cbAfterDelay, 1);
    
    return 0;
}

PLI_INT32 registerRD(p_cb_data cb_data)
{
    // variables
    struct vpiState *state;

    // register cb
    state = (struct vpiState*) cb_data->user_data;
    registerCB(state, readOnlySynch, cbReadOnlySynch, 0);
    return 0;
}

/**********************************************
 ** Next Sim Step
 *********************************************/
PLI_INT32 synchStep(p_cb_data cb_data)
{
    // variables
    struct vpiState *state;
    
    // init
    state = (struct vpiState*) cb_data->user_data;
    
    // if CLaSH send 'finish', finish simulation, else register new cbs
    if (readMessage(state->strR, state->comm, 1) < 0) return abortSim();
    if (strcmp("finish", state->strR) == 0) vpi_control(vpiFinish, 0); 
    else
    {
        // register next event
        if (state->reset-- > 0)
            registerCB(state, registerRD, cbAfterDelay, state->period-1);
        else
            registerCB(state, readWriteSynch, cbReadWriteSynch, 0);
    }
    
    return 0;
}

/**********************************************
 ** EndOfSim CallBack
 *********************************************/
PLI_INT32 endOfSim(p_cb_data cb_data)
{
    // variables
    struct vpiState *state;
    
    // dispose state
    state = (struct vpiState*) cb_data->user_data;
    disposeState(&state);
    
    return 0;
}

/**********************************************
 ** StartOfSim CallBack
 *********************************************/
PLI_INT32 startOfSim(p_cb_data __attribute__((__unused__)) cb_data)
{
    // variables
    p_cb_data cb_data_p;
    struct vpiState *state;

    // init
    cb_data_p = malloc(sizeof(struct t_cb_data));
    state = createState();
    
    if (getSharedComm(&state->comm) < 0) return abortSim();
    
    // read seq settings
    if (exchangeSeq(state) < 0) return abortSimM(state);
    
    // get port specifications
    if (getModuleSpecs(NULL, state) < 0) return abortSimM(state);
    
    // exchange ports info
	if (exchangePortSpecs(state, 1) < 0) return abortSimM(state);
	if (exchangePortSpecs(state, 0) < 0) return abortSimM(state);

    // register end-of-sim 
    registerCB(state, endOfSim, cbEndOfSimulation, -1);
    
    // register event for first simulation step
    cb_data_p->user_data = (PLI_BYTE8 *) state;
    synchStep(cb_data_p);
    
    // free cb-data
    free(cb_data_p);
    
    return 0;
}
