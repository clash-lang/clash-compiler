/*C**********************************************************************
* FILENAME :        cosim_control.c       
*
* DESCRIPTION :
*       Control functions needed for VPI
*                needed for co-simulation between CLaSH and a Verilog simulator
*
* 
* AUTHOR  :    John Verheij        DATE :    August 14, 2016
*
*
*C*/

/* Include */
#include "cosim_control_vpi.h"

int abortSim(void)
{
    vpi_printf("Abort...\n");
    vpi_control(vpiFinish, 1);
    return -1;
}

int abortSimM(struct vpiState *state)
{
    writeMessage("-1", state->comm, 0);
    return abortSim();
}

void registerCB(struct vpiState *state, f_cb f, PLI_INT32 reason, PLI_INT32 time)
{
    // variables
    s_cb_data           cb_data_s;
    s_vpi_time          time_s;
    
    // time
    if (time < 0)
    {
        cb_data_s.time  = NULL;
    }
    else
    {
        cb_data_s.time      = &time_s;
        time_s.type         = vpiSimTime;
        time_s.high         = 0;
        time_s.low          = time;
    }
    
    // settings
    cb_data_s.reason        = reason;
    cb_data_s.cb_rtn        = f;
    cb_data_s.user_data     = (PLI_BYTE8 *) state;
    
    // register
    vpi_free_object(vpi_register_cb(&cb_data_s));
}

void registerCallbacks(void)
{
    registerCB(NULL, startOfSim, cbStartOfSimulation, -1);
}

/* array with function-pointers which will be loaded by the simulator */
void (*vlog_startup_routines[])(void) = {
    registerCallbacks,
    0
};