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
#include "cosim_traversing_vpi.h"

int exchangeSeq(struct vpiState *state)
{
    // get period
    if (readMessage(state->strR, state->comm, 1) < 0) return -1;
    state->period = atoi(state->strR);
    if (state->period < 1)
    {
        vpi_printf("(VPI) period too small [%d]\n", state->period);
        return -1;
    }
    
    // get reset
    if (readMessage(state->strR, state->comm, 1) < 0) return -1;
    state->reset = atoi(state->strR);
    
    return 0;
}

/* Exchange port specifications with CLaSH */
int exchangePortSpecs(struct vpiState *state, int isInput)
{
    // variables
    struct port *tmpPort;
    
    // init
    tmpPort = isInput ? state->inputPorts : state->outputPorts;
    
	// read # ports from CLaSH
    if (readMessage(state->strR, state->comm, 0) < 0) return -1;
    
    // send # ports
    sprintf(state->strR,"%d", isInput ? state->noInput : state->noOutput);
    if (writeMessage(state->strR, state->comm, 0) < 0) return -1;
    
    // read 'bits' from CLaSH, part of protocol
    if (readMessage(state->strR, state->comm, 0) < 0) return -1;
    
    // loop all ports
    while (tmpPort != NULL)
    {
        // send # bits
        sprintf(state->strR,"%d",tmpPort->width);
        if (writeMessage(state->strR, state->comm, 1) < 0) return -1;
        
        tmpPort = tmpPort->next;
    }
    
    // send accept, part of protocol
    if (sendAccept(state->comm) < 0) return -1;
    
    // return 
	return 0;
}

int getPortSpecs(vpiHandle handle, struct vpiState *state, char *modName)
{
    // variables
    vpiHandle itr, port_handle;
    struct port *port = NULL, *pInput = NULL, *pOutput = NULL;
    char *tmpName;
    int tmpType;
    int tmpWidth;

    // iterate through the ports
    itr = vpi_iterate(vpiPort, handle);
    if (itr != NULL)
    {
        while ((port_handle = vpi_scan(itr)) != NULL ) 
        {
            // get port specs
            tmpName         = vpi_get_str(vpiName, port_handle);
            tmpType         = vpi_get(vpiDirection, port_handle);
            tmpWidth        = vpi_get(vpiSize, port_handle);
                    
            // switch based on direction (input or output signal)
            switch(tmpType)
            {
                case vpiInput :
                        
                    // malloc new port struct
                    if (state->inputPorts == NULL)
                    {   
                        state->inputPorts   = (struct port *) malloc (sizeof(struct port));
                        pInput              = state->inputPorts;
                    }
                    else 
                    {
                        pInput->next= (struct port *) malloc (sizeof(struct port));
                        pInput      = pInput->next;
                    }
                            
                    // print port
                    //vpi_printf("\tinput\t'%s'\t- %d bits\n", tmpName, tmpWidth); 
                            
                    // set values
                    port            = pInput;
                    port->vector    = (s_vpi_vecval *) malloc (tmpWidth * sizeof(s_vpi_vecval));
                    state->noInput++;
                            
                    break;
                            
                case vpiOutput :
                        
                    // malloc new port struct
                    if (state->outputPorts == NULL)
                    {   
                        state->outputPorts = (struct port *) malloc (sizeof(struct port));
                        pOutput     = state->outputPorts;
                    }
                    else
                    {
                        pOutput->next= (struct port *) malloc (sizeof(struct port));
                        pOutput     = pOutput->next;
                    }
                            
                    // print port
                    //vpi_printf("\toutput\t'%s'\t- %d bits\n", tmpName, tmpWidth); 
                            
                    // set values
                    port            = pOutput;
                    port->vector    = NULL;
                    state->noOutput++;
                            
                    break;
                            
                default :
                    vpi_printf("The port %s has not a supported direction...\n", tmpName); 
                    break;
            }
                    
            // check if input or output port is found
            if (port != NULL)
            {
                // copy specs
                port->next       = NULL;
                port->type       = tmpType;
                port->width      = ((tmpWidth - 1) >> 5) + 1;
                port->name       = (char*) malloc((strlen(tmpName)+1) * sizeof(char));
                strcpy(port->name, tmpName);
                port->fullName = (char*) malloc((strlen(modName)+strlen(tmpName)+2) *sizeof(char));
                sprintf(port->fullName,"%s.%s", modName, tmpName);
                port->handle     = vpi_handle_by_name(port->fullName, NULL);
            }
        }
    }
    else
    {
        // this is the case when vpiPort is not implemented in the simulator
        // or when the module does not have input or outputs
        vpi_printf("(VPI) Unable to get port iterator...\n");
        return -1;
    }
    
    return 0;
}

/* Iterate all top-modules and get the ports */
int getModuleSpecs (vpiHandle handle, struct vpiState *state)
{
    // variables    
    vpiHandle mod_itr, mod_handle;
    char *mod_name = NULL, *tmp;
    int moduleFound = 0;
    
    if (state == NULL)
    {
        vpi_printf("vpiState is null\n");
        return -1;
    }
    
    // iterate through the top modules
    mod_itr = vpi_iterate(vpiModule, handle);
    if (mod_itr != NULL)
    {
        while ( (mod_handle = vpi_scan(mod_itr))  != NULL)
        {
            // get module name
            tmp                 = vpi_get_str(vpiName, mod_handle);
            
            mod_name            = (char*) malloc((strlen(tmp)+1) * sizeof(char)); 
            strcpy(mod_name, tmp);
            //vpi_printf("\nModule found with name: %s\n", tmpModuleName);
    
            // get the ports in this module
            if (getPortSpecs(mod_handle, state, mod_name) < 0) return -1;
            
            // explicitly free the module name
            free(mod_name);
                
            // set flag that module is found
            moduleFound = 1;
        }
    }
    else
    {
        // this is the case when vpiModule is not implemented in the simulator
        // or when the file does not contain a module
        vpi_printf("Unable to get module iterator\n");
        return -1;
    }
    
    if (!moduleFound)
    {
        vpi_printf("No module found\n");
        return -1;
    }
    
    return 0;
}
