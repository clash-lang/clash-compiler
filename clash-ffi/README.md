# `clash-ffi` - Clash FFI with Simulator Tools

  * See the LICENSE file for license and copyright details

# Clash FFI

This package provides FFI support for interfacing with simulation tools over
a standard interface (e.g. VPI / VHPI / FLI). Currently only VPI is supported.

All interaction with a simulator follows the same general process:

  * the Haskell code performing the FFI is compiled as a shared library. Any
    libraries specified as `foreign-library` in `clash-ffi.cabal` are copied
    into a `lib/` directory in the project on successful build. Haskell FFI
    code *must* export the entry point

    ```c
    void clash_ffi_main(void);
    ```

    This requires a foreign export in user code, i.e.

    ```haskell
    foreign export ccall "clash_ffi_main"
      ffiMain :: IO ()

    ffiMain :: IO ()
    ffiMain = -- Some FFI code
    ```

    This main action is run during the start-of-simulation callback from VPI.
    This means while it can register new callbacks, it should not run forever
    as doing so would mean control is never returned to the simulator.

  * The simulator is started with flags which load the library. For instance,
    with `iverilog` the simulator is invoked with a command similar to

    ```bash
    vvp -L lib -l libclashffi-iverilog-vpi MODULE.vvp
    ```

## Supported API Functions

### VPI

General Functions

| VPI Definition            | Supported | Haskell API Function(s)               |
| :---                      | :---:     | :---                                  |
| vpi_chk_error             | YES       | simulationError, simulationErrorLevel |
| vpi_compare_objects       | YES       | compareHandles                        |
| vpi_control               | YES       | controlSimulator                      |
| vpi_flush                 | YES       | simFlushIO                            |
| vpi_get                   | YES       | getProperty                           |
| vpi_get_cb_info           | YES       | callbackInfo                          |
| vpi_get_data              | NO        |                                       |
| vpi_get_delays            | NO        |                                       |
| vpi_get_str               | YES       | getProperty                           |
| vpi_get_systf_info        | NO        |                                       |
| vpi_get_time              | YES       | simulationTime                        |
| vpi_get_userdata          | NO        |                                       |
| vpi_get_value             | YES       | receiveValue, unsafeReceiveValue      |
| vpi_get_vlog_info         | YES       | simulatorInfo                         |
| vpi_handle                | YES       | childHandle                           |
| vpi_handle_by_index       | YES       | childHandle                           |
| vpi_handle_by_multi_index | YES       | childHandle                           |
| vpi_handle_by_name        | YES       | childHandle                           |
| vpi_handle_multi          | NO        |                                       |
| vpi_iterate               | YES       | iterate, iterateAll                   |
| vpi_mcd_close             | NO        |                                       |
| vpi_mcd_flush             | NO        |                                       |
| vpi_mcd_name              | NO        |                                       |
| vpi_mcd_open              | NO        |                                       |
| vpi_mcd_printf            | NO        |                                       |
| vpi_mcd_vprintf           | NO        |                                       |
| vpi_printf                | YES       | simPutStr, simPutStrLn                |
| vpi_put_data              | NO        |                                       |
| vpi_put_delays            | NO        |                                       |
| vpi_put_userdata          | NO        |                                       |
| vpi_put_value             | YES       | sendValue, unsafeSendValue            |
| vpi_register_cb           | YES       | registerCallback                      |
| vpi_register_systf        | NO        |                                       |
| vpi_remove_cb             | YES       | removeCallback                        |
| vpi_scan                  | YES       | scan                                  |
| vpi_vprintf               | NO        |                                       |

Specific to IEEE 1364

| VPI Definition            | Supported | Haskell API Function(s)               |
| :---                      | :---:     | :---                                  |
| vpi_free_object           | YES       | freeHandle                            |

Specific to IEEE 1800

| VPI Definition            | Supported | Haskell API Function(s)               |
| :---                      | :---:     | :---                                  |
| vpi_get64                 | YES       | getProperty                           |
| vpi_release_handle        | YES       | freeHandle                            |

### VHPI

VHPI is not supported at this time.

### FLI

FLI is not supported at this time.
