#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

// hs_init, hs_exit
#include "HsFFI.h"

#if defined(__GLASGOW_HASKELL__)
// RtsConfig, hs_init_ghc
#include "Rts.h"
#endif

#include "vpi_user.h"

// This is exported from Haskell as the "actual" entry point of the program
// that uses VPI. Cocotb does a similar thing, however they can interactively
// load whatever module / function they want using the python API.
extern void clash_ffi_main(void);

typedef PLI_INT32 (*f_cb)(p_cb_data cb_data);

static bool HASKELL_INITIALIZED = false;

void hs_callback(PLI_INT32 reason, f_cb callback) {
  s_cb_data data;

  data.time = NULL;
  data.reason = reason;
  data.cb_rtn = callback;
  data.user_data = NULL;

  vpiHandle handle = vpi_register_cb(&data);
  vpi_free_object(handle);
}

PLI_INT32 init_ghc(p_cb_data data) {
  s_vpi_vlog_info info;

  if(vpi_get_vlog_info(&info)) {
#if defined(__GLASGOW_HASKELL__)
    RtsConfig conf = defaultRtsConfig;
    conf.rts_opts_enabled = RtsOptsAll;

    // Initialize GHC with RTS options so we can accept them in cmdline args.
    hs_init_ghc(&info.argc, &info.argv, conf);
#else
    hs_init(&info.argc, &info.argv);
#endif

    HASKELL_INITIALIZED = true;
    clash_ffi_main();

    return 0;
  }
  else {
    return -1;
  }
}

PLI_INT32 exit_ghc(p_cb_data data) {
  if(HASKELL_INITIALIZED) {
    hs_exit();
  }

  return 0;
}

void register_ghc_callbacks(void) {
  hs_callback(cbStartOfSimulation, init_ghc);
  hs_callback(cbEndOfSimulation, exit_ghc);
}

void (*vlog_startup_routines[])() = {
  register_ghc_callbacks, 0
};

