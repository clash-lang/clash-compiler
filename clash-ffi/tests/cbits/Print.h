#ifndef PRINT_H
#define PRINT_H

#include "vpi_user.h"

/* Prints a NUL terminated sequence of bytes to the shared pipe
 * according to the Haskell representation of:
 *
 *  'Show [Int]'
 */
void print_bytes(PLI_BYTE8*);

/* Prints the diagnostic level to the shared pipe according to the
 * Haskell representation of:
 *
 *  'Show Clash.FFI.VPI.Error.Level.ErrorLevel'
 */
void print_diagnostic_level(PLI_INT32);

/* Prints an VPI property to the shared pipe according to the Haskell
 * representation of:
 *
 *   'Show Clash.FFI.VPI.Object.Property.Property'
 */
void print_property(PLI_INT32);

/* Prints a time type to the shared pipe according to the Haskell
 * representation of:
 *
 *   'Show Clash.FFI.VPI.Object.Time.TimeType'
 */
void print_time_type(PLI_INT32);


/* Prints the type of an object to the shared pipe according to the
 * Haskell representation of:
 *
 *   'Show Clash.FFI.VPI.Object.Type.ObjectType'
 */
void print_object_type(PLI_INT32);

/* Prints some value format to the shared pipe according to the
 * Haskell representation of:
 *
 *   'Show Clash.FFI.VPI.Object.Value.Format.ValueFormat'
 */
void print_value_format(PLI_INT32);

/* Prints an object referene to the shared pipe according to the
 * Haskell representations of:
 *
 *   'Show Clash.FFI.VPI.Callback.Callback'
 *   'Show Clash.FFI.VPI.Iterator.Iterator'
 *   'Show Clash.FFI.VPI.Module.Module'
 *   'Show Clash.FFI.VPI.Net.Net'
 *   'Show Clash.FFI.VPI.Object.Object'
 *   'Show Clash.FFI.VPI.Parameter.Parameter'
 *   'Show Clash.FFI.VPI.Port.Port'
 *   'Show Clash.FFI.VPI.Reg.Reg'
 */
void print_object_ref(vpiHandle);

/* Prints an optional object referene to the shared pipe according to
 * the Haskell representations of:
 *
 *   'Show (Maybe Clash.FFI.VPI.Callback.Callback)'
 *   'Show (Maybe Clash.FFI.VPI.Iterator.Iterator)'
 *   'Show (Maybe Clash.FFI.VPI.Module.Module)'
 *   'Show (Maybe Clash.FFI.VPI.Net.Net)'
 *   'Show (Maybe Clash.FFI.VPI.Object.Object)'
 *   'Show (Maybe Clash.FFI.VPI.Parameter.Parameter)'
 *   'Show (Maybe Clash.FFI.VPI.Port.Port)'
 *   'Show (Maybe Clash.FFI.VPI.Reg.Reg)'
 */
void print_mobject(vpiHandle);

/* Prints a time reference to the shared pipe according to the Haskell
 * representation of:
 *
 *   'Show Clash.FFI.VPI.Object.Time.Time'
 */
void print_time(p_vpi_time);

/* Prints some value to the shared pipe according to the Haskell
 * representation of:
 *
 *   'Show Clash.FFI.VPI.Object.Value.Value'
 *
 * The second argument determines the size of the value.
 */
void print_value(p_vpi_value, int);

/* Prints a callback reason to the shared pipe according to the
 * Haskell representation of:
 *
 *   'Show Clash.FFI.VPI.Callback.Reason.CallbackReason'
 */
void print_callback_reason(p_cb_data);

#endif
