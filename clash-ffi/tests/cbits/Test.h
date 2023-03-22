#ifndef TEST_H
#define TEST_H

#include <stdbool.h>

#include "vpi_user.h"

/* Some Selection of Strings */
#define NUM_STRINGS 20
#define MAX_STR_LEN 70
extern char some_strings[NUM_STRINGS][MAX_STR_LEN];

/* Shared Values */
extern p_vpi_value stored_value;
extern PLI_INT32 stored_len;
extern PLI_INT32 object_size;

/* Unique Object References */
extern PLI_UINT32 iterator_ref;
extern PLI_UINT32 iterator_module_ref;
extern PLI_UINT32 iterator_port_ref;
extern PLI_UINT32 iterator_parameter_ref;
extern PLI_UINT32 iterator_net_ref;
extern PLI_UINT32 iterator_reg_ref;
extern PLI_UINT32 callback_ref;
extern PLI_UINT32 module_ref;
extern PLI_UINT32 net_ref;
extern PLI_UINT32 net_bit_0_ref;
extern PLI_UINT32 net_bit_1_ref;
extern PLI_UINT32 parameter_ref;
extern PLI_UINT32 port_ref;
extern PLI_UINT32 reg_ref;
extern PLI_UINT32 reg_bit_0_0_ref;
extern PLI_UINT32 reg_bit_0_1_ref;
extern PLI_UINT32 reg_bit_1_0_ref;
extern PLI_UINT32 reg_bit_1_1_ref;
extern PLI_UINT32 special_ref;
extern PLI_UINT32 object_ref;

/* Passes some bit size value to the C interface. This side channel is
 * used to dynamically adjust value sizes to match values that are
 * generated at the Haskell side. This avoids the necessity of
 * creating and managing objects that match the different
 * sizes. Instead, we can generate the objects independently of the
 * generated values and adapt their value size dynamically.
 */
void enforce_size(PLI_INT32);

/* Checks whether the given reference is one of the known iterator
 *references.
 */
bool is_iterator_ref(vpiHandle);

#endif
