#include <stdlib.h>
#include <stdbool.h>

#include "vpi_user.h"

char some_strings[20][70] =
  { "", "00", "aeajhbxpq", "@#$%^&*()_", "r561`123-imvnzf.nkpygaifd"
  , "001384", "argument", "-/.,<>?[]|", "0123456789", "AOQABQILSAOTYH"
  , "adf", "A", "-", "Hi", "Hello", "World", "zzz", "8", "=;+~+;="
  , "1234567890qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM"
  };

p_vpi_value stored_value = NULL;
PLI_INT32 stored_len = -1;
PLI_INT32 object_size = -1;

PLI_UINT32 iterator_ref;
PLI_UINT32 iterator_module_ref;
PLI_UINT32 iterator_port_ref;
PLI_UINT32 iterator_parameter_ref;
PLI_UINT32 iterator_net_ref;
PLI_UINT32 iterator_reg_ref;
PLI_UINT32 callback_ref;
PLI_UINT32 module_ref;
PLI_UINT32 net_ref;
PLI_UINT32 net_bit_0_ref;
PLI_UINT32 net_bit_1_ref;
PLI_UINT32 parameter_ref;
PLI_UINT32 port_ref;
PLI_UINT32 reg_ref;
PLI_UINT32 reg_bit_0_0_ref;
PLI_UINT32 reg_bit_0_1_ref;
PLI_UINT32 reg_bit_1_0_ref;
PLI_UINT32 reg_bit_1_1_ref;
PLI_UINT32 special_ref;
PLI_UINT32 object_ref;

void enforce_size(PLI_INT32 size)
{
  if (size >= 0)
    object_size = size;
}

bool is_iterator_ref(vpiHandle ref)
{
  return
       ref == &iterator_ref
    || ref == &iterator_module_ref
    || ref == &iterator_port_ref
    || ref == &iterator_parameter_ref
    || ref == &iterator_net_ref
    || ref == &iterator_reg_ref;
}
