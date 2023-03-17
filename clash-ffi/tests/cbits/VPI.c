#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "vpi_user.h"

#include "Test.h"
#include "Pipe.h"
#include "Print.h"

PLI_INT32 vpi_control(PLI_INT32 operation, ...)
{
  va_list ap;

  if (pipe_closed())
    return false;

  switch (operation)
    {
    case vpiStop:
      {
        va_start(ap, operation);
        int diagnosticLevel = va_arg(ap, int);
        va_end(ap);

        send("Stop ");
        print_diagnostic_level(diagnosticLevel);

        break;
      }
    case vpiFinish:
      {
        va_start(ap, operation);
        int diagnosticLevel = va_arg(ap, int);
        va_end(ap);

        send("Finish ");
        print_diagnostic_level(diagnosticLevel);

        break;
      }
    case vpiReset:
      {
        va_start(ap, operation);
        int stopValue = va_arg(ap, int);
        int returnValue = va_arg(ap, int);
        int diagnosticLevel = va_arg(ap, int);
        va_end(ap);

        send("Reset ");

        switch (stopValue)
          {
          case 0: send("Interactive");        break;
          case 1: send("Processing");         break;
          default: send("UNKNOWN StopValue"); break;
          }

        send(" ");
        if (returnValue == 0)
          send("Nothing");
        else if (returnValue > 0)
          send("(Just %d)", returnValue);
        else
          send("(Just (%d))", returnValue);

        send(" ");
        print_diagnostic_level(diagnosticLevel);

        break;
      }
    default:
      {
        send("UNKNOWN vpi_control operation");

        break;
      }
    }
  commit_value();

  return true;
}

PLI_INT32 vpi_printf(PLI_BYTE8 *format, ...)
{
  va_list ap;
  int result;
  char buf[4096] = {0,};

  if (pipe_closed())
    return -1;

  va_start(ap, format);
  vsprintf(buf, format, ap);
  va_end(ap);

  print_bytes(buf);
  commit_value();

  return result;
}

PLI_INT32 vpi_flush(void)
{
  if (pipe_closed())
    return 1;

  send("()");
  commit_value();

  return 0;
}

static PLI_INT32 vpi_chk_error_call = 0;

PLI_INT32 vpi_chk_error(p_vpi_error_info error_info_p) {
  int errorLevel = 0;
  PLI_INT32 call = 0;

  if (pipe_closed())
    return vpiInternal;

  call = vpi_chk_error_call++;

  // generate some error instance
  if (error_info_p != NULL)
    {
      error_info_p->state = (call % 3) + 1;
      error_info_p->level = ((call / 3) % 6);
      error_info_p->message = some_strings[call % 20];
      error_info_p->product = some_strings[call % 5];
      error_info_p->code = some_strings[call % 12];
      error_info_p->file = some_strings[call % 14];
      error_info_p->line = call / 18;

      send("ErrorInfo {");

      send("errorState = ");
      switch (error_info_p->state)
        {
        case vpiCompile: send("CompileError");       break;
        case vpiPLI:     send("PliError");           break;
        case vpiRun:     send("RunError");           break;
        default:         send("UNKNOWN ErrorState"); break;
        }

      send(", errorLevel = ");
      errorLevel = error_info_p->level;
    }
  else
    errorLevel = vpi_chk_error_call % 6;

  switch (errorLevel)
    {
    case 0:           send("Success");            break;
    case vpiNotice:   send("Notice");             break;
    case vpiWarning:  send("Warning");            break;
    case vpiError:    send("Error");              break;
    case vpiSystem:   send("System");             break;
    case vpiInternal: send("Internal");           break;
    default:          send("UNKNOWN ErrorLevel"); break;
    }

  if (error_info_p != NULL)
    {
      send(", errorMessage = ");
      print_bytes(error_info_p->message);
      send(", errorProduct = ");
      print_bytes(error_info_p->product);
      send(", errorCode = ");
      print_bytes(error_info_p->code);
      send(", errorFile = ");
      print_bytes(error_info_p->file);
      send(", errorLine = ");
      send("%d", error_info_p->line);
      send("}");
    }
  commit_value();

  return errorLevel;
}

static PLI_INT32 vpi_get_vlog_info_call = 0;

PLI_INT32 vpi_get_vlog_info(p_vpi_vlog_info vlog_info_p)
{
  PLI_INT32 call = 0;

  if (pipe_closed() || vlog_info_p == NULL)
    return false;

  call = vpi_get_vlog_info_call++;

  // generate some info instance
  vlog_info_p->argc = call % NUM_STRINGS;
  vlog_info_p->argv = (PLI_BYTE8**) malloc(sizeof(PLI_BYTE8*) * vlog_info_p->argc);
  for (int i = 0; i < vlog_info_p->argc; i++)
    vlog_info_p->argv[i] = some_strings[i];
  if (vlog_info_p->argc > 1)
    {
      vlog_info_p->argv[1] = (PLI_BYTE8*) malloc(sizeof(PLI_BYTE8) * 3);
      vlog_info_p->argv[1][0] = 48 + (call % 100) / 10;
      vlog_info_p->argv[1][1] = 48 + (call % 10);
      vlog_info_p->argv[1][2] = some_strings[0][0];
    }
  vlog_info_p->product = some_strings[call % 10];
  vlog_info_p->version = some_strings[call % 4];

  send("Info {");
  send("infoArgs = [");
  for (int i = 0; i < vlog_info_p->argc; i++)
    {
      if (i > 0)
        send(",");

      print_bytes(vlog_info_p->argv[i]);
    }
  send("], infoProduct = ");
  print_bytes(vlog_info_p->product);
  send(", infoVersion = ");
  print_bytes(vlog_info_p->version);
  send("}");
  commit_value();

  return true;
}

static PLI_INT32 vpi_get_call = 0;

PLI_INT32 vpi_get(PLI_INT32 property, vpiHandle object)
{
  PLI_INT32 result = -1;
  PLI_INT32 call = 0;

  if (pipe_closed())
    return result;

  call = vpi_get_call++;

  print_property(property);
  commit_value();

  print_object_ref(object);
  commit_value();

  switch (property)
    {
    case vpiType:
      {
        if      (object == NULL)             result = 0;
        else if (is_iterator_ref(object))    result = vpiIterator;
        else if (object == &callback_ref)    result = vpiCallback;
        else if (object == &module_ref)      result = vpiModule;
        else if (object == &special_ref)     result = vpiModule;
        else if (object == &net_ref)         result = vpiNet;
        else if (object == &net_bit_0_ref)   result = vpiNetBit;
        else if (object == &net_bit_1_ref)   result = vpiNetBit;
        else if (object == &parameter_ref)   result = vpiParameter;
        else if (object == &port_ref)        result = vpiPort;
        else if (object == &reg_ref)         result = vpiReg;
        else if (object == &reg_bit_0_0_ref) result = vpiRegBit;
        else if (object == &reg_bit_0_1_ref) result = vpiRegBit;
        else if (object == &reg_bit_1_0_ref) result = vpiRegBit;
        else if (object == &reg_bit_1_1_ref) result = vpiRegBit;

        if (result > 0)
          {
            if (object == &special_ref)
              send("%d", result);
            else
              print_object_type(result);
          }

        break;
      }
    case vpiSize:
      {
        if (object_size >= 0)
          {
            result = object_size;
            object_size = -1;
          }
        else
          result = (call % 21) + 1;

        send("%d", result);
        break;
      }
    case vpiLineNo:
      {
        result = (call % 13) + 1;
        send("%d", result);
        break;
      }
    case vpiDirection:
      {
        result = (call % 5) + 1;

        if (object == &special_ref)
          send("%d", result);
        else
          {
            switch (result)
              {
              case vpiInput:       send("Input");             break;
              case vpiOutput:      send("Output");            break;
              case vpiInout:       send("InOut");             break;
              case vpiMixedIO:     send("MixedIO");           break;
              case vpiNoDirection: send("NoDirection");       break;
              default:             send("UNKNOWN Direction"); break;
              }
          }

        break;
      }
    case vpiNetType:
      {
        result = (call % 13) + 1;

        if (object == &special_ref)
          send("%d", result);
        else
          {
            switch (result)
              {
              case vpiWire:    send("Wire");            break;
              case vpiWand:    send("Wand");            break;
              case vpiWor:     send("Wor");             break;
              case vpiTri:     send("Tri");             break;
              case vpiTri0:    send("Tri0");            break;
              case vpiTri1:    send("Tri1");            break;
              case vpiTriReg:  send("TriReg");          break;
              case vpiTriAnd:  send("TriAnd");          break;
              case vpiTriOr:   send("TriOr");           break;
              case vpiSupply1: send("Supply1");         break;
              case vpiSupply0: send("Supply0");         break;
              case vpiNone:    send("None");            break;
              case vpiUwire:   send("Uwire");           break;
              default:         send("UNKNOWN NetType"); break;
              }
          }

        break;
      }
    case vpiPortIndex:
      {
        result = call % 11;
        send("%d", result);
        break;
      }
    case vpiScalar:
    case vpiVector:
    case vpiSigned:
    case vpiLocalParam:
      {
        result = call % 2;

        if (result)
          send("True");
        else
          send("False");
      }
    default: break;
    }
  commit_value();

  return result;
}

vpiHandle vpi_iterate(PLI_INT32 type, vpiHandle refHandle)
{
  if (pipe_closed())
    return NULL;

  PLI_INT32* ref = NULL;

  switch (type)
    {
    case vpiModule:    ref = &iterator_module_ref;    break;
    case vpiNet:       ref = &iterator_net_ref;       break;
    case vpiParameter: ref = &iterator_parameter_ref; break;
    case vpiPort:      ref = &iterator_port_ref;      break;
    case vpiReg:       ref = &iterator_reg_ref;       break;
    default:           ref = &iterator_ref;           break;
    }

  print_object_type(type);
  commit_value();

  print_mobject(refHandle);
  commit_value();

  print_object_ref(ref);
  commit_value();

  return ref;
}

static PLI_INT32 vpi_scan_call = 0;

vpiHandle vpi_scan(vpiHandle iterator)
{
  PLI_INT32 call = 0;

  if (pipe_closed() || !is_iterator_ref(iterator))
    return NULL;

  call = vpi_scan_call++;

  PLI_UINT32* ref;

  if ((call % 5) == 0)                          ref = NULL;
  else if (iterator == &iterator_module_ref)    ref = &module_ref;
  else if (iterator == &iterator_port_ref)      ref = &port_ref;
  else if (iterator == &iterator_parameter_ref) ref = &parameter_ref;
  else if (iterator == &iterator_net_ref)       ref = &net_ref;
  else if (iterator == &iterator_reg_ref)       ref = &reg_ref;
  else                                          ref = &object_ref;

  print_mobject(ref);
  commit_value();

  return ref;
}

static PLI_INT32 vpi_get_time_call = 0;

void vpi_get_time(vpiHandle object, p_vpi_time time_p)
{
  PLI_INT32 call = 0;

  if (pipe_closed() || time_p == NULL)
    return;

  call = vpi_get_time_call++;

  switch (time_p->type)
    {
    case vpiScaledRealTime:
      {
        time_p->high = 0;
        time_p->low  = 0;
        time_p->real = ((double) call) * 0.32;
        break;
      }
    case vpiSimTime:
      {
        time_p->high = call / 10;
        time_p->low  = call % 10;
        time_p->real = 0.0;
        break;
      }
    default: break;
    }

  print_time_type(time_p->type);
  commit_value();

  print_mobject(object);
  commit_value();

  print_time(time_p);
  commit_value();
}

static PLI_INT32 vpi_get_str_call = 0;

PLI_BYTE8* vpi_get_str(PLI_INT32 property, vpiHandle object)
{
  PLI_BYTE8* result = NULL;
  PLI_INT32 call = 0;

  if (pipe_closed())
    return result;

  call = vpi_get_str_call++;

  print_property(property);
  commit_value();

  print_object_ref(object);
  commit_value();

  switch (property)
    {
    case vpiFullName: result = some_strings[call % 7];  break;
    case vpiName:     result = some_strings[call % 19]; break;
    case vpiFile:     result = some_strings[call % 13]; break;
    default:                                            break;
    }

  if (result != NULL)
    print_bytes(result);
  commit_value();

  return result;
}

static PLI_INT32 vpi_get_value_call = 0;

void vpi_get_value(vpiHandle expr, p_vpi_value value_p)
{
  PLI_INT32 len = -1;
  PLI_INT32 call = 0;

  if (pipe_closed() || expr == NULL || value_p == NULL)
    return;

  call = vpi_get_value_call++;

  if (stored_value != NULL && stored_value->format == value_p->format)
    {
      len = stored_len;
      memcpy(value_p, stored_value, sizeof(s_vpi_value));
      object_size = len;
    }
  else
    {
      switch (value_p->format)
        {
        case vpiBinStrVal:
          {
            len = call % 5;
            object_size = len;
            value_p->value.str = (PLI_BYTE8*) malloc(len * sizeof(PLI_BYTE8));

            for (int i = 0; i < len; i++)
              {
                switch (call % 4)
                  {
                  case 0:  value_p->value.str[i] = '0'; break;
                  case 1:  value_p->value.str[i] = '1'; break;
                  case 2:  value_p->value.str[i] = 'x'; break;
                  default: value_p->value.str[i] = 'z'; break;
                  }
                call >>= 1;
              }

            break;
          }
        case vpiOctStrVal:
          {
            len = call % 3;
            object_size = len * 3;
            value_p->value.str = (PLI_BYTE8*) malloc(len * sizeof(PLI_BYTE8));

            PLI_BYTE8 choice = (PLI_BYTE8) (call % 10);

            for (int i = 0; i < len; i++)
              {
                if (choice >= 0 && choice < 8)
                  value_p->value.str[i] = '0' + choice;
                else
                  value_p->value.str[i] = choice == 8 ? 'x' : 'z';

                // Note: 'X' or 'Z' values are not generated, as they
                // are not supported by clash-ffi.
                call >>= 1;
              }

            break;
          }
        case vpiDecStrVal:
          {
            len = call % 3;
            int upperBound = 1;

            for (int i = 0; i < len; i++)
              upperBound *= 10;
            upperBound -= 1;
            int bits = 0;
            while (upperBound)
              {
                bits++;
                upperBound >>= 1;
              }

            object_size = bits;
            value_p->value.str = (PLI_BYTE8*) malloc(len + 1 * sizeof(PLI_BYTE8));

            PLI_BYTE8 choice = (PLI_BYTE8) (call % 10);

            for (int i = 0; i < len; i++)
              {
                value_p->value.str[i] = '0' + choice;
                call >>= 1;
              }

            // We NUL terminate strings, if encoded as decimals. Note
            // that decimal encodings and binary encodings do not
            // align properly. For example, the value '9' needs 4 bits
            // to be represented in binary, but only 1 character in
            // the decimal encoding. Nevertheless, 4 bit sized values
            // may also require 2 character in decimal
            // encoding. Hence, the reported bit size does not always
            // uniquely determine the number of transmitted
            // characters.
            //
            // We avoid this situation in the other cases by always
            // generating the maximal amount of characters. However,
            // we chose differently here, for testing that this
            // alternative works as well.
            value_p->value.str[len] = 0;

            break;
          }
        case vpiHexStrVal:
          {
            len = call % 3;
            enforce_size(len * 4);
            value_p->value.str = (PLI_BYTE8*) malloc(len * sizeof(PLI_BYTE8));

            PLI_BYTE8 choice = (PLI_BYTE8) (call % 18);

            for (int i = 0; i < len; i++)
              {
                if (choice >= 0 && choice < 10)
                  value_p->value.str[i] = '0' + choice;
               else if (choice >= 10 && choice < 16)
                 value_p->value.str[i] = 'a' + choice - 10;
               else
                 value_p->value.str[i] = choice == 16 ? 'x' : 'z';

               // Note: 'X' or 'Z' values are not generated, as they
               // are not supported by clash-ffi.
               call >>= 1;
             }

           break;
         }
       case vpiScalarVal:
         {
           enforce_size(1);

           switch (call % 7)
             {
             case 0:  value_p->value.scalar = vpi0;        break;
             case 1:  value_p->value.scalar = vpi1;        break;
             case 2:  value_p->value.scalar = vpiZ;        break;
             case 3:  value_p->value.scalar = vpiX;        break;
             case 4:  value_p->value.scalar = vpiH;        break;
                        case 5:  value_p->value.scalar = vpiL;        break;
            default: value_p->value.scalar = vpiDontCare; break;
            }

          break;
        }
      case vpiIntVal:
        {
          enforce_size(8 * sizeof(PLI_INT32));

          bool sign = call % 2;
          call >>= 1;
          value_p->value.integer = call;
          if (sign)
            value_p->value.integer *= -1;
          break;
        }
      case vpiRealVal:
        {
          enforce_size(8 * sizeof(double));

          value_p->value.real = 0.29 * ((double) (call % 1000));
          break;
        }
      case vpiStringVal:
        {
          len = call % 3;
          value_p->value.str = (PLI_BYTE8*) malloc(1 + len * sizeof(PLI_BYTE8));

          for (int i = 0; i < len; i++)
            {
              value_p->value.str[i] = (PLI_BYTE8) (call % 256);
              call >>= 1;
            }

          value_p->value.str[len] = 0;
          break;
        }
      case vpiVectorVal:
        {
          len = call % 45;
          int size = 1 + (len - 1 / 32);
          enforce_size(len);

          p_vpi_vecval vec = (p_vpi_vecval) malloc(size * sizeof(s_vpi_vecval));
          vec->aval = 0;
          vec->bval = 0;

          for (int i = 0; i <= (len % 32); i++)
            for (int j = 0; j < size; j++)
              {
                switch (call % 4)
                  {
                  case 2:                                       break;
                  case 1:   vec[j].aval |= 1;                   break;
                  case 0:                     vec[j].bval |= 1; break;
                  default:  vec[j].aval |= 1; vec[j].bval |= 1; break;
                  }

                if (i < (len % 32) - 1)
                  {
                    vec[j].aval <<= 1;
                    vec[j].bval <<= 1;
                  }

                call >>= 1;
              }

          value_p->value.vector = vec;

          break;
        }
      case vpiTimeVal:
        {
          p_vpi_time time = (p_vpi_time) malloc(sizeof(s_vpi_time));

          time->type = call % 2 ? vpiScaledRealTime : vpiSimTime;
          call >>= 1;

          switch (time->type)
            {
            case vpiScaledRealTime:
              {
                time->high = 0;
                time->low  = 0;
                time->real = ((double) call) * 0.32;
                break;
              }
            case vpiSimTime:
              {
                time->high = call / 10;
                time->low  = call % 10;
                time->real = 0.0;
                break;
              }
            default: break;
            }

          value_p->value.time = time;

          break;
        }
      case vpiObjTypeVal:       //
      case vpiStrengthVal:      //
      case vpiShortIntVal:      //
      case vpiLongIntVal:       //
      case vpiShortRealVal:     //
      case vpiRawTwoStateVal:   //
      case vpiRawFourStateVal:  // currently not supported by clash-ffi
      case vpiSuppressVal:
      default: break;
      }
   }

  stored_len = -1;
  stored_value = NULL;

  print_value_format(value_p->format);
  commit_value();

  print_object_ref(expr);
  commit_value();

  print_value(value_p, len);
  commit_value();
}

vpiHandle vpi_put_value(vpiHandle object, p_vpi_value value_p,
                        p_vpi_time time_p, PLI_INT32 flags)
{
  if (pipe_closed() || object == NULL || value_p == NULL)
    return NULL;

  print_object_ref(object);
  commit_value();

  // Handling fixed sizes for data driven objects would introduce a
  // lot of some overhead, which is out of scope for this testing
  // interface. Instead, we require the size of values to be set
  // dynamically before each 'vpi_put_value' call using the
  // 'enforce_size' method. This is more flexible, since sizes can be
  // adjusted for each test case individually.
  PLI_INT32 len = object_size;
  object_size = -1;
  print_value(value_p, len);
  commit_value();

  switch (flags)
    {
    case vpiNoDelay:
      {
        send("NoDelay");
        break;
      }
    case vpiInertialDelay:
      {
        send("InertialDelay (");
        print_time(time_p);
        send(")");
        break;
      }
    case vpiTransportDelay:
      {
        send("TransportDelay (");
        print_time(time_p);
        send(")");
        break;
      }
    case vpiPureTransportDelay:
      {
        send("PureTransportDelay (");
        print_time(time_p);
        send(")");
        break;
      }
    case vpiForceFlag:
      {
        send("Force");
        break;
      }
    case vpiReleaseFlag:
      {
        send("Release");
        break;
      }
    default:
      {
        send("UNKNOWN DelayMode");
        break;
      }
    }
  commit_value();

  stored_len = len;
  stored_value = value_p;

  return NULL;
}

PLI_INT32 vpi_free_object(vpiHandle object)
{
  if (pipe_closed())
    return -1;

  print_object_ref(object);
  commit_value();

  return true;
}

PLI_INT32 vpi_compare_objects(vpiHandle object1, vpiHandle object2)
{
  if (pipe_closed())
    return -1;

  print_object_ref(object1);
  commit_value();

  print_object_ref(object2);
  commit_value();

  if (object1 == object2)
    send("True");
  else
    send("False");

  commit_value();

  return object1 == object2;
}

vpiHandle vpi_handle(PLI_INT32 type, vpiHandle refHandle)
{
  vpiHandle ref = NULL;

  if (pipe_closed())
    return NULL;

  switch (type)
    {
    case vpiModule:
      {
        if (refHandle == NULL)
          ref = &module_ref;
        break;
      }
    case vpiNet:
      {
        if (refHandle == &module_ref)
          ref = &net_ref;
        break;
      }
    case vpiParameter:
      {
        if (refHandle == &module_ref)
          ref = &parameter_ref;
        break;
      }
    case vpiPort:
      {
        if (refHandle == &module_ref)
          ref = &port_ref;
        break;
      }
    case vpiReg:
      {
        if (refHandle == &module_ref)
          ref = &reg_ref;
        break;
      }
    default: break;
    }

  print_object_type(type);
  commit_value();

  print_mobject(refHandle);
  commit_value();

  if (ref != NULL)
    {
      print_object_ref(ref);
      commit_value();
    }

  return ref;
}

vpiHandle vpi_handle_by_name(PLI_BYTE8 *name, vpiHandle scope)
{
  vpiHandle ref = NULL;

  if (pipe_closed())
    return NULL;

  else if (strcmp(name, "top")      == 0) ref = &module_ref;
  else if (strcmp(name, "top.net")  == 0) ref = &net_ref;
  else if (strcmp(name, "top.port") == 0) ref = &port_ref;
  else if (strcmp(name, "top.reg")  == 0) ref = &reg_ref;
  else if (strcmp(name, "special")  == 0) ref = &special_ref;
  else if (scope == &module_ref && strcmp(name, "net")  == 0) ref = &net_ref;
  else if (scope == &module_ref && strcmp(name, "port") == 0) ref = &port_ref;
  else if (scope == &module_ref && strcmp(name, "reg")  == 0) ref = &reg_ref;

  print_bytes(name);
  commit_value();

  print_mobject(scope);
  commit_value();

  if (ref != NULL)
    {
      print_object_ref(ref);
      commit_value();
    }

  return ref;
}

vpiHandle vpi_handle_by_index(vpiHandle object, PLI_INT32 indx)
{
  vpiHandle ref = NULL;

  if (pipe_closed())
    return NULL;

  else if (object == &net_ref && indx == 0) ref = &net_bit_0_ref;
  else if (object == &net_ref && indx == 1) ref = &net_bit_1_ref;

  send("%d", indx);
  commit_value();

  print_mobject(object);
  commit_value();

  if (ref != NULL)
    {
      print_object_ref(ref);
      commit_value();
    }

  return ref;
}

vpiHandle vpi_handle_by_multi_index(vpiHandle obj, PLI_INT32 num_index,
                                    PLI_INT32 *index_array)
{
  vpiHandle ref = NULL;

  if (pipe_closed() && (num_index > 0 && index_array == NULL))
    return NULL;

  if (obj == &reg_ref && num_index == 2)
    {
      if      (index_array[0] == 0 && index_array[1] == 0) ref = &reg_bit_0_0_ref;
      else if (index_array[0] == 0 && index_array[1] == 1) ref = &reg_bit_0_1_ref;
      else if (index_array[0] == 1 && index_array[1] == 0) ref = &reg_bit_1_0_ref;
      else if (index_array[0] == 1 && index_array[1] == 1) ref = &reg_bit_1_1_ref;
    }

  send("[");
  for (int i = 0; i < num_index; i++)
    {
      if (i > 0)
        send(",");
      send("%d", index_array[i]);
    }
  send("]");
  commit_value();

  print_mobject(obj);
  commit_value();

  if (ref != NULL)
    {
      print_object_ref(ref);
      commit_value();
    }

  return ref;
}

vpiHandle vpi_register_cb(p_cb_data cb_data_p)
{
  if (pipe_closed() || cb_data_p == NULL)
    return NULL;

  send("CallbackInfo {");
  send("cbReason = ");
  print_callback_reason(cb_data_p);
  send(", ");
  send("cbRoutine = <%d>, ", cb_data_p->cb_rtn(NULL));
  send("cbIndex = %d, ", cb_data_p->index);
  send("cbData = ");
  print_bytes(cb_data_p->user_data);
  send("}");
  commit_value();

  print_object_ref(&callback_ref);
  commit_value();

  return &callback_ref;
}

PLI_INT32 vpi_remove_cb(vpiHandle cb_obj)
{
  if (pipe_closed() || cb_obj == NULL)
    return false;

  print_object_ref(cb_obj);
  commit_value();

  return true;
}
