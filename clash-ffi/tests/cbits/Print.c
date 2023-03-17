#include <stdio.h>

#include "Test.h"
#include "Pipe.h"
#include "Print.h"

void print_bytes(PLI_BYTE8 *bytes)
{
  if (pipe_closed())
    return;

  send("[");
  int i = 0;
  while (bytes[i] != 0)
    {
      if (i > 0)
        send(",");
      send("%u", (unsigned char) bytes[i++]);
    }

  send("]");
}

void print_diagnostic_level(PLI_INT32 diagnosticLevel)
{
  if (pipe_closed())
    return;

  switch (diagnosticLevel)
    {
    case 0:  send("NoDiagnostics");           break;
    case 1:  send("TimeAndLocation");         break;
    case 2:  send("TimeLocationAndStats");    break;
    default: send("UNKNOWN DiagnosticLevel"); break;
    }
}

void print_property(PLI_INT32 prop)
{
  if (pipe_closed())
    return;

  switch (prop)
    {
    case vpiUndefined:             send("Undefined");             break;
    case vpiType:                  send("TypeOf");                break;
    case vpiName:                  send("Name");                  break;
    case vpiFullName:              send("FullName");              break;
    case vpiSize:                  send("Size");                  break;
    case vpiFile:                  send("File");                  break;
    case vpiLineNo:                send("LineNo");                break;
    case vpiTopModule:             send("IsTopModule");           break;
    case vpiCellInstance:          send("IsCellInstance");        break;
    case vpiDefName:               send("DefName");               break;
    case vpiProtected:             send("IsProtected");           break;
    case vpiTimeUnit:              send("TimeUnit");              break;
    case vpiTimePrecision:         send("TimePrecision");         break;
    case vpiDefNetType:            send("DefNetType");            break;
    case vpiUnconnDrive:           send("UnconnDrive");           break;
    case vpiDefFile:               send("DefFile");               break;
    case vpiDefLineNo:             send("DefLineNo");             break;
    case vpiScalar:                send("IsScalar");              break;
    case vpiVector:                send("IsVector");              break;
    case vpiExplicitName:          send("ExplicitName");          break;
    case vpiDirection:             send("Direction");             break;
    case vpiConnByName:            send("IsConnByName");          break;
    case vpiNetType:               send("NetType");               break;
    case vpiExplicitScalared:      send("IsExplicitScalared");    break;
    case vpiExplicitVectored:      send("IsExplicitVectored");    break;
    case vpiExpanded:              send("IsExpanded");            break;
    case vpiImplicitDecl:          send("IsImplicitDecl");        break;
    case vpiChargeStrength:        send("ChargeStrength");        break;
    case vpiArray:                 send("IsArray");               break;
    case vpiPortIndex:             send("PortIndex");             break;
    case vpiTermIndex:             send("TermIndex");             break;
    case vpiStrength0:             send("Strength0");             break;
    case vpiStrength1:             send("Strength1");             break;
    case vpiPrimType:              send("PrimType");              break;
    case vpiPolarity:              send("Polarity");              break;
    case vpiDataPolarity:          send("DataPolarity");          break;
    case vpiEdge:                  send("Edge");                  break;
    case vpiPathType:              send("PathType");              break;
    case vpiTchkType:              send("TchkType");              break;
    case vpiOpType:                send("OpType");                break;
    case vpiConstType:             send("ConstType");             break;
    case vpiBlocking:              send("IsBlocking");            break;
    case vpiCaseType:              send("CaseType");              break;
    case vpiNetDeclAssign:         send("IsNetDeclAssign");       break;
    case vpiFuncType:              send("FuncType");              break;
    case vpiUserDefn:              send("IsUserDefn");            break;
    case vpiScheduled:             send("IsScheduled");           break;
    case vpiDefDelayMode:          send("DefDelayMode");          break;
    case vpiDefDecayTime:          send("DefDecayTime");          break;
    case vpiActive:                send("IsActive");              break;
    case vpiAutomatic:             send("IsAutomatic");           break;
    case vpiCell:                  send("Cell");                  break;
    case vpiConfig:                send("Config");                break;
    case vpiConstantSelect:        send("IsConstantSelect");      break;
    case vpiDecompile:             send("Decompile");             break;
    case vpiDefAttribute:          send("DefAttribute");          break;
    case vpiDelayType:             send("DelayType");             break;
    case vpiIteratorType:          send("IteratorType");          break;
    case vpiLibrary:               send("Library");               break;
    case vpiOffset:                send("Offset");                break;
    case vpiResolvedNetType:       send("ResolvedNetType");       break;
    case vpiSaveRestartID:         send("SaveRestartID");         break;
    case vpiSaveRestartLocation:   send("SaveRestartLocation");   break;
    case vpiValid:                 send("IsValid");               break;
    case vpiSigned:                send("IsSigned");              break;
    case vpiLocalParam:            send("IsLocalParam");          break;
    case vpiModPathHasIfNone:      send("ModPathHasIfNone");      break;
    case vpiIndexedPartSelectType: send("IndexedPartSelectType"); break;
    case vpiIsMemory:              send("IsMemory");              break;
    case vpiIsProtected:           send("IsProtected");           break;
    default:                       send("UNKNOWN Property");      break;
    }
}

void print_time_type(PLI_INT32 type)
{
  if (pipe_closed())
    return;

  switch (type)
    {
    case vpiScaledRealTime: send("ScaledReal");       break;
    case vpiSimTime:        send("Sim");              break;
    case vpiSuppressTime:   send("SuppressTime");     break;
    default:                send("UNKNOWN TimeType"); break;
    }
}

void print_object_type(PLI_INT32 type)
{
  if (pipe_closed())
    return;

  switch (type)
    {
    case vpiAlways:            send("ObjAlways");            break;
    case vpiAssignStmt:        send("ObjAssignStmt");        break;
    case vpiAssignment:        send("ObjAssignment");        break;
    case vpiBegin:             send("ObjBegin");             break;
    case vpiCase:              send("ObjCase");              break;
    case vpiCaseItem:          send("ObjCaseItem");          break;
    case vpiConstant:          send("ObjConstant");          break;
    case vpiContAssign:        send("ObjContAssign");        break;
    case vpiDeassign:          send("ObjDeassign");          break;
    case vpiDefParam:          send("ObjDefParam");          break;
    case vpiDelayControl:      send("ObjDelayControl");      break;
    case vpiDisable:           send("ObjDisable");           break;
    case vpiEventControl:      send("ObjEventControl");      break;
    case vpiEventStmt:         send("ObjEventStmt");         break;
    case vpiFor:               send("ObjFor");               break;
    case vpiForce:             send("ObjForce");             break;
    case vpiForever:           send("ObjForever");           break;
    case vpiFork:              send("ObjFork");              break;
    case vpiFuncCall:          send("ObjFuncCall");          break;
    case vpiFunction:          send("ObjFunction");          break;
    case vpiGate:              send("ObjGate");              break;
    case vpiIf:                send("ObjIf");                break;
    case vpiIfElse:            send("ObjIfElse");            break;
    case vpiInitial:           send("ObjInitial");           break;
    case vpiIntegerVar:        send("ObjIntegerVar");        break;
    case vpiInterModPath:      send("ObjInterModPath");      break;
    case vpiIterator:          send("ObjIterator");          break;
    case vpiIODecl:            send("ObjIODecl");            break;
    case vpiMemory:            send("ObjMemory");            break;
    case vpiMemoryWord:        send("ObjMemoryWord");        break;
    case vpiModPath:           send("ObjModPath");           break;
    case vpiModule:            send("ObjModule");            break;
    case vpiNamedBegin:        send("ObjNamedBegin");        break;
    case vpiNamedEvent:        send("ObjNamedEvent");        break;
    case vpiNamedFork:         send("ObjNamedFork");         break;
    case vpiNet:               send("ObjNet");               break;
    case vpiNetBit:            send("ObjNetBit");            break;
    case vpiNullStmt:          send("ObjNullStmt");          break;
    case vpiOperation:         send("ObjOperation");         break;
    case vpiParamAssign:       send("ObjParamAssign");       break;
    case vpiParameter:         send("ObjParameter");         break;
    case vpiPartSelect:        send("ObjPartSelect");        break;
    case vpiPathTerm:          send("ObjPathTerm");          break;
    case vpiPort:              send("ObjPort");              break;
    case vpiPortBit:           send("ObjPortBit");           break;
    case vpiPrimTerm:          send("ObjPrimTerm");          break;
    case vpiRealVar:           send("ObjRealVar");           break;
    case vpiReg:               send("ObjReg");               break;
    case vpiRegBit:            send("ObjRegBit");            break;
    case vpiRelease:           send("ObjRelease");           break;
    case vpiRepeat:            send("ObjRepeat");            break;
    case vpiRepeatControl:     send("ObjRepeatControl");     break;
    case vpiSchedEvent:        send("ObjSchedEvent");        break;
    case vpiSpecParam:         send("ObjSpecParam");         break;
    case vpiSwitch:            send("ObjSwitch");            break;
    case vpiSysFuncCall:       send("ObjSysFuncCall");       break;
    case vpiSysTaskCall:       send("ObjSysTaskCall");       break;
    case vpiTableEntry:        send("ObjTableEntry");        break;
    case vpiTask:              send("ObjTask");              break;
    case vpiTaskCall:          send("ObjTaskCall");          break;
    case vpiTchk:              send("ObjTchk");              break;
    case vpiTchkTerm:          send("ObjTchkTerm");          break;
    case vpiTimeVar:           send("ObjTimeVar");           break;
    case vpiTimeQueue:         send("ObjTimeQueue");         break;
    case vpiUdp:               send("ObjUdp");               break;
    case vpiUdpDefn:           send("ObjUdpDefn");           break;
    case vpiUserSystf:         send("ObjUserSystf");         break;
    case vpiVarSelect:         send("ObjVarSelect");         break;
    case vpiWait:              send("ObjWait");              break;
    case vpiWhile:             send("ObjWhile");             break;
    case vpiAttribute:         send("ObjAttribute");         break;
    case vpiBitSelect:         send("ObjBitSelect");         break;
    case vpiCallback:          send("ObjCallback");          break;
    case vpiDelayTerm:         send("ObjDelayTerm");         break;
    case vpiDelayDevice:       send("ObjDelayDevice");       break;
    case vpiFrame:             send("ObjFrame");             break;
    case vpiGateArray:         send("ObjGateArray");         break;
    case vpiModuleArray:       send("ObjModuleArray");       break;
    case vpiPrimitiveArray:    send("ObjPrimitiveArray");    break;
    case vpiNetArray:          send("ObjNetArray");          break;
    case vpiRange:             send("ObjRange");             break;
    case vpiRegArray:          send("ObjRegArray");          break;
    case vpiSwitchArray:       send("ObjSwitchArray");       break;
    case vpiUdpArray:          send("ObjUdpArray");          break;
    case vpiContAssignBit:     send("ObjContAssignBit");     break;
    case vpiNamedEventArray:   send("ObjNamedEventArray");   break;
    case vpiIndexedPartSelect: send("ObjIndexedPartSelect"); break;
    case vpiGenScopeArray:     send("ObjGenScopeArray");     break;
    case vpiGenScope:          send("ObjGenScope");          break;
    case vpiGenVar:            send("ObjGenVar");            break;
    default:                   send("UNKNOWN ObjectType");   break;
    }
}

void print_value_format(PLI_INT32 format)
{
  if (pipe_closed())
    return;

  switch (format)
    {
    case vpiBinStrVal:       send("BinStrFmt");           break;
    case vpiOctStrVal:       send("OctStrFmt");           break;
    case vpiDecStrVal:       send("DecStrFmt");           break;
    case vpiHexStrVal:       send("HexStrFmt");           break;
    case vpiScalarVal:       send("ScalarFmt");           break;
    case vpiIntVal:          send("IntFmt");              break;
    case vpiRealVal:         send("RealFmt");             break;
    case vpiStringVal:       send("StringFmt");           break;
    case vpiVectorVal:       send("VectorFmt");           break;
    case vpiTimeVal:         send("TimeFmt");             break;
    case vpiObjTypeVal:      send("ObjTypeFmt");          break;
    case vpiStrengthVal:     send("StrengthFmt");         break;
    case vpiShortIntVal:     send("ShortIntFmt");         break;
    case vpiLongIntVal:      send("LongIntFmt");          break;
    case vpiShortRealVal:    send("ShortRealFmt");        break;
    case vpiRawTwoStateVal:  send("RawTwoStateFmt");      break;
    case vpiRawFourStateVal: send("RawFourStateFmt");     break;
    case vpiSuppressVal:     send("SuppressValue");       break;
    default:                 send("UNKNOWN ValueFormat"); break;
    }
}

void print_object_ref(vpiHandle ref)
{
  bool knownType = true;

  if (pipe_closed())
    return;

  else if (is_iterator_ref(ref))  send("Iterator {iteratorObject = ");
  else if (ref == &callback_ref)  send("Callback {callbackObject = ");
  else if (ref == &module_ref)    send("Module {moduleObject = ");
  else if (ref == &special_ref)   send("Module {moduleObject = ");
  else if (ref == &net_ref)       send("Net {netObject = ");
  else if (ref == &parameter_ref) send("Parameter {parameterObject = ");
  else if (ref == &port_ref)      send("Port {portObject = ");
  else if (ref == &reg_ref)       send("Reg {regObject = ");
  else knownType = false;

  if (sizeof(ref) == 8)
    send("Object {objectPtr = 0x%.16" PRIxPTR "}", (uintptr_t) ref);
  else
    send("Object {objectPtr = 0x%.8" PRIxPTR "}", (uintptr_t) ref);

  if (knownType)
    send("}");
}

void print_mobject(vpiHandle object)
{
  if (pipe_closed())
    return;

  if (object == NULL)
    send("Nothing");
  else
    {
      send("Just (");
      print_object_ref(object);
      send(")");
    }
}

void print_time(p_vpi_time time)
{
  if (pipe_closed() || time == NULL)
    return;

  switch (time->type)
    {
    case vpiScaledRealTime:
      {
        send("RealTime %.10f", time->real);
        break;
      }
    case vpiSimTime:
      {
        uint64_t time_v;
        time_v |= (uint64_t) time->high;
        time_v <<= sizeof(time->low) * 8;
        time_v |= (uint64_t) time->low;
        send("SimTime %ld", time_v);
        break;
      }
    default: break;
    }
}

void print_value(p_vpi_value value, int size)
{
  if (pipe_closed() || value == NULL)
    return;

  switch (value->format)
    {
    case vpiObjTypeVal:
    case vpiScalarVal:
      {
        send("BitVal ");

        switch (value->value.scalar)
          {
          case vpi0:
          case vpiL: send("0"); break;
          case vpi1:
          case vpiH: send("1"); break;
          default:   send("."); break;
          }

        break;
      }
    case vpiVectorVal:
      {
        send("BitVectorVal ");

        if (size > 0)
          {
            send("0b");

            while (size--)
              {
                unsigned int sel = 1 << (size % 32);
                unsigned int idx = size / 32;

                if      (sel & value->value.vector[idx].bval) send(".");
                else if (sel & value->value.vector[idx].aval) send("1");
                else                                          send("0");

                if (size > 0 && size % 4 == 0)
                  send("_");
              }
          }
        else
          send("0");

        break;
      }
    case vpiIntVal:
      {
        send("IntVal %d", value->value.integer);
        break;
      }
    case vpiRealVal:
      {
        send("RealVal %.10f", value->value.real);
        break;
      }
    case vpiBinStrVal:
      {
        send("BitVectorVal ");

        if (size > 0)
          {
            send("0b");

            for (int i = 0; i < size; i++)
              {
                switch (value->value.str[i])
                  {
                  case '0': send("0"); break;
                  case '1': send("1"); break;
                  default:  send("."); break;
                  }

                if (size > i + 1 && (size - i - 1) % 4 == 0)
                  send("_");
              }
          }
        else
          send("0");

        break;
      }
    case vpiOctStrVal:
      {
        send("BitVectorVal ");

        if (size > 0)
          {
            send("0b");

            int p = size * 3 - 1;

            for (int i = 0; i < size; i++)
              {
                char buf[4];

                switch (value->value.str[i])
                  {
                  case '0': sprintf(buf, "000"); break;
                  case '1': sprintf(buf, "001"); break;
                  case '2': sprintf(buf, "010"); break;
                  case '3': sprintf(buf, "011"); break;
                  case '4': sprintf(buf, "100"); break;
                  case '5': sprintf(buf, "101"); break;
                  case '6': sprintf(buf, "110"); break;
                  case '7': sprintf(buf, "111"); break;
                  default:  sprintf(buf, "..."); break;
                  }

                send("%c", buf[0]);
                if (p > 0 && (p % 4) == 0) send("_"); p--;
                send("%c", buf[1]);
                if (p > 0 && (p % 4) == 0) send("_"); p--;
                send("%c", buf[2]);
                if (p > 0 && (p % 4) == 0) send("_"); p--;
              }
          }
        else
          send("0");

        break;
      }
    case vpiDecStrVal:
      {
        send("BitVectorVal ");

        if (size > 0)
          {
            send("0b");

            unsigned long int num = 0;
            unsigned long int m = 1;

            for (int i = size - 1; i >= 0; i--)
              {
                num += m * ((unsigned long int) (value->value.str[i] - '0'));
                m *= 10;
              }

            int upperBound = 1;
            for (int i = 0; i < size; i++)
              upperBound *= 10;
            upperBound -= 1;
            int bits = 0;
            while (upperBound)
              {
                bits++;
                upperBound >>= 1;
              }

            for (int i = bits - 1; i >= 0; i--)
              {
                if (num & (1 << i))
                  send("1");
                else
                  send("0");

                if (i > 0 && (i % 4) == 0)
                  send("_");
              }
          }
        else
          send("0");

        break;
      }
    case vpiHexStrVal:
      {
        send("BitVectorVal ");

        if (size > 0)
          {
            send("0b");

            for (int i = 0; i < size; i++)
              {
                switch (value->value.str[i])
                  {
                  case '0': send("0000"); break;
                  case '1': send("0001"); break;
                  case '2': send("0010"); break;
                  case '3': send("0011"); break;
                  case '4': send("0100"); break;
                  case '5': send("0101"); break;
                  case '6': send("0110"); break;
                  case '7': send("0111"); break;
                  case '8': send("1000"); break;
                  case '9': send("1001"); break;
                  case 'a': send("1010"); break;
                  case 'b': send("1011"); break;
                  case 'c': send("1100"); break;
                  case 'd': send("1101"); break;
                  case 'e': send("1110"); break;
                  case 'f': send("1111"); break;
                  default:  send("...."); break;
                  }

                if (size > i + 1)
                  send("_");
              }
          }
        else
          send("0");

        break;
      }
    case vpiStringVal:
      {
        send("StringVal ");
        print_bytes(value->value.str);
        break;
      }
    case vpiTimeVal:
      {
        send("TimeVal ");
        print_time(value->value.time);
        break;
      }
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

void print_callback_reason(p_cb_data cb_data_p)
{
  if (pipe_closed() || cb_data_p == NULL)
    return;

  switch (cb_data_p->reason)
    {
    case cbValueChange:
      {
        send("AfterValueChange (");
        print_object_ref(cb_data_p->obj);
        send(") ");
        print_time_type(cb_data_p->time->type);
        send(" ");
        print_value_format(cb_data_p->value->format);
        break;
      }
    case cbStmt:
      {
        send("BeforeStatement (");
        print_object_ref(cb_data_p->obj);
        send(") ");
        print_time_type(cb_data_p->time->type);
        break;
      }
    case cbForce:
      {
        send("AfterForce ");
        if (cb_data_p->obj != NULL) send("(");
        print_mobject(cb_data_p->obj);
        if (cb_data_p->obj != NULL) send(")");
        send(" ");
        print_time_type(cb_data_p->time->type);
        send(" ");
        print_value_format(cb_data_p->value->format);
        break;
      }
    case cbRelease:
      {
        send("AfterRelease ");
        if (cb_data_p->obj != NULL) send("(");
        print_mobject(cb_data_p->obj);
        if (cb_data_p->obj != NULL) send(")");
        send(" ");
        print_time_type(cb_data_p->time->type);
        send(" ");
        print_value_format(cb_data_p->value->format);
        break;
      }
    case cbAtStartOfSimTime:
      {
        send("AtStartOfSimTime ");
        if (cb_data_p->obj != NULL) send("(");
        print_mobject(cb_data_p->obj);
        if (cb_data_p->obj != NULL) send(")");
        send(" (");
        print_time(cb_data_p->time);
        send(")");
        break;
      }
    case cbReadWriteSynch:
      {
        send("ReadWriteSynch ");
        if (cb_data_p->obj != NULL) send("(");
        print_mobject(cb_data_p->obj);
        if (cb_data_p->obj != NULL) send(")");
        send(" (");
        print_time(cb_data_p->time);
        send(")");
        break;
      }
    case cbReadOnlySynch:
      {
        send("ReadOnlySynch ");
        if (cb_data_p->obj != NULL) send("(");
        print_mobject(cb_data_p->obj);
        if (cb_data_p->obj != NULL) send(")");
        send(" (");
        print_time(cb_data_p->time);
        send(")");
        break;
      }
    case cbNextSimTime:
      {
        send("NextSimTime ");
        if (cb_data_p->obj != NULL) send("(");
        print_mobject(cb_data_p->obj);
        if (cb_data_p->obj != NULL) send(")");
        send(" ");
        print_time_type(cb_data_p->time->type);
        break;
      }
    case cbAfterDelay:
      {
        send("AfterDelay ");
        if (cb_data_p->obj != NULL) send("(");
        print_mobject(cb_data_p->obj);
        if (cb_data_p->obj != NULL) send(")");
        send(" (");
        print_time(cb_data_p->time);
        send(")");
        break;
      }
    case cbEndOfCompile:           send("EndOfCompile");           break;
    case cbStartOfSimulation:      send("StartOfSimulation");      break;
    case cbEndOfSimulation:        send("EndOfSimulation");        break;
    case cbError:                  send("RuntimeError");           break;
    case cbTchkViolation:          send("TchkViolation");          break;
    case cbStartOfSave:            send("StartOfSave");            break;
    case cbEndOfSave:              send("EndOfSave");              break;
    case cbStartOfRestart:         send("StartOfRestart");         break;
    case cbEndOfRestart:           send("EndOfRestart");           break;
    case cbStartOfReset:           send("StartOfReset");           break;
    case cbEndOfReset:             send("EndOfReset");             break;
    case cbEnterInteractive:       send("EnterInteractive");       break;
    case cbExitInteractive:        send("ExitInteractive");        break;
    case cbInteractiveScopeChange: send("InteractiveScopeChange"); break;
    case cbUnresolvedSystf:        send("UnresolvedSysTf");        break;
    case cbAssign:
      {
        send("AfterAssign (");
        print_object_ref(cb_data_p->obj);
        send(") ");
        print_time_type(cb_data_p->time->type);
        send(" ");
        print_value_format(cb_data_p->value->format);
        break;
      }
    case cbDeassign:
      {
        send("AfterDeassign (");
        print_object_ref(cb_data_p->obj);
        send(") ");
        print_time_type(cb_data_p->time->type);
        send(" ");
        print_value_format(cb_data_p->value->format);
        break;
      }
    case cbDisable:
      {
        send("AfterDisable (");
        print_object_ref(cb_data_p->obj);
        send(") ");
        print_time_type(cb_data_p->time->type);
        send(" ");
        print_value_format(cb_data_p->value->format);
        break;
      }
    case cbPLIError:               send("PliError");               break;
    case cbSignal:                 send("Signal");                 break;
    case cbNBASynch:
      {
        send("NbaSynch ");
        if (cb_data_p->obj != NULL) send("(");
        print_mobject(cb_data_p->obj);
        if (cb_data_p->obj != NULL) send(")");
        send(" (");
        print_time(cb_data_p->time);
        send(")");
        break;
      }
    case cbAtEndOfSimTime:
      {
        send("AtEndOfSimTime ");
        if (cb_data_p->obj != NULL) send("(");
        print_mobject(cb_data_p->obj);
        if (cb_data_p->obj != NULL) send(")");
        send(" (");
        print_time(cb_data_p->time);
        send(")");
        break;
      }
    }
}
