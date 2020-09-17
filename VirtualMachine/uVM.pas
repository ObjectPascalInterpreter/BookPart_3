// Developed using Delphi for Windows and Mac platforms.

// *** Ths source is distributed under Apache 2.0 ***

// Copyright (C) 2019-2020 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

{$DEFINE STACKCHECK}

unit uVM;

interface

Uses System.SysUtils, System.Diagnostics, System.TimeSpan, uUtils,
  uMachineStack, System.generics.Collections, uListObject, uStringObject,
  uSymbolTable, uModule, uConstantTable;

const
  MAX_STACK_SIZE = 40000; // Maximum depth of operand stack
  MAX_FRAME_DEPTH = 8000; // Maximum depth of a function call
  MAX_INDICES_DEPTH = 64; // Number of indexable dimensions to a list

type
  TFrame = record
    funcIndex: integer;
    bsp: integer;     // stack base of function arguments
    nArgs: integer;   // number of arguments
    nlocals: integer; // number of local variables
    // This is a reference to the global constant table
    constantTable: TConstantValueTable;
    // This is a reference to the local symbol table
    symbolTable : TSymbolTable;
  end;
  PFrame = ^TFrame;  // Used in one place to pass a TFrame to a function

  TFrameStack = array of TFrame;

  // Debugging callbacks
  TVMCallBack = procedure(st: PMachineStackRecord) of object;
  TVMPrintCallBack = procedure(st: PMachineStackRecord) of object;
  TVMPrintlnCallBack = procedure(st: PMachineStackRecord) of object;

  TVM = class(TObject)
  private
    stack: TMachineStack;
    stackTop: integer;
    stacksize: integer;

    frameStackTop: integer;
    frameStack: TFrameStack;

    callbackPtr: TVMCallBack;
    printCallbackPtr: TVMPrintCallBack;
    printlnCallbackPtr: TVMPrintlnCallBack;

    module: TModule;

    assertCounter: integer;
    bolStopVm : boolean;

    procedure issueMessage (msg : string);


    procedure createStack(n: integer);
    procedure freeStack;
    procedure createFrameStack(n: integer);
    procedure freeFrameStack;

    procedure checkStackOverflow;
    //function  stackEmpty: boolean;  // Not used so commented out.
    procedure callBack(st: PMachineStackRecord);

    //procedure stackInc; inline;  // Not yet implemnted
    //procedure stackDec; inline;  // Not yet implemnted
    procedure callBuiltIn;
    procedure callUserFunction;

    function createList(count: integer): TListObject;

    procedure addOp;
    procedure subOp;
    procedure multOp;
    procedure divOp;
    procedure unaryMinusOp;
    procedure divideIntOp;
    procedure modOp;
    procedure IncOp(symbolIndex: integer);
    procedure localIncOp(symbolIndex: integer);
    procedure DecOp(symbolIndex: integer);
    procedure localDecOp(symbolIndex: integer);
    procedure powerOp;

    procedure storeSymbol(symTabIndex: integer);
    procedure storeLocalSymbol(index: integer);
    procedure loadSymbol(symTabIndex: integer);
    procedure loadLocalSymbol(index: integer);

    procedure storeIndexable;
    procedure storeIndexableString(variable: PMachineStackRecord;  index: integer);
    procedure storeIndexableList(variable: PMachineStackRecord; index: integer);
    procedure storeLocalIndexable;
    procedure storeLocalIndexableString(st: PMachineStackRecord; index: integer);
    procedure storeLocalIndexableList(st: PMachineStackRecord; index: integer);

    procedure loadIndexable;
    procedure loadIndexableList(variable: PMachineStackRecord; index: integer);
    procedure loadIndexableString(st: PMachineStackRecord; index: integer);
    procedure loadLocalIndexable;
    procedure loadLocalIndexableList(aList: TListObject; index: integer);
    procedure loadLocalIndexableString(astr: TStringObject; index: integer);

    function  popModule: TModule;
    function  popFunction: TUserFunction;

    procedure copyToStack (stackElement: PMachineStackRecord; index : integer; frame : PFrame);
    procedure clearAnyStackHeapAllocations;

    procedure printValue;
    procedure printlnValue;
    procedure assertTrue;
    procedure assertFalse;

    procedure isGt;
    procedure isGte;
    procedure isLt;
    procedure isLte;
    procedure isNoteq;
    procedure isEq;
    procedure andOp;
    procedure orOp;
    procedure xorOp;
    procedure notOp;

    procedure  collectGarbage;
    function   getGarbageSize : integer;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   registerPrintCallBack(fcn: TVMPrintCallBack);
    procedure   registerPrintlnCallBack(fcn: TVMPrintlnCallBack);
    procedure   push(value: PMachineStackRecord); overload;
    procedure   push(iValue: integer); overload; inline;
    procedure   push(bValue: boolean); overload; inline;
    procedure   push(dValue: double); overload; inline;
    procedure   push(sValue: TStringObject); overload; inline;
    procedure   push(lValue: TListObject); overload; inline;
    procedure   push (fValue: TUserFunction); overload; inline;
    procedure   pushModule (moduleIndex : integer);
    procedure   pushFunction (functionIndex: integer);
    function    pop: PMachineStackRecord; inline;
    function    stackHasEntry : boolean;
    procedure   stopvm;

    function    popInteger: integer;
    function    popScalar: double;
    function    popString : TStringObject;
    function    popList : TListObject;

    procedure   run(code: TProgram);
    procedure   runModule(module: TModule);

    procedure   setcallBack(proc: TVMCallBack);
    procedure   unsetcallBack;
  end;

var
  vm_elapsedTime: double;
  nTotalTests : integer;

implementation

Uses uOpCodes, Math, uBuiltinFunctions, uMemoryManager, uVMExceptions;

var
  Stopwatch: TStopwatch;

constructor TVM.Create;
begin
  inherited;
  createStack(MAX_STACK_SIZE);
  createFrameStack(MAX_FRAME_DEPTH);

  callbackPtr := nil;
  printCallbackPtr := nil;
  printlnCallbackPtr := nil;
  assertCounter := 1;
end;


destructor TVM.Destroy;
begin
  freeStack;
  freeFrameStack;
  inherited;
end;


procedure TVM.registerPrintCallBack(fcn: TVMPrintCallBack);
begin
  printCallbackPtr := fcn;
end;


procedure TVM.registerPrintlnCallBack(fcn: TVMPrintlnCallBack);
begin
  printlnCallbackPtr := fcn;
end;


procedure TVM.setcallBack(proc: TVMCallBack);
begin
  callbackPtr := proc;
end;


procedure TVM.unsetcallBack;
begin
  callbackPtr := nil;
end;


procedure TVM.createStack(n: integer);
begin
  stacksize := n;
  setLength(stack, n);
  stackTop := -1;
  memCount := 0;
end;


procedure TVM.freeStack;
begin
  setLength(stack, 0);
end;


procedure TVM.createFrameStack(n: integer);
begin
  frameStackTop := -1;
  setLength(frameStack, n);
end;


procedure TVM.freeFrameStack;
begin
  setLength(frameStack, 0);
end;


// -----------------------------------------------------------------------------------
// Debugging Routines

procedure TVM.printValue;
var
  nArgs: integer;
  i: integer;
  printStack: array of PMachineStackRecord;
begin
  nArgs := popInteger;
  setLength(printStack, nArgs);

  for i := 0 to nArgs - 1 do
    printStack[i] := pop;

  for i := nArgs - 1 downto 0 do
      begin
      if Assigned(printCallbackPtr) then
         printCallbackPtr(printStack[i]);
      end;
end;


procedure TVM.printlnValue;
var stRecord : TMachineStackRecord;
begin
  printValue;
  stRecord.sValue := TStringObject.Create(sLineBreak);  // Don't free, garbage collector will handle it.
  stRecord.stackType := stString;
  if Assigned(printCallbackPtr) then
    printCallbackPtr(@stRecord);
end;


procedure TVM.assertTrue;
var
  st: PMachineStackRecord;
begin
  st := pop;
  if st.stackType = stBoolean then
  begin
    inc (nTotalTests);
    if st.bValue = True then
      push(TStringObject.Create('.'))
    else
      push(TStringObject.Create('F'));
    push(1);
    printValue;
  end
  else
    raise ERuntimeException.Create ('Expecting boolean data type when using AssertTrue function');
  inc(assertCounter);
end;


procedure TVM.assertFalse;
var
  st: PMachineStackRecord;
begin
  st := pop;
  if st.stackType = stBoolean then
  begin
    if st.bValue = False then
      push(TStringObject.Create('.'))
    else
      push(TStringObject.Create('F'));
    push(1);
    printValue;
  end
  else
    raise ERuntimeException.Create
      ('Expecting boolean data type when using AssertTrue function');
  inc(assertCounter);
end;


// ---------------------------------------------------------------------------------
// Virtual Machine instructions

function TVM.stackHasEntry : boolean;
begin
  if stackTop > -1 then
     result := True
  else
     result := False;
end;


function TVM.pop: PMachineStackRecord;
begin
{$IFDEF STACKCHECK}
  if stackTop > -1 then
     begin
     result := @stack[stackTop];
     dec(stackTop);
     end
  else
    raise ERuntimeException.Create ('Stack underflow error in pop');
{$ELSE}
   result := @stack[stackTop];
   dec(stackTop);
{$ENDIF}
end;


function TVM.popInteger: integer;
var
  p: PMachineStackRecord;
begin
  if stackTop > -1 then
  begin
    p := @stack[stackTop];
    dec(stackTop);
    if p.stackType <> stInteger then
      raise ERuntimeException.Create ('Expecting integer type');
    result := p.iValue;
  end
  else
    raise ERuntimeException.Create ('Stack underflow error in popInteger');
end;



function TVM.popString : TStringObject;
var
  p: PMachineStackRecord;
begin
  if stackTop > -1 then
  begin
    p := @stack[stackTop];
    dec(stackTop);
    if p.stackType <> stString then
      raise ERuntimeException.Create ('Expecting string type');
    result := p.sValue;
  end
  else
    raise ERuntimeException.Create ('Stack underflow error in popString');
 end;



function TVM.popList : TListObject;
var
  p: PMachineStackRecord;
begin
  if stackTop > -1 then
  begin
    p := @stack[stackTop];
    dec(stackTop);
    if p.stackType <> stList then
      raise ERuntimeException.Create ('Expecting list type');
    result := p.lValue;
  end
  else
    raise ERuntimeException.Create ('Stack underflow error in popList');
 end;



function TVM.popModule: TModule;
var
  p: PMachineStackRecord;
begin
  if stackTop > -1 then
  begin
    p := @stack[stackTop];
    dec(stackTop);
    if p.stackType <> stModule then
      raise ERuntimeException.Create ('Expecting module type');
    result := p.mValue;
  end
  else
    raise ERuntimeException.Create ('Stack underflow error in popModule');
end;


function TVM.popFunction: TUserFunction;
var
  p: PMachineStackRecord;
begin
  if stackTop > -1 then
  begin
    p := @stack[stackTop];
    dec(stackTop);
    if p.stackType <> stFunction then
      raise ERuntimeException.Create ('Expecting function type');
    result := p.fValue;
  end
  else
    raise ERuntimeException.Create ('Stack underflow error in popFunction');
end;


function TVM.popScalar: double;
var
  p: PMachineStackRecord;
begin
  if stackTop > -1 then
  begin
    p := @stack[stackTop];
    dec(stackTop);
    if p.stackType = stInteger then
      exit(p.iValue);
    if p.stackType = stDouble then
      exit(p.dValue);

    raise ERuntimeException.Create('Expecting integer or double type');
  end
  else
    raise ERuntimeException.Create('Stack underflow error in popScalar');
end;


procedure TVM.checkStackOverflow;
begin
  if stackTop = stacksize then
    raise ERuntimeException.Create('Stack overflow error: ' + inttostr(stackTop)
      + ', max permitted depth is: ' + inttostr(stacksize));
end;

// -------------------------------------------
// Not used, but if we need it here it is.
//function TVM.stackEmpty: boolean;
//begin
//  if stackTop = -1 then
//    result := True
//  else
//    result := False;
//end;
// -------------------------------------------


procedure TVM.push(value: PMachineStackRecord);
begin
  inc(stackTop);
{$IFDEF STACKCHECK}
  checkStackOverflow;
{$ENDIF}
  stack[stackTop].stackType := value.stackType;
  case value.stackType of
    stNone:
    begin
    end;
    stInteger: stack[stackTop].iValue := value.iValue;
    stBoolean: stack[stackTop].bValue := value.bValue;
    stDouble: stack[stackTop].dValue := value.dValue;
    stString: stack[stackTop].sValue := value.sValue;
    stList: stack[stackTop].lValue := value.lValue;
  else
    raise ERuntimeException.Create('Unknown type in push method');
  end;
end;

procedure TVM.push(iValue: integer);
begin
  inc(stackTop);
{$IFDEF STACKCHECK}
  checkStackOverflow;
{$ENDIF}
  stack[stackTop].iValue := iValue;
  stack[stackTop].stackType := TStackType.stInteger;
end;


procedure TVM.push(bValue: boolean);
begin
  inc(stackTop);
{$IFDEF STACKCHECK}
  checkStackOverflow;
{$ENDIF}
  stack[stackTop].bValue := bValue;
  stack[stackTop].stackType := TStackType.stBoolean;
end;


procedure TVM.push(dValue: double);
begin
  inc(stackTop);
{$IFDEF STACKCHECK}
  checkStackOverflow;
{$ENDIF}
  stack[stackTop].dValue := dValue;
  stack[stackTop].stackType := TStackType.stDouble;
end;



procedure TVM.push(sValue: TStringObject);
begin
  inc(stackTop);
{$IFDEF STACKCHECK}
  checkStackOverflow;
{$ENDIF}
  stack[stackTop].sValue := sValue;
  stack[stackTop].stackType := TStackType.stString;
end;


procedure TVM.push (lValue: TListObject);
begin
  inc(stackTop);
  {$IFDEF STACKCHECK}
  checkStackOverflow;
{$ENDIF}
  stack[stackTop].lValue := lValue;
  stack[stackTop].stackType := TStackType.stList;
end;


procedure TVM.push (fValue: TUserFunction);
begin
  inc(stackTop);
  {$IFDEF STACKCHECK}
  checkStackOverflow;
{$ENDIF}
  stack[stackTop].fValue := fValue;
  stack[stackTop].stackType := TStackType.stFunction;
end;


procedure TVM.pushFunction (functionIndex: integer);
var module : TModule;
begin
  {$IFDEF STACKCHECK}
  checkStackOverflow;
{$ENDIF}
  module := popModule;
  if module.symbolTable[functionIndex].symbolType = symUserFunc then
     begin
     inc(stackTop);
     stack[stackTop].fValue := module.symbolTable[functionIndex].fValue;
     stack[stackTop].stackType := TStackType.stFunction;
     end
  else
    raise ERuntimeException.Create('Symbol ' + module.symbolTable[functionIndex].symbolName + ' is not a function');
end;


procedure TVM.pushModule (moduleIndex : integer);
begin
  inc(stackTop);
  {$IFDEF STACKCHECK}
  checkStackOverflow;
{$ENDIF}
  stack[stackTop].mValue := moduleList[moduleIndex];
  stack[stackTop].stackType := TStackType.stModule;
end;


//  -------------------------------------------
// Not yet implemented
//procedure TVM.stackInc;
//begin
//  inc(stack[stackTop].iValue);
//end;
//
//
//procedure TVM.stackDec;
//begin
//  dec(stack[stackTop].iValue);
//end;
//  -------------------------------------------


// -------------------------------------------------------------------------

procedure error(const arg: string; st1, st2: PMachineStackRecord);
begin
  raise ERuntimeException.Create(stToStr(st1.stackType) + ' and ' +
    stToStr(st2.stackType) + ' cannot be used with the ' + arg + ' operation');
end;


procedure TVM.addOp;
var
  st1, st2: PMachineStackRecord;
  aList : TListObject; tmp : TStringObject;

begin
  st1 := pop(); // second operand
  st2 := pop(); // first operand
  if (st1.stackType = stNone) or (st2.stackType = stNone) then
     raise ERuntimeException.Create ('Variable undefined');

  case st2.stackType of
    stInteger: case st1.stackType of
        stInteger: push(st1.iValue + st2.iValue);
        stDouble: push(st1.dValue + st2.iValue);
      else
        error('adding', st2, st1);
      end;

    stBoolean: error('adding', st2, st1); // Can't add booleans

    stDouble: case st1.stackType of
        stInteger: push(st1.iValue + st2.dValue);
        stDouble: push(st1.dValue + st2.dValue);
      else
        error('adding', st2, st1);
      end;

    stString: case st1.stackType of
        stString: push(TStringObject.add(st2.sValue, st1.sValue));
      else
        error('adding', st2, st1);
      end;

    stList: case st1.stackType of
        stInteger:
          begin
          aList := st2.lValue.clone;
          aList.append(st1.iValue);
          push(aList);
          end;
        stBoolean:
          begin
          aList := st2.lValue.clone;
          aList.append(st1.bValue);
          push(aList);
          end;
        stDouble:
          begin
          aList := st2.lValue.clone;
          aList.append(st1.dValue);
          push(aList);
          end;
        stString:
          begin
          aList := st2.lValue.clone;
          tmp   := st1.sValue.clone;
          tmp.blockType := btOwned;
          aList.append(tmp);
          push(aList);
          end;
        stList: push(TListObject.addLists(st2.lValue, st1.lValue));
      else
        error('adding', st2, st1);
      end
  else
    raise ERuntimeException.Create ('Internal Error: Unsupported datatype in add');
  end
end;


// Subtraction
procedure TVM.subOp;
var
  st1, st2: PMachineStackRecord;
begin
  st1 := pop();
  st2 := pop();
  if (st1.stackType = stNone) or (st2.stackType = stNone) then
     raise ERuntimeException.Create ('Variable undefined');

  case st2.stackType of
    stInteger: case st1.stackType of
        stInteger: push(st2.iValue - st1.iValue);
        stDouble: push(st2.iValue - st1.dValue);
      else
        error('subtracting', st2, st1);
      end;
    stDouble: case st1.stackType of
        stInteger: push(st2.dValue - st1.iValue);
        stDouble: push(st2.dValue - st1.dValue);
      else
        error('subtracting', st2, st1);
      end
  else
    raise ERuntimeException.Create ('Only integers and floats can be subtracted from each other');
  end;
end;


// Multiplication
procedure TVM.multOp;
var
  st1, st2: PMachineStackRecord;
  i: integer;
  ans: string;
begin
  st1 := pop();
  st2 := pop();
  if (st1.stackType = stNone) or (st2.stackType = stNone) then
     raise ERuntimeException.Create ('Variable undefined');

  case st2.stackType of
    stInteger: case st1.stackType of
        stInteger: push(st1.iValue * st2.iValue);
        stDouble: push(st2.iValue * st1.dValue);
        stString:
        begin
          ans := st1.sValue.value;
          for i := 2 to st2.iValue do
            ans := ans + st1.sValue.value;
          push(TStringObject.Create(ans));
        end;
        stList: push(TListObject.multiply(st2.iValue, st1.lValue));
      else
        error('multipying', st2, st1);
      end;
    stBoolean: error('multiplying', st2, st1);

    stDouble: case st1.stackType of
        stInteger: push(st2.dValue * st1.iValue);
        stDouble: push(st2.dValue * st1.dValue);
      else
        error('multipying', st2, st1);
      end;
    stString:
      begin
      if st1.stackType = stInteger then
      begin
        ans := st2.sValue.value;
        for i := 2 to st1.iValue do
          ans := ans + st2.sValue.value;
        push(TStringObject.Create(ans));
      end;
      end;
    stList:
      begin
      if st1.stackType = stInteger then
        push(TListObject.multiply(st1.iValue, st2.lValue))
      else
        raise ERuntimeException.Create ('Lists can only be multiplied by integers');
      end
  else
    raise ERuntimeException.Create ('Data type not supported by multiplication operator')
  end
end;


// Division
procedure TVM.divOp;
var
  st1, st2: PMachineStackRecord;
begin
  st1 := pop();
  st2 := pop();
  if (st1.stackType = stNone) or (st2.stackType = stNone) then
     raise ERuntimeException.Create ('Variable undefined');

  case st2.stackType of
    stInteger: case st1.stackType of
        stInteger: push(st2.iValue / st1.iValue);
        stDouble: push(st2.iValue / st1.dValue);
      else
        error('dividing', st2, st1);

      end;
    stDouble: case st1.stackType of
        stInteger: push(st2.dValue / st1.iValue);
        stDouble: push(st2.dValue / st1.dValue);
      else
        error('dividing', st2, st1);
      end
  else
    raise ERuntimeException.Create ('Data type not supported by division operator');
  end;
end;


procedure TVM.unaryMinusOp;
var
  st: PMachineStackRecord;
begin
  st := pop();
  case st.stackType of
    stInteger: push(-st.iValue);
    stDouble: push(-st.dValue);
  else
    raise ERuntimeException.Create('Data type not supported by unary operator');
  end;
end;


// Integer division
procedure TVM.divideIntOp;
var
  st1, st2: PMachineStackRecord;
begin
  st1 := pop();
  st2 := pop();
  if (st1.stackType = stNone) or (st2.stackType = stNone) then
     raise ERuntimeException.Create ('Variable undefined');

  case st2.stackType of
    stInteger: case st2.stackType of
        stInteger: push(st2.iValue div st1.iValue);
      else
        raise ERuntimeException.Create ('Expecting integer type with div operator');
      end
  else
    raise ERuntimeException.Create('Expecting integer type with div operator');
  end;
end;


// Compute the modulo, eg 6 mod 4 = 2
procedure TVM.modOp;
var
  st1, st2: PMachineStackRecord;
begin
  st1 := pop();
  st2 := pop();
  if (st1.stackType = stNone) or (st2.stackType = stNone) then
     raise ERuntimeException.Create ('Variable undefined');

  case st2.stackType of
    stInteger:
      begin
      case st1.stackType of
        stInteger: push(st2.iValue mod st1.iValue);
        stDouble : push(Math.FMod (st2.iValue, st1.dValue));
      else
         raise ERuntimeException.Create('Incompatible types in mod operation');
      end;
      end;
    stDouble:
      begin
        case st1.stackType of
         stInteger : push(Math.FMod (st2.dValue, st1.iValue));
         stDouble  : push(Math.FMod (st2.dValue, st1.dValue));
        else
          raise ERuntimeException.Create('Incompatible types in mod operation');
        end;
      end
  else
    raise ERuntimeException.Create('Incompatible types in mod operation');
  end;
end;


// symbolIndex that points to the variable to increament
procedure TVM.IncOp(symbolIndex: integer);
var
  sy: TSymbol;
begin
  sy := module.symbolTable.getFromSymbolTable(symbolIndex);
  case sy.symbolType of
    symInteger: module.symbolTable.storeToSymbolTable(symbolIndex, sy.iValue + 1);
    symDouble: module.symbolTable.storeToSymbolTable(symbolIndex, sy.dValue + 1);
  else
    raise ERuntimeException.Create
      ('Internal error: Illegal use of incBy on a non-integer/double type');
  end;
end;


// stack contains:
// symbolIndex that points to the variable to increament
// incBy, the amount by which to increment the value of the symbol
procedure TVM.localIncOp(symbolIndex: integer);
var
  sy: PMachineStackRecord;
  bsp: integer;
begin
  bsp := frameStack[frameStackTop].bsp;
  sy := @stack[symbolIndex + bsp];

  case sy.stackType of
    stInteger: inc(sy.iValue);
    stDouble: sy.dValue := sy.dValue + 1;
  else
    raise ERuntimeException.Create
      ('Internal error: Illegal use of incBy on a non-integer/double type');
  end;
end;


// symbolIndex that points to the variable to increament
procedure TVM.DecOp(symbolIndex: integer);
var
  sy: TSymbol;
begin
  sy := module.symbolTable.getFromSymbolTable(symbolIndex);
  case sy.symbolType of
    symInteger: module.symbolTable.storeToSymbolTable(symbolIndex, sy.iValue - 1);
    symDouble: module.symbolTable.storeToSymbolTable(symbolIndex, sy.dValue - 1);
  else
    raise ERuntimeException.Create
      ('Internal error: Illegal use of incBy on non-integer/double type');
  end;
end;


// symbolIndex that points to the variable to increament
procedure TVM.localDecOp(symbolIndex: integer);
var
  sy: PMachineStackRecord;
  bsp: integer;
begin
  bsp := frameStack[frameStackTop].bsp;
  sy := @stack[symbolIndex + bsp];

  case sy.stackType of
    stInteger: dec(sy.iValue);
    stDouble: sy.dValue := sy.dValue - 1;
  else
    raise ERuntimeException.Create
      ('Internal error: Illegal use of incBy on non-integer/double type');
  end;
end;


procedure TVM.powerOp;
var
  st1, st2: PMachineStackRecord;
begin
  st1 := pop();
  st2 := pop();
  if (st1.stackType = stNone) or (st2.stackType = stNone) then
     raise ERuntimeException.Create ('Variable undefined');

  case st2.stackType of
    stInteger: case st1.stackType of
        stInteger: push(power(st2.iValue, st1.iValue));
        stDouble: push(power(st2.iValue, st1.dValue));
      else
        error('power', st2, st1);
      end;
    stDouble: case st1.stackType of
        stInteger: push(power(st2.dValue, st1.iValue));
        stDouble: push(power(st2.dValue, st1.dValue));
      else
        error('exponential', st2, st1);
      end;
  else
    raise ERuntimeException.Create('Data type not supported by power operator');
  end;
end;


// ---------------------------------------------------------------------------------------------
// Load and store routines
// ---------------------------------------------------------------------------------------------

// This is called by oStoreSymbol
// This stores what ever is on the stack to the symbol table
procedure TVM.storeSymbol(symTabIndex: integer);
var
  value: PMachineStackRecord;
  module : TModule;
begin
  module := popModule;
  value := pop();
  case value.stackType of
    stInteger:  module.symbolTable.storeToSymbolTable(symTabIndex, value.iValue);
    stDouble:   module.symbolTable.storeToSymbolTable(symTabIndex, value.dValue);
    stBoolean:  module.symbolTable.storeToSymbolTable(symTabIndex, value.bValue);
    stString:   module.symbolTable.storeToSymbolTable(symTabIndex, value.sValue);
    stList:     module.symbolTable.storeToSymbolTable(symTabIndex, value.lValue);
    stFunction: raise ERuntimeException.Create ('assigning a function to a module is not permitted');
  else
    raise ERuntimeException.Create('Internal error: Unrecognized stacktype in storeSymbol');
  end;
end;


// This is called by oLoadSymbol
// This method retrieves a data item from the symbol table
// and pushes it onto the stack.
procedure TVM.loadSymbol(symTabIndex: integer);
var
  symbol: TSymbol;
  module : TModule;
begin
  module := popModule;
  symbol := module.symbolTable.getFromSymbolTable(symTabIndex);
  case symbol.symbolType of
    symUndefined: raise ERuntimeException.Create ('Undefined variable: ' + symbol.symbolName);
    symInteger:   push(symbol.iValue);
    symDouble:    push(symbol.dValue);
    symBoolean:   push(symbol.bValue);
    symString:    push(symbol.sValue);
    symList:      push(symbol.lValue);
    symUserFunc:  push(symbol.fValue);
  else
    raise ERuntimeException.Create('Unknown symbol type in loadSymbol: ' +
      inttostr(integer(symbol.symbolType)));
  end;
end;


procedure TVM.copyToStack(stackElement: PMachineStackRecord; index : integer; frame : PFrame);
begin
  inc(stackTop);
  case stackElement.stackType of
    stInteger:
        begin
        stack[stackTop].stackType := stInteger;
        stack[stackTop].iValue := stackElement.iValue;
        end;
      stBoolean:
        begin
        stack[stackTop].stackType := stBoolean;
        stack[stackTop].bValue := stackElement.bValue;
        end;
    stDouble:
        begin
        stack[stackTop].stackType := stDouble;
        stack[stackTop].dValue := stackElement.dValue;
        end;
    // For both strings and lists, the stack cannot own them, only the
    // symbol table can own string and list objects. Hence we just copy
    // references here.
    stString:
        begin
        stack[stackTop].stackType := stString;
        stack[stackTop].sValue := stackElement.sValue;
        end;
    stList:
        begin
        stack[stackTop].stackType := stList;
        stack[stackTop].lValue := stackElement.lValue;
        end;
    stNone:
      // ###
      raise ERuntimeException.Create ('Undefined variable: ' + frame.symbolTable[index].symbolName);
  else
    raise ERuntimeException.Create ('Internal Error: Unknown symbol in copyToStack');
  end;
end;


procedure TVM.loadLocalSymbol(index: integer);
var
  bsp: integer;
begin
  // Obtain the base of the local stack area from the current activation frame
  bsp := frameStack[frameStackTop].bsp;
  // Push the element at index + bsp onto the stack, note the
  // stack only holds pointers, hence we pass across a pointer
  copyToStack(@stack[index + bsp], index, @frameStack[frameStackTop]);
end;


// Store a value to a local symbol in a user function
procedure TVM.storeLocalSymbol(index: integer);
var
  value: PMachineStackRecord;
  bsp: integer;
begin
  // Store local variables on to the stack relative to bsp
  // index represents the offset from bsp where ther variable is stored
  bsp := frameStack[frameStackTop].bsp;
  value := pop();  // This is the value we will store

  if (stack[bsp + index].stackType = stString) and (stack[bsp + index].sValue <> nil) then
      stack[bsp + index].sValue.blockType := btGarbage; // Mark as garbage

  if (stack[bsp + index].stackType = stList) and (stack[bsp + index].lValue <> nil) then
      stack[bsp + index].lValue.blockType := btGarbage; // Mark as garbage

  case value.stackType of
    stInteger:
      begin
      stack[bsp + index].iValue := value.iValue;
      stack[bsp + index].stackType := stInteger;
      end;
    stBoolean:
      begin
      stack[bsp + index].bValue := value.bValue;
      stack[bsp + index].stackType := stBoolean;
      end;
    stDouble:
      begin
      stack[bsp + index].dValue := value.dValue;
      stack[bsp + index].stackType := stDouble;
      end;
    stString:
      begin
      stack[bsp + index].sValue := value.sValue.clone;
      stack[bsp + index].sValue.blockType := btBound;
      stack[bsp + index].stackType := stString;
      end;
    stList:
      begin
      stack[bsp + index].lValue := value.lValue.clone;
      stack[bsp + index].lValue.blockType := btBound;
      stack[bsp + index].stackType := stList;
      end
  else
    raise ERuntimeException.Create('Unknown symbol type in storeLocalValue');
  end;
end;


// ---------------------------------------------------------------------------------------------
// Boolean operators

// Is Less-Or-Equal
procedure TVM.isLte;
var
  st1, st2: PMachineStackRecord;
begin
  st1 := pop();
  st2 := pop();
  case st1.stackType of
    stInteger: case st2.stackType of
        stInteger: push(st2.iValue <= st1.iValue);
        stDouble: push(st2.dValue <= st1.iValue);
      end;
    stDouble: case st2.stackType of
        stInteger: push(st2.dValue <= st1.dValue);
        stDouble: push(st2.dValue <= st1.dValue);
      end
  else
    raise ERuntimeException.Create('Incompatible data types ''Is Less or Equal Than'' Operation');
  end;
end;


// Is Less-Than
procedure TVM.isLt;
var
  st1, st2: PMachineStackRecord;
begin
  st1 := pop();
  st2 := pop();
  case st1.stackType of
    stInteger: case st2.stackType of
        stInteger: push(st2.iValue < st1.iValue);
        stDouble:  push(st2.dValue < st1.iValue);
        stNone : raise ERuntimeException.Create ('Undefined value in greater than tests');
      end;
    stDouble: case st2.stackType of
        stInteger: push(st2.iValue < st1.dValue);
        stDouble: push(st2.dValue < st1.dValue);
        stNone : raise ERuntimeException.Create ('Undefined value in greater than tests');
      end;
  else
    raise ERuntimeException.Create('Incompatible types in ''Is Less Than'' Operation');
  end;
end;


// Is Greater-Than
procedure TVM.isGt;
var
  st1, st2: PMachineStackRecord;
begin
  st1 := pop();
  st2 := pop();
  case st1.stackType of
    stInteger: case st2.stackType of
        stInteger: push(st2.iValue > st1.iValue);
        stDouble: push(st2.dValue > st1.iValue);
      end;
    stDouble: case st2.stackType of
        stInteger: push(st2.iValue > st1.dValue);
        stDouble: push(st2.dValue > st1.dValue);
      end;
  else
    raise ERuntimeException.Create('Incompatible types in ''Is Greater Than Operation''');
  end;
end;


// Is Greater-Or-Equal
procedure TVM.isGte;
var
  st1, st2: PMachineStackRecord;
begin
  st1 := pop();
  st2 := pop();
  case st1.stackType of
    stInteger: case st2.stackType of
        stInteger: push(st2.iValue >= st1.iValue);
        stDouble: push(st2.dValue >= st1.iValue);
      else
        raise ERuntimeException.Create ('Boolean data types not permitted in isGte comparison');
      end;
    stDouble: case st2.stackType of
        stInteger: push(st2.dValue >= st1.iValue);
        stDouble: push(st2.dValue >= st1.dValue);
      end
  else
    raise ERuntimeException.Create('Incompatible types in ''Is Greater or Equal Than Operation''');
  end;
end;


// Is Not-Equal, we take a short-cut, call isEq then
// invert the boolean on the stack.
procedure TVM.isNoteq;
begin
  isEq;
  stack[stackTop].bValue := not stack[stackTop].bValue;
end;


// Is Equal
procedure TVM.isEq;
var
  st1, st2: PMachineStackRecord;
begin
  st1 := pop();
  st2 := pop();
  if (st1.stackType = stBoolean) and (st2.stackType = stBoolean) then
  begin
    push(st1.bValue = st2.bValue);
    exit;
  end;

  case st1.stackType of
    stInteger:
      case st2.stackType of
        stInteger: push(st2.iValue = st1.iValue);
        stDouble:  push(math.sameValue (st1.iValue, st2.dValue));
      else
        raise ERuntimeException.Create('Incompatible types in equality test');
      end;
    stDouble:
      case st2.stackType of
        stInteger: push(st2.iValue = st1.dValue);
        stDouble:  push(math.sameValue(st2.dValue, st1.dValue));
      else
        raise ERuntimeException.Create('Incompatible types in equality test');
      end;
    stString:
      begin
      if st2.stackType = stString then
         push(st1.sValue.isEqualTo (st2.sValue))
      else
         raise ERuntimeException.Create('Incompatible types in equality test');
      end;
    stList:
      begin
      if st2.stackType = stList then
        push(TListObject.listEquals(st1.lValue, st2.lValue))
      else
        raise ERuntimeException.Create ('Unable to test for equality between lists and non-lists data types');
      end
  else
    raise ERuntimeException.Create ('Incompatible types in equality test');
  end;
end;


procedure TVM.andOp;
var
  st1, st2: PMachineStackRecord;
begin
  st1 := pop();
  st2 := pop();
  if (st1.stackType <> stBoolean) or (st2.stackType <> stBoolean) then
     raise ERuntimeException.Create('Incompatible types in AND operation');
  push(st1.bValue and st2.bValue);
end;


procedure TVM.orOp;
var
  st1, st2: PMachineStackRecord;
begin
  st1 := pop();
  st2 := pop();
  if (st1.stackType <> stBoolean) or (st2.stackType <> stBoolean) then
     raise ERuntimeException.Create('Incompatible types in OR operation');
  push(st1.bValue or st2.bValue);
end;


procedure TVM.xorOp;
var
  st1, st2: PMachineStackRecord;
begin
  st1 := pop();
  st2 := pop();
  if (st1.stackType <> stBoolean) or (st2.stackType <> stBoolean) then
     raise ERuntimeException.Create('Incompatible types in XOR operation');
  push(st1.bValue xor st2.bValue);
end;


procedure TVM.notOp;
var
  st: PMachineStackRecord;
begin
  st := pop();
  if (st.stackType <> stBoolean) then
     raise ERuntimeException.Create('Incompatible types in NOT operation');
  push(not st.bValue);
end;


procedure TVM.callBack(st: PMachineStackRecord);
begin
  if Assigned(callbackPtr) then
    callbackPtr(st);
end;


procedure TVM.callUserFunction;
var
  funcRecord: TUserFunction;
  index, i: integer;
  nPureLocals : integer;
begin
  // Get the function object
  funcRecord := popFunction;

  if Assigned (funcRecord.funcPtr) then
     begin
     funcRecord.funcPtr(self);
     exit;
     end;

  inc(frameStackTop);
  if frameStackTop > MAX_FRAME_DEPTH then
    raise ERuntimeException.Create ('Exceeded maximum recursion depth for functions');

  // Fill in the frame entries, use with to make it more efficient
  with frameStack[frameStackTop] do
     begin
     funcIndex := index;
     nArgs := funcRecord.nArgs;
     nlocals := funcRecord.symbolTable.count;
     constantTable := funcRecord.constantTable;
     symbolTable := funcRecord.symbolTable;
     bsp := stackTop - funcRecord.nArgs + 1;
     end;

  // Make space for the local variables (Note symboltable contains args plus purelocals)
  stackTop := stackTop + funcRecord.symbolTable.count - funcRecord.nArgs;
  nPureLocals := funcRecord.symbolTable.count - funcRecord.nArgs;
  // Initialize the purelocals to undefined so we can detect assignments from unassigned variables
  // The pure locals sit just above the arguments
  for i := 0 to nPureLocals - 1 do
      stack[frameStack[frameStackTop].bsp + i + funcRecord.nArgs].stackType := stNone;

  run(funcRecord.funcCode);
end;


procedure TVM.callBuiltIn;
var
  funcRecord: TBuiltinFunctionRecord;
  index: integer;
begin
  index := popInteger;

  funcRecord := builtinList[index];
  funcRecord.funcPtr(self);
end;


// ------------------------------------------------------------------------------
// List load and storage routines
// ------------------------------------------------------------------------------

// Pop count items off the stack and create a list
function TVM.createList(count: integer): TListObject;
var
  i: integer;
  p: PMachineStackRecord;
begin
  result := TListObject.Create(count);
  result.blockType := btGarbage;

  for i := count - 1 downto 0 do
    begin
    p := pop;
    case p.stackType of
      stInteger:
        begin
        result.list[i].iValue := p.iValue;
        result.list[i].itemType := liInteger;
        end;
      stBoolean:
        begin
        result.list[i].bValue := p.bValue;
        result.list[i].itemType := liBoolean;
        end;
      stDouble:
        begin
        result.list[i].dValue := p.dValue;
        result.list[i].itemType := liDouble;
        end;
      stString:
        begin
        result.list[i].sValue := p.sValue.clone;
        result.list[i].sValue.blockType := btOwned; // Owned means owned by a list
        result.list[i].itemType := liString;
        end;
      stList:
        begin
        result.list[i].lValue := p.lValue.clone;
        result.list[i].lValue.blockType := btOwned;
        result.list[i].itemType := liList;
        end
    else
      raise ERuntimeException.Create('Unrecognized type in createList');
    end;
    end;
end;


procedure TVM.storeIndexableString(variable: PMachineStackRecord;
  index: integer);
var
  value: PMachineStackRecord;
begin
  value := pop;

  if (index < 0) or (index > length(variable.sValue.value) - 1) then
    raise ERuntimeException.Create('string index out of range');

  if variable.stackType <> stString then
    raise ERuntimeException.Create('left-hand side must be a string');

  if value.stackType <> stString then
    raise ERuntimeException.Create('right-hand side must be a string');

  if length(value.sValue.value) > 1 then
    raise ERuntimeException.Create
      ('can only assign a single string character to an indexed string');
  variable.sValue.value[index + 1] := value.sValue.value[1];
end;


procedure TVM.storeIndexableList(variable: PMachineStackRecord; index: integer);
var
  value: PMachineStackRecord;
  element: TListItem;
begin
  if (variable.lValue.list.count = 0) or (index > variable.lValue.list.count) then
     raise ERuntimeException.Create('index out of range');

  // Get the element from the list that we're going to store to
  element := variable.lValue.list[index];

  if (element.itemType = liString) and (element.sValue <> nil) then
     element.sValue.blockType := btGarbage;

  if (element.itemType = liList) and (element.lValue <> nil) then
     element.lValue.blockType := btGarbage;

  value := pop;
  case value.stackType of
    stInteger:
      begin
      element.iValue := value.iValue;
      element.itemType := liInteger;
      end;
    stBoolean:
      begin
      element.bValue := value.bValue;
      element.itemType := liBoolean;
      end;
    stDouble:
      begin
      element.dValue := value.dValue;
      element.itemType := liDouble;
      end;
    stString:
      begin
      element.sValue := value.sValue.clone;
      element.sValue.blockType := btOwned;
      element.itemType := liString;
      end;
    stList:
      begin
      element.lValue := value.lValue.clone;
      element.lValue.blockType := btOwned;
      element.itemType := liList;
      end;
  else
    raise ERuntimeException.Create('Unrecognized stacktype in storeIndexable');
  end
end;


procedure TVM.storeLocalIndexableList(st: PMachineStackRecord; index: integer);
var
  value: PMachineStackRecord;
  element: TListItem;
begin
  value := pop;

  if (index < 0) or (index > st.lValue.list.count - 1) then
     raise ERuntimeException.Create('list index out of range');

  element := st.lValue.list[index];

  if (element.itemType = liString) and (element.sValue <> nil) then
     element.sValue.blockType := btGarbage;

  if (element.itemType = liList) and (element.lValue <> nil) then
     element.lValue.blockType := btGarbage;

 case value.stackType of
    stInteger:
      begin
      element.iValue := value.iValue;
      element.itemType := liInteger;
      end;
    stBoolean:
      begin
      element.bValue := value.bValue;
      element.itemType := liBoolean;
      end;
    stDouble:
      begin
      element.dValue := value.dValue;
      element.itemType := liDouble;
      end;
    stString:
      begin
      element.sValue := value.sValue.clone;
      element.sValue.blockType := btOwned;
      element.itemType := liString;
      end;
    stList:
      begin
      element.lValue := value.lValue.clone;
      element.lValue.blockType := btOwned;
      element.itemType := liList;
      end;
  else
    raise ERuntimeException.Create('Unrecognized stacktype in storeLocalIndexable');
  end
end;


procedure TVM.storeLocalIndexableString(st: PMachineStackRecord;
  index: integer);
var
  value: PMachineStackRecord;
begin
  value := pop;

  if (index < 0) or (index > length(st.sValue.value) - 1) then
    raise ERuntimeException.Create('string index out of range');

  if value.stackType <> stString then
    raise ERuntimeException.Create ('can only assign a single string character to an indexed string');

  if length(value.sValue.value) > 1 then
    raise ERuntimeException.Create ('can only assign a single string character to an indexed string');
  st.sValue.value[index + 1] := value.sValue.value[1];
end;


procedure TVM.storeLocalIndexable;
var
  variable: PMachineStackRecord;
  bsp, index: integer;
begin
  index := popInteger;
  variable := pop;
  bsp := frameStack[frameStackTop].bsp;
  case variable.stackType of
    stList: storeLocalIndexableList(variable, index);
    stString:
    begin
        // check for errors, and update the stack entry that holds the variable
      storeLocalIndexableString(variable, index);
      stack[index + bsp].sValue := variable.sValue;
    end;
  else
    raise ERuntimeException.Create('local variable is not indexable: ' + module.symbolTable[frameStack[frameStackTop].funcIndex].fValue.symbolTable[index].symbolName);
  end;
end;


// Stack Structure to store indexed lists such as x[1,4,2]:
//
// index into indexable object
// index to symbol table containing indexable object
// the item we wish to store in the indexable object
// called by svecIdx
procedure TVM.storeIndexable;
var
  index : integer;
  sym: PMachineStackRecord;
begin
  index := popInteger;
  sym := pop;
  if sym.stackType = stList then
    storeIndexableList(sym, index)
  else
  if sym.stackType = stString then
    storeIndexableString(sym, index)
  else
    raise Exception.Create('variable is not indexable');
end;


// -------------------------------------------------------------------------------------------
// Loading Methods
// -------------------------------------------------------------------------------------------

procedure TVM.loadIndexableString(st: PMachineStackRecord; index: integer);
begin
  if (index < 0) or (index > length(st.sValue.value) - 1) then
    raise ERuntimeException.Create('string index out of range');
  push(TStringObject.Create(st.sValue.value[index + 1]));
end;


procedure TVM.loadIndexableList(variable: PMachineStackRecord; index: integer);
var element: TListItem;
begin
  if (index < 0) or (index > variable.lValue.list.count - 1) then
    raise ERuntimeException.Create('list index out of range');

  element := variable.lValue.list[index];

  case element.itemType of
    liInteger: push(element.iValue);
    liBoolean: push(element.bValue);
    liDouble:  push(element.dValue);
    liString:  push(element.sValue);
    liList:    push(element.lValue);
  else
    raise ERuntimeException.Create ('internal error: unsupported type in loadIndexableList');
  end;
end;


// Stack Structure to access indexed lists or strings such as [1]:
// Reference to variable to be accessed
// Index into indexable object
// Called by oLvecIdx
procedure TVM.loadIndexable;
var
  index: integer;
  variable: PMachineStackRecord;
begin
  index := popInteger;
  variable := pop;
  case variable.stackType of
    stList: loadIndexableList(variable, index);
    stString: loadIndexableString(variable, index);
  else
    raise ERuntimeException.Create('variable is not indexable');
  end;
end;


procedure TVM.loadLocalIndexableList(aList: TListObject; index: integer);
var
  element: TListItem;
begin
  if (index < 0) or (index > aList.list.count) then
    raise ERuntimeException.Create('list index out of range');

  // Get the first element of the list
  element := aList.list[index];

  case element.itemType of
    liInteger: push(element.iValue);
    liBoolean: push(element.bValue);
    liDouble:  push(element.dValue);
    liString:  push(element.sValue);
    liList:    push(element.lValue);
  else
    raise ERuntimeException.Create('unsupported type in loadIndexable');
  end;
end;


procedure TVM.loadLocalIndexableString(astr: TStringObject; index: integer);
begin
  if (index < 0) or (index > length(astr.value) - 1) then
    raise ERuntimeException.Create('string index out of range');
  push(TStringObject.Create(astr.value[index + 1]));
end;


procedure TVM.loadLocalIndexable;
var
  index: integer;
  st: PMachineStackRecord;
begin
  index := popInteger;
  st := pop;
  if st.stackType = stList then
    loadLocalIndexableList(st.lValue, index)
  else
  if st.stackType = stString then
    loadLocalIndexableString(st.sValue, index)
  else
    raise ERuntimeException.Create('variable is not indexable');
end;


procedure TVM.clearAnyStackHeapAllocations;
var
  i: integer;
begin
  for i := 1 to frameStack[frameStackTop].nlocals do
    case stack[stackTop + i].stackType of
      stList:
        begin
        stack[stackTop + i].lValue.blockType := btGarbage; // Mark as garbage
        stack[stackTop + i].lValue := nil;
        end;
      stString:
        begin
        if not stack[stackTop + i].sValue.isConstant then
          begin
          stack[stackTop + i].sValue.blockType := btGarbage; // Mark as garbage
          stack[stackTop + i].sValue := nil;
          end;
        end;
    end;
end;


procedure TVM.stopVm;
begin
  bolStopVm := True;
end;


procedure TVM.issueMessage (msg : string);
begin
  push(TStringObject.Create(msg));
  push (1);
  printlnValue;
end;


procedure TVM.collectGarbage;
begin
  //memoryList.freeGarbage;
  //writeln ('Collecting....');
end;


function TVM.getGarbageSize : integer;
begin
  result := memoryList.getMemmoryListSize;
end;

// -----------------------------------------------------------------------------

procedure TVM.runModule(module: TModule);
begin
  self.module := module;
  run(module.code);
end;


// Usebug this method to run a user defined function
procedure TVM.run (code: TProgram);
var
  ip : integer;
  byteCode: TByteCode;
  count: int64;
  value : PMachineStackRecord;
begin
  ip := 0; count := 0; value := @noneStackType; bolStopVm := False;

  if code.count = 0 then
     exit;

  Stopwatch := TStopwatch.StartNew;
  try
     while True do
        begin
        if bolStopVm then
           begin
           issueMessage ('Ctrl-C Interrupt Detected');
           exit;
           end;
         byteCode := code.code[ip];
         inc (count);

          case byteCode.opCode of
            oNop:        begin end;
            oAdd:        addOp;
            oSub:        subOp;
            oMult:       multOp;
            oDivide:     divOp;
            oDivi:       divideIntOp;
            oUmi:        unaryMinusOp;
            oMod:        modOp;
            oPower:      powerOp;
            oInc:        incOp(byteCode.index);
            oLocalInc:   localIncOp(byteCode.index);
            oDec:        decOp(byteCode.index);
            oLocalDec:   localDecOp(byteCode.index);
            oPushi:      push (byteCode.index);
            oPushb:      push (boolean(byteCode.index));
            oPushd:      push (constantValueTable[byteCode.index].dValue);
            oPushs:      push (constantValueTable[byteCode.index].sValue);
            oPushNone:   push (@noneStackType);
            oPushFunction: pushFunction (byteCode.index);
            oPushModule: pushModule (byteCode.index);
            oPop:        value := pop();  // his pop just throws the data away

            oIsLt:       isLt;
            oIsLte:      isLte;
            oIsGt:       isGt;
            oIsGte:      isGte;
            oIsEq:       isEq;
            oIsNotEq:    isNoteq;
            oAnd:        andOp;
            oOr:         orOp;
            oXor:        xorOp;
            oNot:        notOp;

            // debugging opcodes
            oPrint:       printValue;
            oPrintln:     printlnValue;
            oAssertTrue:  assertTrue;
            oAssertFalse: assertFalse;

            // Branch opcodes
            oJmp: ip := ip + byteCode.index - 1;
            oJmpIfTrue: if pop().bValue then
                ip := ip + byteCode.index - 1;
            oJmpIfFalse: if not pop().bValue then
                ip := ip + byteCode.index - 1;

            // store and load opcodes
            oStore: begin
                    storeSymbol(byteCode.index);
                    if getGarbageSize > 100 then
                       collectGarbage; // Only collect garbage after a store
                    end;
            oLoad:  loadSymbol(byteCode.index);

            oStoreLocal: begin
                         storeLocalSymbol(byteCode.index);
                         if getGarbageSize > 100 then
                            collectGarbage;
                         end;
            oLoadLocal:  loadLocalSymbol(byteCode.index);

           // Method call opcodes
            oCall:     callUserFunction;
            oBuiltin:  callBuiltIn;
            oRet:
              begin
              // Note that anything that is returned isn't bound to any symbol.
              // Bound only happens if the callee assigns the
              // returned value to a symbol.
              value := pop();
              case value.stackType of
                stString: value.sValue := value.sValue.clone;
                stList:   value.lValue := value.lValue.clone;
              end;
              stackTop := stackTop - frameStack[frameStackTop].nlocals;
              // This can only be called after the stackTop has been adjusted
              clearAnyStackHeapAllocations;

              dec(frameStackTop);
              push(value);
              exit;
              end;

           // list handling opcodes
            oCreateList:   push(createList(byteCode.index));
            oLvecIdx:      loadIndexable;
            oSvecIdx:      storeIndexable;

            oLocalLvecIdx: loadLocalIndexable;
            oLocalSvecIdx: storeLocalIndexable;

            oHalt: break;

            oPopAndSend:   callBack(pop);  // Not currently used
          else
            raise ERuntimeException.Create('Unknown opcode encountered in virtual machine execution loop: ' + inttostr(byteCode.opCode));
          end;
          inc(ip);
        end;
   finally
    Stopwatch.stop;
    vm_elapsedTime := Stopwatch.ElapsedMilliseconds;
  end;
end;

end.

