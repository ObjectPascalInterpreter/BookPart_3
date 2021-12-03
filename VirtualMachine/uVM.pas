// Developed using Delphi for Windows and Mac platforms.

// *** This source is distributed under Apache 2.0 ***

// Copyright (C) 2019-2020 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

{DEFINE STACKCHECK}

unit uVM;

interface

Uses System.SysUtils, System.Diagnostics, System.TimeSpan, uUtils,
  uMachineStack, System.generics.Collections, uListObject, uStringObject,
  uArrayObject, uSymbolTable, uConstantTable, uProgramCode, uMemoryManager,
  uObjectSupport, uIntStack;

const
  MAX_STACK_SIZE = 40000; // Maximum depth of operand stack
  MAX_FRAME_DEPTH = 8000; // Maximum depth of a function call
  MAX_INDICES_DEPTH = 64; // Number of indexable dimensions to a list

type
  // Used when user functions are called
  TFrame = record
    bsp: integer;     // stack base of function arguments
    nArgs: integer;   // number of arguments
    nlocals: integer; // number of local variables including any arguments
    pureLocals : integer; // number of local variables NOT including any arguments.
    // This is a reference to the global constant table
    constantTable: TConstantValueTable;
    // This is a reference to the local symbol table
    symbolTable : TLocalSymbolTable;
  end;
  PFrame = ^TFrame;  // Used in one place to pass a TFrame to a function

  TFrameStack = array of TFrame;

  TVMCaptureStringCallBack = procedure (astr : AnsiString);
  TVMReadStringCallBack  = function (const prompt : AnsiString) : PAnsiChar;

  TVMCallBack         = procedure(st: PMachineStackRecord) of object;
  TVMPrintCallBack    = procedure (astr : AnsiString);
  TVMSetColorCallBack = function (st: PMachineStackRecord) : AnsiString;

  // Use to preseve the state of the virtual machine between calls.
  TVMState = record
     module : TModule;
     symbolTable : TSymbolTable;
  end;

  // Only used for debugging
  TStackInfo = record
     stacktop : integer;
  end;

  TVM = class(TObject)
  private
    stack: TMachineStack;
    stackTop: integer;
    stacksize: integer;

    symbolTable : TSymbolTable;
    VMStateStack : TStack<TVMState>;
    //subscriptStack : TStack<integer>;//
    subscriptStack : uIntStack.TStack;     // lightweight integer stack

    frameStackTop: integer;
    frameStack: TFrameStack;

    callbackPtr: TVMCallBack;

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
    function  toStr (st : PMachineStackRecord) : AnsiString;

    //procedure stackInc; inline;  // Not yet implemnted
    //procedure stackDec; inline;  // Not yet implemnted

    procedure callObjectMethod (actualNumArgs : integer; p : PMachineStackRecord);
    procedure callBuiltIn (expectedNumArgs, actualNumArgs : integer; functionObject :TUserFunction);
    procedure callUserFunction (actualNumArgs : integer);

    function createList(count: integer): TListObject;

    procedure addOp;
    procedure subOp;
    procedure multOp;
    procedure divOp;
    procedure dotProductOp;
    procedure unaryMinusOp;
    procedure divideIntOp;
    procedure modOp;
    procedure IncOp(symbolName : string; step : double);
    procedure localIncOp(symbolIndex: integer; step : double);
    procedure DecOp(symbolName : string; step : double);
    procedure localDecOp(symbolIndex: integer; step : double);
    procedure powerOp;

    procedure storeSymbol(symbolName: string);
    procedure storeLocalSymbol(index: integer);
    procedure loadSymbol (symbolName : string);
    procedure loadAttr (symbolName : string);
    procedure storeAttr (symbolName : string);

    procedure loadLocalSymbol(index: integer);

    procedure storeIndexable (nSubscripts : integer);
    procedure storeIndexableString(variable: PMachineStackRecord;  index: integer);
    procedure storeIndexableList(variable: PMachineStackRecord; index: integer);
    procedure storeIndexableArray(variable: PMachineStackRecord; index: integer; nSubscripts : integer);

    procedure storeLocalIndexable;
    procedure storeLocalIndexableString(st: PMachineStackRecord; index: integer);
    procedure storeLocalIndexableList(st: PMachineStackRecord; index: integer);

    procedure loadIndexable (nSubscripts : integer);
    procedure loadIndexableList(variable: PMachineStackRecord; index: integer);
    procedure loadIndexableString(st: PMachineStackRecord; index: integer);
    procedure loadIndexableArray(st: PMachineStackRecord; index: integer; nSubscripts : integer);

    procedure loadLocalIndexable;
    procedure loadLocalIndexableList (aList: TListObject; index: integer);
    procedure loadLocalIndexableString (astr: TStringObject; index: integer);
    procedure loadLocalIndexableArray (arr: TArrayObject; index: integer);

    procedure buildSlice;
    procedure evalSliceObject (numSlices : integer);

    procedure copyToStack (stackElement: PMachineStackRecord; index : integer; frame : PFrame);
    procedure clearAnyStackHeapAllocations;

    procedure printValue;
    procedure printlnValue;
    procedure setColor;
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
    printCallbackPtr    : TVMPrintCallBack;
    printlnCallbackPtr  : TVMPrintCallBack;
    setColorCallBackPtr : TVMCaptureStringCallBack;
    readStringCallbackPtr   : TVMReadStringCallBack;

    recursionLimit : integer;

    interactive : boolean;
    constructor Create;
    destructor  Destroy; override;
    procedure   registerPrintCallBack(fcn: TVMCaptureStringCallBack);
    procedure   registerPrintlnCallBack(fcn: TVMCaptureStringCallBack);
    procedure   registerSetColorCallBack (fcn : TVMCaptureStringCallBack);
    procedure   registerReadStringCallBack (fcn : TVMReadStringCallBack);

    procedure   push(value: PMachineStackRecord); overload;
    procedure   push(iValue: integer); overload; inline;
    procedure   push(bValue: boolean); overload; inline;
    procedure   push(dValue: double); overload; inline;
    procedure   push(sValue: TStringObject); overload; inline;
    procedure   push(lValue: TListObject); overload; inline;
    procedure   push(aValue: TArrayObject); overload; inline;
    procedure   push(fValue: TUserFunction); overload; inline;
    procedure   push(oValue : TMethodDetails); overload;
    procedure   pushModule (module : TModule);
    procedure   pushNone;
    procedure   pushObject (obj : TObject);
    function    peek : PMachineStackRecord;
    procedure   decStackTop;
    function    pop: PMachineStackRecord; inline;
    procedure   dupStack; inline;
    procedure   swapStack;
    function    stackHasEntry : boolean;
    procedure   stopvm;
    procedure   importModule (moduleName : string);

    function    popInteger: integer; inline;
    function    popBoolean: boolean;
    function    popScalar: double;
    function    popArray : TArrayObject;
    function    popString : TStringObject;
    function    popList : TListObject;
    function    popUserFunction : TUserFunction;
    function    popMethodDetails: TMethodDetails;
    function    popModule: TModule;

    procedure   setRecursionLimit (rl : integer);
    function    getRecursionLimit : integer;

    procedure   run(code: TProgram; symbolTable : TSymbolTable);
    procedure   runModule(module: TModule);

    function    getStackInfo : TStackInfo;  // for debuggin purposes)
    procedure   setcallBack(proc: TVMCallBack);
    procedure   unsetcallBack;
  end;

var
  vm_elapsedTime: double;
  nTotalTests : integer;

implementation

Uses uOpCodes,
     Math,
     uRhodusTypes,
     uVMExceptions,
     Rtti,
     uRhodusEngine,
     uBuiltInGlobal,
     uBuiltInMatrix;

var
  Stopwatch: TStopwatch;

constructor TVM.Create;
begin
  inherited;
  recursionLimit := MAX_FRAME_DEPTH;
  createStack(MAX_STACK_SIZE);
  createFrameStack(recursionLimit);
  VMStateStack := TStack<TVMState>.Create;
  // HMS
  //subscriptStack := TStack<integer>.Create;
  uIntStack.create (subscriptStack, uIntStack.MAX_STACK_ENTRIES);

  callbackPtr := nil;
  printCallbackPtr := nil;
  printlnCallbackPtr := nil;
  assertCounter := 1;
  interactive := False;
end;


destructor TVM.Destroy;
begin
  freeStack;
  freeFrameStack;
  VMStateStack.Free;
  // HMS
  //subscriptStack.Free;
  inherited;
end;


function TVM.getStackInfo : TStackInfo;  // for debuggin purposes)
begin
  result.stacktop := stackTop;
end;


procedure TVM.registerSetColorCallback (fcn : TVMCaptureStringCallBack);
begin
 setColorCallBackPtr := fcn;
end;


procedure TVM.registerPrintCallBack(fcn: TVMCaptureStringCallBack);
begin
  printCallbackPtr := fcn;
end;


procedure TVM.registerPrintlnCallBack(fcn: TVMCaptureStringCallBack);
begin
  printlnCallbackPtr := fcn;
end;


procedure TVM.registerReadStringCallBack (fcn : TVMReadStringCallBack);
begin
  readStringCallbackPtr := fcn;
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

function TVM.toStr (st : PMachineStackRecord) : AnsiString;
var fmt : string;
begin
  if st <> nil then
     case st.stackType of
          stNone    : begin end; //write ('undefined value'); end;
          stInteger : begin
                      fmt := SysLibraryRef.find ('integerFormat').sValue.value;
                      result := Format(fmt, [st.iValue]);
                      end;
           stDouble : begin
                      fmt := SysLibraryRef.find ('doubleFormat').sValue.value;
                      result := Format(fmt, [st.dValue]);
                      end;
          stString  : result := st.sValue.value;
          stBoolean : if st.bValue = True then
                         result := 'True'
                      else
                         result := 'False';
          stList    : begin
                      result := st.lValue.listToString;
                      end;
          stArray   : result := st.aValue.arrayToString();

          stModule  : begin
                      result := st.module.name;
                      end;
          stFunction: begin
                      result := st.fValue.name;
                      end
     else
        result := 'Unrecognized value from print' + sLineBreak;
     end;
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
         printCallbackPtr (toStr (printStack[i]));
      end;
end;


procedure TVM.printlnValue;
var stRecord : TMachineStackRecord;
begin
  printValue;
  stRecord.sValue := TStringObject.Create(sLineBreak);  // Don't free, garbage collector will handle it.
  stRecord.stackType := stString;
  if Assigned(printCallbackPtr) then
    printCallbackPtr(sLineBreak);
end;


procedure TVM.setColor;
var color : PMachineStackRecord;
begin
  color := pop;
  if Assigned (setColorCallbackPtr) then
     setColorCallbackPtr (color.sValue.value);
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
    push(1); // one argument for print
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
    raise ERuntimeException.Create ('Expecting boolean data type when using AssertFalse function');
  inc(assertCounter);
end;


// ---------------------------------------------------------------------------------

procedure TVM.setRecursionLimit (rl : integer);
begin
  createFrameStack(rl);
end;

function TVM.getRecursionLimit : integer;
begin
  result := length (frameStack);
end;

function TVM.stackHasEntry : boolean;
begin
  if stackTop > -1 then
     result := True
  else
     result := False;
end;

// Virtual Machine instructions


function TVM.peek : PMachineStackRecord;
begin
  result := @stack[stackTop].stackType;
end;


// A way to pop the stack when we're not interested in the result
procedure TVM.decStackTop;
begin
  dec (stackTop);
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


procedure TVM.dupStack;
var entry : PMachineStackRecord;
begin
  entry := peek;
  inc(stackTop);
{$IFDEF STACKCHECK}
  checkStackOverflow;
{$ENDIF}
  stack[stackTop].stackType := entry.stackType;
  case entry.stackType of
    stNone:
    begin
    end;
    stInteger: stack[stackTop].iValue := entry.iValue;
    stBoolean: stack[stackTop].bValue := entry.bValue;
    stDouble: stack[stackTop].dValue := entry.dValue;
    stString: stack[stackTop].sValue := entry.sValue;
    stList: stack[stackTop].lValue := entry.lValue;
    stArray: stack[stackTop].aValue := entry.aValue;
  else
    raise ERuntimeException.Create('Unknown type in dup method');
  end;
end;

procedure TVM.swapStack;
var tmp : TMachineStackRecord;
begin
  tmp := stack[stackTop];
  stack[stackTop] := stack[stackTop-1];
  stack[stackTop-1] := tmp;
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


function TVM.popBoolean: boolean;
var
  p: PMachineStackRecord;
begin
  if stackTop > -1 then
  begin
    p := @stack[stackTop];
    dec(stackTop);
    if p.stackType <> stBoolean then
      raise ERuntimeException.Create ('Expecting boolean type');
    result := p.bValue;
  end
  else
    raise ERuntimeException.Create ('Stack underflow error in popBoolean');
end;


function TVM.popArray : TArrayObject;
var
  p: PMachineStackRecord;
begin
  if stackTop > -1 then
     begin
     p := @stack[stackTop];
     dec(stackTop);
     if p.stackType <> stArray then
        raise ERuntimeException.Create ('Expecting array type');
     result := p.aValue;
     end
  else
     raise ERuntimeException.Create ('Stack underflow error in popString');
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
var p: PMachineStackRecord;
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


function TVM.popUserFunction : TUserFunction;
var p: PMachineStackRecord;
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
     raise ERuntimeException.Create ('Stack underflow error in popUserFunction');
 end;


function TVM.popMethodDetails: TMethodDetails;
var p: PMachineStackRecord;
begin
  if stackTop > -1 then
     begin
     p := @stack[stackTop];
     dec(stackTop);
     if p.stackType <> stObjectMethod then
        raise ERuntimeException.Create ('Expecting object method type');
     result := p.oValue;
     end
 else
     raise ERuntimeException.Create ('Stack underflow error in popUserFunction');
 end;


function TVM.popModule: TModule;
var p: PMachineStackRecord;
begin
  if stackTop > -1 then
     begin
     p := @stack[stackTop];
     dec(stackTop);
     if p.stackType <> stModule then
        raise ERuntimeException.Create ('Expecting module type but found: ' + stToStr (p.stackType));

     result := p.module;
     end
  else
    raise ERuntimeException.Create ('Stack underflow error in popModule');
end;


function TVM.popScalar: double;
var p: PMachineStackRecord;
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
    stArray: stack[stackTop].aValue := value.aValue;
    stFunction : stack[stackTop].fValue := value.fValue;
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


procedure TVM.push (aValue: TArrayObject);
begin
  inc(stackTop);
  {$IFDEF STACKCHECK}
  checkStackOverflow;
{$ENDIF}
  stack[stackTop].aValue := aValue;
  stack[stackTop].stackType := TStackType.stArray;
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


procedure TVM.push(oValue : TMethodDetails);
begin
  inc(stackTop);
  {$IFDEF STACKCHECK}
  checkStackOverflow;
{$ENDIF}
  stack[stackTop].oValue := oValue;
  stack[stackTop].stackType := TStackType.stObjectMethod;
end;


procedure TVM.pushModule (module : TModule);
begin
  inc(stackTop);
{$IFDEF STACKCHECK}
  checkStackOverflow;
{$ENDIF}

  stack[stackTop].module := module;
  stack[stackTop].stackType := TStackType.stModule;
end;


procedure TVM.pushNone;
begin
  push (@noneStackType);
end;


procedure TVM.pushObject (obj : TObject);
begin
  inc(stackTop);
{$IFDEF STACKCHECK}
  checkStackOverflow;
{$ENDIF}

  stack[stackTop].objValue := obj;
  stack[stackTop].stackType := TStackType.stObject;
end;

// -------------------------------------------------------------------------

procedure error(const arg: string; st1, st2: PMachineStackRecord);
begin
  raise ERuntimeException.Create(stToStr(st1.stackType) + ' and ' +
    stToStr(st2.stackType) + ' cannot be used with the ' + arg + ' operation');
end;


procedure TVM.addOp;
var
  st1, st2 : PMachineStackRecord;
  aList : TListObject; tmp : TStringObject;

begin
  st1 := pop; // second operand
  st2 := pop; // first operand

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
        stFunction:
          begin
          aList := st2.lValue.clone;
          aList.appendUserFunction (st1.fValue);
          push(aList);
          end;
        stModule:
          begin
          aList := st2.lValue.clone;
          aList.appendModule (st1.module);
          push(aList);
          end;      else
        error('adding', st2, st1);
      end;

    stArray:
      case st1.stackType of
        stArray: push(TArrayObject.add(st2.aValue, st1.aValue));
      else
        error('adding', st2, st1);
      end;
  else
    raise ERuntimeException.Create ('Internal Error: Unsupported datatype in add');
  end
end;


// Subtraction
procedure TVM.subOp;
var
  st1, st2: PMachineStackRecord;
begin
  st1 := pop;
  st2 := pop;
  if (st1.stackType = stNone) or (st2.stackType = stNone) then
     raise ERuntimeException.Create ('Variable undefined');

  case st2.stackType of
    stInteger: case st1.stackType of
        stInteger: push(st2.iValue - st1.iValue);
        stDouble: push(st2.iValue - st1.dValue);
      else
        raise ERuntimeException.Create(stToStr(st1.stackType) + ' and ' +
                stToStr(st2.stackType) + ' cannot be used with the subtraction operation');
      end;
    stDouble: case st1.stackType of
        stInteger: push(st2.dValue - st1.iValue);
        stDouble: push(st2.dValue - st1.dValue);
      else
       raise ERuntimeException.Create(stToStr(st2.stackType) + ' and ' +
                stToStr(st1.stackType) + ' cannot be used with the subtraction operation');
      end;
    stArray:
      case st1.stackType of
        stArray: push(TArrayObject.sub(st2.aValue, st1.aValue));
      else
        error('subracting', st2, st1);
      end;
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
  st1 := pop;
  st2 := pop;
  if (st1.stackType = stNone) or (st2.stackType = stNone) then
     raise ERuntimeException.Create ('Variable undefined');

  case st2.stackType of
    stInteger:
     case st1.stackType of
        stInteger: push(st1.iValue * st2.iValue);
        stDouble : push(st2.iValue * st1.dValue);
        stString :
        begin
          ans := st1.sValue.value;
          for i := 2 to st2.iValue do
              ans := ans + st1.sValue.value;
          push(TStringObject.Create(ans));
        end;
        stList  : push(TListObject.multiply(st2.iValue, st1.lValue));
        stArray : push (TArrayObject.arrayIntMult (st1.aValue, st2.iValue));
      else
        error('multipying', st2, st1);
      end;

    stBoolean: error('multiplying', st2, st1);

    stDouble:
      case st1.stackType of
        stInteger : push(st2.dValue * st1.iValue);
        stDouble  : push(st2.dValue * st1.dValue);
        stArray   :  push (TArrayObject.arrayDoubleMult (st1.aValue, st2.dValue))
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
      end;

    stArray :
       case st1.stackType of
         stInteger : push (TArrayObject.arrayIntMult (st2.aValue, st1.iValue));
         stDouble  : push (TArrayObject.arrayDoubleMult (st2.aValue, st1.dValue));
         stArray   : push (TArrayObject.mult (st2.aValue, st1.aValue))
       else
         error('multipying', st2, st1);
       end
  else
    raise ERuntimeException.Create ('Data type not supported by multiplication operator')
  end
end;


procedure TVM.dotProductOp;
var m1, m2 : TArrayObject;
    sum : double;
begin
  m2 := popArray;
  m1 := popArray;

  if (m1.getNumDimensions() = 1) and (m2.getNumDimensions() = 1) then
     begin
     // Dot product of two vectors
     if m1.dim[0] = m2.dim[0] then
        begin
        sum := 0;
        for var i := 0 to m1.dim[0] - 1 do
            sum := sum + m1.dataf[i] * m2.dataf[i];
        push (sum);
        exit;
        end
     else
        raise ERuntimeException.Create('Dot product requires both vectros to be the same size');
     end;

  // Otherwise its matrix-matrix multiplication
  push (TBuiltInMatrix.dotMatMatMult(m1, m2));
end;


// Division
procedure TVM.divOp;
var
  st1, st2: PMachineStackRecord;
begin
  st1 := pop;
  st2 := pop;
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
  st := pop;
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
  st1 := pop;
  st2 := pop;
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
  st1 := pop;
  st2 := pop;
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



// Increment symbolName by step
procedure TVM.IncOp(symbolName : string; step : double);
var symbol : TSymbol;
begin
  module.symbolTable.find (symbolName, symbol);
  case symbol.symbolType of
    symInteger: symbol.iValue := symbol.iValue + trunc (step);
    symDouble:  symbol.dValue := symbol.dValue + step;
  else
    raise ERuntimeException.Create  ('Internal error: Illegal use of incBy on a non-integer/double type');
  end;
end;


// stack contains:
// symbolIndex that points to the variable to increment
// incBy, the amount by which to increment the value of the symbol
procedure TVM.localIncOp(symbolIndex: integer; step : double);
var
  sy: PMachineStackRecord;
  bsp: integer;
begin
  bsp := frameStack[frameStackTop].bsp;
  sy := @stack[symbolIndex + bsp];

  case sy.stackType of
    stInteger: inc(sy.iValue);
    stDouble: sy.dValue := sy.dValue + step;
  else
    raise ERuntimeException.Create ('Internal error: Illegal use of incBy on a non-integer/double type');
  end;
end;


// Decrement symbolName by step
procedure TVM.DecOp(symbolName : string; step : double);
var symbol : TSymbol;
begin
  module.symbolTable.find (symbolName, symbol);
  case symbol.symbolType of
    symInteger: symbol.iValue := symbol.iValue - trunc (step);
    symDouble:  symbol.dValue := symbol.dValue - step;
  else
    raise ERuntimeException.Create ('Internal error: Illegal use of decBy on non-integer/double type');
  end;
end;


// symbolIndex that points to the variable to increament
procedure TVM.localDecOp(symbolIndex: integer; step : double);
var
  sy: PMachineStackRecord;
  bsp: integer;
begin
  bsp := frameStack[frameStackTop].bsp;
  sy := @stack[symbolIndex + bsp];

  case sy.stackType of
    stInteger: dec(sy.iValue);
    stDouble: sy.dValue := sy.dValue - step;
  else
    raise ERuntimeException.Create ('Internal error: Illegal use of incBy on non-integer/double type');
  end;
end;


procedure TVM.powerOp;
var
  st1, st2: PMachineStackRecord;
begin
  st1 := pop;
  st2 := pop;
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
// This stores whatever is on the stack to the symbol table
procedure TVM.storeSymbol(symbolName: string);
var
  value: PMachineStackRecord;
  symbol : TSymbol;
begin
  value := pop();
  if not module.symbolTable.find (symbolName, symbol) then
     raise ERuntimeException.Create('Undeclared variable: ' + symbolName + ' in module: ' + module.name);

  case value.stackType of
    stNone:     begin symbol.symbolType := symUndefined; end;
    stInteger:  module.symbolTable.storeInteger (symbol, value.iValue);
    stDouble:   module.symbolTable.storeDouble (symbol, value.dValue);
    stBoolean:  module.symbolTable.storeBoolean (symbol, value.bValue);
    stString:   module.symbolTable.storeString (symbol, value.sValue);
    stList:     module.symbolTable.storeList   (symbol, value.lValue);
    stArray:    module.symbolTable.storeArray   (symbol, value.aValue);
    stFunction: module.symbolTable.storeFunction (symbol, value.fValue);

    stModule:   begin
                raise ERuntimeException.Create('You cannot store modules: ' + value.module.name);
                end;
    stObjectMethod:
       raise ERuntimeException.Create('You cannot store object methods: ' + value.oValue.name);
  else
    raise ERuntimeException.Create('Internal error: Unrecognized stacktype in storeSymbol: ' + TRttiEnumerationType.GetName(value.stackType));
  end;
end;


// This will load any kind of symbol on to the stack
procedure TVM.loadSymbol (symbolName : string);
var symbol : TSymbol;
begin
  if not module.symbolTable.find (symbolName, symbol) then
     raise ERuntimeException.Create ('Undefined symbol: ' + symbolName);

 case symbol.symbolType of
    symUndefined: raise ERuntimeException.Create ('Variable has no assigned value: ' + symbol.symbolName);
    symInteger:   push(symbol.iValue);
    symDouble:    push(symbol.dValue);
    symBoolean:   push(symbol.bValue);
    symString:    push(symbol.sValue);
    symList:      push(symbol.lValue);
    symArray:     push(symbol.aValue);
    symUserFunc:  begin
                  symbol.fValue.moduleRef := module;
                  push(symbol.fValue);
                  end;
    symModule:    pushModule(TModule (symbol.mValue));
  else
    raise ERuntimeException.Create('Unknown symbol type in loadSymbol: ' +  inttostr(integer(symbol.symbolType)));
  end;
end;


// A secondary is the symbol a in X.a
// The stack will be:
//   primary (X)
//   secondary (a)
// A primary can be a module, a list (eg a.len), or a string (eg a.len)

procedure TVM.loadAttr (symbolName : string);
var m : TModule;
    l : TListObject;
    s : TStringObject;
    a : TArrayObject;
    func : TUserFunction;
    symbol : TSymbol;
    primary : PMachineStackRecord;
    f : TMethodDetails;
begin
  primary := pop();
  case primary.stackType of
   stModule : begin
              m := primary.module;

              if not m.symbolTable.find (symbolName, symbol) then
                 raise ERuntimeException.Create ('Undefined symbol: ' + symbolName);

              case symbol.symbolType of
                symUndefined: raise ERuntimeException.Create ('Variable has no assigned value: ' + symbol.symbolName);
                symInteger:   push(symbol.iValue);
                symDouble:    push(symbol.dValue);
                symBoolean:   push(symbol.bValue);
                symString:    push(symbol.sValue);
                symList:      push(symbol.lValue);
                symArray:     push(symbol.aValue);
                symUserFunc:  begin
                              symbol.fValue.moduleRef := m;
                              push(symbol.fValue);
                              end;
                symModule:    pushModule(TModule (symbol.mValue));
              else
                raise ERuntimeException.Create('Unknown symbol type in loadAttr: ' +  inttostr(integer(symbol.symbolType)));
              end;
              end;

     stList : begin
              l := primary.lValue;
              // Is symbol name a function name?
              f := l.methods.methodList.find (symbolName);
              if f <> nil then
                 begin
                 f.self := primary.lValue;
                 push (f);
                 end
              else
                 raise ERuntimeException.Create('No method <' + symbolName + '> associated with object');
              end;

     stString : begin
              s := primary.sValue;
              // Is symbol name a function name?
              f := s.methods.methodList.find (symbolName);
              if f <> nil then
                 begin
                 f.self := primary.sValue;
                 push (f);
                 end
              else
                 raise ERuntimeException.Create('No method <' + symbolName + '> associated with object');
              end;

     stArray : begin
              a := primary.aValue;
              // Is symbol name a function name?
              f := a.methods.methodList.find (symbolName);
              if f <> nil then
                 begin
                 f.self := primary.aValue;
                 push (f);
                 end
              else
                 raise ERuntimeException.Create('No method <' + symbolName + '> associated with object');
              end;

   stFunction :
              begin
              func := primary.fValue;
              // Is symbol name a function name?
              f := func.userFunctionMethods.methodList.find (symbolName);
              if f <> nil then
                 begin
                 f.self := primary.fValue;
                 push (f);
                 end
              else
                 raise ERuntimeException.Create('No method <' + symbolName + '> associated with object');
              end
  else
     raise ERuntimeException.Create('Primary objects can only be modules, functions, strings, arrays or lists');
  end;
end;


procedure TVM.storeAttr (symbolName : string);
var m : TModule;
    symbol : TSymbol;
    value : PMachineStackRecord;
begin
  m := popModule();
  value := pop();

  if not m.symbolTable.find (symbolName, symbol) then
     raise ERuntimeException.Create ('Undefined symbol: ' + symbolName);

  if symbol.locked then
     raise ERuntimeException.Create ('This symbol is locked, you cannot change it: ' + symbolName);

  case value.stackType of
    stNone:     begin symbol.symbolType := symUndefined; end;
    stInteger:  begin symbol.symbolType := symInteger; symbol.iValue := value.iValue; end;
    stDouble:   begin symbol.symbolType := symDouble; symbol.dValue := value.dValue; end;
    stBoolean:  begin symbol.symbolType := symBoolean; symbol.bValue := value.bValue; end;
    stString:   m.symbolTable.storeString (symbol, value.sValue);
    stList:     m.symbolTable.storeList   (symbol, value.lValue);
    stArray:    m.symbolTable.storeArray   (symbol, value.aValue);
    stFunction: m.symbolTable.storeFunction (symbol, value.fValue);
    stModule:   m.symbolTable.storeModule (symbol, value.module);
  else
    raise ERuntimeException.Create('Internal error: Unrecognized stacktype in storeAttr: ' + TRttiEnumerationType.GetName(symbol.symbolType));
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
    stArray:
        begin
        stack[stackTop].stackType := stArray;
        stack[stackTop].aValue := stackElement.aValue;
        end;
    stFunction:
        begin
        stack[stackTop].stackType := stFunction;
        stack[stackTop].fValue := stackElement.fValue;
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
  // index represents the offset from bsp where the variable is stored
  bsp := frameStack[frameStackTop].bsp;
  value := pop();  // This is the value we will store

  if (stack[bsp + index].stackType = stString) and (stack[bsp + index].sValue <> nil) then
      stack[bsp + index].sValue.blockType := btGarbage; // Mark as garbage

  if (stack[bsp + index].stackType = stList) and (stack[bsp + index].lValue <> nil) then
      stack[bsp + index].lValue.blockType := btGarbage; // Mark as garbage

  if (stack[bsp + index].stackType = stArray) and (stack[bsp + index].aValue <> nil) then
      stack[bsp + index].aValue.blockType := btGarbage; // Mark as garbage

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
      end;
    stArray:
      begin
      stack[bsp + index].aValue := value.aValue.clone;
      stack[bsp + index].aValue.blockType := btBound;
      stack[bsp + index].stackType := stArray;
      end;
    stNone:
      raise ERuntimeException.Create ('No value to assign in function');
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
  st1 := pop;
  st2 := pop;
  case st1.stackType of
    stInteger: case st2.stackType of
        stInteger: push(st2.iValue <= st1.iValue);
        stDouble: push(st2.dValue <= st1.iValue);
      end;
    stDouble: case st2.stackType of
        stInteger: push(st2.iValue <= st1.dValue);
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
  st1 := pop;
  st2 := pop;
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
  st1 := pop;
  st2 := pop;
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
  st1 := pop;
  st2 := pop;
  case st1.stackType of
    stInteger: case st2.stackType of
        stInteger: push(st2.iValue >= st1.iValue);
        stDouble: push(st2.dValue >= st1.iValue);
      else
        raise ERuntimeException.Create ('Boolean data types not permitted in isGte comparison');
      end;
    stDouble: case st2.stackType of
        stInteger: push(st2.iValue >= st1.dValue);
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
  st1 := pop;
  st2 := pop;
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
      end;
    stArray :
      if st2.stackType = stArray then
         push (TArrayObject.isEqualTo(st1.aValue, st2.aValue))
      else
        raise ERuntimeException.Create ('Unable to test for equality between arrays and non-arrays data types');
  else
    raise ERuntimeException.Create ('Incompatible types in equality test');
  end;
end;


procedure TVM.andOp;
var
  st1, st2: PMachineStackRecord;
begin
  st1 := pop;
  st2 := pop;
  if (st1.stackType <> stBoolean) or (st2.stackType <> stBoolean) then
     raise ERuntimeException.Create('Incompatible types in AND operation');
  push(st1.bValue and st2.bValue);
end;


procedure TVM.orOp;
var
  st1, st2: PMachineStackRecord;
begin
  st1 := pop;
  st2 := pop;
  if (st1.stackType <> stBoolean) or (st2.stackType <> stBoolean) then
     raise ERuntimeException.Create('Incompatible types in OR operation');
  push(st1.bValue or st2.bValue);
end;


procedure TVM.xorOp;
var
  st1, st2: PMachineStackRecord;
begin
  st1 := pop;
  st2 := pop;
  if (st1.stackType <> stBoolean) or (st2.stackType <> stBoolean) then
     raise ERuntimeException.Create('Incompatible types in XOR operation');
  push(st1.bValue xor st2.bValue);
end;


procedure TVM.notOp;
var
  st: PMachineStackRecord;
begin
  st := pop;
  if (st.stackType <> stBoolean) then
     raise ERuntimeException.Create('Incompatible types in NOT operation');
  push(not st.bValue);
end;


procedure TVM.callBack(st: PMachineStackRecord);
begin
  if Assigned(callbackPtr) then
    callbackPtr(st);
end;


// Call something like a.len()
procedure TVM.callObjectMethod (actualNumArgs : integer; p : PMachineStackRecord);
var selfValue : PMachineStackRecord;
begin
  if p.oValue.nArgs <> VARIABLE_ARGS then
     if actualNumArgs <> p.oValue.nArgs then
        raise ERuntimeException.Create('Expecting ' + inttostr (p.oValue.nArgs) + ' arguments in function call [' + p.oValue.name + '] but received ' + inttostr (actualNumArgs) + ' arguments');

  if p.oValue.nArgs = VARIABLE_ARGS then
     push(actualNumArgs);
  // The order of stack entries from the top will be
  //   Arguments
  //   Object method -> pointer to self
  p.oValue.method(self);
end;


procedure TVM.callBuiltIn (expectedNumArgs, actualNumArgs : integer; functionObject :TUserFunction);
begin
  if expectedNumArgs = VARIABLE_ARGS then
     push(actualNumArgs);
  functionObject.builtInPtr(self);
  // At this point the stack is:
  //   functionObject
  //   return result from builtin

  // Bring the functionObject forward and remove it,
  // what's left is therefore the return result.
  swapStack;
  dec (stackTop);  // remove the function object
end;


// nArgs is the number of arguments the user included in the function call
// The stack will have arguments followed by the function object
procedure TVM.callUserFunction (actualNumArgs : integer);
var
  functionObject: TUserFunction;
  i: integer;
  nPureLocals, tbsp : integer;
  expectedNumArgs : integer;
  symTableCount : integer;
  p: PMachineStackRecord;
  oldModule : TModule;
begin
  // Get the function object
  p := @stack[stackTop-actualNumArgs];
  // Check first that its actually something we can call
  if (p.stackType <> stFunction) and (p.stackType <> stObjectMethod) then
     raise ERuntimeException.Create(stToStr (p.stackType) + ' is not something that can be called as a function');

  // This deals with calls such as a.len(), where len() is an object method
  if p.stackType = stObjectMethod then
     begin
     callObjectMethod (actualNumArgs, p);
     exit;
     end;

  functionObject := p.fValue;
  expectedNumArgs := functionObject.nArgs;

  // If it's a non-variable argument call check that the right
  // number of arguments are avaiable to the function
  if expectedNumArgs <> VARIABLE_ARGS then
     if actualNumArgs <> expectedNumArgs then
        raise ERuntimeException.Create('Expecting ' + inttostr (functionObject.nArgs) + ' arguments in function call but received ' + inttostr (actualNumArgs) + ' arguments');

  // Support for special builtins, eg Math
  if functionObject.isBuiltInFunction then
     begin
     callBuiltIn (expectedNumArgs, actualNumArgs, functionObject);
     exit;
     end;

  // Finally, its a normal method call to a real user defined function
  inc(frameStackTop);
  if frameStackTop > MAX_FRAME_DEPTH then
    raise ERuntimeException.Create ('Exceeded maximum recursion depth for functions');

  // Fill in the frame entries - some code optimizations first
  // reduces the amount of machine code generated
  tbsp := stackTop - functionObject.nArgs + 1;
  symTableCount := functionObject.localSymbolTable.count;
  nPureLocals := symTableCount - expectedNumArgs;
  // Fill in the stack frame
  with frameStack[frameStackTop] do
     begin
     nArgs := expectedNumArgs;
     nlocals := symTableCount;
     purelocals := nPureLocals;
     symbolTable := functionObject.localSymbolTable;
     bsp := tbsp;
     end;

  // Make space for the local variables (Note symbolTable contains arguments plus purelocals)
  stackTop := stackTop + symTableCount - expectedNumArgs;

  // Initialize the purelocals to undefined so we can detect assignments
  // from unassigned variables. The pure locals sit just above the arguments
  for i := 0 to nPureLocals - 1 do
      stack[tbsp + i + expectedNumArgs].stackType := stNone;

  // This check is to see if the argument is passed as a literal to the function
  // eg callme ([1,2,3,4]). Since a literal isn't assigned to anything
  // it is marked as garbage. However, if this enters a function, we must
  // make sure the garbage collector doesn't collect it since it will be
  // used inside the function.
  for i := 0 to expectedNumArgs - 1 do
      if stack[tbsp + i].stackType = stList then
         if stack[tbsp + i].lValue.blockType = btGarbage then
            stack[tbsp + i].lValue.blockType := btTemporary; // To stop the garbage collector

  // This is to make sure the user function knows what module its in.
  oldModule := module;
  try
    module := functionObject.moduleRef;

    // And finally we make the call
    run(functionObject.codeBlock, module.symbolTable);

    // deal with the special case where a user has passed a literal (list, array, etc)
    // which is not owned by anyone. Such arguments come in as btTemporary inorder
    // to prevent the garbage collector from freeing up the argument.
    for i := 0 to expectedNumArgs - 1 do
        if stack[tbsp + i].stackType = stList then
           if stack[tbsp + i].lValue.blockType = btTemporary then
              stack[tbsp + i].lValue.blockType := btGarbage;

  finally
     module := oldModule;
  end;
  // Note the function object will get popped off by the return code in run()
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
  result := TListObject.Create(count); // This also adds the list to the memory pool
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
      stObject:
        begin

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
        end;
      stArray:
        begin
        result.list[i].aValue := p.aValue.clone;
        result.list[i].aValue.blockType := btOwned;
        result.list[i].itemType := liArray;
        end;
      stFunction :
        begin
        result.list[i].fValue := p.fValue.clone;
        (result.list[i].fValue as TUserFunction).blockType := btOwned;
        result.list[i].itemType := liFunction;
        end
    else
      raise ERuntimeException.Create('Unrecognized type in createList');
    end;
    end;
end;


procedure TVM.storeIndexableString(variable: PMachineStackRecord; index: integer);
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
    raise ERuntimeException.Create ('can only assign a single string character to an indexed string');

  variable.sValue.value[index + 1] := value.sValue.value[1];
end;


procedure TVM.storeIndexableArray(variable: PMachineStackRecord; index: integer; nSubscripts : integer);
var value: double;
    idx : array of integer;
    i : integer;
begin
  value := popScalar;

  //if (index < 0) or (index > length(variable.sValue.value) - 1) then
  //  raise ERuntimeException.Create('string index out of range');

  if variable.stackType <> stArray then
    raise ERuntimeException.Create('left-hand side must be a array');

  //if subscriptStack.Count + 1 < nSubscripts then
  if uIntStack.getCount (subscriptStack) + 1 < nSubscripts then
     begin
     //subscriptStack.Push(index);
     uIntStack.Push(subscriptStack, index);
     push (variable);
     end
  else
     begin
     uIntStack.Push(subscriptStack, index);
     setLength (idx, uIntStack.getCount (subscriptStack));// HMMS subscriptStack.stackPtr);

     //subscriptStack.Push(index);
     //setLength (idx, subscriptStack.Count);// HMMS subscriptStack.stackPtr);
     // Index backwards since the stack entries are backwards
     //for i := subscriptStack.Count - 1 downto 0 do
     //    idx[i] := subscriptStack.Pop ();
     for i := uIntStack.getCount (subscriptStack) - 1 downto 0 do
         idx[i] := uIntStack.Pop(subscriptStack);

     variable.aValue.setValue (idx, value);

     uIntStack.clear (subscriptStack);
     end;
end;


procedure TVM.storeIndexableList(variable: PMachineStackRecord; index: integer);
var
  value: PMachineStackRecord;
  element: TListItem;
begin
  if (variable.lValue.list.count  < 0) or (index > variable.lValue.list.count - 1) then
     raise ERuntimeException.Create('index out of range: ' + inttostr (index));


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
    stFunction:
      begin
      element.fValue := value.fValue;
      element.itemType := liFunction;
      end;
  else
    raise ERuntimeException.Create('Unrecognized stacktype in storeIndexable: ' + TRttiEnumerationType.GetName (value.stackType));
  end
end;


procedure TVM.storeLocalIndexableList(st: PMachineStackRecord; index: integer);
var
  value: PMachineStackRecord;
  element: TListItem;
begin
  value := pop;

  if (index < 0) or (index > st.lValue.list.count - 1) then
     raise ERuntimeException.Create('list index out of range: ' + inttostr (index));

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
    stFunction:
      begin
      element.fValue := value.fValue;
      element.itemType := liFunction;
      end;  else
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
        // update the stack entry that holds the variable
      storeLocalIndexableString(variable, index);
      stack[index + bsp].sValue := variable.sValue;
    end;
  else
    raise ERuntimeException.Create(stToStr(variable.stackType) + ' local variable is not indexable');
  end;
end;


// Stack Structure to store indexed lists such as x[1,4,2]:
//
// index into indexable object
// index to symbol table containing indexable object
// the item we wish to store in the indexable object
// called by svecIdx
procedure TVM.storeIndexable (nSubscripts : integer);
var
  index : integer;
  symbol : PMachineStackRecord;
begin
  index := popInteger;
  symbol := pop;   // Comes from push symbol
  case symbol.stackType of
     stList   : storeIndexableList(symbol, index);
     stString : storeIndexableString(symbol, index);
     stArray  : storeIndexableArray(symbol, index, nSubscripts);
  else
    raise Exception.Create(stToStr(symbol.stackType) + ' variable is not indexable');
  end;

end;


// -------------------------------------------------------------------------------------------
// Loading Methods
// -------------------------------------------------------------------------------------------

procedure TVM.loadIndexableString(st: PMachineStackRecord; index: integer);
begin
  if (index < 0) or (index > length(st.sValue .value) - 1) then
    raise ERuntimeException.Create('string index out of range');
  push(TStringObject.Create(st.sValue.value[index + 1]));
end;


procedure TVM.loadIndexableArray(st: PMachineStackRecord; index: integer; nSubscripts : integer);
var idx : array of integer;
    i : integer;
begin
  // For an n dimensional array we will collect the subscripts.
  // HMSif subscriptStack.Count + 1 < nSubscripts then
  if uIntStack.getCount (subscriptStack) + 1 < nSubscripts then
     begin
     //subscriptStack.Push(index);
     uIntStack.Push(subscriptStack, index);
     push (st);
     end
  else
     begin
     if nSubscripts = 1 then
        begin
        // This needs to be relaxed so that users can extract portions of an array
        if st.aValue.getNumDimensions > 1 then
           raise ERuntimeException.Create('Array has more dimensions that specified in indexing');
        push (st.aValue.getValue(index));
        exit;
        end;

     //subscriptStack.Push(index);
     //setLength (idx, subscriptStack.Count);
     uIntStack.Push(subscriptStack, index);
     setLength (idx, uIntStack.getCount (subscriptStack));

     // Index backwards since the stack entries are backwards
     //for i := subscriptStack.Count - 1 downto 0 do
     //    idx[i] := subscriptStack.Pop();
     for i := uIntStack.getCount (subscriptStack)  - 1 downto 0 do
         idx[i] := uIntStack.Pop(subscriptStack);

     push (st.aValue.getValue (idx));
     uIntStack.clear (subscriptStack);
     end;
end;


procedure TVM.loadIndexableList(variable: PMachineStackRecord; index: integer);
var element: TListItem;
begin
  if (index < 0) or (index > variable.lValue.list.count - 1) then
    raise ERuntimeException.Create('list index out of range: ' + inttostr (index));

  element := variable.lValue.list[index];

  case element.itemType of
    liInteger: push(element.iValue);
    liBoolean: push(element.bValue);
    liDouble:  push(element.dValue);
    liString:  push(element.sValue);
    liList:    push(element.lValue);
    liFunction:push(TUserFunction (element.fValue));
  else
    raise ERuntimeException.Create ('internal error: unsupported type in loadIndexableList');
  end;
end;


// Stack Structure to access indexed lists or strings such as [1]:
// Reference to variable to be accessed
// Index into indexable object
// Called by oLvecIdx
procedure TVM.loadIndexable (nSubscripts : integer);
var
  index: integer;
  variable: PMachineStackRecord;
begin
  index := trunc (popScalar);
  variable := pop;
  case variable.stackType of
    stList  : loadIndexableList(variable, index);
    stString: loadIndexableString(variable, index);
    stArray : loadIndexableArray (variable, index, nSubscripts);
  else
    raise ERuntimeException.Create(stToStr(variable.stackType) +  ' variable is not indexable');
  end;
end;


procedure TVM.loadLocalIndexableList(aList: TListObject; index: integer);
var
  element: TListItem;
begin
  if (index < 0) or (index > aList.list.count - 1) then
    raise ERuntimeException.Create('list index out of range: ' + inttostr (index));

  // Get the first element of the list
  element := aList.list[index];

  case element.itemType of
    liInteger: push(element.iValue);
    liBoolean: push(element.bValue);
    liDouble:  push(element.dValue);
    liString:  push(element.sValue);
    liList:    push(element.lValue);
    liFunction:push(element.fValue as TUserFunction);
  else
    raise ERuntimeException.Create('unsupported type in loadIndexable');
  end;
end;


procedure TVM.loadLocalIndexableString(astr: TStringObject; index: integer);
begin
  if (index < 0) or (index >= length(astr.value) - 1) then
    raise ERuntimeException.Create('string index out of range');
  push(TStringObject.Create(astr.value[index + 1]));
end;


procedure TVM.loadLocalIndexableArray(arr: TArrayObject; index: integer);
begin
  raise ERuntimeException.Create('loadLocalIndexableArray not implemented');
  //if (index < 0) or (index >= length(arr.data) - 1) then
  //  raise ERuntimeException.Create('string index out of range');
  //push(TArrayObject.Create(arr.data[index]));
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
    raise ERuntimeException.Create(stToStr(st.stackType) + ' variable is not indexable');
end;


procedure TVM.importModule (moduleName : string);
var vm : TVM;
    module : TModule;
    symbol : TSymbol;
begin
  // Find the module
  if not symbolTable.find (moduleName, symbol) then
     raise ERuntimeException.Create('Symbol: ' + moduleName + ' could not be found.');
  if symbol.symbolType <> symModule then
     raise ERuntimeException.Create('Symbol: ' + moduleName + ' is not a module.');

  module := TModule (symbol.mValue);

  vm := TVM.Create;
  vm.printCallbackPtr := self.printCallbackPtr;
  vm.printlnCallbackPtr := self.printlnCallBackPtr;
  vm.setColorCallBackPtr :=self.setColorCallBackPtr;
  try
    vm.runModule (module);
  finally
    vm.Free;
  end;
end;


procedure TVM.buildSlice;
var lower, upper : integer;
    obj : PMachineStackRecord;
    p1, p2: PMachineStackRecord;
begin
  upper := popInteger;
  lower := popInteger;

  pushObject (TSliceObject.Create (lower, upper));
end;


procedure TVM.evalSliceObject (numSlices : integer);
var i : integer;
    obj : TSliceObject;
    sliceObjlist : TSliceObjectList;
    value : PMachineStackRecord;
begin
  setLength (sliceObjlist, numSlices);
  for i := numSlices - 1 downto 0  do
      sliceObjlist[i] := TSliceObject (pop.objValue);
  value := pop;

  case value.stackType of
     stString :
        begin
        if numSlices > 1 then
           raise ERuntimeException.Create('Only a single slice can be applied to a string');
        push (value.sValue.slice (sliceObjlist[0].lower, sliceObjlist[0].upper));
        end;
     stList :
        begin
        if numSlices > 1 then
           raise ERuntimeException.Create('Only a single slice can be applied to a list');
        push (value.lValue.slice (sliceObjlist[0].lower, sliceObjlist[0].upper));
        end;
     stArray :
        begin
        push (value.aValue.slice(sliceObjList));
        end
  else
     raise ERuntimeException.Create('You can only slice strings, arrays, or lists');
  end;

  // Free slices
  for i := 0 to numSlices - 1 do
      sliceObjlist[i].Free;
end;


// Any data ytpes such as strings or lists thatwere creating inside the user
// function will need to be marked as garbage once the function returns.
procedure TVM.clearAnyStackHeapAllocations;
var
  i: integer;
  purelocals, startOfLocals : integer;
begin
  purelocals := frameStack[frameStackTop].pureLocals;
  // We only check variables declared within the function, not the arguments coming in.
  // Arguments coming in could be owned by outside function variables.
  //for i := 2 + frameStack[frameStackTop].nArgs to frameStack[frameStackTop].nlocals do
  startOfLocals := 2 + frameStack[frameStackTop].nArgs;
  for i := startOfLocals to startOfLocals + purelocals - 1 do
    case stack[stackTop + i].stackType of
      stList:
        begin
        stack[stackTop + i].lValue.blockType := btGarbage; // Mark as garbage
        stack[stackTop + i].lValue := nil;
        end;
      stString:
        begin
        if not stack[stackTop + i].sValue.isConstant then // Not sure why it has to be constant
          begin
          stack[stackTop + i].sValue.blockType := btGarbage; // Mark as garbage
          stack[stackTop + i].sValue := nil;
          end;
        end;
      stDouble : begin end;
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
  memoryList.freeGarbage;
  //writeln ('Collecting....');
end;


function TVM.getGarbageSize : integer;
begin
  result := memoryList.getMemoryListSize;
end;

// -----------------------------------------------------------------------------

procedure TVM.runModule(module: TModule);
var vmstate : TVMState;
begin
  vmstate.module := self.module;
  vmstate.symbolTable := self.symbolTable;
  VMStateStack.push (vmstate);

  self.module := module;

  run(module.moduleProgram, module.symbolTable);

  vmstate := VMStateStack.Pop;
  self.module := vmstate.module;
  self.symbolTable := vmstate.symbolTable;
end;


// Use this method to run a user defined function
procedure TVM.run (code: TProgram; symbolTable : TSymbolTable);
var
  ip, g : integer;
  value : PMachineStackRecord;
  c : TCode;
begin
  ip := 0; {count := 0;} value := @noneStackType; bolStopVm := False;
  self.symboltable := symbolTable;
  if code.count = 0 then
     exit;

  //Stopwatch := TStopwatch.StartNew;
  try
    c := code.code;
    while True do
        begin
        if bolStopVm then
           begin
           issueMessage ('Ctrl-C Interrupt Detected');
           // Need to issue exception here inorder to get out completly
           exit;
           end;

          case c[ip].opCode of
            oNop:        begin end;
            oAdd:        addOp;
            oSub:        subOp;
            oMult:       multOp;
            oDivide:     divOp;
            oDivi:       divideIntOp;
            oDotProduct: dotProductOp;
            oUmi:        unaryMinusOp;
            oMod:        modOp;
            oPower:      powerOp;
            oInc:        incOp(c[ip].symbolName, c[ip].float);
            oLocalInc:   localIncOp(c[ip].index, c[ip].float);
            oDec:        decOp(c[ip].symbolName, c[ip].float);
            oLocalDec:   localDecOp(c[ip].index, c[ip].float);
            oPushi:      push (c[ip].index);
            oPushb:      push (boolean(c[ip].index));
            oPushd:      push (module.moduleProgram.constantValueTable[c[ip].index].dValue);
            oPushs:      push (module.moduleProgram.constantValueTable[c[ip].index].sValue);
            oPushNone:   push (@noneStackType);
            oPop:        begin
                         // If the next instrction is halt, we will leave the item
                         // on the stack and let the caller deal with it. This is
                         // mainly useful when used in interactive mode so that the
                         // console can print the stack item to the console, any
                         // other time we want to the pop executed
                         if c[ip+1].opCode <> oHalt then
                            pop();
                         end;
            oDup:        dupStack;
            oPopDup:     pop();
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
            oSetColor:    setColor;
            oAssertTrue:  assertTrue;
            oAssertFalse: assertFalse;

            // Branch opcodes
            oJmp:        ip := ip + c[ip].index - 1;
            oJmpIfTrue:  if pop().bValue then
                            ip := ip + c[ip].index - 1;
            oJmpIfFalse: if not pop().bValue then
                            ip := ip + c[ip].index - 1;


        // These two are used when we load and store synmbols within the current module
         oStoreSymbol:  begin
                         storeSymbol (c[ip].symbolName);
                         g := getGarbageSize;
                         if (g > 50) then
                            collectGarbage; // Only collect garbage after a store
                         end;
          oLoadSymbol: loadSymbol (c[ip].symbolName);

       // This are used to load and store symbols when we reference modules outside the current one
          oLoadAttr: loadAttr (c[ip].symbolName);
          oStoreAttr: storeAttr (c[ip].symbolName);

       // These are used to load and store symbols in user functions
          oStoreLocal:  begin
                        storeLocalSymbol(c[ip].index);
                        if getGarbageSize > 10 then
                           collectGarbage;
                        end;
           oLoadLocal:  loadLocalSymbol(c[ip].index);

          oBuildSlice : buildSlice();
           oSliceAll  : push(SLICE_ALL);
           oSliceObj  : evalSliceObject (c[ip].index);

        oImportModule: importModule (c[ip].moduleName);

          // Method call opcodes
            oCall :     callUserFunction (c[ip].index);
            oRet :
              begin
              // Note that anything that is returned isn't bound to any symbol.
              // Binding only happens if the callee assigns the
              // returned value to a symbol.
              value := pop();
              case value.stackType of
                stString: value.sValue := value.sValue.clone;
                stList:   value.lValue := value.lValue.clone;
              end;
              stackTop := stackTop - frameStack[frameStackTop].nlocals - 1;  // -1 to also remove the function object
              // This can only be called after the stackTop has been adjusted
              clearAnyStackHeapAllocations;

              dec(frameStackTop);
              push(value);
              exit;
              end;

            // list handling opcodes
            oCreateList:   push(createList(c[ip].index));
            oLvecIdx:      loadIndexable (c[ip].index);
            oSvecIdx:      storeIndexable (c[ip].index);

            oLocalLvecIdx: loadLocalIndexable;
            oLocalSvecIdx: storeLocalIndexable;

            oHalt: break;

            oPopAndSend:   callBack(pop);  // Not currently used
          else
            raise ERuntimeException.Create('Unknown opcode encountered in virtual machine execution loop: ' + inttostr(c[ip].opCode));
          end;
          inc(ip);
        end;
   finally
    //Stopwatch.stop;
    //vm_elapsedTime := Stopwatch.ElapsedMilliseconds;
  end;
end;

initialization
end.

