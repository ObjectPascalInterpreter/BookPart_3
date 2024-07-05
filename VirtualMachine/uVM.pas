// Developed using Delphi for Windows and Mac platforms.

// *** This source is distributed under Apache 2.0 ***

// Copyright (C) 2019-2020 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

{DEFINE STACKCHECK}

unit uVM;

interface

Uses System.SysUtils, System.Diagnostics, System.TimeSpan, uUtils,
  uMachineStack, System.generics.Collections,
  uListObject,
  uStringObject,
  uArrayObject,
  uVectorObject,
  uMatrixObject,
  uValueObject,
  uSymbolTable,
  uConstantTable,
  uProgramCode,
  uMemoryManager,
  uObjectSupport,
  uIntStack,
  uJumpTables;

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

  TVMCaptureStringCallBack = procedure (const astr : AnsiString);
  TVMReadStringCallBack  = function (const prompt : AnsiString) : PAnsiChar;

  TVMCallBack         = procedure(st: PMachineStackRecord) of object;
  TVMPrintCallBack    = procedure (const astr : AnsiString);
  TVMSetColorCallBack = function (st: PMachineStackRecord) : AnsiString;
  TVMDebugCallback    = procedure (vm : TObject);

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
    lineNumber : integer;  // set to the source code line number associated with the current bytecode
    bolDebugger : boolean;

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

    function  createList(count: integer): TListObject;

    function  createVector (count : integer) : TVectorObject;
    function  createMatrix (count : integer) : TMatrixObject;

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

    procedure compatibilityError (const arg: string; st1, st2: PMachineStackRecord);

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
    procedure storeIndexableMatrix(variable: PMachineStackRecord; index: integer; nSubscripts : integer);

    procedure storeLocalIndexable;
    procedure storeLocalIndexableString(st: PMachineStackRecord; index: integer);
    procedure storeLocalIndexableList(st: PMachineStackRecord; index: integer);

    procedure loadIndexable (nSubscripts : integer);
    procedure loadIndexableList(variable: PMachineStackRecord; index: integer);
    procedure loadIndexableString(st: PMachineStackRecord; index: integer);
    procedure loadIndexableArray(st: PMachineStackRecord; index: integer; nSubscripts : integer);
    procedure loadIndexableMatrix(st: PMachineStackRecord; index: integer; nSubscripts : integer);

    procedure loadLocalIndexable;
    procedure loadLocalIndexableList (aList: TListObject; index: integer);
    procedure loadLocalIndexableString (astr: TStringObject; index: integer);

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
    printCallbackPtr       : TVMPrintCallBack;
    printlnCallbackPtr     : TVMPrintCallBack;
    setColorCallBackPtr    : TVMCaptureStringCallBack;
    readStringCallbackPtr  : TVMReadStringCallBack;
    debugCallBackPtr       : TVMDebugCallBack;

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
    procedure   push (vValue: TVectorObject); overload; inline;
    procedure   push (mValue: TMatrixObject);  overload; inline;
    procedure   push (voValue: TValueObject);  overload; inline;
    procedure   push(fValue: TUserFunction); overload; inline;
    procedure   push(oValue : TMethodDetails); overload;
    procedure   push (module : TModule); overload; inline;
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
    procedure   raiseError (msg: string);

    procedure   callUserFunction (actualNumArgs : integer);

    function    popInteger: integer; inline;
    function    popBoolean: boolean;
    function    popScalar: double;
    function    popArray : TArrayObject;
    function    popVector : TVectorObject;
    function    popMatrix : TMatrixObject;
    function    popString : TStringObject;
    function    popList : TListObject;
    function    popUserFunction : TUserFunction;
    function    popMethodDetails: TMethodDetails;
    function    popModule: TModule;

    procedure   setRecursionLimit (rl : integer);
    function    getRecursionLimit : integer;

    procedure   run(code: TProgram; symbolTable : TSymbolTable);
    procedure   runModule(module: TModule);

    procedure   enableDebugging;
    procedure   setDebuggerFlag (flag : boolean);
    function    getDebuggerFlag : boolean;
    function    getStackInfo : TStackInfo;  // for debuggin purposes)
    procedure   setcallBack(proc: TVMCallBack);
    procedure   setDebugCallBack(proc: TVMDebugCallback);
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
  debugCallBackPtr := nil;
  assertCounter := 1;
  interactive := False;
  bolDebugger := False;
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


procedure TVM.enableDebugging;
begin
  bolDebugger := True;
end;

procedure TVM.setDebuggerFlag (flag : boolean);
begin
  bolDebugger := flag;
end;

function TVM.getDebuggerFlag : boolean;
begin
  result := bolDebugger;
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

procedure TVM.setDebugCallBack(proc: TVMDebugCallback);
begin
  debugCallBackPtr := proc;
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
                      result := AnsiString (Format(fmt, [st.iValue]));
                      end;
           stDouble : begin
                      fmt := SysLibraryRef.find ('doubleFormat').sValue.value;
                      result := AnsiString (Format(fmt, [st.dValue]));
                      end;
          stString  : result := AnsiString (st.sValue.value);
          stBoolean : if st.bValue = True then
                         result := 'True'
                      else
                         result := 'False';
          stList    : begin
                      result := AnsiString (st.lValue.listToString);
                      end;
          stArray   : result := AnsiString (st.aValue.arrayToString());

          stMatrix  : result := AnsiString (st.mValue.matrixToString());

      stValueObject : result := AnsiString (st.voValue.valueToString());

          stModule  : begin
                      result := AnsiString (st.module.name);
                      end;
          stFunction: begin
                      result := AnsiString (st.fValue.name);
                      end
     else
        result := 'Unrecognized value type from print' + sLineBreak;
     end;
end;


procedure TVM.raiseError (msg : string);
begin
  raise ERuntimeException.Create(msg + ' at line number: ' + inttostr (lineNumber));
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
     setColorCallbackPtr (AnsiString (color.sValue.value));
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
    raiseError ('Expecting boolean data type when using AssertTrue function');
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
    raiseError ('Expecting boolean data type when using AssertFalse function');
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
    raiseError ('Stack underflow error in pop');
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
    stMatrix: stack[stackTop].mValue := entry.mValue;
    stValueObject: stack[stackTop].voValue := entry.voValue;
  else
    raiseError ('Unknown type in dup method');
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
  result := 0;
  if stackTop > -1 then
  begin
    p := @stack[stackTop];
    dec(stackTop);
    if p.stackType <> stInteger then
      raiseError ('Expecting integer type');
    result := p.iValue;
  end
  else
    raiseError ('Stack underflow error in popInteger');
end;


function TVM.popBoolean: boolean;
var
  p: PMachineStackRecord;
begin
  result := False;
  if stackTop > -1 then
  begin
    p := @stack[stackTop];
    dec(stackTop);
    if p.stackType <> stBoolean then
      raiseError ('Expecting boolean type');
    result := p.bValue;
  end
  else
    raiseError ('Stack underflow error in popBoolean');
end;


function TVM.popArray : TArrayObject;
var
  p: PMachineStackRecord;
begin
  result := nil;
  if stackTop > -1 then
     begin
     p := @stack[stackTop];
     dec(stackTop);
     if p.stackType <> stArray then
        raiseError ('Expecting array type');
     result := p.aValue;
     end
  else
     raiseError ('Stack underflow error in popString');
 end;


function TVM.popVector : TVectorObject;
var
  p: PMachineStackRecord;
begin
  result := nil;
  if stackTop > -1 then
     begin
     p := @stack[stackTop];
     dec(stackTop);
     if p.stackType <> stVector then
        raiseError ('Expecting vector type');
     result := p.vValue;
     end
  else
     raiseError ('Stack underflow error in popVector');
 end;


function TVM.popMatrix : TMatrixObject;
var
  p: PMachineStackRecord;
begin
  result := nil;
  if stackTop > -1 then
     begin
     p := @stack[stackTop];
     dec(stackTop);
     if p.stackType <> stMatrix then
        raiseError ('Expecting matrix type');
     result := p.mValue;
     end
  else
     raiseError ('Stack underflow error in popMatrix');
 end;


function TVM.popString : TStringObject;
var
  p: PMachineStackRecord;
begin
  result := nil;
  if stackTop > -1 then
     begin
     p := @stack[stackTop];
     dec(stackTop);
     if p.stackType <> stString then
        raiseError ('Expecting string type');
     result := p.sValue;
     end
  else
     raiseError ('Stack underflow error in popString');
 end;


function TVM.popList : TListObject;
var p: PMachineStackRecord;
begin
  result := nil;
  if stackTop > -1 then
     begin
     p := @stack[stackTop];
     dec(stackTop);
     if p.stackType <> stList then
        raiseError ('Expecting list type');
     result := p.lValue;
     end
 else
     raiseError ('Stack underflow error in popList');
 end;


function TVM.popUserFunction : TUserFunction;
var p: PMachineStackRecord;
begin
  result := nil;
  if stackTop > -1 then
     begin
     p := @stack[stackTop];
     dec(stackTop);
     if p.stackType <> stFunction then
        raiseError ('Expecting function type');
     result := p.fValue;
     end
 else
     raiseError ('Stack underflow error in popUserFunction');
 end;


function TVM.popMethodDetails: TMethodDetails;
var p: PMachineStackRecord;
begin
  result := nil;
  if stackTop > -1 then
     begin
     p := @stack[stackTop];
     dec(stackTop);
     if p.stackType <> stObjectMethod then
        raiseError ('Expecting object method type');
     result := p.oValue;
     end
 else
     raiseError ('Stack underflow error in popUserFunction');
 end;


function TVM.popModule: TModule;
var p: PMachineStackRecord;
begin
  result := nil;
  if stackTop > -1 then
     begin
     p := @stack[stackTop];
     dec(stackTop);
     if p.stackType <> stModule then
        raiseError ('Expecting module type but found: ' + stToStr (p.stackType));

     result := p.module;
     end
  else
    raiseError ('Stack underflow error in popModule');
end;


function TVM.popScalar: double;
var p: PMachineStackRecord;
begin
  result := 0.0;
  if stackTop > -1 then
     begin
     p := @stack[stackTop];
     dec(stackTop);
     if p.stackType = stInteger then
        exit(p.iValue);
     if p.stackType = stDouble then
        exit(p.dValue);
     if p.stackType = stValueObject then
        begin
        case p.voValue.valueType of
           vtInteger : exit (p.voValue.iValue);
           vtDouble : exit (p.voValue.dValue);
        end;
        end;

     raiseError ('Expecting integer or double type');
     end
  else
    raiseError ('Stack underflow error in popScalar');
end;


procedure TVM.checkStackOverflow;
begin
  if stackTop = stacksize then
    raiseError ('Stack overflow error: ' + inttostr(stackTop)
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
    stVector: stack[stackTop].vValue := value.vValue;
    stMatrix: stack[stackTop].mValue := value.mValue;
    stValueObject: stack[stackTop].voValue := value.voValue;
    stFunction : stack[stackTop].fValue := value.fValue;
  else
    raise ERuntimeException.Create('Internal Error: Unknown type in push method');
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


procedure TVM.push (vValue: TVectorObject);
begin
  inc(stackTop);
  {$IFDEF STACKCHECK}
  checkStackOverflow;
{$ENDIF}
  stack[stackTop].vValue := vValue;
  stack[stackTop].stackType := TStackType.stVector;
end;


procedure TVM.push (mValue: TMatrixObject);
begin
  inc(stackTop);
  {$IFDEF STACKCHECK}
  checkStackOverflow;
{$ENDIF}
  stack[stackTop].mValue := mValue;
  stack[stackTop].stackType := TStackType.stMatrix;
end;


procedure TVM.push (voValue: TValueObject);
begin
  inc(stackTop);
  {$IFDEF STACKCHECK}
  checkStackOverflow;
{$ENDIF}
  stack[stackTop].voValue := voValue;
  stack[stackTop].stackType := TStackType.stValueObject;
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


procedure TVM.push (module : TModule);
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

procedure TVM.compatibilityError (const arg: string; st1, st2: PMachineStackRecord);
begin
  raiseError (stToStr(st1.stackType) + ' and ' +
    stToStr(st2.stackType) + ' cannot be used with the ' + arg + ' operation');
end;


// Addition
procedure TVM.addOp;
var
  st1, st2 : PMachineStackRecord;
  result : TMachineStackRecord;
begin
  st2 := pop; // second operand
  st1 := pop; // first operand

  addJumpTable[st1.stackType, st2.stackType] (st2, st1, result);
  push (@result);
end;


// Subtraction
procedure TVM.subOp;
var
  st1, st2: PMachineStackRecord;
  result : TMachineStackRecord;
begin
  st1 := pop;
  st2 := pop;
  if (st1.stackType = stNone) or (st2.stackType = stNone) then
     raiseError ('Variable undefined in sub');

  subJumpTable[st1.stackType, st2.stackType] (st2, st1, result);
  push (@result);
end;


// Multiplication
procedure TVM.multOp;
var
  st1, st2: PMachineStackRecord;
  result : TMachineStackRecord;
begin
  st1 := pop;
  st2 := pop;
  if (st1.stackType = stNone) or (st2.stackType = stNone) then
     raiseError  ('Variable undefined in mult');

  multJumpTable[st1.stackType, st2.stackType] (st2, st1, result);
  push (@result);
end;


procedure TVM.dotProductOp;
var m1, m2 : TMatrixObject;
    sum : double; 
    st1, st2: PMachineStackRecord;
begin
  st1 := pop;
  st2 := pop;

  case st1.stackType of
      stMatrix :
         case st2.stackType of
           stMatrix :
              push (TMatrixObject.dotmult (st2.mValue, st1.mValue));
         end;
  end;

//  m2 := popMatrix;
//  m1 := popMatrix;
//
//  if (m1.getNumDimensions() = 1) and (m2.getNumDimensions() = 1) then
//     begin
//     // Dot product of two vectors
//     if m1.dim[0] = m2.dim[0] then
//        begin
//        sum := 0;
//        for var i := 0 to m1.dim[0] - 1 do
//            sum := sum + m1.dataf[i] * m2.dataf[i];
//        push (sum);
//        exit;
//        end
//     else
//        raiseError ('Dot product requires both vectros to be the same size');
//     end;
//
//  // Otherwise its matrix-matrix multiplication
//  push (TBuiltInMatrix.dotMatMatMult(m1, m2));
end;


// Division
procedure TVM.divOp;
var
  st1, st2: PMachineStackRecord;
  result : TMachineStackRecord;
begin
  st1 := pop;
  st2 := pop;
  if (st1.stackType = stNone) or (st2.stackType = stNone) then
     raise ERuntimeException.Create ('Variable undefined in division operation');

  divJumpTable[st1.stackType, st2.stackType] (st2, st1, result);
  push (@result);
end;


procedure TVM.unaryMinusOp;
var
  st: PMachineStackRecord;
begin
  st := pop;
  case st.stackType of
    stInteger: push(-st.iValue);
    stDouble: push(-st.dValue);
    stMatrix: push (TMatrixObject.minus (st.mValue));
    stArray: push (TArrayObject.minus (st.aValue));
  else
    raiseError ('Data type not supported by unary operator');
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
     raiseError  ('Variable undefined in divideInt');

  case st2.stackType of
    stInteger: case st2.stackType of
        stInteger: push(st2.iValue div st1.iValue);
      else
        raiseError  ('Expecting integer type with div operator');
      end
  else
    raiseError ('Expecting integer type with div operator');
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
     raiseError  ('Variable undefined in mod');

  case st2.stackType of
    stInteger:
      begin
      case st1.stackType of
        stInteger: push(st2.iValue mod st1.iValue);
        stDouble : push(Math.FMod (st2.iValue, st1.dValue));
        stValueObject : push (Math.FMod (st2.iValue, TValueObject.getValue(st1.voValue)));
      else
         raiseError ('Incompatible types in mod operation');
      end;
      end;
    stDouble:
      begin
        case st1.stackType of
         stInteger : push(Math.FMod (st2.dValue, st1.iValue));
         stDouble  : push(Math.FMod (st2.dValue, st1.dValue));
        stValueObject : push (Math.FMod (st2.dValue, TValueObject.getValue(st1.voValue)));
        else
          raiseError ('Incompatible types in mod operation');
        end;
      end;
    stValueObject :
     case st1.stackType of
        stInteger: push(Math.FMod (TValueObject.getValue(st2.voValue), st1.iValue));
        stDouble: push(Math.FMod (TValueObject.getValue(st2.voValue), st1.dValue));
        stValueObject : push(Math.FMod (TValueObject.getValue(st2.voValue), TValueObject.getValue(st1.voValue)));
     else
        compatibilityError('dividing', st2, st1);
     end;
  else
    raiseError ('Incompatible types in mod operation');
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
    raiseError ('Internal error: Illegal use of incBy on a non-integer/double type');
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
    raiseError ('Internal error: Illegal use of incBy on a non-integer/double type');
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
    raiseError ('Internal error: Illegal use of decBy on non-integer/double type');
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
    raiseError ('Internal error: Illegal use of incBy on non-integer/double type');
  end;
end;


procedure TVM.powerOp;
var
  st1, st2: PMachineStackRecord;
begin
  st1 := pop;
  st2 := pop;
  if (st1.stackType = stNone) or (st2.stackType = stNone) then
     raiseError ('Variable undefined in power');

  case st2.stackType of
    stInteger: case st1.stackType of
        stInteger: push(power(st2.iValue, st1.iValue));
        stDouble: push(power(st2.iValue, st1.dValue));
        stValueObject : push (power (st2.iValue, TValueObject.getValue (st1.voValue)));
      else
        compatibilityError('power', st2, st1);
      end;
    stDouble: case st1.stackType of
        stInteger: push(power(st2.dValue, st1.iValue));
        stDouble: push(power(st2.dValue, st1.dValue));
        stValueObject : push (power (st2.dValue, TValueObject.getValue (st1.voValue)));
      else
        compatibilityError('exponential', st2, st1);
      end;
  stValueObject :
     case st1.stackType of
        stInteger: push(power (TValueObject.getValue(st2.voValue), st1.iValue));
        stDouble: push(power (TValueObject.getValue(st2.voValue), st1.dValue));
        stValueObject : push(power (TValueObject.getValue(st2.voValue), TValueObject.getValue(st1.voValue)));
      else
        compatibilityError('dividing', st2, st1);
       end
  else
    raiseError ('Data type not supported by power operator');
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
  ar : TArrayObject;
begin
  value := pop();

  if not module.symbolTable.find (symbolName, symbol) then
     raiseError ('Undeclared variable: ' + symbolName + ' in module: ' + module.name);

   case value.stackType of
    stNone:     begin symbol.symbolType := symUndefined; end;
    stInteger:  module.symbolTable.storeInteger (symbol, value.iValue);
    stDouble:   module.symbolTable.storeDouble (symbol, value.dValue);
    stBoolean:  module.symbolTable.storeBoolean (symbol, value.bValue);
    stString:   module.symbolTable.storeString (symbol, value.sValue);
    stList:     module.symbolTable.storeList   (symbol, value.lValue);
    stArray:    module.symbolTable.storeArray   (symbol, value.aValue);
    stVector:   module.symbolTable.storeVector   (symbol, value.vValue);
    stMatrix:   module.symbolTable.storeMatrix   (symbol, value.mValue);
 stValueObject: module.symbolTable.storeValueObject (symbol, value.voValue);
    stFunction: module.symbolTable.storeFunction (symbol, value.fValue);

    stModule:   begin
                //module.symbolTable.storeModule(symbol, value.module);
                raiseError ('You cannot store a module in a variable: ' + value.module.name);
                end;
    stObjectMethod:
       raiseError ('You cannot store an object method in a variable: ' + value.oValue.name);
  else
    raiseError ('Internal error: Unrecognized stacktype in storeSymbol: ' + TRttiEnumerationType.GetName(value.stackType));
  end;
end;


// This will load any kind of symbol on to the stack
procedure TVM.loadSymbol (symbolName : string);
var symbol : TSymbol;
begin
  if not module.symbolTable.find (symbolName, symbol) then
     raiseError ('Undefined symbol: ' + symbolName);

 case symbol.symbolType of
    symUndefined: raiseError('Variable has no assigned value: ' + symbol.symbolName);
    symInteger:   push(symbol.iValue);
    symDouble:    push(symbol.dValue);
    symBoolean:   push(symbol.bValue);
    symString:    push(symbol.sValue);
    symList:      push(symbol.lValue);
    symArray:     push(symbol.aValue);
    symVector:     push(symbol.vValue);
    symMatrix:     push(symbol.matValue);
    symUserFunc:  begin
                  symbol.fValue.moduleRef := module;
                  push(symbol.fValue);
                  end;
    symModule:    push(TModule (symbol.mValue));
  else
    raiseError('Unknown symbol type in loadSymbol: ' +  inttostr(integer(symbol.symbolType)));
  end;
end;


// For an expresson such as X.a
// X is called the primary symbol
// a is caled the secondary symbol
// The stack will be:
//   primary (X)
//   secondary (a)
// A primary can be a module, a function, a list (eg a.len), a string (eg a.len), matrix, builtin constant

procedure TVM.loadAttr (symbolName : string);
var m : TModule;
    l : TListObject;
    s : TStringObject;
    a : TArrayObject;
    mat : TMatrixObject;
    vo : TValueObject;
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
                 raiseError  ('Undefined symbol: ' + symbolName);

              case symbol.symbolType of
                symUndefined:   raiseError ('Variable has no assigned value: ' + symbol.symbolName);
                symInteger:     push(symbol.iValue);
                symDouble:      push(symbol.dValue);
                symBoolean:     push(symbol.bValue);
                symString:      push(symbol.sValue);
                symList:        push(symbol.lValue);
                symArray:       push(symbol.aValue);
                symValueObject: push(symbol.voValue);
                symUserFunc:    begin
                                symbol.fValue.moduleRef := m;
                                push(symbol.fValue);
                                end;
                symModule:     push(TModule (symbol.mValue));
              else
                raiseError ('Unknown symbol type in loadAttr: ' +  inttostr(integer(symbol.symbolType)));
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
                 raiseError ('No method <' + symbolName + '> associated with object');
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
                 raiseError ('No method <' + symbolName + '> associated with object');
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
                 raiseError('No method <' + symbolName + '> associated with object');
              end;

    stMatrix : begin
              mat := primary.mValue;
              // Is symbol name a function name?
              f := mat.methods.methodList.find (symbolName);
              if f <> nil then
                 begin
                 f.self := primary.mValue;
                 push (f);
                 end
              else
                 raiseError('No method <' + symbolName + '> associated with object');
              end;

stValueObject : begin
              vo := primary.voValue;
              // Is symbol name a function name?
              f := vo.methods.methodList.find (symbolName);
              if f <> nil then
                 begin
                 f.self := primary.voValue;
                 push (f);
                 end
              else
                 raiseError('No method <' + symbolName + '> associated with object');
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
                 raiseError('No method <' + symbolName + '> associated with object');
              end
  else
     raiseError ('The attribute has no associated help. Only modules, functions, builtin constants, strings, arrays, matrices or lists have attributes');
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
     raiseError ('Undefined symbol: ' + symbolName);

  if symbol.locked then
     raiseError ('This symbol is locked, you cannot change it: ' + symbolName);

  case value.stackType of
    stNone:     begin symbol.symbolType := symUndefined; end;
    stInteger:  begin
                if symbol.symbolType = TSymbolElementType.symValueObject then
                   begin
                   symbol.voValue.iValue := value.iValue;
                   symbol.voValue.valueType := vtInteger;
                   end
                else
                   begin
                   symbol.symbolType := symInteger;
                   symbol.iValue := value.iValue;
                   end;
                end;
    stDouble:   begin
                if symbol.symbolType = TSymbolElementType.symValueObject then
                   begin
                   symbol.voValue.dValue := value.dValue;
                   symbol.voValue.valueType := vtDouble;
                   end
                else
                   begin
                   symbol.symbolType := symDouble;
                   symbol.dValue := value.dValue;
                   end;
                end;
    stBoolean:  begin symbol.symbolType := symBoolean; symbol.bValue := value.bValue; end;
    stString:   m.symbolTable.storeString (symbol, value.sValue);
    stList:     m.symbolTable.storeList   (symbol, value.lValue);
    stArray:    m.symbolTable.storeArray   (symbol, value.aValue);
    stMatrix:   m.symbolTable.storeMatrix   (symbol, value.mValue);
    stValueObject:   m.symbolTable.storeValueObject(symbol, value.voValue);
    stFunction: m.symbolTable.storeFunction (symbol, value.fValue);
    stModule:   m.symbolTable.storeModule (symbol, value.module);
  else
    raiseError ('Internal error: Unrecognized stacktype in storeAttr: ' + TRttiEnumerationType.GetName(symbol.symbolType));
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
    stMatrix:
        begin
        stack[stackTop].stackType := stMatrix;
        stack[stackTop].mValue := stackElement.mValue;
        end;
    stValueObject:
        begin
        stack[stackTop].stackType := stValueObject;
        stack[stackTop].voValue := stackElement.voValue;
        end;
    stFunction:
        begin
        stack[stackTop].stackType := stFunction;
        stack[stackTop].fValue := stackElement.fValue;
        end;
    stNone:
      // ###
      raiseError ('Undefined variable: ' + frame.symbolTable[index].symbolName);
  else
    raiseError  ('Internal Error: Unknown symbol in copyToStack');
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

  if (stack[bsp + index].stackType = stMatrix) and (stack[bsp + index].mValue <> nil) then
      stack[bsp + index].mValue.blockType := btGarbage; // Mark as garbage

  if (stack[bsp + index].stackType = stValueObject) and (stack[bsp + index].voValue <> nil) then
      stack[bsp + index].voValue.blockType := btGarbage; // Mark as garbage

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
    stMatrix:
      begin
      stack[bsp + index].mValue := value.mValue.clone;
      stack[bsp + index].mValue.blockType := btBound;
      stack[bsp + index].stackType := stMatrix;
      end;
    stValueObject:
      begin
      stack[bsp + index].voValue := value.voValue.clone;
      stack[bsp + index].voValue.blockType := btBound;
      stack[bsp + index].stackType := stValueObject
      end;
    stNone:
      raiseError  ('No value to assign in function');
  else
    raiseError ('Unknown symbol type in storeLocalValue');
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
    raiseError ('Incompatible data types ''Is Less or Equal Than'' Operation');
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
        stNone : raiseError  ('Undefined value in greater than tests');
      end;
    stDouble: case st2.stackType of
        stInteger: push(st2.iValue < st1.dValue);
        stDouble: push(st2.dValue < st1.dValue);
        stNone : raiseError ('Undefined value in greater than tests');
      end;
  else
    raiseError ('Incompatible types in ''Is Less Than'' Operation');
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
    raiseError ('Incompatible types in ''Is Greater Than Operation''');
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
        raiseError  ('Boolean data types not permitted in isGte comparison');
      end;
    stDouble: case st2.stackType of
        stInteger: push(st2.iValue >= st1.dValue);
        stDouble: push(st2.dValue >= st1.dValue);
      end
  else
    raiseError ('Incompatible types in ''Is Greater or Equal Than Operation''');
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
        stValueObject : push (st2.voValue.isEqualTo(st1.iValue));
      else
        raiseError ('Incompatible types in equality test');
      end;
    stDouble:
      case st2.stackType of
        stInteger: push(st2.iValue = st1.dValue);
        stDouble:  push(math.sameValue(st2.dValue, st1.dValue));
        stValueObject : push (st2.voValue.isEqualTo(st1.dValue));
      else
        raiseError ('Incompatible types in equality test');
      end;
    stString:
      begin
      if st2.stackType = stString then
         push(st1.sValue.isEqualTo (st2.sValue))
      else
         raiseError ('Incompatible types in equality test');
      end;
    stValueObject:
      begin
      if st2.stackType = stDouble then
         push(st1.voValue.isEqualTo (st2.dValue))
      else
         raiseError ('Incompatible types in equality test');
      end;
    stList:
      begin
      if st2.stackType = stList then
        push(TListObject.listEquals(st1.lValue, st2.lValue))
      else
        raiseError  ('Unable to test for equality between lists and non-lists data types');
      end;
    stArray :
      if st2.stackType = stArray then
         push (TArrayObject.isEqualTo(st1.aValue, st2.aValue))
      else
        raiseError  ('Unable to test for equality between arrays and non-arrays data types');
    stMatrix :
      if st2.stackType = stMatrix then
         push (TMatrixObject.isEqualTo(st1.mValue, st2.mValue))
      else
        raiseError  ('Unable to test for equality between matrices and non-arrays data types');
  else
    raiseError  ('Incompatible types in equality test');
  end;
end;


procedure TVM.andOp;
var
  st1, st2: PMachineStackRecord;
begin
  st1 := pop;
  st2 := pop;
  if (st1.stackType <> stBoolean) or (st2.stackType <> stBoolean) then
     raiseError ('Incompatible types in AND operation');
  push(st1.bValue and st2.bValue);
end;


procedure TVM.orOp;
var
  st1, st2: PMachineStackRecord;
begin
  st1 := pop;
  st2 := pop;
  if (st1.stackType <> stBoolean) or (st2.stackType <> stBoolean) then
     raiseError ('Incompatible types in OR operation');
  push(st1.bValue or st2.bValue);
end;


procedure TVM.xorOp;
var
  st1, st2: PMachineStackRecord;
begin
  st1 := pop;
  st2 := pop;
  if (st1.stackType <> stBoolean) or (st2.stackType <> stBoolean) then
     raiseError ('Incompatible types in XOR operation');
  push(st1.bValue xor st2.bValue);
end;


procedure TVM.notOp;
var
  st: PMachineStackRecord;
begin
  st := pop;
  if (st.stackType <> stBoolean) then
     raiseError ('Incompatible types in NOT operation');
  push(not st.bValue);
end;


procedure TVM.callBack(st: PMachineStackRecord);
begin
  if Assigned(callbackPtr) then
    callbackPtr(st);
end;


// Call something like a.len()
procedure TVM.callObjectMethod (actualNumArgs : integer; p : PMachineStackRecord);
begin
  if p.oValue.nArgs <> VARIABLE_ARGS then
     if actualNumArgs <> p.oValue.nArgs then
        raiseError ('Expecting ' + inttostr (p.oValue.nArgs) + ' arguments in function call [' + p.oValue.name + '] but received ' + inttostr (actualNumArgs) + ' arguments');

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
     raiseError (stToStr (p.stackType) + ' is not something that can be called as a function');

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
        raiseError ('Expecting ' + inttostr (functionObject.nArgs) + ' arguments in function call but received ' + inttostr (actualNumArgs) + ' arguments');

  // Support for special builtins, eg Math
  if functionObject.isBuiltInFunction then
     begin
     callBuiltIn (expectedNumArgs, actualNumArgs, functionObject);
     exit;
     end;

  // Finally, its a normal method call to a real user defined function
  inc(frameStackTop);
  if frameStackTop > MAX_FRAME_DEPTH then
    raiseError ('Exceeded maximum recursion depth for functions');

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


// -----------------------------------------------------------------------------
// Matrix support routines
// -----------------------------------------------------------------------------

function TVM.CreateVector (count : integer) : TVectorObject;
var
  i: integer;
  p: PMachineStackRecord;
begin
  result := TVectorObject.Create;
  result.blockType := btGarbage;

  result.size := count;
  for i := count - 1 downto 0 do
    begin
    p := pop;
    case p.stackType of
      stInteger:
        begin
        result[i] := p.iValue;
        end;
      stDouble:
        begin
        result[i] := p.dValue;
        end;
    else
       raiseError ('Vectors can only contain integers or floats');
    end;
    end;
end;



function TVM.createMatrix (count : integer) : TMatrixObject;
var
  i: integer;
  p: PMachineStackRecord;
  vec : TVectorObject;
begin
  result := TMatrixObject.Create;
  result.blockType := btGarbage;

  result.setNumRows (count);
  for i := count - 1 downto 0 do
      begin
      vec := popVector;
      result.addRow (i, vec);
      end;
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
      stMatrix:
        begin
        result.list[i].mValue := p.mValue.clone;
        result.list[i].mValue.blockType := btOwned;
        result.list[i].itemType := liMatrix
        end;
      stValueObject:
        begin
        result.list[i].voValue := p.voValue.clone;
        result.list[i].voValue.blockType := btOwned;
        result.list[i].itemType := liValueObject;
        end;
      stFunction :
        begin
        result.list[i].fValue := p.fValue.clone;
        (result.list[i].fValue as TUserFunction).blockType := btOwned;
        result.list[i].itemType := liFunction;
        end
    else
      raiseError('Unrecognized type in createList');
    end;
    end;
end;


procedure TVM.storeIndexableString(variable: PMachineStackRecord; index: integer);
var
  value: PMachineStackRecord;
begin
  value := pop;

  if (index < 0) or (index > length(variable.sValue.value) - 1) then
    raiseError ('string index out of range');

  if variable.stackType <> stString then
    raiseError ('left-hand side must be a string');

  if value.stackType <> stString then
    raiseError ('right-hand side must be a string');

  if length(value.sValue.value) > 1 then
    raiseError ('can only assign a single string character to an indexed string');

  variable.sValue.value[index + 1] := value.sValue.value[1];
end;


procedure TVM.storeIndexableArray(variable: PMachineStackRecord; index: integer; nSubscripts : integer);
var value: double;
    idx : array of integer;
    i : integer;
begin
  value := popScalar;

  if variable.stackType <> stArray then
    raiseError ('left-hand side must be a array');

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


procedure TVM.storeIndexableMatrix(variable: PMachineStackRecord; index: integer; nSubscripts : integer);
var value: double;
    idx : array of integer;
    i : integer;
begin
  value := popScalar;

  if variable.stackType <> stMatrix then
    raiseError ('left-hand side must be a array');

  //if subscriptStack.Count + 1 < nSubscripts then
  if uIntStack.getCount (subscriptStack) + 1 < nSubscripts then
     begin
     uIntStack.Push(subscriptStack, index);
     push (variable);
     end
  else
     begin
     uIntStack.Push(subscriptStack, index);
     setLength (idx, uIntStack.getCount (subscriptStack));// HMMS subscriptStack.stackPtr);

     // Index backwards since the stack entries are backwards
     for i := uIntStack.getCount (subscriptStack) - 1 downto 0 do
         idx[i] := uIntStack.Pop(subscriptStack);

     variable.mValue.setVal (idx[0], idx[1], value);

     uIntStack.clear (subscriptStack);
     end;
end;

procedure TVM.storeIndexableList(variable: PMachineStackRecord; index: integer);
var
  value: PMachineStackRecord;
  element: TListItem;
begin
  if (variable.lValue.list.count  < 0) or (index > variable.lValue.list.count - 1) then
     raiseError ('index out of range: ' + inttostr (index));


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
    stMatrix:
      begin
      element.mValue := value.mValue.clone;
      element.mValue.blockType := btOwned;
      element.itemType := liMatrix;
      end;
    stValueObject:
      begin
      element.voValue := value.voValue.clone;
      element.voValue.blockType := btOwned;
      element.itemType := liValueObject;
      end;
    stFunction:
      begin
      element.fValue := value.fValue;
      element.itemType := liFunction;
      end;
  else
    raiseError ('Unrecognized stacktype in storeIndexable: ' + TRttiEnumerationType.GetName (value.stackType));
  end
end;


procedure TVM.storeLocalIndexableList(st: PMachineStackRecord; index: integer);
var
  value: PMachineStackRecord;
  element: TListItem;
begin
  value := pop;

  if (index < 0) or (index > st.lValue.list.count - 1) then
     raiseError ('list index out of range: ' + inttostr (index));

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
     stMatrix:
      begin
      element.mValue := value.mValue.clone;
      element.mValue.blockType := btOwned;
      element.itemType := liMatrix;
      end;
     stValueObject:
      begin
      element.voValue := value.voValue.clone;
      element.voValue.blockType := btOwned;
      element.itemType := liValueObject ;
      end;
    stFunction:
      begin
      element.fValue := value.fValue;
      element.itemType := liFunction;
      end;  else
    raiseError ('Unrecognized stacktype in storeLocalIndexable');
  end
end;


procedure TVM.storeLocalIndexableString(st: PMachineStackRecord;
  index: integer);
var
  value: PMachineStackRecord;
begin
  value := pop;

  if (index < 0) or (index > length(st.sValue.value) - 1) then
    raiseError ('string index out of range');

  if value.stackType <> stString then
    raiseError ('can only assign a single string character to an indexed string');

  if length(value.sValue.value) > 1 then
    raiseError ('can only assign a single string character to an indexed string');
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
    raiseError (stToStr(variable.stackType) + ' local variable is not indexable');
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
  if peek.stackType <> stInteger then
     raiseError ('index must be an integer while accessing lists, arrays or strings');

  index := popInteger;
  symbol := pop;   // Comes from push symbol
  case symbol.stackType of
     stList   : storeIndexableList(symbol, index);
     stString : storeIndexableString(symbol, index);
     stArray  : storeIndexableArray(symbol, index, nSubscripts);
     stMatrix : storeIndexableMatrix(symbol, index, nSubscripts);
  else
    raiseError (stToStr(symbol.stackType) + ' variable is not indexable');
  end;

end;


// -------------------------------------------------------------------------------------------
// Loading Methods
// -------------------------------------------------------------------------------------------

procedure TVM.loadIndexableString(st: PMachineStackRecord; index: integer);
begin
  if (index < 0) or (index > length(st.sValue .value) - 1) then
    raiseError ('string index out of range');
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
           raiseError ('Array has more dimensions that specified in indexing');
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


procedure TVM.loadIndexableMatrix(st: PMachineStackRecord; index: integer; nSubscripts : integer);
var idx : array of integer;
    i : integer;
begin
  if nSubscripts < 2 then
     raise ERuntimeException.Create ('You need two subscripts to access a matrix element');

  // For an n dimensional array we will collect the subscripts.
  // HMS if subscriptStack.Count + 1 < nSubscripts then
  if uIntStack.getCount (subscriptStack) + 1 < nSubscripts then
     begin
     uIntStack.Push(subscriptStack, index);
     push (st);
     end
  else
     begin
     uIntStack.Push(subscriptStack, index);
     setLength (idx, uIntStack.getCount (subscriptStack));

     // Index backwards since the stack entries are backwards
     for i := uIntStack.getCount (subscriptStack)  - 1 downto 0 do
         idx[i] := uIntStack.Pop(subscriptStack);

     push (st.mValue.getVal (idx[0], idx[1]));
     uIntStack.clear (subscriptStack);
     end;
end;


procedure TVM.loadIndexableList(variable: PMachineStackRecord; index: integer);
var element: TListItem;
begin
  if (index < 0) or (index > variable.lValue.list.count - 1) then
    raiseError('list index out of range: ' + inttostr (index));

  element := variable.lValue.list[index];

  case element.itemType of
    liInteger: push(element.iValue);
    liBoolean: push(element.bValue);
    liDouble:  push(element.dValue);
    liString:  push(element.sValue);
    liList:    push(element.lValue);
    liArray:   push(element.aValue);
    liMatrix:  push(element.mValue);
    liFunction:push(TUserFunction (element.fValue));
  else
    raiseError ('internal error: unsupported type in loadIndexableList');
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
    stMatrix : loadIndexableMatrix (variable, index, nSubscripts);
  else
    raiseError (stToStr(variable.stackType) +  ' variable is not indexable');
  end;
end;


procedure TVM.loadLocalIndexableList(aList: TListObject; index: integer);
var
  element: TListItem;
begin
  if (index < 0) or (index > aList.list.count - 1) then
    raiseError ('list index out of range: ' + inttostr (index));

  // Get the first element of the list
  element := aList.list[index];

  case element.itemType of
    liInteger:     push(element.iValue);
    liBoolean:     push(element.bValue);
    liDouble:      push(element.dValue);
    liString:      push(element.sValue);
    liList:        push(element.lValue);
    liArray:       push(element.aValue);
    liValueObject: push(element.voValue);
    liFunction:    push(element.fValue as TUserFunction);
  else
    raiseError('unsupported type in loadIndexable');
  end;
end;


procedure TVM.loadLocalIndexableString(astr: TStringObject; index: integer);
begin
  if (index < 0) or (index >= length(astr.value) - 1) then
    raiseError('string index out of range');
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
    raiseError (stToStr(st.stackType) + ' variable is not indexable');
end;


procedure TVM.importModule (moduleName : string);
var vm : TVM;
    module : TModule;
    symbol : TSymbol;
begin
  // Find the module
  if not symbolTable.find (moduleName, symbol) then
     raiseError('Symbol: ' + moduleName + ' could not be found.');
  if symbol.symbolType <> symModule then
     raiseError ('Symbol: ' + moduleName + ' is not a module.');

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
begin
  upper := popInteger;
  lower := popInteger;

  pushObject (TSliceObject.Create (lower, upper));
end;

 // Stack has a slice list followed by the object we want to slice (value)
procedure TVM.evalSliceObject (numSlices : integer);
var i : integer;
    sliceObjlist : TSliceObjectList;
    value : PMachineStackRecord;
begin
  setLength (sliceObjlist, numSlices);
  for i := numSlices - 1 downto 0  do
      sliceObjlist[i] := TSliceObject (pop.objValue);
  value := pop;

  try

  case value.stackType of
     stString :
        begin
        if numSlices > 1 then
           raiseError ('Only a single slice can be applied to a string');
        push (value.sValue.slice (sliceObjlist[0].lower, sliceObjlist[0].upper));
        end;
     stList :
        begin
        if numSlices > 1 then
           raiseError ('Only a single slice can be applied to a list');
        push (value.lValue.slice (sliceObjlist[0].lower, sliceObjlist[0].upper));
        end;
     stArray :
        begin
        push (value.aValue.slice(sliceObjList));
        end;
     stMatrix :
        begin
        push (value.mValue.slice(sliceObjList));
        end
  else
     raiseError ('You can only slice strings, arrays, or lists');
  end;

  finally
    // Free slices
    for i := 0 to length (sliceObjList) - 1 do
        sliceObjlist[i].Free;
  end;
end;


// Any data types such as strings or lists that were creating inside the user
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
      stInteger :
         begin
         end;
      stDouble :
         begin
         end;
      stMatrix :
         begin
         stack[stackTop + i].mValue.blockType := btGarbage; // Mark as garbage
         stack[stackTop + i].mValue := nil;
         end;
      stValueObject :
         begin
         stack[stackTop + i].voValue.blockType := btGarbage; // Mark as garbage
         stack[stackTop + i].voValue := nil;
         end;
      stNone :
         begin

         end
      else
        raise ERuntimeException.Create('Other types not implemented in clearAnyStackHeapAllocations: ' + stToStr (stack[stackTop + i].stackType));
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
  alist : TListObject;
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
        lineNumber := c[ip].lineNumber;

        if bolStopVm then
           begin
           issueMessage ('Ctrl-C Interrupt Detected');
           // Need to issue exception here in order to get out completly
           exit;
           end;

        if bolDebugger then
           begin
           if Assigned (debugCallBackPtr) then
              debugCallBackPtr (self);
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
                stMatrix : value.mValue := value.mValue.clone;
                stNone : begin end;
                stInteger : begin end;
                stDouble : begin end;
                stBoolean : begin end;
              else
                 raiseError('oRet not implemented for type');
              end;
              // Clear the pushed arguments from the stack
              stackTop := stackTop - frameStack[frameStackTop].nlocals - 1;  // -1 to also remove the function object
              // This can only be called after the stackTop has been adjusted
              clearAnyStackHeapAllocations;

              dec(frameStackTop);
              push(value); // push the result back on to the stack
              exit;
              end;

            // list handling opcodes
            oCreateList:   push(createList(c[ip].index));
            oLvecIdx:      loadIndexable (c[ip].index);
            oSvecIdx:      storeIndexable (c[ip].index);

            oLocalLvecIdx: loadLocalIndexable;
            oLocalSvecIdx: storeLocalIndexable;

            oCreateVector  : push (createVector (c[ip].index));
            oCreateMatrix  : push (createMatrix (c[ip].index));
            //oAddMatrixItem : addMatrixItem;

            oHalt: break;

            oPopAndSend:   callBack(pop);  // Not currently used
          else
            raiseError('Unknown opcode encountered in virtual machine execution loop: ' + inttostr(c[ip].opCode));
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

