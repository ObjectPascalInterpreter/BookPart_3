// Developed using Delphi for Windows and Mac platforms.

// *** This source is distributed under Apache 2.0 ***

// Copyright (C) 2019-2020 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

// Turn this on (add $) if you want pop and push to be be checked

{DEFINE STACKCHECK}

unit uVM;

interface

Uses System.SysUtils,
  System.Diagnostics,
  System.TimeSpan,
  uUtils,
  uMachineStack,
  System.generics.Collections,
  uListObject,
  uStringObject,
  uArrayObject,
  uVectorObject,
  uMatrixObject,
  uValueObject,
  uDataObject,
  uSymbolTable,
  uConstantTable,
  uProgramCode,
  uMemoryManager,
  uDataObjectMethods,
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

  // Input/Output callnbacks ,makes it easier to host the
  // system in another application
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

  // Vitual Machine class
  TVM = class(TObject)
  private
    stack: TMachineStack;
    stackTop: integer;
    stacksize: integer;

    symbolTable : TSymbolTable;
    VMStateStack : TStack<TVMState>;
    subscriptStack : uIntStack.TStack;     // lightweight integer stack

    frameStackTop: integer;
    frameStack: TFrameStack;

    callbackPtr: TVMCallBack;

    module: TModule;

    assertCounter: integer;
    bolStopVm : boolean;
    lineNumber : integer;  // set to the source code line number associated with the current bytecode
    bolDebugger : boolean;
    bolCountOpCodes : boolean;  // Experiment to count opcode usage

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

    // Calls thing likes len in  a.len()
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
    // Attributes are anything after a period, eg len in a.len()
    procedure loadAttr (symbolName : string);
    procedure storeAttr (symbolName : string);

    procedure loadLocalSymbol(index: integer);

    // Sotre and load methods for different objects
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
    procedure loadLocalIndexableString (astr: TDataObject; index: integer);

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
    procedure  recordOpCodeCounts (opcode : TByteCode);
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
    procedure   push(obj: TMatrixObject);  overload; inline;
    procedure   push(voValue: TValueObject);  overload; inline;
    procedure   push(obj: TDataObject); overload; inline;
    procedure   push(oValue : TMethodDetails); overload;
    procedure   push(module : TModule); overload; inline;
    procedure   pushNone;
    procedure   pushSliceObject (obj : TObject);
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
    // runModule called run above
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
  // Diagnostic variables
  vm_elapsedTime: double;
  nTotalTests : integer;
  countOpCodes : array[0..255] of integer;

implementation

Uses uOpCodes,
     Math,
     uRhodusTypes,
     uVMExceptions,
     Rtti,
     uHelpUnit,
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
  uIntStack.create (subscriptStack, uIntStack.MAX_STACK_ENTRIES);

  callbackPtr := nil;
  printCallbackPtr := nil;
  printlnCallbackPtr := nil;
  debugCallBackPtr := nil;
  assertCounter := 1;
  interactive := False;
  bolDebugger := False;
  bolCountOpCodes := False;
end;


destructor TVM.Destroy;
begin
  freeStack;
  freeFrameStack;
  VMStateStack.Free;
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
    sym : TSymbol;
begin
  if st <> nil then
     case st.stackType of
          symNonExistant  : result := AnsiString ('non-existant value in toStr');
          symInteger : begin
                      sym := SysLibraryRef.find ('integerFormat');
                      if sym <> nil then
                         begin
                         fmt := (sym.dataObject as TStringObject).value;
                         result := AnsiString (Format(fmt, [st.iValue]));
                         end
                      else
                         raiseError('integerFormat variable in module sys not found');
                      end;
           symDouble : begin
                      sym := SysLibraryRef.find ('doubleFormat');
                      if sym <> nil then
                         begin
                         fmt := (sym.dataObject as TStringObject).value;
                         result := AnsiString (Format(fmt, [st.dValue]));
                         end
                      else
                         raiseError('doubleFormat variable in module sys not found');
                      end;
          symString  : result := AnsiString (TStringObject(st.dataObject).value);
          symBoolean : if st.bValue = True then
                         result := 'True'
                      else
                         result := 'False';
          symList    : result := AnsiString (st.dataObject.ToString);

          symArray   : result := AnsiString (st.dataObject.ToString());

          symMatrix  : result := AnsiString (st.dataObject.ToString());

      symValueObject : result := AnsiString (st.dataObject.ToString());

          symModule  : result := AnsiString (st.module.moduleName);

          symUserFunc: result := AnsiString (TUserFunction (st.dataObject).methodName)
     else
        result := 'Unrecognized value type in toStr' + sLineBreak;
     end;
end;


procedure TVM.raiseError (msg : string);
begin
  raise ERuntimeException.Create('Runtime Error' + sLineBreak +
                '        --- at line number: ' + inttostr (lineNumber) + sLineBreak +
                '        --- in ' + module.moduleName + sLineBreak + msg);
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
  stRecord.dataObject := TStringObject.Create(sLineBreak);  // Don't free, garbage collector will handle it.
  stRecord.stackType := symString;
  if Assigned(printCallbackPtr) then
    printCallbackPtr(sLineBreak);
end;


procedure TVM.setColor;
var color : PMachineStackRecord;
begin
  color := pop;
  if Assigned (setColorCallbackPtr) then
     setColorCallbackPtr (AnsiString (TStringObject(color.dataObject).value));
end;


procedure TVM.assertTrue;
var
  st: PMachineStackRecord;
begin
  st := pop;
  if st.stackType = symBoolean then
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
  if st.stackType = symBoolean then
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


// Stack manipulation methods

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


// Duplicate the stack entry
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
    symNonExistant:
    begin
    end;
    symInteger: stack[stackTop].iValue := entry.iValue;
    symBoolean: stack[stackTop].bValue := entry.bValue;
    symDouble: stack[stackTop].dValue := entry.dValue;
    symString: stack[stackTop].dataObject := entry.dataObject;
    symList: stack[stackTop].dataObject := entry.dataObject;
    symArray: stack[stackTop].dataObject := entry.dataObject;
    symMatrix: stack[stackTop].dataObject := entry.dataObject;
    symValueObject: stack[stackTop].dataObject := entry.dataObject;
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
    if p.stackType <> symInteger then
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
    if p.stackType <> symBoolean then
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
     if p.stackType <> symArray then
        raiseError ('Expecting array type');
     result := TArrayObject (p.dataObject);
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
     if p.stackType <> symVector then
        raiseError ('Expecting vector type');
     result := TVectorObject (p.dataObject);
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
     if p.stackType <> symMatrix then
        raiseError ('Expecting matrix type');
     result := p.dataObject as TMatrixObject;
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
     if p.stackType <> symString then
        raiseError ('Expecting string type');
     result := TStringObject(p.dataObject);
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
     if p.stackType <> symList then
        raiseError ('Expecting list type');
     result := TListObject (p.dataObject);
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
     if p.stackType <> symUserFunc then
        raiseError ('Expecting function type');
     result := TUserFunction (p.dataObject);
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
     if p.stackType <> symObjectMethod then
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
     if p.stackType <> symModule then
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
     if p.stackType = symInteger then
        exit(p.iValue);
     if p.stackType = symDouble then
        exit(p.dValue);
     if p.stackType = symValueObject then
        begin
        case (p.dataObject as TValueObject).valueType of
           vtInteger : exit ((p.dataObject as TValueObject).iValue);
           vtDouble : exit ((p.dataObject as TValueObject).dValue);
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
    symNonExistant:  begin end;
    symInteger: stack[stackTop].iValue := value.iValue;
    symBoolean: stack[stackTop].bValue := value.bValue;
    symDouble : stack[stackTop].dValue := value.dValue;

    // All dataObjects
    symString..symUserFunc : stack[stackTop].dataObject := value.dataObject;

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
  stack[stackTop].stackType := symInteger;
end;


procedure TVM.push(bValue: boolean);
begin
  inc(stackTop);
{$IFDEF STACKCHECK}
  checkStackOverflow;
{$ENDIF}
  stack[stackTop].bValue := bValue;
  stack[stackTop].stackType := symBoolean;
end;


procedure TVM.push(dValue: double);
begin
  inc(stackTop);
{$IFDEF STACKCHECK}
  checkStackOverflow;
{$ENDIF}
  stack[stackTop].dValue := dValue;
  stack[stackTop].stackType := symDouble;
end;


procedure TVM.push (obj: TMatrixObject);
begin
  inc(stackTop);
  {$IFDEF STACKCHECK}
  checkStackOverflow;
{$ENDIF}
  stack[stackTop].dataObject := obj;
  stack[stackTop].stackType := symMatrix;
end;


procedure TVM.push (voValue: TValueObject);
begin
  inc(stackTop);
  {$IFDEF STACKCHECK}
  checkStackOverflow;
{$ENDIF}
  stack[stackTop].dataObject := voValue;
  stack[stackTop].stackType := symValueObject;
end;

procedure TVM.push (obj: TDataObject);
begin
  inc(stackTop);
  {$IFDEF STACKCHECK}
  checkStackOverflow;
{$ENDIF}
  stack[stackTop].dataObject := obj;
  stack[stackTop].stackType := obj.objectType;
end;


procedure TVM.push(oValue : TMethodDetails);
begin
  inc(stackTop);
  {$IFDEF STACKCHECK}
  checkStackOverflow;
{$ENDIF}
  stack[stackTop].oValue := oValue;
  stack[stackTop].stackType := symObjectMethod;
end;


procedure TVM.push (module : TModule);
begin
  inc(stackTop);
{$IFDEF STACKCHECK}
  checkStackOverflow;
{$ENDIF}
  stack[stackTop].module := module;
  stack[stackTop].stackType := symModule;
end;


procedure TVM.pushNone;
begin
  push (@noneStackType);
end;


procedure TVM.pushSliceObject (obj : TObject);
begin
  inc(stackTop);
{$IFDEF STACKCHECK}
  checkStackOverflow;
{$ENDIF}
  stack[stackTop].sliceValue := obj;
  stack[stackTop].stackType := symSliceObject;
end;

// -------------------------------------------------------------------------

procedure TVM.compatibilityError (const arg: string; st1, st2: PMachineStackRecord);
begin
  raiseError (stToStr(st1.stackType) + ' and ' +
    stToStr(st2.stackType) + ' cannot be used with the ' + arg + ' operation');
end;


// opCode methods

// Addition
procedure TVM.addOp;
var
  st1, st2 : PMachineStackRecord;
  result : TMachineStackRecord;
begin
  st2 := pop; // second operand
  st1 := pop; // first operand

  try
    addJumpTable[st1.stackType, st2.stackType] (st2, st1, result);
    push (@result);
  except
     on e: exception do
        raiseError(e.message);
  end;
end;


// Subtraction
procedure TVM.subOp;
var
  st1, st2: PMachineStackRecord;
  result : TMachineStackRecord;
begin
  st1 := pop;  // second operand
  st2 := pop;  // first operand
  if (st1.stackType = symNonExistant) or (st2.stackType = symNonExistant) then
     raiseError ('Variable undefined in sub');

  try
    subJumpTable[st1.stackType, st2.stackType] (st2, st1, result);
    push (@result);
  except
     on e: exception do
        raiseError(e.message);
  end;
end;


// Multiplication
procedure TVM.multOp;
var
  st1, st2: PMachineStackRecord;
  result : TMachineStackRecord;
begin
  st1 := pop;
  st2 := pop;
  if (st1.stackType = symNonExistant) or (st2.stackType = symNonExistant) then
     raiseError  ('Variable undefined in mult');

  try
    multJumpTable[st1.stackType, st2.stackType] (st2, st1, result);
    push (@result);
  except
     on e: exception do
        raiseError(e.message);
  end;
end;


// Implements matrix multiplicaton operator @, as in c = a@b
procedure TVM.dotProductOp;
var st1, st2: PMachineStackRecord;
begin
  st1 := pop;
  st2 := pop;

  case st1.stackType of
      symMatrix :
         case st2.stackType of
           symMatrix :
              push (TMatrixObject.dotmult (TMatrixObject (st2.dataObject), TMatrixObject (st1.dataObject)));
      else
         raiseError ('Dot product must be between two matrices');
         end;
  end;
end;


// Division
procedure TVM.divOp;
var
  st1, st2: PMachineStackRecord;
  result : TMachineStackRecord;
begin
  st1 := pop;
  st2 := pop;
  if (st1.stackType = symNonExistant) or (st2.stackType = symNonExistant) then
     raise ERuntimeException.Create ('Variable undefined in division operation');

  try
    divJumpTable[st1.stackType, st2.stackType] (st2, st1, result);
    push (@result);
  except
     on e: exception do
        raiseError(e.message);
  end;
end;


procedure TVM.unaryMinusOp;
var
  st: PMachineStackRecord;
begin
  st := pop;
  case st.stackType of
    symInteger: push(-st.iValue);
    symDouble : push(-st.dValue);
    symMatrix : push (TMatrixObject.minus (TMatrixObject (st.dataObject)));
    symArray  : push (TArrayObject.minus (TArrayObject(st.dataObject)));
    symValueObject : push (TValueObject.minus (TValueObject(st.dataObject)));
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
  if (st1.stackType = symNonExistant) or (st2.stackType = symNonExistant) then
     raiseError  ('Variable undefined in divideInt');

  case st2.stackType of
    symInteger: case st2.stackType of
        symInteger: push(st2.iValue div st1.iValue);
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
  if (st1.stackType = symNonExistant) or (st2.stackType = symNonExistant) then
     raiseError  ('Variable undefined in mod');

  case st2.stackType of
    symInteger:
      begin
      case st1.stackType of
        symInteger: push(st2.iValue mod st1.iValue);
        symDouble : push(Math.FMod (st2.iValue, st1.dValue));
        symValueObject : push (Math.FMod (st2.iValue, TValueObject.getValue(st1.dataObject as TValueObject)));
      else
         raiseError ('Incompatible types in mod operation');
      end;
      end;
    symDouble:
      begin
        case st1.stackType of
         symInteger : push(Math.FMod (st2.dValue, st1.iValue));
         symDouble  : push(Math.FMod (st2.dValue, st1.dValue));
        symValueObject : push (Math.FMod (st2.dValue, TValueObject.getValue(st1.dataObject as TValueObject)));
        else
          raiseError ('Incompatible types in mod operation');
        end;
      end;
    symValueObject :
     case st1.stackType of
        symInteger: push(Math.FMod (TValueObject.getValue(st2.dataObject as TValueObject), st1.iValue));
        symDouble: push(Math.FMod (TValueObject.getValue(st2.dataObject as TValueObject), st1.dValue));
        symValueObject : push(Math.FMod (TValueObject.getValue(st2.dataObject as TValueObject), TValueObject.getValue(st1.dataObject as TValueObject)));
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
    symInteger: inc(sy.iValue);
    symDouble: sy.dValue := sy.dValue + step;
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
    symInteger: dec(sy.iValue);
    symDouble: sy.dValue := sy.dValue - step;
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
  if (st1.stackType = symNonExistant) or (st2.stackType = symNonExistant) then
     raiseError ('Variable undefined in power');

  case st2.stackType of
    symInteger: case st1.stackType of
        symInteger: push(power(st2.iValue, st1.iValue));
        symDouble: push(power(st2.iValue, st1.dValue));
        symValueObject : push (power (st2.iValue, TValueObject.getValue (st1.dataObject as TValueObject)));
      else
        compatibilityError('power', st2, st1);
      end;
    symDouble: case st1.stackType of
        symInteger: push(power(st2.dValue, st1.iValue));
        symDouble: push(power(st2.dValue, st1.dValue));
        symValueObject : push (power (st2.dValue, TValueObject.getValue (st1.dataObject as TValueObject)));
      else
        compatibilityError('power', st2, st1);
      end;
  symValueObject :
     case st1.stackType of
        symInteger: push(power (TValueObject.getValue(st2.dataObject as TValueObject), st1.iValue));
        symDouble: push(power (TValueObject.getValue(st2.dataObject as TValueObject), st1.dValue));
        symValueObject : push(power (TValueObject.getValue(st2.dataObject as TValueObject), TValueObject.getValue(st1.dataObject as TValueObject)));
      else
        compatibilityError('power', st2, st1);
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
begin
  value := pop();

  if not module.symbolTable.find (symbolName, symbol) then
     raiseError ('Undeclared variable: ' + symbolName + ' in module: ' + module.moduleName);

   case value.stackType of
    symNonExistant:     begin symbol.symbolType := symUndefined; end;
    symInteger:  module.symbolTable.storeInteger (symbol, value.iValue);
    symDouble:   module.symbolTable.storeDouble (symbol, value.dValue);
    symBoolean:  module.symbolTable.storeBoolean (symbol, value.bValue);
    symString:   module.symbolTable.storeString (symbol, value.dataObject as TStringObject);
    symList:     module.symbolTable.storeList   (symbol, value.dataObject as TListObject);
    symArray:    module.symbolTable.storeArray   (symbol, value.dataObject as TArrayObject);
    symVector:   module.symbolTable.storeVector   (symbol, value.dataObject as TVectorObject);
    symMatrix:   module.symbolTable.storeMatrix   (symbol, value.dataObject as TMatrixObject);
 symValueObject: module.symbolTable.storeValueObject(symbol, value.dataObject as TValueObject);
    symUserFunc: module.symbolTable.storeFunction(symbol, value.dataObject as TUserFunction);

    //symObject:   module.symbolTable.storeObject (symbol, value.dataObject);
    symModule:   begin
                //module.symbolTable.storeModule(symbol, value.module);
                raiseError ('You cannot store a module in a variable: ' + value.module.moduleName);
                end;
    symObjectMethod:
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
    symUndefined:  raiseError('Variable has no assigned value: ' + symbol.symbolName);
    symInteger:    push(symbol.iValue);
    symDouble:     push(symbol.dValue);
    symBoolean:    push(symbol.bValue);
    symString:     push(symbol.dataObject);
    symList:       push(symbol.dataObject);
    symArray:      push(symbol.dataObject);
    symVector:     push(symbol.dataObject);
    symMatrix:     push(symbol.dataObject);
    //symObject:     push(symbol.dataObject);
    symValueObject:push((symbol.dataObject as TValueObject).getScalar());
    symUserFunc:  begin
                  (symbol.dataObject as TUserFunction).moduleRef := module;
                  push(symbol.dataObject);
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
    mat : TMatrixObject;
    vo : TValueObject;
    symbol : TSymbol;
    primary : PMachineStackRecord;
    f : TMethodDetails;
    data : TDataObject;
    methodDetails : TMethodDetails;
begin
  primary := pop();
  case primary.stackType of
   symModule : begin
              m := primary.module;

              if not m.symbolTable.find (symbolName, symbol) then
                 raiseError  ('Undefined symbol: ' + symbolName);

              case symbol.symbolType of
                symUndefined:   raiseError ('Variable has no assigned value: ' + symbol.symbolName);
                symInteger:     push(symbol.iValue);
                symDouble:      push(symbol.dValue);
                symBoolean:     push(symbol.bValue);
                symString:      push(symbol.dataObject);
                symList:        push(symbol.dataObject);
                symArray:       push(symbol.dataObject);
                symObject:      push (symbol.dataObject);
                symValueObject: push(symbol.dataObject);
                symUserFunc:    begin
                                (symbol.dataObject as TUserFunction).moduleRef := m;
                                push(symbol.dataObject);
                                end;
                symModule:     push(TModule (symbol.mValue));
              else
                raiseError ('Unknown symbol type in loadAttr: ' +  inttostr(integer(symbol.symbolType)));
              end;
              end;

     symList : begin
              data := primary.dataObject;
              // Is symbol name a function name?
              f := data.methods.methodList.find (symbolName);
              if f <> nil then
                 begin
                 f.self := primary.dataObject;
                 push (f);
                 end
              else
                 raiseError ('No method <' + symbolName + '> associated with object');
              end;

     symString : begin
              data := primary.dataObject;
              // Is symbol name a function name?
              f := data.methods.methodList.find (symbolName);
              if f <> nil then
                 begin
                 f.self := primary.dataObject;
                 push (f);
                 end
              else
                 raiseError ('No method <' + symbolName + '> associated with object');
              end;

     symArray : begin
              data := primary.dataObject;
              // Is symbol name a function name?
              f := data.methods.methodList.find (symbolName);
              if f <> nil then
                 begin
                 f.self := primary.dataObject;
                 push (f);
                 end
              else
                 raiseError('No method <' + symbolName + '> associated with object');
              end;

    symMatrix : begin
              mat := TMatrixObject (primary.dataObject);
              // Is symbol name a function name?
              f := mat.methods.methodList.find (symbolName);
              if f <> nil then
                 begin
                 f.self := primary.dataObject;
                 push (f);
                 end
              else
                 raiseError('No method <' + symbolName + '> associated with object');
              end;

symValueObject : begin
              vo := primary.dataObject as TValueObject;
              // Is symbol name a function name?
              f := vo.methods.methodList.find (symbolName);
              if f <> nil then
                 begin
                 f.self := primary.dataObject as TValueObject;
                 push (f);
                 end
              else
                 raiseError('No method <' + symbolName + '> associated with object');
              end;

   symUserFunc :
              begin
              data := primary.dataObject;
              // Is symbol name a function name?
              f := TUserFunction(data).userFunctionMethods.methodList.find (symbolName);
              if f <> nil then
                 begin
                 f.self := primary.dataObject;
                 push (f);
                 end
              else
                 raiseError('No method <' + symbolName + '> associated with object');
              end;
   symObjectMethod :
             begin
             if symbolName = 'help' then
                raiseError ('Help cannot be used with data objects. To get help on a method, use for example var.help ("nameOfMethod")');

             methodDetails := primary.oValue;
             push (methodDetails);
             end;
   symInteger, symDouble, symBoolean:
             begin
             // Let's fake the value as a valueObject so that we can get
             // it called via the valueObjects help method.
             vo := TValueObject.Create(33);
             vo.blockType := btGarbage;
             vo.help := THelp.Create;
             case primary.stackType  of
               symInteger : vo.help.description := 'This is an integer that has a value of: ' + inttostr (primary.iValue);
               symDouble : vo.help.description := 'This is a double that has a value of: ' + floattostr (primary.dValue);
               symBoolean : vo.help.description :=  'This is a boolean that has a value of: '+ BoolToStr(primary.bValue, True);
             end;
             // Is symbol name a function name?
             f := vo.methods.methodList.find (symbolName);
             if f <> nil then
                begin
                f.self := vo;
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
    symNonExistant:     begin symbol.symbolType := symUndefined; end;
    symInteger:  begin
                if symbol.symbolType = TElementType.symValueObject then
                   begin
                   (symbol.dataObject as TValueObject).iValue := value.iValue;
                   (symbol.dataObject as TValueObject).valueType := vtInteger;
                   end
                else
                   begin
                   symbol.symbolType := symInteger;
                   symbol.iValue := value.iValue;
                   end;
                end;
    symDouble:   begin
                if symbol.symbolType = TElementType.symValueObject then
                   begin
                   (symbol.dataObject as TValueObject).dValue := value.dValue;
                   (symbol.dataObject as TValueObject).valueType := vtDouble;
                   end
                else
                   begin
                   symbol.symbolType := symDouble;
                   symbol.dValue := (value.dataObject as TValueObject).dValue;
                   end;
                end;
    symBoolean:  begin symbol.symbolType := symBoolean; symbol.bValue := value.bValue; end;
    symString:   m.symbolTable.storeString (symbol, value.dataObject as TStringObject);
    symList:     m.symbolTable.storeList   (symbol, value.dataObject as TListObject);
    symArray:    m.symbolTable.storeArray   (symbol, value.dataObject as TArrayObject);
    symMatrix:   m.symbolTable.storeMatrix  (symbol, value.dataObject as TMatrixObject);
    symValueObject:   m.symbolTable.storeValueObject(symbol, value.dataObject as TValueObject);
    symUserFunc: m.symbolTable.storeFunction (symbol, value.dataObject as TUserFunction);
    symModule:   m.symbolTable.storeModule (symbol, value.module);
  else
    raiseError ('Internal error: Unrecognized stacktype in storeAttr: ' + TRttiEnumerationType.GetName(symbol.symbolType));
  end;
end;

procedure TVM.copyToStack(stackElement: PMachineStackRecord; index : integer; frame : PFrame);
begin
  inc(stackTop);
  case stackElement.stackType of
    symInteger:
        begin
        stack[stackTop].stackType := symInteger;
        stack[stackTop].iValue := stackElement.iValue;
        end;
      symBoolean:
        begin
        stack[stackTop].stackType := symBoolean;
        stack[stackTop].bValue := stackElement.bValue;
        end;
    symDouble:
        begin
        stack[stackTop].stackType := symDouble;
        stack[stackTop].dValue := stackElement.dValue;
        end;
    // For both strings and lists, the stack cannot own them, only the
    // symbol table can own string and list objects. Hence we just copy
    // references here.
    symString:
        begin
        stack[stackTop].stackType := symString;
        stack[stackTop].dataObject := stackElement.dataObject;
        end;
    symList:
        begin
        stack[stackTop].stackType := symList;
        stack[stackTop].dataObject := stackElement.dataObject;
        end;
    symArray:
        begin
        stack[stackTop].stackType := symArray;
        stack[stackTop].dataObject := stackElement.dataObject;
        end;
    symMatrix:
        begin
        stack[stackTop].stackType := symMatrix;
        stack[stackTop].dataObject := stackElement.dataObject;
        end;
    symValueObject:
        begin
        stack[stackTop].stackType := symValueObject;
        stack[stackTop].dataObject := stackElement.dataObject;
        end;
    symUserFunc:
        begin
        stack[stackTop].stackType := symUserFunc;
        stack[stackTop].dataObject := stackElement.dataObject;
        end;
    symNonExistant:
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

  if (stack[bsp + index].stackType = symString) and (stack[bsp + index].dataObject <> nil) then
      stack[bsp + index].dataObject.blockType := btGarbage; // Mark as garbage

  if (stack[bsp + index].stackType = symList) and (stack[bsp + index].dataObject <> nil) then
      stack[bsp + index].dataObject.blockType := btGarbage; // Mark as garbage

  if (stack[bsp + index].stackType = symArray) and (stack[bsp + index].dataObject <> nil) then
      stack[bsp + index].dataObject.blockType := btGarbage; // Mark as garbage

  if (stack[bsp + index].stackType = symMatrix) and (stack[bsp + index].dataObject <> nil) then
      stack[bsp + index].dataObject.blockType := btGarbage; // Mark as garbage

  if (stack[bsp + index].stackType = symValueObject) and (stack[bsp + index].dataObject <> nil) then
      stack[bsp + index].dataObject.blockType := btGarbage; // Mark as garbage

  case value.stackType of
    symInteger:
      begin
      stack[bsp + index].iValue := value.iValue;
      stack[bsp + index].stackType := symInteger;
      end;
    symBoolean:
      begin
      stack[bsp + index].bValue := value.bValue;
      stack[bsp + index].stackType := symBoolean;
      end;
    symDouble:
      begin
      stack[bsp + index].dValue := value.dValue;
      stack[bsp + index].stackType := symDouble;
      end;
    symString:
      begin
      stack[bsp + index].dataObject := value.dataObject.clone;
      stack[bsp + index].dataObject.blockType := btBound;
      stack[bsp + index].stackType := symString;
      end;
    symList:
      begin
      stack[bsp + index].dataObject := value.dataObject.clone;
      stack[bsp + index].dataObject.blockType := btBound;
      stack[bsp + index].stackType := symList;
      end;
    symArray:
      begin
      stack[bsp + index].dataObject := value.dataObject.clone;
      stack[bsp + index].dataObject.blockType := btBound;
      stack[bsp + index].stackType := symArray;
      end;
    symMatrix:
      begin
      stack[bsp + index].dataObject := value.dataObject.clone as TMatrixObject;
      stack[bsp + index].dataObject.blockType := btBound;
      stack[bsp + index].stackType := symMatrix;
      end;
    symValueObject:
      begin
      stack[bsp + index].dataObject := value.dataObject.clone as TValueObject;
      stack[bsp + index].dataObject.blockType := btBound;
      stack[bsp + index].stackType := symValueObject
      end;
    symNonExistant:
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
    symInteger: case st2.stackType of
        symInteger: push(st2.iValue <= st1.iValue);
        symDouble: push(st2.dValue <= st1.iValue);
      end;
    symDouble: case st2.stackType of
        symInteger: push(st2.iValue <= st1.dValue);
        symDouble: push(st2.dValue <= st1.dValue);
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
    symInteger: case st2.stackType of
        symInteger: push(st2.iValue < st1.iValue);
        symDouble:  push(st2.dValue < st1.iValue);
        symNonExistant : raiseError  ('Undefined value in greater than tests');
      end;
    symDouble: case st2.stackType of
        symInteger: push(st2.iValue < st1.dValue);
        symDouble: push(st2.dValue < st1.dValue);
        symNonExistant : raiseError ('Undefined value in greater than tests');
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
    symInteger: case st2.stackType of
        symInteger: push(st2.iValue > st1.iValue);
        symDouble: push(st2.dValue > st1.iValue);
      end;
    symDouble: case st2.stackType of
        symInteger: push(st2.iValue > st1.dValue);
        symDouble: push(st2.dValue > st1.dValue);
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
    symInteger: case st2.stackType of
        symInteger: push(st2.iValue >= st1.iValue);
        symDouble: push(st2.dValue >= st1.iValue);
      else
        raiseError  ('Boolean data types not permitted in isGte comparison');
      end;
    symDouble: case st2.stackType of
        symInteger: push(st2.iValue >= st1.dValue);
        symDouble: push(st2.dValue >= st1.dValue);
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
  if (st1.stackType = symBoolean) and (st2.stackType = symBoolean) then
  begin
    push(st1.bValue = st2.bValue);
    exit;
  end;

  case st1.stackType of
    symInteger:
      case st2.stackType of
        symInteger: push(st2.iValue = st1.iValue);
        symDouble:  push(math.sameValue (st1.iValue, st2.dValue));
        symValueObject : push ((st2.dataObject as TValueObject).isEqualTo(st1.iValue));
      else
        raiseError ('Incompatible types in equality test');
      end;
    symDouble:
      case st2.stackType of
        symInteger: push(st2.iValue = st1.dValue);
        symDouble:  push(math.sameValue(st2.dValue, st1.dValue));
        symValueObject : push ((st2.dataObject as TValueObject).isEqualTo(st1.dValue));
      else
        raiseError ('Incompatible types in equality test');
      end;
    symString:
      begin
      if st2.stackType = symString then
         push(TStringObject(st1.dataObject).isEqualTo (TStringObject(st2.dataObject)))
      else
         raiseError ('Incompatible types in equality test');
      end;
    symValueObject:
      begin
      case st2.stackType of
          symDouble : push((st1.dataObject as TValueObject).isEqualTo (st2.dValue));
          symValueObject : push((st1.dataObject as TValueObject).isEqualTo ((st2.dataObject  as TValueObject).getScalar()))
      else
         raiseError ('Incompatible types in ValueObject equality test');
      end;
      end;
     symList:
      begin
      if st2.stackType = symList then
        push(TListObject.listEquals(TListObject (st1.dataObject), TListObject (st2.dataObject)))
      else
        raiseError  ('Unable to test for equality between lists and non-lists data types');
      end;
    symArray :
      if st2.stackType = symArray then
         push (TArrayObject.isEqualTo(TArrayObject (st1.dataObject), TArrayObject (st2.dataObject)))
      else
        raiseError  ('Unable to test for equality between arrays and non-arrays data types');
    symMatrix :
      if st2.stackType = symMatrix then
         push (TMatrixObject.isEqualTo(TMatrixObject (st1.dataObject), TMatrixObject (st2.dataObject)))
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
  if (st1.stackType <> symBoolean) or (st2.stackType <> symBoolean) then
     raiseError ('Incompatible types in AND operation');
  push(st1.bValue and st2.bValue);
end;


procedure TVM.orOp;
var
  st1, st2: PMachineStackRecord;
begin
  st1 := pop;
  st2 := pop;
  if (st1.stackType <> symBoolean) or (st2.stackType <> symBoolean) then
     raiseError ('Incompatible types in OR operation');
  push(st1.bValue or st2.bValue);
end;


procedure TVM.xorOp;
var
  st1, st2: PMachineStackRecord;
begin
  st1 := pop;
  st2 := pop;
  if (st1.stackType <> symBoolean) or (st2.stackType <> symBoolean) then
     raiseError ('Incompatible types in XOR operation');
  push(st1.bValue xor st2.bValue);
end;


procedure TVM.notOp;
var
  st: PMachineStackRecord;
begin
  st := pop;
  if (st.stackType <> symBoolean) then
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
var argMsg : string;
begin
  if p.oValue.nArgs <> VARIABLE_ARGS then
     if actualNumArgs <> p.oValue.nArgs then
        begin
        if p.oValue.nArgs > 1 then
           argMsg := ' arguments'
        else
           argMsg := ' argument';
        raiseError ('Expecting ' + inttostr (p.oValue.nArgs) + argMsg + ' in function call [' + p.oValue.name + '] but received ' + inttostr (actualNumArgs) + ' arguments');
        end;

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


// actualNumArgs is the number of arguments the user included in the function call
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
  argMsg : string;
begin
  // Get the function object
  p := @stack[stackTop-actualNumArgs];
  // Check first that its actually something we can call
  if (p.stackType <> symUserFunc) and (p.stackType <> symObjectMethod) then
     raiseError (stToStr (p.stackType) + ' is not something that can be called as a function');

  // This deals with calls such as a.len(), where len() is an object method
  if p.stackType = symObjectMethod then
     begin
     callObjectMethod (actualNumArgs, p);
     exit;
     end;

  functionObject := TUserFunction (p.dataObject);
  expectedNumArgs := functionObject.nArgs;

  // If it's a non-variable argument call check that the right
  // number of arguments are avaiable to the function
  if expectedNumArgs <> VARIABLE_ARGS then
     if actualNumArgs <> expectedNumArgs then
        begin
        if TUserFunction (p.dataObject).nArgs > 1 then
           argMsg := ' arguments'
        else
           argMsg := ' argument';
        raiseError ('Expecting ' + inttostr (TUserFunction (p.dataObject).nArgs) + argMsg + ' in function call [' + TUserFunction (p.dataObject).methodName + '] but received ' + inttostr (actualNumArgs) + ' arguments');
        end;

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
      stack[tbsp + i + expectedNumArgs].stackType := symNonExistant;

  // This check is to see if the argument is passed as a literal to the function
  // eg callme ([1,2,3,4]). Since a literal isn't assigned to anything
  // it is marked as garbage. However, if this enters a function, we must
  // make sure the garbage collector doesn't collect it since it will be
  // used inside the function.
  for i := 0 to expectedNumArgs - 1 do
      case stack[tbsp + i].stackType of
         symList:
          begin
          if stack[tbsp + i].dataObject.blockType = btGarbage then
            stack[tbsp + i].dataObject.blockType := btTemporary; // To stop the garbage collector
          end;
         symMatrix:
          begin
          if stack[tbsp + i].dataObject.blockType = btGarbage then
            stack[tbsp + i].dataObject.blockType := btTemporary; // To stop the garbage collector
          end;
         symString:
          begin
          if stack[tbsp + i].dataObject.blockType = btGarbage then
            stack[tbsp + i].dataObject.blockType := btTemporary; // To stop the garbage collector
          end;
       symArray:
          begin
          if stack[tbsp + i].dataObject.blockType = btGarbage then
            stack[tbsp + i].dataObject.blockType := btTemporary; // To stop the garbage collector
          end;
       symObject:
         begin
          if stack[tbsp + i].dataObject.blockType = btGarbage then
            stack[tbsp + i].dataObject.blockType := btTemporary; // To stop the garbage collector
         end;
      end;


  // This is to make sure the user function knows what module its in.
  oldModule := module;
  try
    module := functionObject.moduleRef;

    // And finally we make the call
    run(functionObject.codeBlock, module.symbolTable);

    // deal with the special case where a user has passed a literal (list, array, etc)
    // which is not owned by anyone. Such arguments come in as btTemporary in order
    // to prevent the garbage collector from freeing up the argument.
    for i := 0 to expectedNumArgs - 1 do
        if stack[tbsp + i].stackType = symList then
           if stack[tbsp + i].dataObject.blockType = btTemporary then
              stack[tbsp + i].dataObject.blockType := btGarbage;

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
      symInteger:
        begin
        result[i] := p.iValue;
        end;
      symDouble:
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
      symInteger:
        begin
        result.list[i].iValue := p.iValue;
        result.list[i].itemType := symInteger;
        end;
      symBoolean:
        begin
        result.list[i].bValue := p.bValue;
        result.list[i].itemType := symBoolean;
        end;
      symDouble:
        begin
        result.list[i].dValue := p.dValue;
        result.list[i].itemType := symDouble;
        end;
      symSliceObject:
        begin

        end;
      symString:
        begin
        result.list[i].dataObject := p.dataObject.clone;
        result.list[i].dataObject.blockType := btOwned; // Owned means owned by a list
        result.list[i].itemType := symString;
        end;
      symList:
        begin
        result.list[i].dataObject := p.dataObject.clone;
        result.list[i].dataObject.blockType := btOwned;
        result.list[i].itemType := symList;
        end;
      symArray:
        begin
        result.list[i].dataObject := p.dataObject.clone;
        result.list[i].dataObject.blockType := btOwned;
        result.list[i].itemType := symArray;
        end;
      symMatrix:
        begin
        result.list[i].dataObject := p.dataObject.clone as TMatrixObject;
        result.list[i].dataObject.blockType := btOwned;
        result.list[i].itemType := symMatrix
        end;
      symValueObject:
        begin
        result.list[i].dataObject := p.dataObject.clone as TValueObject;
        result.list[i].dataObject.blockType := btOwned;
        result.list[i].itemType := symValueObject;
        end;
      symUserFunc :
        begin
        result.list[i].dataObject := p.dataObject.clone;
        (result.list[i].dataObject as TUserFunction).blockType := btOwned;
        result.list[i].itemType := symUserFunc;
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

  if (index < 0) or (index > length(TStringObject(variable.dataObject).value) - 1) then
    raiseError ('string index out of range');

  if variable.stackType <> symString then
    raiseError ('left-hand side must be a string');

  if value.stackType <> symString then
    raiseError ('right-hand side must be a string');

  if length(TStringObject(value.dataObject).value) > 1 then
    raiseError ('can only assign a single string character to an indexed string');

  TStringObject(variable.dataObject).value[index + 1] := TStringObject(value.dataObject).value[1];
end;


procedure TVM.storeIndexableArray(variable: PMachineStackRecord; index: integer; nSubscripts : integer);
var value: double;
    idx : array of integer;
    i : integer;
begin
  value := popScalar;

  if variable.stackType <> symArray then
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

     TArrayObject (variable.dataObject).setValue (idx, value);

     uIntStack.clear (subscriptStack);
     end;
end;


procedure TVM.storeIndexableMatrix(variable: PMachineStackRecord; index: integer; nSubscripts : integer);
var value: double;
    idx : array of integer;
    i : integer;
begin
  value := popScalar;

  if variable.stackType <> symMatrix then
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

     (variable.dataObject as TMatrixObject).setVal (idx[0], idx[1], value);

     uIntStack.clear (subscriptStack);
     end;
end;

procedure TVM.storeIndexableList(variable: PMachineStackRecord; index: integer);
var
  value: PMachineStackRecord;
  element: TListItem;
begin
  if (TListObject (variable.dataObject).list.count  < 0) or (index > TListObject (variable.dataObject).list.count - 1) then
     raiseError ('index out of range: ' + inttostr (index));


  // Get the element from the list that we're going to store to
  element := TListObject (variable.dataObject).list[index];

  if (element.itemType = symString) and (element.dataObject <> nil) then
     element.dataObject.blockType := btGarbage;

  if (element.itemType = symList) and (element.dataObject <> nil) then
     element.dataObject.blockType := btGarbage;

  value := pop;
  case value.stackType of
    symInteger:
      begin
      element.iValue := value.iValue;
      element.itemType := symInteger;
      end;
    symBoolean:
      begin
      element.bValue := value.bValue;
      element.itemType := symBoolean;
      end;
    symDouble:
      begin
      element.dValue := value.dValue;
      element.itemType := symDouble;
      end;
    symString:
      begin
      element.dataObject := value.dataObject.clone;
      element.dataObject.blockType := btOwned;
      element.itemType := symString;
      end;
    symList:
      begin
      element.dataObject := value.dataObject.clone;
      element.dataObject.blockType := btOwned;
      element.itemType := symList;
      end;
    symMatrix:
      begin
      element.dataObject := value.dataObject.clone as TMatrixObject;
      element.dataObject.blockType := btOwned;
      element.itemType := symMatrix;
      end;
    symValueObject:
      begin
      element.dataObject := value.dataObject.clone as TValueObject;
      element.dataObject.blockType := btOwned;
      element.itemType := symValueObject;
      end;
    symUserFunc:
      begin
      element.dataObject := value.dataObject;
      element.itemType := symUserFunc;
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

  if (index < 0) or (index > TListObject (st.dataObject).list.count - 1) then
     raiseError ('list index out of range: ' + inttostr (index));

  element := TListObject (st.dataObject).list[index];

  if (element.itemType = symString) and (element.dataObject <> nil) then
     element.dataObject.blockType := btGarbage;

  if (element.itemType = symList) and (element.dataObject <> nil) then
     element.dataObject.blockType := btGarbage;

 case value.stackType of
    symInteger:
      begin
      element.iValue := value.iValue;
      element.itemType := symInteger;
      end;
    symBoolean:
      begin
      element.bValue := value.bValue;
      element.itemType := symBoolean;
      end;
    symDouble:
      begin
      element.dValue := value.dValue;
      element.itemType := symDouble;
      end;
    symString:
      begin
      element.dataObject := value.dataObject.clone;
      element.dataObject.blockType := btOwned;
      element.itemType := symString;
      end;
    symList:
      begin
      element.dataObject := value.dataObject.clone;
      element.dataObject.blockType := btOwned;
      element.itemType := symList;
      end;
     symMatrix:
      begin
      element.dataObject := value.dataObject.clone as TMatrixObject;
      element.dataObject.blockType := btOwned;
      element.itemType := symMatrix;
      end;
    symValueObject:
      begin
      element.dataObject := value.dataObject.clone as TValueObject;
      element.dataObject.blockType := btOwned;
      element.itemType := symValueObject ;
      end;
    symUserFunc:
      begin
      element.dataObject := value.dataObject;
      element.itemType := symUserFunc;
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

  if (index < 0) or (index > length(TStringObject(st.dataObject).value) - 1) then
    raiseError ('string index out of range');

  if value.stackType <> symString then
    raiseError ('can only assign a single string character to an indexed string');

  if length (TStringObject(value.dataObject).value) > 1 then
      raiseError ('can only assign a single string character to an indexed string');
  TStringObject(st.dataObject).value[index + 1] := TStringObject(value.dataObject).value[1];
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
    symList: storeLocalIndexableList(variable, index);
    symString:
    begin
        // update the stack entry that holds the variable
      storeLocalIndexableString(variable, index);
      stack[index + bsp].dataObject := variable.dataObject;
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
  if peek.stackType <> symInteger then
     raiseError ('index must be an integer while accessing lists, arrays or strings');

  index := popInteger;
  symbol := pop;   // Comes from push symbol
  case symbol.stackType of
     symList   : storeIndexableList(symbol, index);
     symString : storeIndexableString(symbol, index);
     symArray  : storeIndexableArray(symbol, index, nSubscripts);
     symMatrix : storeIndexableMatrix(symbol, index, nSubscripts);
  else
    raiseError (stToStr(symbol.stackType) + ' variable is not indexable');
  end;

end;


// -------------------------------------------------------------------------------------------
// Loading Methods
// -------------------------------------------------------------------------------------------

procedure TVM.loadIndexableString(st: PMachineStackRecord; index: integer);
begin
  if (index < 0) or (index > length(TStringObject(st.dataObject) .value) - 1) then
    raiseError ('string index out of range');
  push(TStringObject.Create(TStringObject(st.dataObject).value[index + 1]));
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
        if TArrayObject (st.dataObject).getNumDimensions > 1 then
           raiseError ('Array has more dimensions that specified in indexing');
        push (TArrayObject (st.dataObject).getValue(index));
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

     push (TArrayObject (st.dataObject).getValue (idx));
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

     push (TMatrixObject (st.dataObject).getVal (idx[0], idx[1]));
     uIntStack.clear (subscriptStack);
     end;
end;


procedure TVM.loadIndexableList(variable: PMachineStackRecord; index: integer);
var element: TListItem;
begin
  if (index < 0) or (index > TListObject (variable.dataObject).list.count - 1) then
    raiseError('list index out of range: ' + inttostr (index));

  element := TListObject (variable.dataObject).list[index];

  case element.itemType of
    symInteger: push(element.iValue);
    symBoolean: push(element.bValue);
    symDouble:  push(element.dValue);
    symString:  push(element.dataObject);
    symList:    push(element.dataObject);
    symArray:   push(element.dataObject);
    symMatrix:  push(element.dataObject);
    symObject : push (element.dataObject);

    symUserFunc:push(TUserFunction (element.dataObject));
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
    symList  : loadIndexableList(variable, index);
    symString: loadIndexableString(variable, index);
    symArray : loadIndexableArray (variable, index, nSubscripts);
    symMatrix : loadIndexableMatrix (variable, index, nSubscripts);
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
    symInteger:     push(element.iValue);
    symBoolean:     push(element.bValue);
    symDouble:      push(element.dValue);
    symString:      push(element.dataObject);
    symList:        push(element.dataObject);
    symArray:       push(element.dataObject);
    symValueObject: push(element.dataObject);
    symUserFunc:    push(element.dataObject as TUserFunction);
  else
    raiseError('unsupported type in loadIndexable');
  end;
end;


procedure TVM.loadLocalIndexableString(astr: TDataObject; index: integer);
begin
  if (index < 0) or (index >= length(TStringObject(astr).value) - 1) then
    raiseError('string index out of range');
  push(TStringObject.Create(TStringObject(astr).value[index + 1]));
end;


procedure TVM.loadLocalIndexable;
var
  index: integer;
  st: PMachineStackRecord;
begin
  index := popInteger;
  st := pop;
  if st.stackType = symList then
    loadLocalIndexableList(TListObject (st.dataObject), index)
  else
  if st.stackType = symString then
    loadLocalIndexableString(st.dataObject, index)
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

  pushSliceObject (TSliceObject.Create (lower, upper));
end;

 // Stack has a slice list followed by the object we want to slice (value)
procedure TVM.evalSliceObject (numSlices : integer);
var i : integer;
    sliceObjlist : TSliceObjectList;
    value : PMachineStackRecord;
begin
  try
    setLength (sliceObjlist, numSlices);
    for i := numSlices - 1 downto 0  do
        sliceObjlist[i] := TSliceObject (pop.sliceValue);
    value := pop;

  case value.stackType of
     symString :
        begin
        if numSlices > 1 then
           raiseError ('Only a single slice can be applied to a string');
        push (TStringObject(value.dataObject).slice (sliceObjlist[0].lower, sliceObjlist[0].upper));
        end;
     symList :
        begin
        if numSlices > 1 then
           raiseError ('Only a single slice can be applied to a list');
        push (TListObject (value.dataObject).slice (sliceObjlist[0].lower, sliceObjlist[0].upper));
        end;
     symArray :
        begin
        push (TArrayObject (value.dataObject).slice(sliceObjList));
        end;
     symMatrix :
        begin
        push (TMatrixObject (value.dataObject).slice(sliceObjList));
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
      symList:
        begin
        stack[stackTop + i].dataObject.blockType := btGarbage; // Mark as garbage
        stack[stackTop + i].dataObject := nil;
        end;
      symString:
        begin
        if not TStringObject(stack[stackTop + i].dataObject).isConstant then // Not sure why it has to be constant
          begin
          stack[stackTop + i].dataObject.blockType := btGarbage; // Mark as garbage
          stack[stackTop + i].dataObject := nil;
          end;
        end;
      symInteger,
      symDouble,
      symBoolean :
         begin
         end;
      symMatrix :
         begin
         stack[stackTop + i].dataObject.blockType := btGarbage; // Mark as garbage
         stack[stackTop + i].dataObject := nil;
         end;
      symValueObject :
         begin
         stack[stackTop + i].dataObject.blockType := btGarbage; // Mark as garbage
         stack[stackTop + i].dataObject := nil;
         end;
      symNonExistant :
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


procedure TVM.recordOpCodeCounts (opcode : TByteCode);
begin
  countOpCodes[integer (opCode.opCode)] := countOpCodes[integer (opCode.opCode)] + 1;

//  case opCode of
//          oNop:        begin end;
//            oAdd:        p;
//            oSub:        subOp;
//            oMult:       multOp;
//            oDivide:     divOp;
//            oDivi:       divideIntOp;
//            oDotProduct: dotProductOp;  // To support the '@' operator, eg a@b
//            oUmi:        unaryMinusOp;
//            oMod:        modOp;
//            oPower:      powerOp;
//            oInc:        incOp(c[ip].symbolName, c[ip].float);
//            oLocalInc:   localIncOp(c[ip].index, c[ip].float);
//            oDec:        decOp(c[ip].symbolName, c[ip].float);
//            oLocalDec:   localDecOp(c[ip].index, c[ip].float);
//            oPushi:      push (c[ip].index);
//            oPushb:      push (boolean(c[ip].index));
//            oPushd:      push (module.moduleProgram.constantValueTable[c[ip].index].dValue);
//            oPushs:      push (module.moduleProgram.constantValueTable[c[ip].index].sValue);
//            oPushNone:   push (@noneStackType);
//            oPop:        begin
//                         // If the next instrction is halt, we will leave the item
//                         // on the stack and let the caller deal with it. This is
//                         // mainly useful when used in interactive mode so that the
//                         // console can print the stack item to the console, any
//                         // other time we want to the pop executed
//                         if c[ip+1].opCode <> oHalt then
//                            pop();
//                         end;
//            oDup:        dupStack;
//            oPopDup:     pop();
//            oIsLt:       isLt;
//            oIsLte:      isLte;
//            oIsGt:       isGt;
//            oIsGte:      isGte;
//            oIsEq:       isEq;
//            oIsNotEq:    isNoteq;
//            oAnd:        andOp;
//            oOr:         orOp;
//            oXor:        xorOp;
//            oNot:        notOp;
//
//            // debugging opcodes
//            oPrint:       printValue;
//            oPrintln:     printlnValue;
//            oSetColor:    setColor;
//            oAssertTrue:  assertTrue;
//            oAssertFalse: assertFalse;
//
//            // Branch opcodes
//            oJmp:        ip := ip + c[ip].index - 1;
//            oJmpIfTrue:  if pop().bValue then
//                            ip := ip + c[ip].index - 1;
//            oJmpIfFalse: if not pop().bValue then
//                            ip := ip + c[ip].index - 1;
//
//
//         // These two are used when we load and store synmbols within the current module
//         oStoreSymbol:  begin
//                         storeSymbol (c[ip].symbolName);
//                         g := getGarbageSize;
//                         if (g > 50) then
//                            collectGarbage; // Only collect garbage after a store
//                         end;
//          oLoadSymbol: loadSymbol (c[ip].symbolName);
//
//          // This are used to load and store symbols when we reference modules outside the current one
//          oLoadAttr: loadAttr (c[ip].symbolName);
//          oStoreAttr: storeAttr (c[ip].symbolName);
//
//          // These are used to load and store symbols in user functions
//          oStoreLocal:  begin
//                        storeLocalSymbol(c[ip].index);
//                        if getGarbageSize > 10 then
//                           collectGarbage;
//                        end;
//           oLoadLocal:  loadLocalSymbol(c[ip].index);
//
//          oBuildSlice : buildSlice();
//           oSliceAll  : push(SLICE_ALL);
//           oSliceObj  : evalSliceObject (c[ip].index);
//
//        oImportModule: importModule (c[ip].moduleName);
//
//            // Method call opcodes
//            oCall :     callUserFunction (c[ip].index);
//            oRet :
//              begin
//              // Note that anything that is returned isn't bound to any symbol.
//              // Binding only happens if the callee assigns the
//              // returned value to a symbol.
//              value := pop();
//              case value.stackType of
//                symString: value.dataObject := value.dataObject.clone;
//                symList:   value.dataObject := value.dataObject.clone;
//                symMatrix : value.dataObject := value.dataObject.clone;
//                symArray : value.dataObject := value.dataObject.clone;
//                symNonExistant : begin end;
//                symInteger : begin end;
//                symDouble : begin end;
//                symBoolean : begin end;
//              else
//                 raiseError('oRet not implemented for this type');
//              end;
//              // Clear the pushed arguments from the stack
//              stackTop := stackTop - frameStack[frameStackTop].nlocals - 1;  // -1 to also remove the function object
//              // This can only be called after the stackTop has been adjusted
//              clearAnyStackHeapAllocations;
//
//              dec(frameStackTop);
//              push(value); // push the result back on to the stack
//              exit;
//              end;
//
//            // list handling opcodes
//            oCreateList:   push(createList(c[ip].index));
//            oLvecIdx:      loadIndexable (c[ip].index);
//            oSvecIdx:      storeIndexable (c[ip].index);
//
//            oLocalLvecIdx: loadLocalIndexable;
//            oLocalSvecIdx: storeLocalIndexable;
//
//            oCreateVector  : push (createVector (c[ip].index));
//            oCreateMatrix  : push (createMatrix (c[ip].index));
//            //oAddMatrixItem : addMatrixItem;
//
//            oHalt: break;
//
//            oPopAndSend:   callBack(pop);  // Not currently used
//
//  end;
end;


// Use this method to run a user defined function
procedure TVM.run (code: TProgram; symbolTable : TSymbolTable);
var
  ip, g : integer;
  value : PMachineStackRecord;
  c : TCode;
begin
  ip := 0; {count := 0;} bolStopVm := False;
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
           // Need to issue exception here in order to get out completely
           exit;
           end;

        if bolDebugger then
           begin
           if Assigned (debugCallBackPtr) then
              debugCallBackPtr (self);
           if bolCountOpCodes then
              countOpCodes[integer (c[ip].opCode)] := countOpCodes[integer (c[ip].opCode)] + 1;
           end;

          case c[ip].opCode of
            oNop:        begin end;
            oAdd:        addOp;
            oSub:        subOp;
            oMult:       multOp;
            oDivide:     divOp;
            oDivi:       divideIntOp;
            oDotProduct: dotProductOp;  // To support the '@' operator, eg a@b
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
                symString: value.dataObject := value.dataObject.clone;
                symList:   value.dataObject := value.dataObject.clone;
                symMatrix : value.dataObject := value.dataObject.clone;
                symArray : value.dataObject := value.dataObject.clone;
                symNonExistant : begin end;
                symInteger : begin end;
                symDouble : begin end;
                symBoolean : begin end;
              else
                 raiseError('oRet not implemented for this type');
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

