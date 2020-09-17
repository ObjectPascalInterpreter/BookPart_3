unit uSymbolTable;

// Ths source is distributed under Apache 2.0

// Copyright (C) 2019-2020 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

Uses Classes, Generics.Collections, uListObject, uStringObject, uOpCodes,
     uConstantTable, uVMExceptions;

type
   // There are too many dependencies between these classes to separate them
   // into individual files, so we group them together here.

   TSymbolTable = class;

   TByteCode = packed record
         opCode : TOpCode;   // 1 byte
         index : integer;    // 4 bytes
   end;
   TCode = TArray<TByteCode>;

   TProgram = class (TObject)
        const
          ALLOC_BY = 512;
        private
          actualLength : integer;
          procedure checkSpace;
        public
           code : TCode;
           function  count : integer;
           procedure clear;
           procedure append (byteCode : TByteCode);
           procedure compactCode;
           function  addByteCode (opCode : TOpCode) : integer; overload;
           procedure addByteCode (opCode : TOpCode; iValue : integer); overload;
           procedure addByteCode (opCode : TOpCode; dValue : double); overload;
           procedure addByteCode (opCode : TOpCode; bValue : boolean); overload;
           procedure addByteCode (opCode : TOpCode; sValue : string); overload;

           procedure appendProgram (aProgram : TProgram);
           function  getCurrentInstructionPointer : integer;
           function  createInstructionSpace : integer;
           procedure setGotoLabel (Location, value : integer);

           constructor Create; overload;
           constructor Create (mycode : array of TByteCode);  overload;
           destructor  Destroy; override;
   end;


   TxBuiltInFunction = procedure (vm : TObject) of object;
   TUserFunction = class (TObject)
       name : string;
       nArgs : integer;    // should this number be on the stack?
       symbolTable : TSymbolTable;
       constantTable : TConstantValueTable;
       globalVariableList : TStringList;
       funcCode : TProgram;
       helpStr : string;
       funcPtr : TxBuiltInFunction;

       function    getSize : integer;
       constructor Create; overload;
       constructor Create (functionName : string); overload;
       constructor Create (functionName : string; nArgs : integer; funcPtr : TxBuiltInFunction); overload;
       destructor  Destroy; override;
   end;

   // symNonExistant: Requests for symbols that aren't even in the symbol table
   TSymbolElementType = (symNonExistant = -1, symUndefined, symInteger, symDouble, symBoolean, symString, symList, symUserFunc);

   TSymbol = class (TObject)
       symbolType : TSymbolElementType;
       symbolName : string;
       helpStr : string;
       locked : boolean;  // If true can't be written to.
       iValue : integer;
       dValue : double;
       bValue : boolean;
       sValue : TStringObject;
       lValue : TListObject;
       fValue : TUserFunction;

       constructor Create;
       destructor  Destroy; override;
   end;


   TSymbolTable = class (TObjectList<TSymbol>)
      private
      public
        function addSymbol (name : string) : integer; overload;
        function addSymbol (name : string; dValue : double;  locked : boolean; helpStr : string) : integer; overload;
        function addSymbol (fValue : TUserFunction; locked : boolean) : integer; overload;
        function find (name : string; var index : integer) : boolean;
        function isUserFunction (name : string; var index : integer) : boolean;
        function reverseFind (name : string; var index : integer): boolean;

        procedure storeToSymbolTable (index, ivalue : integer); overload;
        procedure storeToSymbolTable (index : integer; bValue : boolean); overload;
        procedure storeToSymbolTable (index : integer; dValue : double); overload;
        procedure storeToSymbolTable (index : integer; lValue : TListObject); overload;
        procedure storeToSymbolTable (index : integer; sValue : TStringObject); overload;
        procedure storeToSymbolTable (index : integer; fValue : TUserFunction); overload;

        function  getFromSymbolTable (index : integer) : TSymbol;

        procedure checkForExistingData (index : integer); inline;

        constructor Create;
        destructor  Destroy; override;
   end;


   function  createByteCode (opCode : TOpCode; iValue : integer) : TByteCode; overload;
   function  createByteCode (opCode : TOpCode; dValue : double) : TByteCode; overload;
   function  createByteCode (opCode : TOpCode; bValue : boolean) : TByteCode; overload;
   function  createByteCode (opCode : TOpCode) : TByteCode; overload;
   function  createByteCode (opCode : TOpCode; sValue : string) : TByteCode; overload


implementation

Uses uMemoryManager, uSyntaxAnalysis;

constructor TUserFunction.Create;
begin
  constantTable := TObjectList<TConstantValueElement>.Create;
  inherited;
end;


constructor TUserFunction.Create (functionName : string);
begin
  Create;
  nArgs := 0;
  name := functionName;
  funcCode := TProgram.Create;
  symbolTable := TSymbolTable.Create;
  globalVariableList := TStringList.Create;
  globalVariableList.Sorted := True;  // sorted list
  funcPtr := nil;
end;


constructor TUserFunction.Create (functionName : string; nArgs : integer; funcPtr : TxBuiltInFunction);
begin
  Create;
  self.nArgs := nArgs;
  name := functionName;
  //symbolTable := TSymbolTable.Create;
  //globalVariableList := TStringList.Create;
  //globalVariableList.Sorted := True;  // sorted list
  self.funcPtr := funcPtr;
end;


destructor TUserFunction.Destroy;
begin
  funcCode.Free;
  symbolTable.Free;
  constantTable.Free;
  globalVariableList.Free;
  inherited;
end;


function TUserFunction.getSize : integer;
begin
   result := self.InstanceSize +
      self.funcCode.InstanceSize +
      length (self.funcCode.code) +
      self.symbolTable.InstanceSize +
      self.constantTable.InstanceSize;
end;

// ---------------------------------------------------------------------------------

constructor TSymbol.Create;
begin
  lValue := nil;
  sValue := nil;
  fValue := nil;
  symbolType := symUndefined;
  locked := False;
end;


destructor TSymbol.Destroy;
begin
  inherited;
end;


// ---------------------------------------------------------------------------------

constructor TSymbolTable.Create;
begin
  inherited;
  self.OwnsObjects := False;
end;


destructor TSymbolTable.Destroy;
var i : integer;
begin
  for i := Count - 1 downto 0 do
      begin
      case self[i].symbolType of
         symList   : self[i].lValue.blockType := btGarbage;
         symString : self[i].sValue.blockType := btGarbage;
         symUserFunc : self[i].fValue.Free;
      end;
      self[i].free;
      end;
  inherited;
end;


function TSymbolTable.addSymbol (fValue : TUserFunction; locked : boolean) : integer;
var symbol : TSymbol;
begin
  symbol := TSymbol.Create;
  symbol.fValue := TUserFunction (fValue);
  symbol.symbolName := fValue.name;
  symbol.symbolType := symUserFunc;
  symbol.locked := locked;
  result := Add (symbol);
end;


function TSymbolTable.addSymbol (name : string; dValue : double; locked : boolean; helpStr : string) : integer;
var symbol : TSymbol;
begin
  symbol := TSymbol.Create;
  symbol.dValue := dValue;
  symbol.symbolName := name;
  symbol.symbolType := symDouble;
  symbol.helpStr := helpStr;
  symbol.locked := locked;;
  result := Add (symbol);
end;


function TSymbolTable.find (name : string; var index : integer) : boolean;
var i : integer;
begin
  result := false;
  for i := 0 to Count - 1 do
      if self[i].symbolName = name then
         begin
         index := i;
         exit (True);
         end;
end;


function TSymbolTable.isUserFunction (name : string; var index : integer) : boolean;
begin
  if find (name, index) then
     begin
     if self[index].symbolType = symUserFunc then
        exit (True);
     end
  else
     exit (False);
end;


function TSymbolTable.reverseFind (name : string; var index : integer): boolean;
var i : integer;
begin
  result := false;
  for i := count - 1  downto 0 do
      if self[i].symbolName = name then
         begin
         index := i;
         exit (True);
         end;
end;


function TSymbolTable.addSymbol (name : string) : integer;
var symbol : TSymbol;
begin
  symbol := TSymbol.Create;
  symbol.symbolName := name;
  symbol.lValue := nil;
  symbol.sValue := nil;
  symbol.symbolType := symUndefined;
  result := Add (symbol);
end;



procedure TSymbolTable.checkForExistingData (index : integer);
begin
   case self[index].symbolType of
     symList   : begin
                 if self[index].lValue <> nil then
                     self[index].lValue.blockType := btGarbage
                 else
                     raise ERuntimeException.Create('Internal Error: checkingForExistingData (list)');
                 end;
     symString : begin
                 if self[index].sValue <> nil then
                    self[index].sValue.blockType := btGarbage
                 else
                     raise ERuntimeException.Create('Internal Error: checkingForExistingData (string)');
                 end;
     symUserFunc:begin
                 if self[index].fValue <> nil then
                    begin
                    self[index].fValue.Free;
                    self[index].symbolType := symUndefined;
                    end
                 else
                     raise ERuntimeException.Create('Internal Error: checkingForExistingData (userfunction)');
                 end;
   end;
end;


procedure TSymbolTable.storeToSymbolTable (index, iValue : integer);
begin
  if self[index].locked then
     raise ERuntimeException.Create ('Value is locked, you cannot change it');

  checkForExistingData (index);

  self[index].symbolType := symInteger;
  self[index].iValue := iValue;
end;


procedure TSymbolTable.storeToSymbolTable (index : integer; bValue : boolean);
begin
  if self[index].locked then
     raise ERuntimeException.Create ('Value is locked, you cannot change it');

 checkForExistingData (index);

  self[index].symbolType := symBoolean;
  self[index].bValue := bValue;
end;


procedure TSymbolTable.storeToSymbolTable (index : integer; dValue : double);
begin
  if self[index].locked then
     raise ERuntimeException.Create ('Value is locked, you cannot change it');

  checkForExistingData (index);

  self[index].symbolType := symDouble;
  self[index].dValue := dValue;
end;


procedure TSymbolTable.storeToSymbolTable (index : integer; sValue : TStringObject);
begin
 if self[index].locked then
     raise ERuntimeException.Create ('Value is locked, you cannot change it');

  checkForExistingData (index);

  if (sValue.blockType = btConstant) or (sValue.blockType = btBound) then
     self[index].sValue := sValue.clone
  else
     self[index].sValue := sValue;

  self[index].sValue.blockType := btBound;
  self[index].symbolType := symString;
end;


// Assign a list object to a symbol
procedure TSymbolTable.storeToSymbolTable (index : integer; lValue : TListObject);
begin
 if self[index].locked then
     raise ERuntimeException.Create ('Value is locked, you cannot change it');

  checkForExistingData (index);

  if  (lValue.blockType = btConstant) or
      (lValue.blockType = btBound) or
      (lValue.blockType = btOwned) then
         self[index].lValue := lValue.clone
  else
     self[index].lValue := lValue;
  self[index].lValue.blockType := btBound;
  self[index].symbolType := symList;
end;


// This isn't currently used
procedure TSymbolTable.storeToSymbolTable (index : integer; fValue : TUserFunction);
begin
 if self[index].locked then
     raise ERuntimeException.Create ('Value is locked, you cannot change it');

  checkForExistingData (index);

  self[index].symbolType := symUserFunc;
  self[index].fValue := fValue;
end;


function TSymbolTable.getFromSymbolTable (index : integer) : TSymbol;
begin
  result := self[index];
end;


// ---------------------------------------------------------------------------------------------

constructor TProgram.Create;
begin
  inherited;
  actualLength := 0;
  setLength (code, ALLOC_BY);
end;


destructor TProgram.Destroy;
begin
  inherited;
end;


constructor TProgram.Create (mycode : array of TByteCode);
var i : integer;
begin
  Create;
  for i := 0 to length (mycode) - 1 do
      append(mycode[i]);
end;


procedure TProgram.Clear;
begin
  actualLength := 0;
  setLength (code, 0);
end;


function TProgram.count : integer;
begin
  result := actualLength;
end;


function TProgram.getCurrentInstructionPointer : integer;
begin
  result := actualLength;
end;


function TProgram.createInstructionSpace : integer;
begin
  checkSpace;
  result := actualLength;
  inc(actualLength);
end;


// Set a gotolabel with the value 'value' at the location 'location'
procedure TProgram.setGotoLabel (Location, value : integer);
begin
  code[Location].index := value;
end;


procedure TProgram.checkSpace;
begin
  if actualLength = length(code) then
     setLength(code, length(code) + ALLOC_BY);
end;


procedure TProgram.append (byteCode : TByteCode);
begin
  checkSpace;
  code[actualLength] := byteCode;
  inc(actualLength);
end;


procedure TProgram.compactCode;
begin
  setLength (code, actualLength);
end;


function TProgram.addByteCode (opCode : TOpCode) : integer;
begin
  checkSpace;
  code[actualLength].opCode := opCode;
  result := actualLength;
  inc(actualLength);
end;


procedure TProgram.addByteCode (opCode : TOpCode; iValue : integer);
begin
  checkSpace;
  code[actualLength] := createByteCode (opCode, iValue);
  inc(actualLength);
end;


procedure TProgram.addByteCode (opCode : TOpCode; dValue : double);
begin
  checkSpace;
  code[actualLength] := createByteCode (opCode, dValue);
  inc(actualLength);
end;


procedure TProgram.addByteCode (opCode : TOpCode; bValue : boolean);
begin
  checkSpace;
  code[actualLength] := createByteCode (opCode, bValue);
  inc(actualLength);
end;


procedure TProgram.addByteCode (opCode : TOpCode; sValue : string);
begin
  checkSpace;
  code[actualLength] := createByteCode (opCode, sValue);
  inc(actualLength);
end;


procedure TProgram.appendProgram (aProgram : TProgram);
var i : integer;
begin
  for i := 0 to aProgram.count - 1 do
      append (aProgram.code[i]);
end;

// --------------------------------------------------------------------


function createByteCode (opCode : TOpCode; iValue : integer) : TByteCode;
begin
  result.opCode := opCode;
  result.index := iValue;
end;


function createByteCode (opCode : TOpCode; dValue : double) : TByteCode;
begin
  result.opCode := opCode;
  result.index := constantValueTable.Add (TConstantValueElement.Create(dValue));;
end;


function  createByteCode (opCode : TOpCode; bValue : boolean) : TByteCode;
begin
  result.opCode := opCode;
  result.index := integer (bValue);
end;


function createByteCode (opCode : TOpCode; sValue : string) : TByteCode;
begin
  result.opCode := opCode;
  result.index := constantValueTable.Add (TConstantValueElement.Create (sValue));
end;


function createByteCode (opCode : TOpCode) : TByteCode;
begin
  result.opCode := opCode;
  result.index := 0;
end;



initialization
end.
   



//function addFunction (name : string; nargs, nLocalVariables : integer; code : array of TByteCode) : integer;  overload;
//function addFunction (name : string; nargs, nLocalVariables : integer; aProgram : TProgram) : integer; overload;

//
//function addFunction (name : string; nargs, nLocalVariables : integer; code : array of TByteCode) : integer;
//var func : TUserFunction;
//begin
//  func := TUserFunction.Create;
//  func.name := name;
//  func.nargs := nargs;
//  func.nlocals := nLocalVariables;
//  func.theProgram := TProgram.Create (code);
//
//  result := symbolTable.addSymbol (func);
//end;
//
//
//function addFunction (name : string; nargs, nLocalVariables : integer; aProgram : TProgram) : integer;
//var func : TUserFunction;
//begin
//  func := TUserFunction.Create;
//  func.name := name;
//  func.nargs := nargs;
//  func.nlocals := nLocalVariables;
//  func.theProgram := aProgram;
//
//  result := symbolTable.addSymbol (func);
//end;