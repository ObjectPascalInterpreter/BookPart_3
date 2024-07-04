unit uProgramCode;


// This source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

Uses Classes, SysUtils, uOpCodes, uConstantTable;

type
   TByteCode = packed record
         opCode : TOpCode;   // 1 byte
         index : integer;    // 4 bytes
         float : double;     // This is currently only used by the inc and dec op codes
         moduleName : string;// Use by import module
         symbolName : string;
         lineNumber : integer;
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
           constantValueTable : TConstantValueTable;  // This memory is handled differently from the memorymanager
           function  count : integer;
           procedure clearCode;
           procedure append (byteCode : TByteCode);
           procedure compactCode;
           function  addByteCode (opCode : TOpCode; lineNumber : integer) : integer; overload;
           procedure addByteCode (opCode : TOpCode; iValue, lineNumber : integer); overload;
           procedure addByteCode (opCode : TOpCode; bValue : boolean; lineNumber : integer); overload;
           procedure addByteCode (opCode : TOpCode; const symbolName : string; increment : double; lineNumber : integer);  overload;
           procedure addModuleByteCode (opCode : TOpCode; const moduleName : string);
           procedure addSymbolByteCode (opCode : TOpCode; const symbolName : string);
           procedure addFullSymbolByteCode (opCode : TOpCode; const moduleName, symbolName : string; lineNumber : integer);
           procedure addLocalForByteCode (opCode : TOpCode; symbolIndex : integer; stepValue : double);
           procedure addStoreByteCode (opCode : TOpCode; const symbolName : string);
           procedure addLoadByteCode (opCode : TOpCode; const symbolName : string);

           procedure appendProgram (aProgram : TProgram);
           function  getCurrentInstructionPointer : integer;
           function  createInstructionSpace : integer;
           procedure setGotoLabel (Location, value : integer);

           function    clone : TProgram;
           constructor Create; overload;
           constructor Create (mycode : array of TByteCode);  overload;
           destructor  Destroy; override;
   end;

function  createByteCode (opCode : TOpCode; iValue : integer) : TByteCode; overload;
function  createByteCode (opCode : TOpCode; bValue : boolean) : TByteCode; overload;
function  createByteCode (opCode : TOpCode) : TByteCode; overload;

implementation


function createByteCode (opCode : TOpCode; iValue : integer) : TByteCode;
begin
  result.opCode := opCode;
  result.index := iValue;
end;


function  createByteCode (opCode : TOpCode; bValue : boolean) : TByteCode;
begin
  result.opCode := opCode;
  result.index := integer (bValue);
end;


function createByteCode (opCode : TOpCode) : TByteCode;
begin
  result.opCode := opCode;
  result.index := 0;
end;


// ----------------------------------------------------------------------

constructor TProgram.Create;
begin
  inherited;
  actualLength := 0;
  setLength (code, ALLOC_BY);
  constantValueTable := TConstantValueTable.Create;
end;


destructor TProgram.Destroy;
begin
  setlength (code, 0);
  constantValueTable.Free;
  inherited;
end;


constructor TProgram.Create (mycode : array of TByteCode);
var i : integer;
begin
  Create;
  for i := 0 to length (mycode) - 1 do
      append(mycode[i]);
end;


function TProgram.clone: TProgram;
begin
  result := TProgram.Create;
  result.appendProgram (self);
  result.constantValueTable.free;
  result.constantValueTable := constantValueTable.clone;
end;


// Doesn't clear the constantValueTable
procedure TProgram.ClearCode;
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


function TProgram.addByteCode (opCode : TOpCode; lineNumber : integer) : integer;
begin
  checkSpace;
  code[actualLength].opCode := opCode;
  result := actualLength;
  inc(actualLength);
end;


procedure TProgram.addByteCode (opCode : TOpCode; iValue, lineNumber : integer);
begin
  checkSpace;
  code[actualLength] := createByteCode (opCode, iValue);
  code[actualLength].lineNumber := lineNumber;
  inc(actualLength);
end;


procedure TProgram.addByteCode (opCode : TOpCode; bValue : boolean; lineNumber : integer);
begin
  checkSpace;
  code[actualLength] := createByteCode (opCode, bValue);
  inc(actualLength);
end;


procedure TProgram.addStoreByteCode (opCode : TOpCode; const symbolName : string);
begin
  checkSpace;
  code[actualLength] := createByteCode (opCode);
  code[actualLength].symbolName := symbolName;
  inc(actualLength);
end;


procedure TProgram.addLoadByteCode (opCode : TOpCode; const symbolName : string);
begin
  checkSpace;
  code[actualLength] := createByteCode (opCode);
  code[actualLength].symbolName := symbolName;
  inc(actualLength);
end;


procedure TProgram.addSymbolByteCode (opCode : TOpCode; const symbolName : string);
begin
  checkSpace;
  code[actualLength] := createByteCode (opCode);
  code[actualLength].symbolName := symbolName;
  inc(actualLength);
end;


procedure TProgram.addModuleByteCode (opCode : TOpCode; const moduleName : string);
begin
  checkSpace;
  code[actualLength].opCode := opCode;
  code[actualLength].moduleName := moduleName;
  inc(actualLength);
end;


procedure TProgram.addFullSymbolByteCode (opCode : TOpCode; const moduleName, symbolName : string; lineNumber : integer);
begin
  checkSpace;
  code[actualLength] := createByteCode (opCode);
  code[actualLength].symbolName := symbolName;
  code[actualLength].moduleName := moduleName;
  code[actualLength].lineNumber := lineNumber;
  inc(actualLength);
end;


procedure TProgram.addByteCode (opCode : TOpCode; const symbolName : string; increment : double; lineNumber : integer);
begin
  checkSpace;
  code[actualLength] := createByteCode (opCode, 0);
  code[actualLength].float := increment;
  code[actualLength].symbolName := symbolName;
  code[actualLength].lineNumber := lineNumber;
  inc(actualLength);
end;


procedure TProgram.addLocalForByteCode (opCode : TOpCode; symbolIndex : integer; stepValue : double);
begin
  checkSpace;
  code[actualLength] := createByteCode (opCode, 0);
  code[actualLength].index := symbolIndex;
  code[actualLength].float := stepValue;

  inc(actualLength);
end;


procedure TProgram.appendProgram (aProgram : TProgram);
var i : integer;
begin
  for i := 0 to aProgram.count - 1 do
      append (aProgram.code[i]);
end;

// --------------------------------------------------------------------



end.
