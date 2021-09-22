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
         index1, index2 : integer;    // 2 * 4 bytes
         float : double;   // This is currently only used by the inc and dec op codes
         moduleName : string;
         symbolName : string;
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
           constantValueTable : TConstantValueTable;
           function  count : integer;
           procedure clear;
           procedure append (byteCode : TByteCode);
           procedure compactCode;
           function  addByteCode (opCode : TOpCode) : integer; overload;
           procedure addByteCode (opCode : TOpCode; iValue : integer); overload;
           //procedure addByteCode (opCode : TOpCode; dValue : double); overload;
           procedure addByteCode (opCode : TOpCode; bValue : boolean); overload;
           //procedure addByteCode (opCode : TOpCode; sValue : string); overload;
           procedure addByteCode (opCode : TOpCode; index1, index2 : integer); overload;
           procedure addByteCode (opCode : TOpCode; const symbolName : string; increment : double);  overload;
           procedure addModuleByteCode (opCode : TOpCode; const moduleName : string);
           procedure addSymbolByteCode (opCode : TOpCode; const symbolName : string);
           procedure addFullSymbolByteCode (opCode : TOpCode; const moduleName, symbolName : string);
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
//function  createByteCode (opCode : TOpCode; dValue : double) : TByteCode; overload;
function  createByteCode (opCode : TOpCode; bValue : boolean) : TByteCode; overload;
function  createByteCode (opCode : TOpCode) : TByteCode; overload;

implementation


function createByteCode (opCode : TOpCode; iValue : integer) : TByteCode;
begin
  result.opCode := opCode;
  result.index1 := iValue;
end;


//function createByteCode (opCode : TOpCode; dValue : double) : TByteCode;
//begin
//  result.opCode := opCode;
//  result.index1 := constantValueTable.Add (TConstantValueElement.Create(dValue));
//end;


function  createByteCode (opCode : TOpCode; bValue : boolean) : TByteCode;
begin
  result.opCode := opCode;
  result.index1 := integer (bValue);
end;


function createByteCode (opCode : TOpCode) : TByteCode;
begin
  result.opCode := opCode;
  result.index1 := 0;
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


procedure TProgram.Clear;
begin
  actualLength := 0;
  setLength (code, 0);
  constantValueTable.Clear;
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
  code[Location].index1 := value;
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


//procedure TProgram.addByteCode (opCode : TOpCode; dValue : double);
//begin
//  checkSpace;
//  code[actualLength] := createByteCode (opCode, dValue);
//  inc(actualLength);
//end;


procedure TProgram.addByteCode (opCode : TOpCode; bValue : boolean);
begin
  checkSpace;
  code[actualLength] := createByteCode (opCode, bValue);
  inc(actualLength);
end;


//procedure TProgram.addByteCode (opCode : TOpCode; sValue : string);
//begin
//  checkSpace;
//  code[actualLength] := createByteCode (opCode, sValue);
//  inc(actualLength);
//end;


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


procedure TProgram.addByteCode (opCode : TOpCode; index1, index2 : integer);
begin
  checkSpace;
  code[actualLength] := createByteCode (opCode, index1);
  code[actualLength].index2 := index2;
  inc(actualLength);
end;


procedure TProgram.addFullSymbolByteCode (opCode : TOpCode; const moduleName, symbolName : string);
begin
  checkSpace;
  code[actualLength] := createByteCode (opCode);
  code[actualLength].symbolName := symbolName;
  code[actualLength].moduleName := moduleName;
  inc(actualLength);
end;


//procedure TProgram.addByteCode (opCode : TOpCode; const moduleName, symbolName : string; float : double);
procedure TProgram.addByteCode (opCode : TOpCode; const symbolName : string; increment : double);
begin
  checkSpace;
  code[actualLength] := createByteCode (opCode, 0);
  code[actualLength].float := increment;
  code[actualLength].symbolName := symbolName;
  inc(actualLength);
end;


procedure TProgram.addLocalForByteCode (opCode : TOpCode; symbolIndex : integer; stepValue : double);
begin
  checkSpace;
  code[actualLength] := createByteCode (opCode, 0);
  code[actualLength].index1 := symbolIndex;
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
