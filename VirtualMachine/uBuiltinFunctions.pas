unit uBuiltinFunctions;

// Developed using Delphi for Windows and Mac platforms.

// *** This source is distributed under Apache 2.0 ***

// Copyright (C) 2019-2020 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

Uses System.SysUtils, uUtils, uMachineStack, System.generics.Collections, uVM, uSymbolTable;

type
   TBuiltInFunction = procedure (vm : TVM);

   TBuiltinFunctionRecord = record
       name : string;
       nArguments : integer;
       helpStr : string;
       funcPtr : TBuiltInFunction;
   end;

   TBuiltinList = class (TList<TBuiltInFunctionRecord>)
     public
        function find(const functionName: string; var index : integer): boolean;
   end;

   procedure addToModule (module : TModule);

var builtinList : TBuiltinList;

implementation

Uses Math, StrUtils, uVMExceptions, uStringObject, uListObject, uCompile;


function TBuiltinList.find(const functionName: string; var index : integer) : boolean;
var i : integer;
begin
  for i := 0 to Count-1 do
    if Self[i].name = functionName then
       begin
       index := i;
       exit (true);
       end;
  result := false;
end;


procedure addBuiltIns (name : string; nArguments : integer; helpStr : string; fcn : TBuiltInFunction);
var builtin : TBuiltInFunctionRecord;
begin
  builtin.name := name;
  builtin.nArguments := nArguments;
  builtin.helpStr := helpStr;
  builtin.funcPtr := fcn;
  builtinList.Add (builtin);
end;


procedure addToModule (module : TModule);
begin
end;

initialization
  builtinList := TBuiltinList.Create;
finalization
  builtinList.Free;
end.

