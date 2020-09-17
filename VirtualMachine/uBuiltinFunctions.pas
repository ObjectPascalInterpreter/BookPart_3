unit uBuiltinFunctions;

// Developed using Delphi for Windows and Mac platforms.

// *** Ths source is distributed under Apache 2.0 ***

// Copyright (C) 2019-2020 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

Uses System.SysUtils, uUtils, uMachineStack, System.generics.Collections, uVM;

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


var builtinList : TBuiltinList;

implementation

Uses Math, uVMExceptions, uStringObject, uSymbolTable, uModule;

procedure argMustBeNumber;
begin
  raise ERuntimeException.Create('argument must be a number');
end;


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


procedure myInt (vm : TVM);
var x : PMachineStackRecord; tmp : int32;
begin
  x := vm.pop;
  case x.stackType of
       stInteger : vm.push (x.iValue);
       stDouble  : begin
                   // Do it this way inorder to get a range check error
                   // if dValue can't be case to a 32-bit integer
                   tmp := trunc (x.dValue);
                   vm.push (int32 (tmp));
                   end;
  else
     argMustBeNumber;
  end;
end;


procedure readNumber (vm : TVM);
var s : string;
    iValue : integer;
    dValue : double;
begin
  readln(s);
  while (not TryStrToInt(s, iValue)) and (not TryStrToFloat(s, dValue)) do
      begin
      writeln ('Number error: ' + s + ' is not a number, try again');
      readln (s);
      end;
  if TryStrToInt(s, iValue) then
     vm.push (iValue)
  else
    vm.push (dValue);
end;


procedure readString (vm : TVM);
var s : string;
    sObj : TStringObject;
begin
  readln(s);
  sobj := TStringObject.create (s);
  vm.push (sObj);
end;


function getModuleHelp (m : TModule) : string;
var i : integer; f : TUserFunction;
begin
  result := 'Module: ' + m.helpStr + sLineBreak;
  for i := 0 to m.symbolTable.Count - 1 do
      begin
      case m.symbolTable[i].symbolType of
         symUserFunc :
             begin
             f := m.symbolTable[i].fValue;
             result := result + f.name + ': ' + f.helpStr + sLineBreak;
             end
      else
         result := result + m.symbolTable[i].symbolName + ': ' + m.symbolTable[i].helpStr + sLineBreak;
      end;
      end;
end;


procedure myHelp (vm : TVM);
var x : PMachineStackRecord;
    so : TStringObject;
begin
  x := vm.pop;
  case x.stackType of
    stInteger  : vm.push (TStringObject.create ('Integer Value'));
    stBoolean  : vm.push (TStringObject.create ('Boolean Value'));
    stDouble   : vm.push (TStringObject.create ('Double Value'));
    stString   : vm.push (TStringObject.create ('String Value'));
    stList     : vm.push (TStringObject.create ('List Value'));
    stModule   : vm.push (TStringObject.create (getModuleHelp (x.mValue)));
    stFunction : vm.push (TStringObject.create ('User Function: ' + x.fvalue.helpStr));
  else
    vm.push (TStringObject.create ('Undefined Value'));
  end;

end;


procedure mypwd (vm : TVM);
begin
   vm.push(TStringObject.create (GetCurrentDir));
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



initialization
  builtinList := TBuiltinList.Create;
  addBuiltIns ('readNumber', 0, 'read an integer from the console', readNumber);
  addBuiltIns ('readString', 0, 'read a string from the console', readString);
  addBuiltIns ('int', 1, 'convert float to integer: int (3.4)', myInt);
  addBuiltIns ('pwd', 0, 'Return the path to the current working directory', myPwd);

  addBuiltIns ('helpx', 1, 'Get help on object', myHelp);
finalization
  builtinList.Free;
end.

