unit uLibModule;


// This source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com


interface

Uses SysUtils, Classes, uSymbolTable;

type
  TModuleLib = class (TModule)

    procedure   addMethod(methodPtr : TxBuiltInFunction; nArgs : integer; name, helpStr : string);
    procedure   addValue (name : string; value : double; helpStr : string);

    procedure   callDir (vmObj : TObject);
    constructor Create (name : string; helpStr : string);
  end;


implementation

Uses uVM, uStringObject, uListObject, uMemoryManager, uVMExceptions, uMachineStack;


constructor TModuleLib.Create (name : string; helpStr : string);
var f : TUserFunction;
begin
  inherited Create (name);
  self.helpStr := helpStr;
  self.compiled := True;

  f := TUserFunction.Create('dir', 0, callDir);
  f.helpStr := 'Get a list of the supported methods and values';
  self.symbolTable.addSymbol (f, True); // // locked = True
end;


procedure TModuleLib.callDir (vmObj : TObject);
var l : TListObject;
    vm : TVM;
    key : string;
begin
  vm := TVM (vmObj);
  l := TListObject.create (0);
  if name = TSymbol.globalId then
     begin
     for key in self.symbolTable.keys do
         l.append (TStringObject.create (key));
     end
  else
     begin
     for key in self.symbolTable.keys do
         begin
         // Comment out, lets list all symbols
         //if self.symbolTable.Items[key].symbolType <> symModule then
            l.append (TStringObject.create (key));
         end;
     end;
  vm.push (l);
end;


procedure TModuleLib.addValue (name : string; value : double; helpStr : string);
begin
  self.symbolTable.addSymbol(name, value, True, helpStr);
end;


procedure TModuleLib.addMethod (methodPtr : TxBuiltInFunction; nArgs : integer; name, helpStr : string);
var f : TUserFunction;
begin
  f := TUserFunction.Create(name, nArgs, methodPtr);
  f.helpStr := helpStr;
  self.symbolTable.addSymbol (f, True);  // locked = True
end;


end.
