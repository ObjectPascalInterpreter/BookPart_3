unit uLibModule;


// This source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com


interface

Uses SysUtils, Classes, uSymbolTable, uListObject, uStringObject;

type
  TModuleLib = class (TModule)

    procedure   addMethod(methodPtr : TxBuiltInFunction; nArgs : integer; name, helpStr : string);
    procedure   addDoubleValue (name : string; value : double; helpStr : string; locked : boolean);
    procedure   addStringValue (name, value, helpStr : string; locked : boolean);
    procedure   addListValue  (name : string; value : TListObject; helpStr : string; locked : boolean);

    procedure   callDir (vmObj : TObject);
    procedure   callContains (vmObj : TObject);
    constructor Create (name : string; helpStr : string);
    destructor  Destroy; override;
  end;


implementation

Uses uVM, uMemoryManager, uVMExceptions, uMachineStack;


constructor TModuleLib.Create (name : string; helpStr : string);
var f : TUserFunction;
begin
  inherited Create (name);
  self.helpStr := helpStr;
  self.compiled := True;

  f := TUserFunction.Create('dir', 0, callDir);
  f.helpStr := 'Get a list of the supported methods and values';
  self.symbolTable.addSymbol (f, True); // // locked = True

  f := TUserFunction.Create ('contains', 1, callContains);
  f.helpStr := 'Returns true if the module contains the given symbol, e.g mod.contains ("cos")';
  self.symbolTable.addSymbol (f, True); // // locked = True
end;


destructor TModuleLib.Destroy;
begin
  inherited;
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


procedure TModuleLib.callContains (vmObj : TObject);
var vm : TVM;
    key, arg : string;
begin
  vm := TVM (vmObj);
  arg := vm.popString.value;
  if name = TSymbol.globalId then
     begin
     for key in self.symbolTable.keys do
         if key = arg then
            begin
            vm.push(True);
            exit;
            end;
     vm.push(False);
     exit;
     end
  else
     begin
     for key in self.symbolTable.keys do
         begin
         if key = arg then
            begin
            vm.push(True);
            exit;
            end;
         end;
     vm.push (False);
     exit;
     end;
end;


procedure TModuleLib.addDoubleValue (name : string; value : double; helpStr : string; locked : boolean);
begin
  self.symbolTable.addSymbol(name, value, locked, helpStr);
end;


procedure TModuleLib.addListValue  (name : string; value : TListObject; helpStr : string; locked : boolean);
begin
  self.symbolTable.addSymbol(name, value, locked, helpStr);
end;


procedure TModuleLib.addStringValue (name, value, helpStr : string; locked : boolean);
begin
  self.symbolTable.addSymbol(name, TStringObject.Create (value), locked, helpStr);
end;


procedure TModuleLib.addMethod (methodPtr : TxBuiltInFunction; nArgs : integer; name, helpStr : string);
var f : TUserFunction;
begin
  f := TUserFunction.Create(name, nArgs, methodPtr);
  f.helpStr := helpStr;
  self.symbolTable.addSymbol (f, True);  // locked = True
end;



end.
