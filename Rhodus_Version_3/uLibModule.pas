unit uLibModule;


// This source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com


interface

Uses SysUtils,
     Classes,
     uSymbolTable,
     uListObject,
     uStringObject,
     uValueObject,
     uHelpUnit;

type
  TModuleLib = class (TModule)

    procedure   addMethod(methodPtr : TxBuiltInFunction; nArgs : integer; methodName, helpStr : string); overload;
    procedure   addMethod (methodPtr : TxBuiltInFunction; nArgs : integer; methodName : string);  overload;
    procedure   addStringValue (name, value : string; locked : boolean);
    procedure   addListValue  (name : string; value : TListObject; locked : boolean);
    procedure   addObjectValue (name : string; value : TValueObject; locked : boolean);

    procedure   findSymbol (vm : TObject);
    procedure   callDir (vm : TObject);
    procedure   callContains (vm : TObject);
    constructor Create (moduleName : string);
    destructor  Destroy; override;
  end;


implementation

Uses uRhodusTypes,
     uVM,
     uMemoryManager,
     uVMExceptions,
     uBuiltInGlobal, // need access to the mainModule
     uMachineStack;


constructor TModuleLib.Create (modulename : string);
var f : TUserFunction;
begin
  inherited Create (moduleName);
  self.compiled := True;

  // Add the builtin default methods every module has

  f := TUserFunction.Create('dir', 0, callDir);
  f.moduleRef := self;
  f.help := THelp.Create ('Get a list of the supported methods and values');
  self.symbolTable.addSymbol (f, True); // // locked = True

  f := TUserFunction.Create ('contains', 1, callContains);
  f.moduleRef := self;
  f.help := THelp.Create ('Returns true if the module includes the given symbol, e.g math.contains ("cos")');
  self.symbolTable.addSymbol (f, True); // // locked = True

  f := TUserFunction.Create ('find', 1, findSymbol);
  f.moduleRef := self;
  f.help := THelp.Create ('Returns true if the module contains the symbol e.g math.find ("cos")');
  self.symbolTable.addSymbol (f, True); // // locked = True

  f := TUserFunction.Create ('help', 0, getHelp);
  f.moduleRef := self;
  self.symbolTable.addSymbol (f, True); // // locked = True
end;


destructor TModuleLib.Destroy;
begin
  inherited;
end;


procedure TModuleLib.findSymbol (vm : TObject);
var astr : TStringObject;
    sym : TSymbol;
begin
  astr := TVM (vm).popString();

  // There is a special case for the mainModule as its not in the
  // modujle list so we explictly check for it
  if astr.value = TSymbol.mainModuleId then
      TVM (vm).push(mainModule)
  else
     begin
     sym := self.find(astr.value);
     if sym = nil then
        raise ERuntimeException.Create('Unable to locate symbol: ' + astr.value);

     case sym.symbolType of
       symInteger : TVM (vm).push(sym.iValue);
       symDouble : TVM (vm).push(sym.dValue);
       symUserFunc : TVM (vm).push(sym.fValue);
       symModule : TVM (vm).push(sym.mValue);
     else
        raise ERuntimeException.Create('Type not supported in findSymbol');
     end;
  end;
end;


procedure TModuleLib.callDir (vm : TObject);
var l : TListObject;
    key : string;
begin
  l := TListObject.create (0);
  if moduleName = TSymbol.globalId then
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
  TVM (vm).push (l);
end;


procedure TModuleLib.callContains (vm : TObject);
var key, arg : string;
begin
  vm := TVM (vm);
  arg := TVM (vm).popString.value;
  if moduleName = TSymbol.globalId then
     begin
     for key in self.symbolTable.keys do
         if key = arg then
            begin
            TVM (vm).push(True);
            exit;
            end;
     TVM (vm).push(False);
     exit;
     end
  else
     begin
     for key in self.symbolTable.keys do
         begin
         if key = arg then
            begin
            TVM (vm).push(True);
            exit;
            end;
         end;
     TVM (vm).push (False);
     exit;
     end;
end;


procedure TModuleLib.addObjectValue (name : string; value : TValueObject; locked : boolean);
begin
  self.symbolTable.addSymbol(name, value, locked, THelp.CreateValue (moduleName, name));
end;


procedure TModuleLib.addListValue  (name : string; value : TListObject; locked : boolean);
begin
  self.symbolTable.addSymbol(name, value, locked, THelp.CreateValue (moduleName, name));
end;


procedure TModuleLib.addStringValue (name, value : string; locked : boolean);
begin
  self.symbolTable.addSymbol(name, TStringObject.Create (value), locked, THelp.CreateValue (moduleName, name));
end;


procedure TModuleLib.addMethod (methodPtr : TxBuiltInFunction; nArgs : integer; methodName, helpStr : string);
var f : TUserFunction;
begin
  f := TUserFunction.Create(methodName, nArgs, methodPtr);
  f.help := THelp.Create (helpStr);
  f.moduleRef := self;
  self.symbolTable.addSymbol (f, True);  // locked = True
end;


procedure TModuleLib.addMethod (methodPtr : TxBuiltInFunction; nArgs : integer; methodName : string);
var f : TUserFunction;
begin
  f := TUserFunction.Create(methodName, nArgs, methodPtr);
  f.help := THelp.CreateMethod(moduleName, methodName);
  f.moduleRef := self;
  self.symbolTable.addSymbol (f, True);  // locked = True
end;



end.
