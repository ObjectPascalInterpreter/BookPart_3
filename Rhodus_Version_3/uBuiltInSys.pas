unit uBuiltInSys;

{
  Unit:    uBuiltInSys.pas
  Author:  Herbert M sauro
  Date:    10/2021
  Purpose: This file implements the system methods library for the Rhodus interpreter.

  Ths source is distributed under Apache 2.0
  See https://www.apache.org/licenses/LICENSE-2.0.txt for further information

  Copyright (C)  2019-2024 Herbert M Sauro

  Author Contact Information:
  email: hsauro@gmail.com
}


interface

Uses SysUtils, Classes, uLibModule, System.Diagnostics;

type
  TBuiltInSys = class (TModuleLib)

     class var defaultDoubleFormat : string;
     class var defaultIntegerFormat : string;

     procedure setRecursionLimit (vm: TObject);
     procedure getRecursionLimit (vm: TObject);
     procedure getMaxIntSize  (vm: TObject);
     constructor Create;
     destructor  Destroy; override;
  end;

  procedure initialiseSysModuleVariables;

implementation

Uses Windows,
     uSymbolTable,
     uVM,
     uVMExceptions,
     uStringObject,
     uListObject,
     uMemoryManager,
     uRhodusTypes,
     uHelpUnit,
     uBuiltInConfig;

var
    path : TListObject;

// The path variabvle has to be separated out from the sys module
// because each time the sys module is imported into a new module
// it is recreated. We need to protect the path variable from this.
procedure initialiseSysModuleVariables;
begin
  // Default path
  path := TListObject.Create(0);
  path.append(TStringObject.create('.'));
  path.blockType := btBound;   // To make sure the garbage collector doesn't get it.
end;

function makePathVariable : TListObject;
begin
  result := TListObject.Create(0);
  result.append(TStringObject.create('.'));
  result.blockType := btBound;   // To make sure the garbage collector doesn't get it.

end;


// --------------------------------------------------------------------------------------------

constructor TBuiltInSys.Create;
var argv : TListObject;
    astr : TStringObject;
    sym : TSymbol;
begin
  inherited Create ('sys');

  sym := addStringValue ('version',  string (uBuiltInConfig.RHODUS_VERSION), True);//'returns the current version number for Rhodus', True);
  //sym.obj.blockType := btBound;
  sym := addStringValue ('doubleFormat',  TBuiltInSys.defaultDoubleFormat, True);//'default output format string for double values', False);
  //sym.obj.blockType := btBound;
  sym := addStringValue ('integerFormat',  TBuiltInSys.defaultIntegerFormat, True);//'default output format string for integer values', False);
  //sym.obj.blockType := btBound;  // protect the string object from the garbage collector.

  argv := TListObject.Create(0);
  for var i := 0 to ParamCount do
      begin
      astr := TStringObject.Create (ParamStr(i));
      argv.append(astr);
      end;
  argv.blockType := btBound;   // To make sure the garbage collector doesn't get it.

  addListValue ('argv', argv, True);//'The list of command line arguments passed', True);

  addListValue ('path', makePathVariable, True);//'Search path for Rhodus import libraries', True);

  addMethod (setRecursionLimit,  1, 'setRecursionLimit', 'Set the function recursion limit');
  addMethod (getRecursionLimit,  0, 'getRecursionLimit', 'Get the current function recursion limit');
  addMethod (getMaxIntSize,      0, 'maxIntSize', 'Returns the maximum initeger size');
end;


destructor TBuiltInSys.Destroy;
begin
  inherited;
end;

procedure TBuiltInSys.getRecursionLimit (vm: TObject);
begin
  TVM (vm).push(TVM (vm).getRecursionLimit());
end;


procedure TBuiltInSys.setRecursionLimit (vm: TObject);
var rl : integer;
begin
  rl := TVM (vm).popInteger;
  if rl > TVM (vm).recursionLimit then
     TVM (vm).setRecursionLimit (rl)
  else
     raise ERuntimeException.Create('The current recursion limit is: ' + inttostr (TVM (vm).getRecursionLimit()) + '. You cannot go below that');
  TVM (vm).pushNone;
end;


procedure TBuiltInSys.getMaxIntSize  (vm: TObject);
begin
  TVM (vm).push (MaxInt);
end;

initialization
  TBuiltInSys.defaultDoubleFormat := '%g';
  TBuiltInSys.defaultIntegerFormat := '%d';
end.
