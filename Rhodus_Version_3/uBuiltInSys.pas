unit uBuiltInSys;


// This source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com


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
  end;

implementation

Uses Windows,
     uSymbolTable,
     uVM,
     uVMExceptions,
     uStringObject,
     uListObject,
     uMemoryManager,
     uBuiltInConfig;

// --------------------------------------------------------------------------------------------

constructor TBuiltInSys.Create;
var argv : TListObject;
    path : TListObject;
    astr : TStringObject;
begin
  inherited Create ('sys', 'System module');

  addStringValue ('version',  uBuiltInConfig.RHODUS_VERSION, 'returns the current version number for Rhodus', True);
  addStringValue ('doubleFormat',  TBuiltInSys.defaultDoubleFormat, 'default output format string for double values', False);
  addStringValue ('integerFormat',  TBuiltInSys.defaultIntegerFormat, 'default output format string for integer values', False);

  argv := TListObject.Create(0);
  for var i := 0 to ParamCount do
      begin
      astr := TStringObject.Create (ParamStr(i));
      argv.append(astr);
      end;
  argv.blockType := btBound;   // To make sure the garbage collector doesn't get it.

  addListValue ('argv', argv, 'The list of command line arguments passed', True);

  path := TListObject.Create(0);
  path.append(TStringObject.create('.'));
  path.blockType := btBound;   // To make sure the garbage collector doesn't get it.

  addListValue ('path', path, 'Search path for Rhodus import libraries', True);

  addMethod (setRecursionLimit,  1, 'setRecursionLimit', 'Set the function recursion limit');
  addMethod (getRecursionLimit,  0, 'getRecursionLimit', 'Get the current function recursion limit');
  addMethod (getMaxIntSize,      0, 'maxIntSize', 'Returns the maximum initeger size');
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
