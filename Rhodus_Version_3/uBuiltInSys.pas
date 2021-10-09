unit uBuiltInSys;


// This source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com


interface

Uses SysUtils, Classes, uLibModule, System.Diagnostics;

type
  TBuiltInSys= class (TModuleLib)

     //procedure   getArgv (vm : TObject);
     constructor Create;
  end;

implementation

Uses Windows, uSymbolTable, uVM, uStringObject, uListObject, uMemoryManager, uBuiltInConfig;

// --------------------------------------------------------------------------------------------

constructor TBuiltInSys.Create;
var argv : TListObject;
    path : TListObject;
begin
  inherited Create ('sys', 'System module');

  addStringValue ('version',  uBuiltInConfig.RHODUS_VERSION, 'returns the current version number for Rhodus', True);

  argv := TListObject.Create(0);
  for var i := 0 to ParamCount do
      argv.append(TStringObject.Create (ParamStr(i)));
  argv.blockType := btBound;   // To make sure the garbage collector doesn't get it.

  addListValue ('argv', argv, 'The list of command line arguments passed', True);


  path := TListObject.Create(0);
  path.append(TStringObject.create('.'));
  path.blockType := btBound;   // To make sure the garbage collector doesn't get it.

  addListValue ('path', path, 'Search path for Rhodus import libraries', True);
end;



//procedure TBuiltInSys.getArgv (vm : TObject);
//var list : TListObject;
//begin
//  list := TListObject.Create(0);
//  for var i := 0 to ParamCount do
//      list.append(TStringObject.Create (ParamStr(i)));
//
//  TVM (vm).push(list);
//end;



end.
