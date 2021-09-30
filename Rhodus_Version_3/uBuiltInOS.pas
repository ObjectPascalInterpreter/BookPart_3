unit uBuiltInOS;


// This source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com


interface

Uses SysUtils, Classes, uLibModule, System.Diagnostics;

const
   RHODUS_VERSION : string = '3.0.0.5';

type
  TBuiltInOS = class (TModuleLib)

     procedure   getVersion (vm : TObject);
     procedure   getPwd (vm : TObject);
     procedure   setPwd (vm : TObject);
     constructor Create;
  end;

  TBuiltInTime = class (TModuleLib)

     lFreq : int64;
     procedure getTimeSeconds (vm : TObject);
     constructor Create;
  end;

implementation

Uses Windows, uSymboLTable, uVM, uStringObject, uListObject, uMemoryManager;

// --------------------------------------------------------------------------------------------

constructor TBuiltInOS.Create;
var path : TListObject;
begin
  inherited Create ('os', 'Operating system module');

  addMethod (getpwd, 0, 'getcwd', 'Return the path to the current working directory');
  addMethod (setpwd, 1, 'setcwd', 'Sets the current wroking dirctory');
  addMethod (getversion, 0, 'version', 'Get the current version number for Rhodus');

  path := TListObject.Create(0);
  path.append(TStringObject.create('.'));
  path.blockType := btBound;   // To make sure the garbage collector doesn't get it.

  addListValue ('path', path, 'Search path for Rhodus import libraries', True);
end;


procedure TBuiltInOS.getPwd (vm : TObject);
begin
   TVM (vm).push(TStringObject.create (GetCurrentDir));
end;


procedure TBuiltInOS.setPwd (vm : TObject);
var astr : string;
begin
  astr := TVM (vm).popString.value;
  SetCurrentDir(astr);
  TVM (vm).pushNone;
end;

procedure TBuiltInOs.getVersion (vm : TObject);
begin
  TVM (vm).push (TStringObject.create(RHODUS_VERSION));
end;

// --------------------------------------------------------------------------------------------


constructor TBuiltInTime.Create;
begin
  inherited Create ('time', 'Time functions');

  QueryPerformanceFrequency(lFreq);
  addMethod (getTimeSeconds, 0, 'getTickCount', 'Return the eturns the number of milliseconds since the system was started');
end;


procedure TBuiltInTime.getTimeSeconds (vm : TObject);
var icount1 : Int64;
begin
  QueryPerformanceCounter(icount1);
  TVM (vm).push (trunc (1000*icount1/lFreq));
end;


end.
