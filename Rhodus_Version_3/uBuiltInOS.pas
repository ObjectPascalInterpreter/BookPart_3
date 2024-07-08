unit uBuiltInOS;

{
  Unit:    uBuiltInOS.pas
  Author:  Herbert M sauro
  Date:    10/2021
  Purpose: This file implements the OS library for the Rhodus interpreter.

  Ths source is distributed under Apache 2.0
  See https://www.apache.org/licenses/LICENSE-2.0.txt for further information

  Copyright (C)  2019-2024 Herbert M Sauro

  Author Contact Information:
  email: hsauro@gmail.com
}

interface

Uses SysUtils, Classes, uLibModule, System.Diagnostics;


type
  TBuiltInOS = class (TModuleLib)

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

Uses Windows,
     uSymboLTable,
     uVM,
     uStringObject,
     uListObject,
     uHelpUnit,
      uMemoryManager;

// --------------------------------------------------------------------------------------------

constructor TBuiltInOS.Create;
begin
  inherited Create ('os');

  addMethod (getpwd, 0, 'getcwd', 'Return the path to the current working directory');
  addMethod (setpwd, 1, 'setcwd', 'Sets the current wroking dirctory');
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

// --------------------------------------------------------------------------------------------


constructor TBuiltInTime.Create;
begin
  inherited Create ('time');

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
