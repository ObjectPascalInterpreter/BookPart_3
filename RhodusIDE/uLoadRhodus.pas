unit uLoadRhodus;

interface

Uses uRhodusLibTypes;

type
  TRhodusInitialise = function (var config : TRhodusConfig) : THandle; stdcall;
  TRhodusRun = function  (handle : THandle; code : AnsiString) : integer; stdcall;
  TRhodusTerminate = procedure (handle : THandle); stdcall;
  TRhodusGetLastError = function (handle : THandle) : PRhodusError; stdcall;
  TRhodusGetSettings = function (handle : THandle) : PRhodusSettings; cdecl;

var
    graphicsMethods : TGraphicsMethods;
    rhodus_initialize : TRhodusInitialise;
    rhodus_run : TRhodusRun;
    rhodus_terminate : TRhodusTerminate;
    rhodus_getLastError : TRhodusGetLastError;
    rhodus_getSettings : TRhodusGetSettings;

    config : TRhodusConfig;

function loadRhodusDll : boolean;

implementation

Uses Winapi.Windows;

function loadRhodusDll : boolean;
var handle: THandle;
begin
  result := True;
  handle := LoadLibrary('librhodus.dll');
  if handle = 0 then
     exit (False);

  // We should really check that we succesfully retreive each function pointer
  @rhodus_initialize := GetProcAddress(Handle, 'rhodus_initialize');
  @rhodus_run := GetProcAddress(Handle, 'rhodus_run');
  @rhodus_terminate := GetProcAddress(Handle, 'rhodus_terminate');
  @rhodus_getLastError := GetProcAddress(Handle, 'rhodus_getLastError');
  @rhodus_getSettings := GetProcAddress(Handle, 'rhodus_getSettings');
end;


end.
