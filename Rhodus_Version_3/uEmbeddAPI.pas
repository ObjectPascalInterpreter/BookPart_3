unit uEmbeddAPI;

interface

Uses Classes, SysUtils;

type
  TRhodusConfig = record

  end;

function rhodus_initialize (var config : TRhodusConfig) : THandle;

implementation


function rhodus_initialize (var config : TRhodusConfig) : THandle;
begin

end;

function runCode (handle : THandle; code : AnsiString) : integer;
begin

end;

procedure rhodus_terminate (handle : THandle);
begin

end;

end.
