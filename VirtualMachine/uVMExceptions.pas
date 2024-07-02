unit uVMExceptions;

// This source is distributed under Apache 2.0

// Copyright (C) 2020 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com


interface

Uses SysUtils, Classes;

type
  ERuntimeException = class(Exception);
  EInternalException = class(Exception);

  procedure raiseError (msg : string; lineNumber : integer);
  procedure raiseInternalError (msg : string);

implementation

procedure raiseError (msg : string; lineNumber : integer);
begin
  raise ERuntimeException.Create(msg + ' at line number: ' + inttostr (lineNumber));
end;

procedure raiseInternalError (msg : string);
begin
  raise EInternalException.Create(msg);
end;


end.
