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

  procedure raiseInternalError (msg : string);

implementation

procedure raiseInternalError (msg : string);
begin
  raise EInternalException.Create(msg);
end;


end.
