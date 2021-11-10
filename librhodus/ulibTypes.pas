unit ulibTypes;

interface

type
  TPrint = procedure (astr : AnsiString);

  TRhodusConfig = record
      printPtr : TPrint;
      printlnPtr : TPrint;
  end;

  TRhodusSettings = record
      versionStr : PAnsiChar;
  end;
  PRhodusSettings = ^TRhodusSettings;

  TRhodusError  = record
      errorCode : integer;
      errorMsg : PAnsiChar;
  end;
  PRhodusError = ^TRhodusError;

implementation

end.
