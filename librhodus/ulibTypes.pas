unit ulibTypes;

interface

type
  TPrint = procedure (astr : AnsiString);
  TReadString = function (const prompt : AnsiString) : AnsiString;

  TRhodusConfig = record
      printPtr : TPrint;
      printlnPtr : TPrint;
      readStringPtr : TReadString;
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
