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

implementation

end.
