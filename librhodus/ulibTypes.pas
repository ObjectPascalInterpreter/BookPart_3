unit ulibTypes;

interface

type
  TPrint = procedure (astr : AnsiString);

  TRhodusConfig = record
      printPtr : TPrint;
      printlnPtr : TPrint;
  end;

implementation

end.
