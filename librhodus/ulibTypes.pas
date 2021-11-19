unit ulibTypes;

interface

type
  TRhodusPoint = record w, h : integer; end;
  TLineTo = procedure (x, y : double);
  TGraphicsMethods = record
     redrawRequest : procedure;
     getCanvasSize : function : TRhodusPoint;
     moveTo : procedure (x, y : double);
     lineTo : procedure (x, y : double);
     clear  : procedure;
  end;
  PGraphicsMethods = ^TGraphicsMethods;


  TPrint = procedure (astr : AnsiString);
  TReadString = function (const prompt : AnsiString) : PAnsiChar;
  TGraphicsHandler = function : PGraphicsMethods;

  TRhodusConfig = record
      printPtr : TPrint;
      printlnPtr : TPrint;
      readStringPtr : TReadString;
      graphicsHandlerPtr : PGraphicsMethods;
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
