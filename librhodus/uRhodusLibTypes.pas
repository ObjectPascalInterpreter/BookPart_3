unit uRhodusLibTypes;

interface

type
  TRhodusPoint = record w, h : integer; end;
  TLineTo = procedure (x, y : double);
  TGraphicsMethods = record
     refresh : procedure;
     getCanvasSize : function : TRhodusPoint;
     setPenColor : procedure (r, g, b : integer);
     setPenWidth : procedure (width : double);
     setBrushColor : procedure (r, g, b : integer);
     setPixel : procedure (x, y : integer);
     moveTo : procedure (x, y : double);
     lineTo : procedure (x, y : double);
     lineWithColor : procedure (penColor : AnsiString; x1, y1, x2, y2 : double);
     drawRectangle : procedure (x, y, w, h : double);
     drawFilledRectangle : procedure (x, y, w, h : double);
     drawEllipse : procedure (x1, y1, x2, y2 : double);
     drawFilledEllipse : procedure (x1, y1, x2, y2 : double);
     beginupdate : procedure;
     endupdate : procedure;
     clearColor : procedure (r, g, b : integer);
  end;
  PGraphicsMethods = ^TGraphicsMethods;


  TPrint = procedure (const astr : AnsiString);
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
