unit uBuiltInGraphics;

// This source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

Uses SysUtils, Classes, uLibModule, uRhodusLibTypes, System.UIConsts, System.UITypes;

type
  TBuiltInGraphics = class (TModuleLib)
    private
       pen_r, pen_g, pen_b : integer;
       brush_r, brush_g, brush_b : integer;
       penwidth : double;
       updateflag : boolean;
       procedure   checkGraphicsSubsystem;
    public
       procedure   clear (vm : TObject);
       procedure   getCanvasSize (vm : TObject);
       procedure   setPenColor (vm : TObject);
       procedure   setPenWidth (vm : TObject);
       procedure   setBrushColor (vm : TObject);
       procedure   setPixel (vm : TObject);
       procedure   line (vm : TObject);
       procedure   moveTo  (vm : TObject);
       procedure   lineTo    (vm : TObject);
       procedure   drawRect (vm : TObject);
       procedure   drawFilledRect (vm : TObject);
       procedure   drawEllipse (vm: TObject);
       procedure   drawFilledEllipse (vm: TObject);
       procedure   beginUpdate (vm : TObject);
       procedure   endUpdate (vm : TObject);
       procedure   refresh (vm : TObject);
       procedure   pause (vm : TObject);

       constructor Create;
  end;


procedure setGaphicsCallBackTable (graphicsMethods : PGraphicsMethods);


implementation

Uses Math,
     uSymbolTable,
     uRhodusTypes,
     uVM,
     uStringObject,
     uListObject,
     uVMExceptions,
     uMemoryManager;

type
  TRGBTriple = packed record
    rgbtBlue: Byte;
    rgbtGreen: Byte;
    rgbtRed: Byte;
  end;

var graphicsMethodsPtr : PGraphicsMethods;

procedure setGaphicsCallBackTable (graphicsMethods : PGraphicsMethods);
begin
  graphicsMethodsPtr := graphicsMethods;
end;

constructor TBuiltInGraphics.Create;
begin
  inherited Create ('graphics', 'Graphics module');

  addMethod (clear,            VARIABLE_ARGS, 'clear',    'Clear the background canvas: clear () or clear ("red")');
  addMethod (getCanvasSize,     0, 'size',     'Returns the size of the canvas as a list: size ()');
  addMethod (setPenColor,      VARIABLE_ARGS, 'pencolor', 'Set the pen color using rgb or color name: pencolor (255, 34, 123)');
  addMethod (setPenWidth,       1, 'penwidth', 'Set the pen width: graphics.penwidth (4)');
  addMethod (setBrushColor,    VARIABLE_ARGS, 'brushcolor', 'Set the brush color using rgb of color name: brushcolor ("red")');
  addMethod (setPixel,          2, 'pixel', 'Set the pixel to the pen color: pixel (100, 120)');
  addMethod (moveTo,            2, 'moveto',   'Move the current plotting cursor to x, y: graphics.moveto (100,200)');
  addMethod (lineTo,            2, 'lineto',   'Draw a line from the previous moveto/lineto to the new pt: lineto (x, y)');
  addMethod (line,             VARIABLE_ARGS, 'line',     'Draw a line between two points: line(x1, y1, x2, y2)');
  addMethod (drawRect,          4, 'rect',     'Draw a rectangle: rect (x, y, w, h)');
  addMethod (drawFilledRect,    4, 'fillrect', 'Draw a filled rectangle: filrect (x, y, w, h');
  addMethod (drawEllipse,       4, 'ellipse',  'Draw an ellipse with top/left coordiante x, y and width and height w, h: ellipse(100, 100, w, h)');
  addMethod (drawFilledEllipse, 4, 'fillellipse', 'Draw a filled ellipse');
  addMethod (pause,             1, 'pause', '');
  addMethod (beginUpdate,       0, 'beginupdate', 'Suppress output to the GUI canvas');
  addMethod (endUpdate,         0, 'endupdate',   'Draw the background canvas to the GUI image');
  addMethod (refresh,           0, 'refresh',     'Copy the background canvas to the GUI image');
  end;


procedure TBuiltInGraphics.checkGraphicsSubsystem;
begin
   if graphicsMethodsPtr = nil then
      raise ERuntimeException.Create('No graphics subsystem detected.');
end;


procedure TBuiltInGraphics.clear (vm : TObject);
var nArgs : integer;
    scolor : string;
    acolor : TAlphaColor;
    r, g, b : integer;
begin
   nArgs := TVM (vm).popInteger;
   if nArgs = 1 then
      begin
      scolor := TVM (vm).popString.value;

      try
        acolor := StringToAlphaColor(scolor);
      except
        on Exception do
          raise ERuntimeException.Create(scolor + ' is not a valud color in function clear');
      end;
      r := TAlphaColorRec(acolor).R;
      g := TAlphaColorRec(acolor).G;
      b := TAlphaColorRec(acolor).B;
      end
   else
      begin
      // Oldlace
      r := 254;
      g := 245;
      b := 230;
      end;

   checkGraphicsSubsystem;

   if @graphicsMethodsPtr.clearColor <> nil then
      graphicsMethodsPtr.clearColor (r, g, b)
   else
      raise ERuntimeException.Create('clear subsystem not available.');

   TVM (vm).pushNone;
end;


procedure TBuiltInGraphics.pause (vm : TObject);
var ms : integer;
    iStart, iStop: integer;
begin
  ms := TVM (vm).popInteger;

  sleep (ms);
  checkGraphicsSubsystem;

  TVM (vm).pushNone;
end;


procedure TBuiltInGraphics.setPenColor (vm : TObject);
var nArgs : integer;
    scolor : string;
    acolor : TAlphaColor;
begin
  // has a variable number of arguments
  nArgs := TVM (vm).popInteger;
  case nArgs of
     1 : begin
         scolor := TVM (vm).popString.value;
         try
           acolor := StringToAlphaColor(scolor);
         except
           on e:Exception do
              raise ERuntimeException.Create(scolor + ' is not a valid color in pencolor.');
         end;
         pen_r := TAlphaColorRec(acolor).R;
         pen_g := TAlphaColorRec(acolor).G;
         pen_b := TAlphaColorRec(acolor).B;
         end;
     3 : begin
         pen_b := trunc (TVM (vm).popScalar);
         pen_g := trunc (TVM (vm).popScalar);
         pen_r := trunc (TVM (vm).popScalar);
         end
  else
     raise ERuntimeException.Create('Expecting one or three arguments in pencolor.');
  end;

  checkGraphicsSubsystem;

  if @graphicsMethodsPtr.setPenColor <> nil then
     graphicsMethodsPtr.setPenColor (pen_r, pen_g, pen_b)
  else
     raise ERuntimeException.Create('setPenColor subsystem not available.');

  TVM (vm).pushNone;
end;


procedure TBuiltInGraphics.setBrushColor (vm : TObject);
var nArgs : integer;
    scolor : string;
    acolor : TAlphaColor;
begin
  // has a variable number of arguments
  nArgs := TVM (vm).popInteger;
  case nArgs of
     1 : begin
         scolor := TVM (vm).popString.value;
         try
           acolor := StringToAlphaColor(scolor);
         except
           on e:Exception do
              raise ERuntimeException.Create(scolor + ' is not a valid color in brushcolor.');
         end;
         brush_r := TAlphaColorRec(acolor).R;
         brush_g := TAlphaColorRec(acolor).G;
         brush_b := TAlphaColorRec(acolor).B;
         end;
     3 : begin
         brush_b := trunc (TVM (vm).popScalar);
         brush_g := trunc (TVM (vm).popScalar);
         brush_r := trunc (TVM (vm).popScalar);
         end
  else
     raise ERuntimeException.Create('Expecting one or three arguments in brushcolor.');
  end;

  checkGraphicsSubsystem;

  if @graphicsMethodsPtr.setBrushColor <> nil then
     graphicsMethodsPtr.setBrushColor (brush_r, brush_g, brush_b)
  else
     raise ERuntimeException.Create('setBrushColor subsystem not available.');

  TVM (vm).pushNone;
end;


procedure TBuiltInGraphics.setPenWidth (vm : TObject);
begin
  penwidth := TVM (vm).popScalar;

  checkGraphicsSubsystem;

  if @graphicsMethodsPtr.setPenWidth <> nil then
     graphicsMethodsPtr.setPenWidth (penwidth)
  else
     raise ERuntimeException.Create('setPenWidth subsystem not available.');

  TVM (vm).pushNone;
end;


procedure TBuiltInGraphics.setPixel (vm : TObject);
var x, y : integer;
begin
  y := TVM (vm).popInteger;
  x := TVM (vm).popInteger;

  checkGraphicsSubsystem;

  if @graphicsMethodsPtr.setPixel <> nil then
     graphicsMethodsPtr.setPixel (x, y)
  else
     raise ERuntimeException.Create('setPixel subsystem not available.');

  TVM (vm).pushNone;
end;


procedure TBuiltInGraphics.drawRect (vm : TObject);
var x, y, w, h : double;
begin
  h := TVM (vm).popScalar;
  w := TVM (vm).popScalar;
  y := TVM (vm).popScalar;
  x := TVM (vm).popScalar;

  checkGraphicsSubsystem;

  if @graphicsMethodsPtr.drawRectangle <> nil then
     graphicsMethodsPtr.drawRectangle (x, y, w, h)
  else
     raise ERuntimeException.Create('drawRectangle subsystem not available.');

  TVM (vm).pushNone;
end;


procedure TBuiltInGraphics.drawFilledRect (vm : TObject);
var x, y, w, h : double;
begin
  h := TVM (vm).popScalar;
  w := TVM (vm).popScalar;
  y := TVM (vm).popScalar;
  x := TVM (vm).popScalar;

  checkGraphicsSubsystem;

  if @graphicsMethodsPtr.drawFilledRectangle <> nil then
     graphicsMethodsPtr.drawFilledRectangle (x, y, w, h)
  else
     raise ERuntimeException.Create('drawFilledRectangle subsystem not available.');

  TVM (vm).pushNone;
end;


procedure TBuiltInGraphics.drawEllipse (vm: TObject);
var x1, y1, x2, y2 : double;
begin
  y2 := TVM (vm).popScalar;
  x2 := TVM (vm).popScalar;
  y1 := TVM (vm).popScalar;
  x1 := TVM (vm).popScalar;

  checkGraphicsSubsystem;

  if @graphicsMethodsPtr.drawEllipse <> nil then
     graphicsMethodsPtr.drawEllipse (x1, y1, x1+x2, y1+y2)
  else
     raise ERuntimeException.Create('drawEllipse subsystem not available.');

  TVM (vm).pushNone;
end;


procedure TBuiltInGraphics.drawFilledEllipse (vm: TObject);
var x1, y1, x2, y2 : double;
begin
  y2 := TVM (vm).popScalar;
  x2 := TVM (vm).popScalar;
  y1 := TVM (vm).popScalar;
  x1 := TVM (vm).popScalar;

  checkGraphicsSubsystem;

  if @graphicsMethodsPtr.drawFilledEllipse <> nil then
     graphicsMethodsPtr.drawFilledEllipse (x1, y1, x1+x2, y1+y2)
  else
     raise ERuntimeException.Create('drawFilledEllipse subsystem not available.');

  TVM (vm).pushNone;
end;

procedure TBuiltInGraphics.moveTo (vm : TObject);
var x, y : double;
begin
   y := TVM (vm).popScalar;
   x := TVM (vm).popScalar;

   checkGraphicsSubsystem;

   if @graphicsMethodsPtr.moveTo <> nil then
      graphicsMethodsPtr.moveTo (x, y)
   else
      raise ERuntimeException.Create('moveto subsystem not available.');

   TVM (vm).pushNone;
end;


procedure TBuiltInGraphics.lineTo (vm : TObject);
var x, y : double;
begin
   y := TVM (vm).popScalar;
   x := TVM (vm).popScalar;

   checkGraphicsSubsystem;

   if @graphicsMethodsPtr.lineTo <> nil then
      graphicsMethodsPtr.lineTo (x, y)
   else
      raise ERuntimeException.Create('lineto subsystem not available.');

   TVM (vm).pushNone;
end;


procedure TBuiltInGraphics.line (vm : TObject);
var x1, y1, x2, y2 : double;
    nArgs : integer;
    penColor : AnsiString;
begin
   nArgs := TVM (vm).popInteger;
   if nArgs = 4 then
      begin
      penColor := '';
      y2 := TVM (vm).popScalar;
      x2 := TVM (vm).popScalar;
      y1 := TVM (vm).popScalar;
      x1 := TVM (vm).popScalar;
      end;
   if nArgs = 5 then
      begin
      y2 := TVM (vm).popScalar;
      x2 := TVM (vm).popScalar;
      y1 := TVM (vm).popScalar;
      x1 := TVM (vm).popScalar;
      penColor := AnsiString (TVM (vm).popString.value);
      end;

   checkGraphicsSubsystem;
   if @graphicsMethodsPtr.lineWithColor <> nil then
      graphicsMethodsPtr.lineWithColor (AnsiString (penColor), x1, y1, x2, y2)
   else
      raise ERuntimeException.Create('line subsystem not available.');

   TVM (vm).pushNone;
end;

procedure TBuiltInGraphics.getCanvasSize (vm : TObject);
var p : TRhodusPoint;
    lt : TListObject;
begin
  checkGraphicsSubsystem;
  if @graphicsMethodsPtr.getCanvasSize <> nil then
     p := graphicsMethodsPtr.getCanvasSize;

  lt := TListObject.Create(2);
  lt.list[0].itemType := liInteger;
  lt.list[0].iValue := p.w;
  lt.list[1].itemType := liInteger;
  lt.list[1].iValue := p.h;
  TVM (vm).push(lt);
end;


procedure TBuiltInGraphics.beginUpdate (vm : TObject);
begin
  updateflag := True;

   checkGraphicsSubsystem;

   if @graphicsMethodsPtr.beginupdate <> nil then
      graphicsMethodsPtr.beginupdate ()
   else
      raise ERuntimeException.Create('beginupdate subsystem not available.');

  TVM (vm).pushNone;
end;


procedure TBuiltInGraphics.endUpdate (vm : TObject);
begin
  updateflag := False;

  checkGraphicsSubsystem;

  if @graphicsMethodsPtr.endupdate <> nil then
     graphicsMethodsPtr.endupdate ()
  else
     raise ERuntimeException.Create('endupdate subsystem not available.');

  TVM (vm).pushNone;
end;


procedure TBuiltInGraphics.refresh (vm : TObject);
begin
  checkGraphicsSubsystem;

  if @graphicsMethodsPtr.refresh <> nil then
     graphicsMethodsPtr.refresh ()
  else
     raise ERuntimeException.Create('refresh subsystem not available.');

  TVM (vm).pushNone;
end;


end.
