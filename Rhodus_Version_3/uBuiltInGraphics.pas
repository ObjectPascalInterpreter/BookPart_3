unit uBuiltInGraphics;

// This source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

Uses SysUtils, Classes, uLibModule, ulibTypes;

type
  TBuiltInGraphics = class (TModuleLib)
    private
       pen_r, pen_g, pen_b : integer;
       brush_r, brush_g, brush_b : integer;
       penwidth : double;
       procedure   checkGraphicsSubsystem;
    public
       procedure   clear (vm : TObject);
       procedure   getCanvasSize (vm : TObject);
       procedure   setPenColor (vm : TObject);
       procedure   setPenWidth (vm : TObject);
       procedure   setBrushColor (vm : TObject);
       procedure   moveTo  (vm : TObject);
       procedure   lineTo    (vm : TObject);
       procedure   drawRect (vm : TObject);
       procedure   drawFilledRect (vm : TObject);
       procedure   drawEllipse (vm: TObject);
       procedure   drawFilledEllipse (vm: TObject);
       procedure   pause (vm : TObject);

       constructor Create;
  end;

procedure setGaphicsCallBackTable (graphicsMethods : PGraphicsMethods);


implementation

Uses Math,
     uSymbolTable,
     uVM,
     uStringObject,
     uListObject,
     uVMExceptions,
     uMemoryManager;

var graphicsMethodsPtr : PGraphicsMethods;

procedure setGaphicsCallBackTable (graphicsMethods : PGraphicsMethods);
begin
  graphicsMethodsPtr := graphicsMethods;
end;

constructor TBuiltInGraphics.Create;
begin
  inherited Create ('graphics', 'Graphics module');

  addMethod (clear,          0, 'clear',   'Set the seed for the random number generator: seed (23)');
  addMethod (getCanvasSize,  0, 'size',   'Set the seed for the random number generator: seed (23)');
  addMethod (setPenColor,    3, 'pencolor', '');
  addMethod (setPenWidth,    1, 'penwidth', '');
  addMethod (setBrushColor,  3, 'brushcolor', '');
  addMethod (moveTo,            2, 'moveto',  'Set the seed for the random number generator: seed (23)');
  addMethod (lineTo,            2, 'lineto',  'Return a uniformly distributed random number: random()');
  addMethod (drawRect,          4, 'rect',  'Return a uniformly distributed random number: random()');
  addMethod (drawFilledRect,    4, 'fillrect',  'Return a uniformly distributed random number: random()');
  addMethod (drawEllipse,       4,  'ellipse',  'Return a uniformly distributed random number: random()');
  addMethod (drawFilledEllipse, 4, 'fillellipse',  'Return a uniformly distributed random number: random()');
  addMethod (pause,             1, 'pause', '');
end;


procedure TBuiltInGraphics.checkGraphicsSubsystem;
begin
   if graphicsMethodsPtr = nil then
      raise ERuntimeException.Create('No graphics subsystem detected.');
end;


procedure TBuiltInGraphics.clear (vm : TObject);
begin
   checkGraphicsSubsystem;

   if @graphicsMethodsPtr.clear <> nil then
      graphicsMethodsPtr.clear
   else
      raise ERuntimeException.Create('clear subsystem not available.');

   TVM (vm).pushNone;
end;


procedure TBuiltInGraphics.pause (vm : TObject);
var ms : integer;
    iStart, iStop: integer;
begin
  ms := TVM (vm).popInteger;

  checkGraphicsSubsystem;

  TVM (vm).pushNone;
end;


procedure TBuiltInGraphics.setPenColor (vm : TObject);
begin
  pen_b := TVM (vm).popInteger;
  pen_g := TVM (vm).popInteger;
  pen_r := TVM (vm).popInteger;

  checkGraphicsSubsystem;

  if @graphicsMethodsPtr.setPenColor <> nil then
     graphicsMethodsPtr.setPenColor (pen_r, pen_g, pen_b)
  else
     raise ERuntimeException.Create('setPenColor subsystem not available.');

  TVM (vm).pushNone;
end;


procedure TBuiltInGraphics.setBrushColor (vm : TObject);
begin
  brush_b := TVM (vm).popInteger;
  brush_g := TVM (vm).popInteger;
  brush_r := TVM (vm).popInteger;

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

end.
