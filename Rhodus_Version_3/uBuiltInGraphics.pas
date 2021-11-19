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
       procedure   checkGraphicsSubsystem;
    public
       procedure   clear (vm : TObject);
       procedure   getCanvasSize (vm : TObject);
       procedure   moveTo  (vm : TObject);
       procedure   lineTo    (vm : TObject);

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

  addMethod (clear,         0, 'clear',   'Set the seed for the random number generator: seed (23)');
  addMethod (getCanvasSize, 0, 'size',   'Set the seed for the random number generator: seed (23)');
  addMethod (moveTo,        2, 'moveto',  'Set the seed for the random number generator: seed (23)');
  addMethod (lineTo,        2, 'lineto',  'Return a uniformly distributed random number: random()');
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
