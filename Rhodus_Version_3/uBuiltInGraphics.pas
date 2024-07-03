unit uBuiltInGraphics;

// This source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

Uses SysUtils, Classes, uLibModule, uRhodusLibTypes, System.UIConsts, System.UITypes;

type
  TBuiltInGraphics = class(TModuleLib)
  private
    pen_r, pen_g, pen_b: integer;
    brush_r, brush_g, brush_b: integer;
    penwidth: double;
    updateflag: boolean;
    procedure checkGraphicsSubsystem;
  public
    procedure clear(vm: TObject);
    procedure getCanvasSize(vm: TObject);
    procedure setPenColor(vm: TObject);
    procedure setPenWidth(vm: TObject);
    procedure setBrushColor(vm: TObject);
    procedure setPixel(vm: TObject);
    procedure line(vm: TObject);
    procedure moveTo(vm: TObject);
    procedure lineTo(vm: TObject);
    procedure drawRect(vm: TObject);
    procedure drawFilledRect(vm: TObject);
    procedure drawCircle (vm: TObject);
    procedure drawFilledCircle (vm: TObject);
    procedure drawEllipse(vm: TObject);
    procedure drawFilledEllipse(vm: TObject);
    procedure beginUpdate(vm: TObject);
    procedure endUpdate(vm: TObject);
    procedure refresh(vm: TObject);
    procedure pause(vm: TObject);
    procedure getColorList(vm: TObject);

    constructor Create;
  end;

procedure setGraphicsCallBackTable(graphicsMethods: PGraphicsMethods);

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

var
  graphicsMethodsPtr: PGraphicsMethods;

procedure setGraphicsCallBackTable(graphicsMethods: PGraphicsMethods);
begin
  graphicsMethodsPtr := graphicsMethods;
end;

constructor TBuiltInGraphics.Create;
begin
  inherited Create('graphics', 'Graphics module');

  addMethod(clear, VARIABLE_ARGS, 'clear', 'Clear the background canvas: clear () or clear ("red")');
  addMethod(getCanvasSize, 0, 'size', 'Returns the size of the canvas as a list: size ()');
  addMethod(setPenColor, VARIABLE_ARGS, 'setPenColor', 'Set the pen color using rgb or color name: pencolor (255, 34, 123)');
  addMethod(setPenWidth, 1, 'setPenWidth', 'Set the pen width: graphics.penwidth (4)');
  addMethod(setBrushColor, VARIABLE_ARGS, 'setBrushColor', 'Set the brush color using rgb of color name: brushcolor ("red")');
  addMethod(setPixel, 2, 'pixel', 'Set the pixel to the pen color: pixel (100, 120)');
  addMethod(getColorList, 0, 'getColorNames', 'Returns the list of color names');
  addMethod(moveTo, 2, 'moveto', 'Move the current plotting cursor to x, y: graphics.moveto (100,200)');
  addMethod(lineTo, 2, 'lineto', 'Draw a line from the previous moveto/lineto to the new pt: lineto (x, y)');
  addMethod(line, VARIABLE_ARGS, 'line', 'Draw a line between two points: line(x1, y1, x2, y2)');
  addMethod(drawRect, 4, 'rect', 'Draw a rectangle: rect (x, y, w, h)');
  addMethod(drawFilledRect, 4, 'fillRect', 'Draw a filled rectangle: filrect (x, y, w, h');
  addMethod(drawCircle, 3, 'circle', 'Draw a circle; circle (x, y, radius');
  addMethod(drawFilledCircle, 3, 'filledCircle', 'Draw a filled circle; circle (x, y, radius)');
  addMethod(drawEllipse, 4, 'ellipse',
    'Draw an ellipse with top/left coordiante x, y and width and height w, h: ellipse(100, 100, w, h)');
  addMethod(drawFilledEllipse, 4, 'fillEllipse', 'Draw a filled ellipse');
  addMethod(pause, 1, 'pause', 'Pause for millisecs time, eg pause (100)');
  addMethod(beginUpdate, 0, 'beginUpdate', 'Suppress output to the GUI canvas');
  addMethod(endUpdate, 0, 'endUpdate', 'Draw the background canvas to the GUI image');
  addMethod(refresh, 0, 'refresh', 'Copy the background canvas to the GUI image');
end;


procedure TBuiltInGraphics.checkGraphicsSubsystem;
begin
  if graphicsMethodsPtr = nil then
    raise ERuntimeException.Create('No graphics subsystem detected.');
end;


procedure TBuiltInGraphics.clear(vm: TObject);
var
  nArgs: integer;
  scolor: string;
  acolor: TAlphaColor;
  r, g, b: integer;
begin
  nArgs := TVM(vm).popInteger;
  if nArgs = 1 then
    begin
      scolor := TVM(vm).popString.value;

      try
        acolor := StringToAlphaColor(scolor);
      except
        on Exception do
          raise ERuntimeException.Create(scolor + ' is not a valud color in function clear');
      end;
      r := TAlphaColorRec(acolor).r;
      g := TAlphaColorRec(acolor).g;
      b := TAlphaColorRec(acolor).b;
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
    graphicsMethodsPtr.clearColor(r, g, b)
  else
    raise ERuntimeException.Create('clear subsystem not available.');

  TVM(vm).pushNone;
end;

procedure TBuiltInGraphics.pause(vm: TObject);
var
  ms: integer;
  iStart, iStop: integer;
begin
  ms := TVM(vm).popInteger;

  sleep(ms);
  checkGraphicsSubsystem;

  TVM(vm).pushNone;
end;

procedure TBuiltInGraphics.getColorList(vm: TObject);
var
  lt: TListObject;
begin
  lt := TListObject.Create(0);
  lt.append(TStringObject.Create('Snow'));
  lt.append(TStringObject.Create('FloralWhite'));
  lt.append(TStringObject.Create('LavenderBlush'));
  lt.append(TStringObject.Create('OldLace'));
  lt.append(TStringObject.Create('Ivory'));
  lt.append(TStringObject.Create('CornSilk'));
  lt.append(TStringObject.Create('Beige'));
  lt.append(TStringObject.Create('AntiqueWhite'));
  lt.append(TStringObject.Create('Wheat'));
  lt.append(TStringObject.Create('AliceBlue'));
  lt.append(TStringObject.Create('GhostWhite'));
  lt.append(TStringObject.Create('Lavender'));
  lt.append(TStringObject.Create('Seashell'));
  lt.append(TStringObject.Create('LightYellow'));
  lt.append(TStringObject.Create('PapayaWhip'));
  lt.append(TStringObject.Create('NavajoWhite'));
  lt.append(TStringObject.Create('Moccasin'));
  lt.append(TStringObject.Create('Burlywood'));
  lt.append(TStringObject.Create('Azure'));
  lt.append(TStringObject.Create('Mintcream'));
  lt.append(TStringObject.Create('Honeydew'));
  lt.append(TStringObject.Create('Linen'));
  lt.append(TStringObject.Create('LemonChiffon'));
  lt.append(TStringObject.Create('BlanchedAlmond'));
  lt.append(TStringObject.Create('Bisque'));
  lt.append(TStringObject.Create('PeachPuff'));
  lt.append(TStringObject.Create('Tan'));
  lt.append(TStringObject.Create('Yellow'));
  lt.append(TStringObject.Create('DarkOrange'));
  lt.append(TStringObject.Create('Red'));
  lt.append(TStringObject.Create('DarkRed'));
  lt.append(TStringObject.Create('Maroon'));
  lt.append(TStringObject.Create('IndianRed'));
  lt.append(TStringObject.Create('Salmon'));
  lt.append(TStringObject.Create('Coral'));
  lt.append(TStringObject.Create('Gold'));
  lt.append(TStringObject.Create('Tomato'));
  lt.append(TStringObject.Create('Crimson'));
  lt.append(TStringObject.Create('Brown'));
  lt.append(TStringObject.Create('Chocolate'));
  lt.append(TStringObject.Create('SandyBrown'));
  lt.append(TStringObject.Create('LightSalmon'));
  lt.append(TStringObject.Create('LightCoral'));
  lt.append(TStringObject.Create('Orange'));
  lt.append(TStringObject.Create('OrangeRed'));
  lt.append(TStringObject.Create('Firebrick'));
  lt.append(TStringObject.Create('SaddleBrown'));
  lt.append(TStringObject.Create('Sienna'));
  lt.append(TStringObject.Create('Peru'));
  lt.append(TStringObject.Create('DarkSalmon'));
  lt.append(TStringObject.Create('RosyBrown'));
  lt.append(TStringObject.Create('PaleGoldenrod'));
  lt.append(TStringObject.Create('LightGoldenrodYellow'));
  lt.append(TStringObject.Create('Olive'));
  lt.append(TStringObject.Create('ForestGreen'));
  lt.append(TStringObject.Create('GreenYellow'));
  lt.append(TStringObject.Create('Chartreuse'));
  lt.append(TStringObject.Create('LightGreen'));
  lt.append(TStringObject.Create('Aquamarine'));
  lt.append(TStringObject.Create('SeaGreen'));
  lt.append(TStringObject.Create('GoldenRod'));
  lt.append(TStringObject.Create('Khaki'));
  lt.append(TStringObject.Create('OliveDrab'));
  lt.append(TStringObject.Create('Green'));
  lt.append(TStringObject.Create('YellowGreen'));
  lt.append(TStringObject.Create('LawnGreen'));
  lt.append(TStringObject.Create('PaleGreen'));
  lt.append(TStringObject.Create('MediumAquamarine'));
  lt.append(TStringObject.Create('MediumSeaGreen'));
  lt.append(TStringObject.Create('DarkGoldenRod'));
  lt.append(TStringObject.Create('DarkKhaki'));
  lt.append(TStringObject.Create('DarkOliveGreen'));
  lt.append(TStringObject.Create('Darkgreen'));
  lt.append(TStringObject.Create('LimeGreen'));
  lt.append(TStringObject.Create('Lime'));
  lt.append(TStringObject.Create('SpringGreen'));
  lt.append(TStringObject.Create('MediumSpringGreen'));
  lt.append(TStringObject.Create('DarkSeaGreen'));
  lt.append(TStringObject.Create('LightSeaGreen'));
  lt.append(TStringObject.Create('PaleTurquoise'));
  lt.append(TStringObject.Create('LightCyan'));
  lt.append(TStringObject.Create('LightBlue'));
  lt.append(TStringObject.Create('LightSkyBlue'));
  lt.append(TStringObject.Create('CornFlowerBlue'));
  lt.append(TStringObject.Create('DarkBlue'));
  lt.append(TStringObject.Create('Indigo'));
  lt.append(TStringObject.Create('MediumTurquoise'));
  lt.append(TStringObject.Create('Turquoise'));
  lt.append(TStringObject.Create('Cyan'));
  lt.append(TStringObject.Create('PowderBlue'));
  lt.append(TStringObject.Create('SkyBlue'));
  lt.append(TStringObject.Create('RoyalBlue'));
  lt.append(TStringObject.Create('MediumBlue'));
  lt.append(TStringObject.Create('MidnightBlue'));
  lt.append(TStringObject.Create('DarkTurquoise'));
  lt.append(TStringObject.Create('CadetBlue'));
  lt.append(TStringObject.Create('DarkCyan'));
  lt.append(TStringObject.Create('Teal'));
  lt.append(TStringObject.Create('DeepSkyBlue'));
  lt.append(TStringObject.Create('DodgerBlue'));
  lt.append(TStringObject.Create('Blue'));
  lt.append(TStringObject.Create('Navy'));
  lt.append(TStringObject.Create('DarkViolet'));
  lt.append(TStringObject.Create('DarkOrchid'));
  lt.append(TStringObject.Create('Magenta'));
  lt.append(TStringObject.Create('DarkMagenta'));
  lt.append(TStringObject.Create('MediumVioletRed'));
  lt.append(TStringObject.Create('PaleVioletRed'));
  lt.append(TStringObject.Create('BlueViolet'));
  lt.append(TStringObject.Create('MediumOrchid'));
  lt.append(TStringObject.Create('MediumPurple'));
  lt.append(TStringObject.Create('Purple'));
  lt.append(TStringObject.Create('DeepPink'));
  lt.append(TStringObject.Create('LightPink'));
  lt.append(TStringObject.Create('Violet'));
  lt.append(TStringObject.Create('Orchid'));
  lt.append(TStringObject.Create('Plum'));
  lt.append(TStringObject.Create('Thistle'));
  lt.append(TStringObject.Create('HotPink'));
  lt.append(TStringObject.Create('Pink'));
  lt.append(TStringObject.Create('LightSteelBlue'));
  lt.append(TStringObject.Create('MediumSlateBlue'));
  lt.append(TStringObject.Create('LightSlateGray'));
  lt.append(TStringObject.Create('White'));
  lt.append(TStringObject.Create('Lightgrey'));
  lt.append(TStringObject.Create('Gray'));
  lt.append(TStringObject.Create('SteelBlue'));
  lt.append(TStringObject.Create('SlateBlue'));
  lt.append(TStringObject.Create('SlateGray'));
  lt.append(TStringObject.Create('WhiteSmoke'));
  lt.append(TStringObject.Create('Silver'));
  lt.append(TStringObject.Create('DimGray'));
  lt.append(TStringObject.Create('MistyRose'));
  lt.append(TStringObject.Create('DarkSlateBlue'));
  lt.append(TStringObject.Create('DarkSlategray'));
  lt.append(TStringObject.Create('Gainsboro'));
  lt.append(TStringObject.Create('DarkGray'));
  lt.append(TStringObject.Create('Black'));
  TVM(vm).push(lt);
end;

procedure TBuiltInGraphics.setPenColor(vm: TObject);
var
  nArgs: integer;
  scolor: string;
  acolor: TAlphaColor;
begin
  // has a variable number of arguments
  nArgs := TVM(vm).popInteger;
  case nArgs of
    1:
      begin
        scolor := TVM(vm).popString.value;
        try
          acolor := StringToAlphaColor(scolor);
        except
          on e: Exception do
            raise ERuntimeException.Create(scolor + ' is not a valid color in pencolor.');
        end;
        pen_r := TAlphaColorRec(acolor).r;
        pen_g := TAlphaColorRec(acolor).g;
        pen_b := TAlphaColorRec(acolor).b;
      end;
    3:
      begin
        pen_b := trunc(TVM(vm).popScalar);
        pen_g := trunc(TVM(vm).popScalar);
        pen_r := trunc(TVM(vm).popScalar);
      end
  else
    raise ERuntimeException.Create('Expecting one or three arguments in pencolor.');
  end;

  checkGraphicsSubsystem;

  if @graphicsMethodsPtr.setPenColor <> nil then
    graphicsMethodsPtr.setPenColor(pen_r, pen_g, pen_b)
  else
    raise ERuntimeException.Create('setPenColor subsystem not available.');

  TVM(vm).pushNone;
end;

procedure TBuiltInGraphics.setBrushColor(vm: TObject);
var
  nArgs: integer;
  scolor: string;
  acolor: TAlphaColor;
begin
  // has a variable number of arguments
  nArgs := TVM(vm).popInteger;
  case nArgs of
    1:
      begin
        scolor := TVM(vm).popString.value;
        try
          acolor := StringToAlphaColor(scolor);
        except
          on e: Exception do
            raise ERuntimeException.Create(scolor + ' is not a valid color in brushcolor.');
        end;
        brush_r := TAlphaColorRec(acolor).r;
        brush_g := TAlphaColorRec(acolor).g;
        brush_b := TAlphaColorRec(acolor).b;
      end;
    3:
      begin
        brush_b := trunc(TVM(vm).popScalar);
        brush_g := trunc(TVM(vm).popScalar);
        brush_r := trunc(TVM(vm).popScalar);
      end
  else
    raise ERuntimeException.Create('Expecting one or three arguments in brushcolor.');
  end;

  checkGraphicsSubsystem;

  if @graphicsMethodsPtr.setBrushColor <> nil then
    graphicsMethodsPtr.setBrushColor(brush_r, brush_g, brush_b)
  else
    raise ERuntimeException.Create('setBrushColor subsystem not available.');

  TVM(vm).pushNone;
end;

procedure TBuiltInGraphics.setPenWidth(vm: TObject);
begin
  penwidth := TVM(vm).popScalar;

  checkGraphicsSubsystem;

  if @graphicsMethodsPtr.setPenWidth <> nil then
    graphicsMethodsPtr.setPenWidth(penwidth)
  else
    raise ERuntimeException.Create('setPenWidth subsystem not available.');

  TVM(vm).pushNone;
end;

procedure TBuiltInGraphics.setPixel(vm: TObject);
var
  x, y: integer;
begin
  y := TVM(vm).popInteger;
  x := TVM(vm).popInteger;

  checkGraphicsSubsystem;

  if @graphicsMethodsPtr.setPixel <> nil then
    graphicsMethodsPtr.setPixel(x, y)
  else
    raise ERuntimeException.Create('setPixel subsystem not available.');

  TVM(vm).pushNone;
end;

procedure TBuiltInGraphics.drawRect(vm: TObject);
var
  x, y, w, h: double;
begin
  h := TVM(vm).popScalar;
  w := TVM(vm).popScalar;
  y := TVM(vm).popScalar;
  x := TVM(vm).popScalar;

  checkGraphicsSubsystem;

  if @graphicsMethodsPtr.drawRectangle <> nil then
    graphicsMethodsPtr.drawRectangle(x, y, w, h)
  else
    raise ERuntimeException.Create('drawRectangle subsystem not available.');

  TVM(vm).pushNone;
end;

procedure TBuiltInGraphics.drawFilledRect(vm: TObject);
var
  x, y, w, h: double;
begin
  h := TVM(vm).popScalar;
  w := TVM(vm).popScalar;
  y := TVM(vm).popScalar;
  x := TVM(vm).popScalar;

  checkGraphicsSubsystem;

  if @graphicsMethodsPtr.drawFilledRectangle <> nil then
    graphicsMethodsPtr.drawFilledRectangle(x, y, w, h)
  else
    raise ERuntimeException.Create('drawFilledRectangle subsystem not available.');

  TVM(vm).pushNone;
end;


procedure TBuiltInGraphics.drawCircle (vm: TObject);
var
  x, y, radius: double;
begin
  radius := TVM(vm).popScalar;
  y := TVM(vm).popScalar;
  x := TVM(vm).popScalar;

  checkGraphicsSubsystem;

  if @graphicsMethodsPtr.drawCircle <> nil then
    graphicsMethodsPtr.drawCircle(x, y, radius)
  else
    raise ERuntimeException.Create('drawEllipse subsystem not available.');

  TVM(vm).pushNone;
end;


procedure TBuiltInGraphics.drawFilledCircle (vm: TObject);
var
  x, y, radius: double;
begin
  radius := TVM(vm).popScalar;
  y := TVM(vm).popScalar;
  x := TVM(vm).popScalar;

  checkGraphicsSubsystem;

  if @graphicsMethodsPtr.drawFilledCircle <> nil then
    graphicsMethodsPtr.drawFilledCircle(x, y, radius)
  else
    raise ERuntimeException.Create('drawEllipse subsystem not available.');

  TVM(vm).pushNone;

end;


procedure TBuiltInGraphics.drawEllipse(vm: TObject);
var
  x1, y1, x2, y2: double;
begin
  y2 := TVM(vm).popScalar;
  x2 := TVM(vm).popScalar;
  y1 := TVM(vm).popScalar;
  x1 := TVM(vm).popScalar;

  checkGraphicsSubsystem;

  if @graphicsMethodsPtr.drawEllipse <> nil then
    graphicsMethodsPtr.drawEllipse(x1, y1, x1 + x2, y1 + y2)
  else
    raise ERuntimeException.Create('drawEllipse subsystem not available.');

  TVM(vm).pushNone;
end;


procedure TBuiltInGraphics.drawFilledEllipse(vm: TObject);
var
  x1, y1, x2, y2: double;
begin
  y2 := TVM(vm).popScalar;
  x2 := TVM(vm).popScalar;
  y1 := TVM(vm).popScalar;
  x1 := TVM(vm).popScalar;

  checkGraphicsSubsystem;

  if @graphicsMethodsPtr.drawFilledEllipse <> nil then
    graphicsMethodsPtr.drawFilledEllipse(x1, y1, x1 + x2, y1 + y2)
  else
    raise ERuntimeException.Create('drawFilledEllipse subsystem not available.');

  TVM(vm).pushNone;
end;

procedure TBuiltInGraphics.moveTo(vm: TObject);
var
  x, y: double;
begin
  y := TVM(vm).popScalar;
  x := TVM(vm).popScalar;

  checkGraphicsSubsystem;

  if @graphicsMethodsPtr.moveTo <> nil then
    graphicsMethodsPtr.moveTo(x, y)
  else
    raise ERuntimeException.Create('moveto subsystem not available.');

  TVM(vm).pushNone;
end;

procedure TBuiltInGraphics.lineTo(vm: TObject);
var
  x, y: double;
begin
  y := TVM(vm).popScalar;
  x := TVM(vm).popScalar;

  checkGraphicsSubsystem;

  if @graphicsMethodsPtr.lineTo <> nil then
    graphicsMethodsPtr.lineTo(x, y)
  else
    raise ERuntimeException.Create('lineto subsystem not available.');

  TVM(vm).pushNone;
end;

procedure TBuiltInGraphics.line(vm: TObject);
var
  x1, y1, x2, y2: double;
  nArgs: integer;
  penColor: AnsiString;
begin
  nArgs := TVM(vm).popInteger;
  if nArgs = 4 then
    begin
      penColor := '';
      y2 := TVM(vm).popScalar;
      x2 := TVM(vm).popScalar;
      y1 := TVM(vm).popScalar;
      x1 := TVM(vm).popScalar;
    end;
  if nArgs = 5 then
    begin
      y2 := TVM(vm).popScalar;
      x2 := TVM(vm).popScalar;
      y1 := TVM(vm).popScalar;
      x1 := TVM(vm).popScalar;
      penColor := AnsiString(TVM(vm).popString.value);
    end;

  checkGraphicsSubsystem;
  if @graphicsMethodsPtr.lineWithColor <> nil then
    graphicsMethodsPtr.lineWithColor(AnsiString(penColor), x1, y1, x2, y2)
  else
    raise ERuntimeException.Create('line subsystem not available.');

  TVM(vm).pushNone;
end;

procedure TBuiltInGraphics.getCanvasSize(vm: TObject);
var
  p: TRhodusPoint;
  lt: TListObject;
begin
  checkGraphicsSubsystem;
  if @graphicsMethodsPtr.getCanvasSize <> nil then
    p := graphicsMethodsPtr.getCanvasSize;

  lt := TListObject.Create(2);
  lt.list[0].itemType := liInteger;
  lt.list[0].iValue := p.w;
  lt.list[1].itemType := liInteger;
  lt.list[1].iValue := p.h;
  TVM(vm).push(lt);
end;

procedure TBuiltInGraphics.beginUpdate(vm: TObject);
begin
  updateflag := True;

  checkGraphicsSubsystem;

  if @graphicsMethodsPtr.beginUpdate <> nil then
    graphicsMethodsPtr.beginUpdate()
  else
    raise ERuntimeException.Create('beginupdate subsystem not available.');

  TVM(vm).pushNone;
end;

procedure TBuiltInGraphics.endUpdate(vm: TObject);
begin
  updateflag := False;

  checkGraphicsSubsystem;

  if @graphicsMethodsPtr.endUpdate <> nil then
    graphicsMethodsPtr.endUpdate()
  else
    raise ERuntimeException.Create('endupdate subsystem not available.');

  TVM(vm).pushNone;
end;

procedure TBuiltInGraphics.refresh(vm: TObject);
begin
  checkGraphicsSubsystem;

  if @graphicsMethodsPtr.refresh <> nil then
    graphicsMethodsPtr.refresh()
  else
    raise ERuntimeException.Create('refresh subsystem not available.');

  TVM(vm).pushNone;
end;

end.
