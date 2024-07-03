unit uBuiltInPlotter;


// This source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

Uses SysUtils, Classes,
     uRhodusLibTypes,
     uLibModule;

type
  TBuiltInPlotter = class (TModuleLib)

  private
     procedure checkPlottingSubsystem;

  public
     procedure   plotSineWave (vm : TObject);
     procedure   clear (vm : TObject);
     //procedure   plot (vm : TObject);

//     procedure   turnLeft (vm : TObject);
//     procedure   turnRight (vm : TObject);
//     procedure   goForward (vm : TObject);
//     procedure   goBack (vm : TObject);
//     procedure   penDown (vm : TObject);
//     procedure   penUp (vm : TObject);
//     procedure   clearTurtle (vm : TObject);
//     procedure   resetTurtle (vm : TObject);
//     procedure   getPosition (vm : TObject);
//     procedure   setPosition (vm : TObject);
//     procedure   getWidth (vm : TObject);
//     procedure   getHeight (vm : TObject);
//     procedure   setPenColor (vm : TObject);
//     procedure   setPenWidth (vm : TObject);
//     procedure   getColors (vm : TObject);
//     procedure   setBackGroundColor (vm : TObject);

     constructor Create;
     destructor  Destroy; override;
  end;

procedure setPlottingCallBackTable(plottingMethods: PPlottingMethods);

implementation

Uses Math, uSymbolTable, uVM, uStringObject, uListObject,
     uVMExceptions, uMachineStack, uMemoryManager,
     System.Types, Vcl.Graphics, StrUtils,
     uArrayObject;

const
   availableColors : TArray<String> = ['clRed', 'clBlue','clGreen', 'clBlack', 'clGray',
              'clYellow', 'clPurple'];

var plottingMethodsPtr : PPlottingMethods;


procedure setPlottingCallBackTable(plottingMethods: PPlottingMethods);
begin
  plottingMethodsPtr := plottingMethods;
end;


procedure TBuiltInPlotter.checkPlottingSubsystem;
begin
  if plottingMethodsPtr = nil then
    raise ERuntimeException.Create('No plotting subsystem detected.');
end;


constructor TBuiltInPlotter.Create;
begin
  inherited Create ('plot', 'Plot Module');

  addMethod(plotsinewave, 0, 'sw',   'Return the sine of a radian value: sin (1.2)');
  addMethod(clear,        0, 'clear', 'Clear the plot: plot.clear ()');
  //addMethod(plot,         2, 'plot', 'Plot: plot.plot (x, y)');

//  addMethod(goForward,    1, 'fd',  'Return the cosine of a radian value: cos (1.2)');
//  addMethod(goBack,       1, 'bk',     'Return the cosine of a radian value: cos (1.2)');
//  addMethod(penDown,      0, 'pendown',     'Return the cosine of a radian value: cos (1.2)');
//  addMethod(penUp,        0, 'penup',     'Return the cosine of a radian value: cos (1.2)');
//  addMethod(clearTurtle,  0, 'clear',     'Return the cosine of a radian value: cos (1.2)');
//  addMethod(resetTurtle,  0, 'reset',     'Return the cosine of a radian value: cos (1.2)');
//  addMethod(getPosition,  0, 'pos',     'Return the cosine of a radian value: cos (1.2)');
//  addMethod(setPosition,  2, 'setpos',     'Return the cosine of a radian value: cos (1.2)');
//  addMethod(getWidth,     0, 'width',     'Return the cosine of a radian value: cos (1.2)');
//  addMethod(getHeight,    0, 'height',     'Return the cosine of a radian value: cos (1.2)');
//
//  addMethod(setBackGroundColor, 1, 'bcolor', 'Set background color');
  //addMethod(setPenColor,  1, 'pencolor', '');
  //addMethod(getColors,    0, 'getcolors', '');
  //addMethod(setPenWidth,  1, 'penwidth', '');
end;


destructor TBuiltInPlotter.Destroy;
begin
  inherited;
end;


procedure TBuiltInPlotter.plotSineWave (vm : TObject);
begin
  checkPlottingSubsystem;

  if @plottingMethodsPtr.plotSineWave <> nil then
    plottingMethodsPtr.plotSineWave
  else
    raise ERuntimeException.Create('clear subsystem not available.');

  TVM (vm).push(@noneStackType);
end;


procedure TBuiltInPlotter.clear (vm : TObject);
begin
  checkPlottingSubsystem;

  if @plottingMethodsPtr.clear <> nil then
    plottingMethodsPtr.clear
  else
    raise ERuntimeException.Create('clear subsystem not available.');

  TVM (vm).push(@noneStackType);
end;

//procedure TBuiltInPlotter.plot (vm : TObject);
//var x, y : TArrayObject;
//begin
//   y := TVM (vm).popArray;
//   x := TVM (vm).popArray;
//
//   if connectionMade then
//      begin
//      p.plot (x, y);
//      TVM (vm).push(@noneStackType);
//      end
//   else
//      raise ERuntimeException.Create('Unable to access turtle server');
//end;

//
//procedure TBuiltInTurtle.goBack (vm : TObject);
//var d : double;
//begin
//   d := TVM (vm).popScalar;
//   if connectionMade then
//      begin
//      t.goBack (d);
//      TVM (vm).push(@noneStackType);
//      end
//   else
//      raise ERuntimeException.Create('Unable to access turtle server');
//end;
//
//
//procedure TBuiltInTurtle.penDown (vm : TObject);
//begin
//   if connectionMade then
//      begin
//      t.penDown;
//      TVM (vm).push(@noneStackType);
//      end
//   else
//      raise ERuntimeException.Create('Unable to access turtle server');
//end;
//
//
//procedure TBuiltInTurtle.getColors (vm : TObject);
//var colorList : TListObject; i : integer;
//    astr : string;
//begin
//   colorList := TListObject.Create (0);
//   for i := 0 to length (availableColors) - 1 do
//       begin
//       astr := RightStr (availableColors[i], length (availableColors[i]) - 2);
//       colorList.append (TStringObject.Create (lowercase (astr)));
//       end;
//
//   TVM (vm).push(colorList);
//end;
//
//
//procedure TBuiltInTurtle.penUp (vm : TObject);
//begin
//   if connectionMade then
//      begin
//      t.penUp;
//      TVM (vm).push(@noneStackType);
//      end
//   else
//      raise ERuntimeException.Create('Unable to access turtle server');
//end;
//
//
//procedure TBuiltInTurtle.clearTurtle (vm : TObject);
//begin
//   if connectionMade then
//      begin
//      t.clearScreen;
//      TVM (vm).push(@noneStackType);
//      end
//   else
//      raise ERuntimeException.Create('Unable to access turtle server');
//end;
//
//
//procedure TBuiltInTurtle.resetTurtle (vm : TObject);
//begin
//  if connectionMade then
//      begin
//      t.resetPosition();
//      TVM (vm).push(@noneStackType);
//      end
//   else
//      raise ERuntimeException.Create('Unable to access turtle server');
//end;
//
//
//procedure TBuiltInTurtle.getPosition (vm : TObject);
//var p : TPointF; l : TListObject;
//begin
//   if connectionMade then
//      begin
//      p := t.getPosition;
//      l := TListObject.Create (0);
//      l.append (p.x); l.append (p.y);
//      TVM (vm).push(l);
//      end
//   else
//      raise ERuntimeException.Create('Unable to access turtle server');
//end;
//
//procedure TBuiltInTurtle.setPosition (vm : TObject);
//var x, y : double;
//begin
//  y := TVM (vm).popScalar;
//  x := TVM (vm).popScalar;
//
//  if connectionMade then
//     begin
//     t.setPosition (x, y);
//     TVM (vm).push(@noneStackType);
//      end
//  else
//      raise ERuntimeException.Create('Unable to access turtle server');
//end;
//
//
//procedure TBuiltInTurtle.getWidth (vm : TObject);
//begin
//  if connectionMade then
//     begin
//     TVM (vm).push(t.getWidth());
//      end
//  else
//      raise ERuntimeException.Create('Unable to access turtle server');
//end;
//
//
//procedure TBuiltInTurtle.getHeight (vm : TObject);
//begin
//  if connectionMade then
//     begin
//     TVM (vm).push(t.getHeight());
//      end
//  else
//      raise ERuntimeException.Create('Unable to access turtle server');
//end;
//
//
//procedure TBuiltInTurtle.setPenColor (vm : TObject);
//var c : string;
//    i : integer;
//begin
//  c  := TVM (vm).popString.value;
//
//  if connectionMade then
//     begin
//     for i := 0 to length (availableColors) - 1 do
//         // chop off cl in the string color name
//         if c = rightStr (lowercase (availableColors[i]), length (availableColors[i]) - 2) then
//            t.setPenColor (integer (StringToColor (availableColors[i])));
//     TVM (vm).push(@noneStackType);
//     end
//  else
//      raise ERuntimeException.Create('Unable to access turtle server');
//end;
//
//
//procedure TBuiltInTurtle.setPenWidth (vm : TObject);
//var w : integer;
//begin
//  w := TVM (vm).popInteger;
//
//  if connectionMade then
//     begin
//     t.setPenWidth (w);
//     TVM (vm).push(@noneStackType);
//      end
//  else
//      raise ERuntimeException.Create('Unable to access turtle server');
//end;
//
//
//procedure TBuiltInTurtle.setBackGroundColor (vm : TObject);
//var c : string;
//    i : integer;
//begin
//  c := TVM (vm).popString.value;
//
// if connectionMade then
//     begin
//     for i := 0 to length (availableColors) - 1 do
//         // chop off cl in the string color name
//         if c = rightStr (lowercase (availableColors[i]), length (availableColors[i]) - 2) then
//            t.setBackGroundColor (integer (StringToColor (availableColors[i])));
//     TVM (vm).push(@noneStackType);
//     end;
//end;


end.
