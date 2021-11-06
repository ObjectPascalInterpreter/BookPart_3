unit uBuiltInMath;


// This source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com


interface

Uses SysUtils, Classes, uLibModule;

type
  TBuiltInMath = class (TModuleLib)

     procedure   getSin (vm : TObject);
     procedure   getCos(vm : TObject);
     procedure   getTan (vm : TObject);
     procedure   getASin (vm : TObject);
     procedure   getACos(vm : TObject);
     procedure   getATan (vm : TObject);
     procedure   getDegrees (vm : TObject);
     procedure   getRadians (vm : TObject);

     procedure   getLn (vm : TObject);
     procedure   getLog10 (vm : TObject);
     procedure   getExp (vm : TObject);
     procedure   getSqrt (vm : TObject);
     procedure   getAbs (vm : TObject);
     procedure   getRound (vm : TObject);
     procedure   getCeil (vm : TObject);
     procedure   getFloor (vm : TObject);
     procedure   getTrunc (vm : TObject);
     procedure   getMax (vm : TObject);
     procedure   getMin (vm : TObject);

     //procedure   getComb (vm : TObject);

     constructor Create;
  end;


implementation

Uses Math,
     uSymbolTable,
     uVM,
     uStringObject,
     uListObject,
     uMemoryManager;


constructor TBuiltInMath.Create;
begin
  inherited Create ('math', 'Math Module');

  addMethod (getSin,   1, 'sin',    'Returns the sine of a radian value: sin (1.2)');
  addMethod (getCos,   1, 'cos',    'Returns the cosine of a radian value: cos (1.2)');
  addmethod (getTan,   1, 'tan',    'Computes tangent of a radian angle: tan (x)');
  addMethod (getASin,  1, 'asin',   'Returns in radians the arcsine of a value: asin (1.2)');
  addMethod (getACos,  1, 'acos',   'Returns in radians the arccosine of a value: acos (1.2)');
  addmethod (getATan,  1, 'atan',   'Computes in radians arctangent of a angle: atan (x)');

  addmethod (getDegrees, 1, 'toDegrees',  'Converts radians to degrees: toDegrees(radians)');
  addmethod (getRadians, 1, 'toRadians',  'Converts degrees to radians: toRadians(degrees)');

  addMethod (getSqrt,  1, 'sqrt',  'Computes the square rootof a number. Negative values are not supported: sqrt (9)');
  addMethod (getExp,   1, 'exp',   'Computes e raised to the power of a value: exp (10)');
  addMethod (getLn,    1, 'ln',    'Computes the natural logarithm of a value: ln (123)');
  addMethod (getLog10, 1, 'log',   'Computes the logarithm to base 10 of a value: log (1000)');
  addMethod (getAbs,   1, 'abs',   'Returns the absolute value of a number:: abs (-1.2)');
  addMethod (getRound, 1, 'round', 'Returns a value to the nearest whole number: round (3.4)');
  addMethod (getCeil,  1, 'ceil',  'Rounds variables up toward positive infinity: ceil (-1.2)');
  addMethod (getFloor, 1, 'floor', 'Returns the largest integer less than or equal to a given number: floor (2.3');
  addMethod (getTrunc, 1, 'trunc', 'Returns the integer part of floating point number: trunc (3.13)');
  addMethod (getMax,   2, 'max',   'Returns the maximunm of two numbers: max (3, 5)');
  addMethod (getMin,   2, 'min',   'Returns the minimum of two numbers: min (3, 5)');

  //addMethod (getComb,  2, 'comb',   'Returns the number of ways to choose k items from n items without repetition or order: comb (5, 2)');

  addDoubleValue ('pi', Pi,      'The value of pi', True);  // True = locked
  addDoubleValue ('e',  exp (1), 'The value of e', True);
end;


procedure TBuiltInMath.getSin (vm : TObject);
var d : double;
begin
   d := TVM (vm).popScalar;
   TVM (vm).push(sin (d));
end;


procedure TBuiltInMath.getCos (vm : TObject);
var d : double;
begin
   d := TVM (vm).popScalar;
   TVM (vm).push (cos (d));
end;


procedure TBuiltInMath.getTan (vm : TObject);
var d : double;
begin
   d := TVM (vm).popScalar;
   TVM (vm).push (tan (d));
end;


procedure TBuiltInMath.getASin (vm : TObject);
var d : double;
begin
   d := TVM (vm).popScalar;
   TVM (vm).push(arcsin (d));
end;


procedure TBuiltInMath.getACos (vm : TObject);
var d : double;
begin
   d := TVM (vm).popScalar;
   TVM (vm).push (arccos (d));
end;


procedure TBuiltInMath.getATan (vm : TObject);
var d : double;
begin
   d := TVM (vm).popScalar;
   TVM (vm).push (arctan (d));
end;


procedure TBuiltInMath.getSqrt (vm : TObject);
var d : double;
begin
   d := TVM (vm).popScalar;
   TVM (vm).push (sqrt (d));
end;


procedure TBuiltInMath.getExp (vm : TObject);
var d : double;
begin
   d := TVM (vm).popScalar;
   TVM (vm).push (exp (d));
end;


procedure TBuiltInMath.getLn (vm : TObject);
var d : double;
begin
   d := TVM (vm).popScalar;
   TVM (vm).push (ln (d));
end;


procedure TBuiltInMath.getLog10 (vm : TObject);
var d : double;
begin
   d := TVM (vm).popScalar;
   TVM (vm).push (Log10 (d));
end;


procedure TBuiltInMath.getAbs (vm : TObject);
var d : double;
begin
   d := TVM (vm).popScalar;
   TVM (vm).push (Abs (d));
end;


procedure TBuiltInMath.getRound (vm : TObject);
var d : double;
begin
   d := TVM (vm).popScalar;
   TVM (vm).push (Round (d));
end;


procedure TBuiltInMath.getCeil (vm : TObject);
var d : double;
begin
   d := TVM (vm).popScalar;
   TVM (vm).push (Ceil (d))
end;

procedure TBuiltInMath.getFloor (vm : TObject);
var d : double;
begin
   d := TVM (vm).popScalar;
   TVM (vm).push (floor (d));
end;


procedure TBuiltInMath.getTrunc (vm : TObject);
var d : double;
begin
   d := TVM (vm).popScalar;
   TVM (vm).push (trunc (d));
end;


procedure TBuiltInMath.getMax (vm : TObject);
var d1, d2 : double;
begin
   d1 := TVM (vm).popScalar;
   d2 := TVM (vm).popScalar;
   TVM (vm).push (Max (d1, d2));
end;

procedure TBuiltInMath.getMin (vm : TObject);
var d1, d2 : double;
begin
   d1 := TVM (vm).popScalar;
   d2 := TVM (vm).popScalar;
   TVM (vm).push (Min (d1, d2));
end;


procedure TBuiltInMath.getDegrees (vm : TObject);
var d : double;
begin
   d := TVM (vm).popScalar;
   TVM (vm).push (RadToDeg (d));
end;


procedure TBuiltInMath.getRadians (vm : TObject);
var d : double;
begin
   d := TVM (vm).popScalar;
   TVM (vm).push (DegToRad (d));
end;


//
//procedure TBuiltInMath.getComb (vm : TObject);
//var m, n, i : integer;
//    combination: array of integer;
//begin
//   m := TVM (vm).popInteger;
//   n := TVM (vm).popInteger;
//
//   setLength (combination, m);
//    //TVM (vm).push (
//end;




// procedure generate(m: integer);
//  var
//   n, i: integer;
//  begin
//   if (m > m_max) then
//    begin
//    for i := 1 to m_max do
//     write (combination[i], ' ');
//    writeln;
//    end
//   else
//    for n := 1 to n_max do
//     if ((m = 1) or (n > combination[m-1])) then
//      begin
//       combination[m] := n;
//       generate(m + 1);
//      end;
//   end;


end.
