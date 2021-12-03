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
     procedure   getSqr (vm : TObject);
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
     uMachineStack,
     uVMExceptions,
     uMemoryManager;


procedure raiseMathError (functionName : string);
begin
  raise ERuntimeException.Create('Argument to math function <' + functionName + '> can only be an integer, double or an array');
end;


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

  addMethod (getSqr,   1, 'sqr',   'Computes the square number or array: sqr (5)');
  addMethod (getSqrt,  1, 'sqrt',  'Computes the square root of a number or array. Negative values are not supported: sqrt (9)');
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
var st : PMachineStackRecord;
begin
   st := TVM (vm).pop;
   case st.stackType of
       stInteger : TVM (vm).push(sin (st.iValue));
       stDouble : TVM (vm).push (sin (st.dValue));
       stArray : begin
                 TVM (vm).push(st.aValue.applyUniFunction(sin));
                 end
  else
      raiseMathError ('sin');
  end;
end;


procedure TBuiltInMath.getCos (vm : TObject);
var st : PMachineStackRecord;
begin
   st := TVM (vm).pop;
   case st.stackType of
       stInteger : TVM (vm).push(cos (st.iValue));
       stDouble : TVM (vm).push (cos (st.dValue));
       stArray : begin
                 TVM (vm).push(st.aValue.applyUniFunction(cos));
                 end
  else
      raiseMathError ('cos');
  end;
end;


procedure TBuiltInMath.getTan (vm : TObject);
var st : PMachineStackRecord;
begin
   st := TVM (vm).pop;
   case st.stackType of
       stInteger : TVM (vm).push(tan (st.iValue));
       stDouble : TVM (vm).push (tan (st.dValue));
       stArray : begin
                 TVM (vm).push(st.aValue.applyUniFunction(tan));
                 end
  else
      raiseMathError ('tan');
  end;
end;


procedure TBuiltInMath.getASin (vm : TObject);
var st : PMachineStackRecord;
begin
   st := TVM (vm).pop;
   case st.stackType of
       stInteger : TVM (vm).push(arcsin (st.iValue));
       stDouble : TVM (vm).push (arcsin (st.dValue));
       stArray : begin
                 TVM (vm).push(st.aValue.applyUniFunction(arcsin));
                 end
  else
      raiseMathError ('arcsin');
  end;
end;


procedure TBuiltInMath.getACos (vm : TObject);
var st : PMachineStackRecord;
begin
   st := TVM (vm).pop;
   case st.stackType of
       stInteger : TVM (vm).push(arccos (st.iValue));
       stDouble : TVM (vm).push (arccos (st.dValue));
       stArray : begin
                 TVM (vm).push(st.aValue.applyUniFunction(arccos));
                 end
  else
      raiseMathError ('arccos');
  end;
end;


procedure TBuiltInMath.getATan (vm : TObject);
var st : PMachineStackRecord;
begin
   st := TVM (vm).pop;
   case st.stackType of
       stInteger : TVM (vm).push(arctan (st.iValue));
       stDouble : TVM (vm).push (arctan (st.dValue));
       stArray : begin
                 TVM (vm).push(st.aValue.applyUniFunction(arctan));
                 end
  else
      raiseMathError ('arctan');
  end;
end;


procedure TBuiltInMath.getSqrt (vm : TObject);
var st : PMachineStackRecord;
begin
   st := TVM (vm).pop;
   case st.stackType of
       stInteger : TVM (vm).push(sqrt (st.iValue));
       stDouble : TVM (vm).push (sqrt (st.dValue));
       stArray : begin
                 TVM (vm).push(st.aValue.applyUniFunction(sqrt));
                 end
  else
      raiseMathError ('sqrt');
  end;
end;


function dSqr (const value : extended) : extended;
begin
  result := sqr (value);
end;

procedure TBuiltInMath.getSqr (vm : TObject);
var st : PMachineStackRecord;
begin
   st := TVM (vm).pop;
   case st.stackType of
       stInteger : TVM (vm).push(sqr (st.iValue));
       stDouble : TVM (vm).push (sqr (st.dValue));
       stArray : begin
                 TVM (vm).push(st.aValue.applyUniFunction(dSqr));
                 end
  else
      raiseMathError ('sqr');
  end;
end;


procedure TBuiltInMath.getExp (vm : TObject);
var st : PMachineStackRecord;
begin
   st := TVM (vm).pop;
   case st.stackType of
       stInteger : TVM (vm).push(exp (st.iValue));
       stDouble : TVM (vm).push (exp (st.dValue));
       stArray : begin
                 TVM (vm).push(st.aValue.applyUniFunction(exp));
                 end
  else
      raiseMathError ('exp');
  end;
end;


procedure TBuiltInMath.getLn (vm : TObject);
var st : PMachineStackRecord;
begin
   st := TVM (vm).pop;
   case st.stackType of
       stInteger : TVM (vm).push(ln (st.iValue));
       stDouble : TVM (vm).push (ln (st.dValue));
       stArray : begin
                 TVM (vm).push(st.aValue.applyUniFunction(ln));
                 end
  else
      raiseMathError ('ln');
  end;
end;


procedure TBuiltInMath.getLog10 (vm : TObject);
var st : PMachineStackRecord;
begin
   st := TVM (vm).pop;
   case st.stackType of
       stInteger : TVM (vm).push(log10 (st.iValue));
       stDouble : TVM (vm).push (log10 (st.dValue));
       stArray : begin
                 TVM (vm).push(st.aValue.applyUniFunction(log10));
                 end
  else
      raiseMathError ('log10');
  end;
end;


function iAbs (value : integer) : integer;
begin
  result := abs (value);
end;

function dAbs (const value : extended) : extended;
begin
  result := abs (value);
end;

procedure TBuiltInMath.getAbs (vm : TObject);
var st : PMachineStackRecord;
begin
   st := TVM (vm).pop;
   case st.stackType of
       stInteger : TVM (vm).push(iabs (st.iValue));
       stDouble : TVM (vm).push (dAbs (st.dValue));
       stArray : begin
                 TVM (vm).push(st.aValue.applyUniFunction(dabs));
                 end
  else
      raiseMathError ('abs');
  end;
end;


function dRound (const value : extended) : extended;
begin
  result := round (value);
end;

procedure TBuiltInMath.getRound (vm : TObject);
var st : PMachineStackRecord;
begin
   st := TVM (vm).pop;
   case st.stackType of
       stInteger : TVM (vm).push(round (st.iValue));
       stDouble : TVM (vm).push (round (st.dValue));
       stArray : begin
                 TVM (vm).push(st.aValue.applyUniFunction(dRound));
                 end
  else
      raiseMathError ('round');
  end;
end;


function dCeil (const value : extended) : extended;
begin
  result := ceil (value);
end;

procedure TBuiltInMath.getCeil (vm : TObject);
var st : PMachineStackRecord;
begin
   st := TVM (vm).pop;
   case st.stackType of
       stInteger : TVM (vm).push(ceil (st.iValue));
       stDouble : TVM (vm).push (ceil (st.dValue));
       stArray : begin
                 TVM (vm).push(st.aValue.applyUniFunction(dCeil));
                 end
  else
      raiseMathError ('ceil');
  end;
end;


function dFloor (const value : extended) : extended;
begin
  result := ceil (value);
end;


procedure TBuiltInMath.getFloor (vm : TObject);
var st : PMachineStackRecord;
begin
   st := TVM (vm).pop;
   case st.stackType of
       stInteger : TVM (vm).push(floor (st.iValue));
       stDouble : TVM (vm).push (floor (st.dValue));
       stArray : begin
                 TVM (vm).push(st.aValue.applyUniFunction(dFloor));
                 end
  else
      raiseMathError ('floor');
  end;
end;



function dTrunc (const value : extended) : extended;
begin
  result := trunc (value);
end;

procedure TBuiltInMath.getTrunc (vm : TObject);
var st : PMachineStackRecord;
begin
   st := TVM (vm).pop;
   case st.stackType of
       stInteger : TVM (vm).push(trunc (st.iValue));
       stDouble : TVM (vm).push (trunc (st.dValue));
       stArray : begin
                 TVM (vm).push(st.aValue.applyUniFunction(dTrunc));
                 end
  else
      raiseMathError ('trun');
  end;
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
