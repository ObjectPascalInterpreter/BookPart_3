unit uBuiltInMath;

{
  Unit:    uBuiltInMath.pas
  Author:  Herbert M sauro
  Date:    10/2021
  Purpose: This file implements the Math module for the Rhodus interpreter.

  Ths source is distributed under Apache 2.0
  See https://www.apache.org/licenses/LICENSE-2.0.txt for further information

  Copyright (C)  2019-2024 Herbert M Sauro

  Author Contact Information:
  email: hsauro@gmail.com
}


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
     uHelpUnit,
     uStringObject,
     uListObject,
     uValueObject,
     uMatrixObject,
     uMachineStack,
     uVMExceptions,
     uMemoryManager;

var default_epsilon : double = 1e-6;

procedure raiseMathError (functionName : string);
begin
  raise ERuntimeException.Create('Argument to math function <' + functionName + '> can only be an integer, double or an array');
end;



constructor TBuiltInMath.Create;
begin
  inherited Create ('math');

  addMethod (getSin,   1, 'sin');
  addMethod (getCos,   1, 'cos');
  addmethod (getTan,   1, 'tan');
  addMethod (getASin,  1, 'asin');
  addMethod (getACos,  1, 'acos');
  addmethod (getATan,  1, 'atan');

  addmethod (getDegrees, 1, 'toDegrees');
  addmethod (getRadians, 1, 'toRadians');

  addMethod (getSqr,   1, 'sqr');
  addMethod (getSqrt,  1, 'sqrt');
  addMethod (getExp,   1, 'exp');
  addMethod (getLn,    1, 'ln');
  addMethod (getLog10, 1, 'log');
  addMethod (getAbs,   1, 'abs');
  addMethod (getRound, 1, 'round');
  addMethod (getCeil,  1, 'ceil');
  addMethod (getFloor, 1, 'floor');
  addMethod (getTrunc, 1, 'trunc');
  addMethod (getMax,   2, 'max');
  addMethod (getMin,   2, 'min');

  //addMethod (getComb,  2, 'comb',   'Returns the number of ways to choose k items from n items without repetition or order: comb (5, 2)');

  addObjectValue ('pi', TValueObject.Create (Pi), True);    // True = locked
  addObjectValue ('e', TValueObject.Create (exp(1)), True);
  addObjectValue ('eps', TValueObject.Create (default_epsilon), False);
end;


procedure TBuiltInMath.getSin (vm : TObject);
var st : PMachineStackRecord;
begin
   st := TVM (vm).pop;
   case st.stackType of
       stInteger : TVM (vm).push(sin (st.iValue));
       stDouble  : TVM (vm).push (sin (st.dValue));
       stValueObject : TVM (vm).push (sin (TValueObject.getValue(st.voValue)));
       stMatrix: TVM(vm).push(TMatrixObject.applyUniFunction(st.mValue, sin));

       stArray   : begin
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
       stValueObject : TVM (vm).push (cos (TValueObject.getValue(st.voValue)));
       stMatrix: TVM(vm).push(TMatrixObject.applyUniFunction(st.mValue, cos));
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
       stValueObject : TVM (vm).push (tan (TValueObject.getValue(st.voValue)));
      stMatrix: TVM(vm).push(TMatrixObject.applyUniFunction(st.mValue, tan));

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
  stValueObject : TVM (vm).push (arcsin (TValueObject.getValue(st.voValue)));
      stMatrix: TVM(vm).push(TMatrixObject.applyUniFunction(st.mValue, arcsin));
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
       stValueObject : TVM (vm).push (arccos (TValueObject.getValue(st.voValue)));
      stMatrix: TVM(vm).push(TMatrixObject.applyUniFunction(st.mValue, arccos));

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
       stValueObject : TVM (vm).push (arctan (TValueObject.getValue(st.voValue)));
       stMatrix: TVM(vm).push(TMatrixObject.applyUniFunction(st.mValue, arctan));
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
       stValueObject : TVM (vm).push (sqrt (TValueObject.getValue(st.voValue)));
      stMatrix: TVM(vm).push(TMatrixObject.applyUniFunction(st.mValue, sqrt));

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
       stValueObject : TVM (vm).push (sqr (TValueObject.getValue(st.voValue)));
      stMatrix: TVM(vm).push(TMatrixObject.applyUniFunction(st.mValue, dsqr));

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
       stValueObject : TVM (vm).push (exp (TValueObject.getValue(st.voValue)));
      stMatrix: TVM(vm).push(TMatrixObject.applyUniFunction(st.mValue, exp));

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
       stValueObject : TVM (vm).push(ln (st.voValue.dValue));
       stMatrix: TVM(vm).push(TMatrixObject.applyUniFunction(st.mValue, ln));

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
       stValueObject : TVM (vm).push (log10 (TValueObject.getValue(st.voValue)));
      stMatrix: TVM(vm).push(TMatrixObject.applyUniFunction(st.mValue, log10));

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
       stValueObject : TVM (vm).push (dAbs (TValueObject.getValue(st.voValue)));
      stMatrix: TVM(vm).push(TMatrixObject.applyUniFunction(st.mValue, dabs));

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
       stValueObject : TVM (vm).push (dround (TValueObject.getValue(st.voValue)));
      stMatrix: TVM(vm).push(TMatrixObject.applyUniFunction(st.mValue, dRound));

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
       stValueObject : TVM (vm).push (ceil (TValueObject.getValue(st.voValue)));
      stMatrix: TVM(vm).push(TMatrixObject.applyUniFunction(st.mValue, dCeil));

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
       stValueObject : TVM (vm).push (floor (TValueObject.getValue(st.voValue)));
      stMatrix: TVM(vm).push(TMatrixObject.applyUniFunction(st.mValue, dFloor));

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
       stValueObject : TVM (vm).push (trunc (TValueObject.getValue(st.voValue)));
      stMatrix: TVM(vm).push(TMatrixObject.applyUniFunction(st.mValue, dtrunc));

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
