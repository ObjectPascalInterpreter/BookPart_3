unit uBuiltInMath;

interface

Uses SysUtils, Classes, uModule, uBuiltInModule;

type
  TBuiltInMath = class (TBuiltInModule)

     procedure   getSin (vm : TObject);
     procedure   getCos(vm : TObject);
     procedure   getTan (vm : TObject);
     procedure   getLn (vm : TObject);
     procedure   getLog10 (vm : TObject);
     procedure   getExp (vm : TObject);
     procedure   getSqrt (vm : TObject);
     procedure   getAbs (vm : TObject);
     procedure   getRound (vm : TObject);
     procedure   getCeil (vm : TObject);
     constructor Create;
  end;

implementation

Uses Math, uSymbolTable, uVM, uStringObject, uListObject, uMemoryManager;


constructor TBuiltInMath.Create;
begin
  inherited Create ('math', 'Math Module');

  addMethod(getSin,    1, 'sin',  'Return the sine of a radian value: sin (1.2)');
  addMethod(getCos,    1, 'cos',  'Return the cosine of a radian value: cos (1.2)');
  addmethod(getTan,    1, 'tan',  'Compute tangent of a radian angle: tan (x)');
  addMethod (getSqrt,  1, 'sqrt', 'Compute the square rootof a number. Negative values are not supported: sqrt (9)');
  addMethod (getExp,   1, 'exp',  'Compute e raised to the power of a value: exp (10)');
  addMethod (getLn,    1, 'ln',   'Compute the natural logarithm of a value: ln (123)');
  addMethod (getLog10 ,1, 'log',  'Compute the logarithm to base 10 of a value: log (1000)');
  addMethod (getAbs,   1, 'abs',  'Return the absolute value of a number:: abs (-1.2)');
  addMethod (getRound, 1, 'round', 'Returns a value that is the value of the call argument rounded to the nearest whole number: round (3.4)');
  addMethod (getCeil,  1, 'ceil',  'Rounds variables up toward positive infinity: ceil (-1.2)');

  addValue ('pi', Pi,      'The value of pi');
  addValue ('e',  exp (1), 'The value of e');
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

end.
