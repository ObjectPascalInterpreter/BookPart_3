unit uBuiltInRandom;

interface


Uses SysUtils, Classes, uModule, uBuiltInModule;

type
  TBuiltInRandom = class (TBuiltInModule)

     procedure   getRandom  (vm : TObject);
     procedure   setSeed    (vm : TObject);
     procedure   getRandInt (vm : TObject);
     procedure   getGauss   (vm : TObject);

     constructor Create;
  end;


implementation

Uses Math, uSymbolTable, uVM, uStringObject, uListObject, uMemoryManager;

constructor TBuiltInRandom.Create;
begin
  inherited Create ('random', 'Random Number Module');

  addMethod (setSeed,    0, 'seed',    'Set the seed for the random number generator: seed (23)');
  addMethod (getRandom,  0, 'random',  'Return a uniformly distributed random number: random()');
  addmethod (getRandInt, 1, 'randint', 'Return an integer random number between 0 and a-1:  randint (10)');
  addmethod (getGauss,   2, 'gauss',   'Return a random number drawn from a Guassian distributrion with mean and stsndard deviation:  gauss (1, 0.5)');
end;


procedure TBuiltInRandom.getRandom (vm : TObject);

begin
   TVM (vm).push(random);
end;


procedure TBuiltInRandom.setSeed (vm : TObject);
var d : integer;
begin
   d := TVM (vm).popInteger;
   randSeed := d;
end;


procedure TBuiltInRandom.getRandInt (vm : TObject);
var a : integer;
begin
   a := TVM (vm).popInteger;
   TVM (vm).push(random (a));
end;


procedure TBuiltInRandom.getGauss (vm : TObject);
var mean, sd : double;
begin
   sd := TVM (vm).popScalar;
   mean := TVM (vm).popScalar;
   TVM (vm).push(RandG (mean, sd));
end;


end.
