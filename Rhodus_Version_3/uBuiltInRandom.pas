unit uBuiltInRandom;

{
  Unit:    uBuiltInRandom.pas
  Author:  Herbert M sauro
  Date:    10/2021
  Purpose: This file implements the random number library for the Rhodus interpreter.

  Ths source is distributed under Apache 2.0
  See https://www.apache.org/licenses/LICENSE-2.0.txt for further information

  Copyright (C)  2019-2024 Herbert M Sauro

  Author Contact Information:
  email: hsauro@gmail.com
}

interface

Uses SysUtils, Classes, uLibModule;

type
  TBuiltInRandom = class (TModuleLib)

     procedure   getRandom  (vm : TObject);
     procedure   setSeed    (vm : TObject);
     procedure   getRandInt (vm : TObject);
     procedure   getGauss   (vm : TObject);
     procedure   getRandList (vm : TObject);

     constructor Create;
  end;


implementation

Uses Math,
     uSymbolTable,
     uVM,
     uHelpUnit,
     uStringObject,
     uListObject,
     uRhodusTypes,
     uMemoryManager;

constructor TBuiltInRandom.Create;
begin
  inherited Create ('random');

  addMethod (setSeed,     0, 'seed');
  addMethod (getRandom,   0, 'rnd');
  addmethod (getRandInt,  1, 'rndi', 'Return an integer random number between 0 and a-1:  rndi (10)');
  addmethod (getRandList, 2, 'rndList', 'Return a list of given length containing random integers:  rndlist (num elements, range)');
  addmethod (getGauss,    2, 'rndn',   'Return a random number drawn from a Guassian distributrion with mean and standard deviation: x = rndn (1, 0.5)');
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


procedure TBuiltInRandom.getRandList (vm : TObject);
var n, range : integer;
    l : TListObject;
    i : integer;
begin
  range := TVM (vm).popInteger;
  n := TVM (vm).popInteger;
  l := TListObject.Create (n);
  for i := 0 to n - 1 do
      begin
      l.list[i].iValue := random (range);
      l.list[i].itemType := symInteger;
      end;
  TVM (vm).push (l);
end;

end.
