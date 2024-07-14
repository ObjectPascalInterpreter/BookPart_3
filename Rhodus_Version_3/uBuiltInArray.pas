unit uBuiltInArray;

{
  Unit:    uBuiltInArray.pas
  Author:  Herbert M sauro
  Date:    10/2021
  Purpose: This file implements the arrays module for the Rhodus interpreter.
           Arrays can have any number of dimensions. There is a separate
           object type for 2D matrices

           Array are creatde from lists using the array function:
           eg
              x = array ([[1,2,3,4], [6,5,4,2]])

           If the array is 2D, it can be converted to a matrix type using

           m = x.toMatrix()

           If matrix can be converted to a 2D array:

           a = m.toArray()

           Both arrays and matricies can beconvert to lists:

           l = a.toList()
           l = m.toList()

  Ths source is distributed under Apache 2.0
  See https://www.apache.org/licenses/LICENSE-2.0.txt for further information

  Copyright (C)  2019-2024 Herbert M Sauro

  Author Contact Information:
  email: hsauro@gmail.com
}

interface

Uses SysUtils, Classes, uLibModule;

type
  TBuiltInArray = class (TModuleLib)
     procedure   getMake (vm : TObject);
     procedure   getRndu (vm : TObject);
     procedure   getRndi (vm : TObject);
     procedure   getRndn (vm : TObject);
     procedure   isEqual (vm : TObject);
     procedure   getRange (vm : TObject);
     procedure   getMean(vm: TObject);
     procedure   getBin(vm: TObject);
     constructor Create;
     destructor  Destroy; override;
  end;


implementation

Uses Math,
     uVM,
     uRhodusTypes,
     uSymbolTable,
     uStringObject,
     uListObject,
     uArrayObject,
     uMachineStack,
     uMemoryManager,
     uHelpUnit,
     uVMExceptions;


constructor TBuiltInArray.Create;
begin
  inherited Create ('arrays');

  addMethod(getMake,     1, 'make',  'Create an array of given length: ar = arrays.make (10, [10])');
  addMethod(getRange,    3, 'range', 'Create an array of evenly spaced values: ar = arrays.make (0, 5, 10)');
  addMethod(getRndu,    VARIABLE_ARGS, 'rndu',  'Create an array of given dimensions of uniformly random numbers: ar = arrays.rand (4,4,2)');
  addMethod(getRndi,     3, 'rndi', 'Create am array of uniformly random integers: ar = arrays.rndi (lower, upper, [3,3])');
  addMethod(getRndn,     VARIABLE_ARGS, 'rndn', 'Create an array of normally distributed random numbers: ar = arrays.rndi (lower, upper, [3,3])');
  addMethod(isEqual,     2, 'equal', 'Return true if the two arrays are equal: arrays.equal (m1,m2)');
  addMethod(getMean,     1, 'mean', 'Return mean values of a 1D array: m = arrays.mean (a)');
  addMethod(getBin,      4, 'bin', 'm = arrays.bin (x, 0, 5, 20)');

  //addMethod(isEqual,     2, 'appendrow', 'Return true if the two arrays are equal: arrays.equal (m1,m2)');
  end;


destructor TBuiltInArray.Destroy;
begin
  inherited;
end;


procedure TBuiltInArray.getMean(vm: TObject);
var i : integer;
    a : TArrayObject;
    sum : double;
begin
  a := TVM (vm).popArray;
  if a.ndims > 1 then
     ERuntimeException.Create('Array must be one dimensional');

  sum := 0;
  for i := 0 to a.dim[0] - 1 do
      sum := sum + a.getValue(i);
  TVM (vm).push(sum/a.dim[0]);
end;


procedure TBuiltInArray.getBin(vm: TObject);
var nBins, bin, i : integer;
    dMax, dMin, dx : double;
    data : TArrayObject;
    bins : TArrayObject;
    BinIndex: Integer;
    BinWidth: Double;
begin
  nBins := TVM (vm).popInteger;
  dMax := TVM (vm).popScalar;
  dMin := TVM (vm).popScalar;

  data := TVM (vm).popArray;

  bins :=  TArrayObject.Create (nBins);

  BinWidth := (dMax - dMin) / nBins;

  for i := 0 to Data.dim[0] - 1 do
  begin
    if (Data.getValue1D(i) >= dMin) and (Data.getValue1D(i) <= dMax) then
    begin
      BinIndex := Floor((Data.getValue1D(i) - dMin) / BinWidth);
      if BinIndex >= nBins then
        BinIndex := nBins - 1; // Ensure the maximum value falls into the last bin/      Inc(Bins[BinIndex]);
      Bins.setValue1D(BinIndex, Bins.getValue1D(BinIndex) + 1);
    end;
  end;
  TVM (vm).push (bins);
end;


procedure TBuiltInArray.getRndu(vm: TObject);
var ar : TArrayObject;
    nArgs : integer;
    i : integer;
    idx : TIndexArray;
begin
   nArgs := TVM (vm).popInteger;
   setLength (idx, nArgs);
   for i := nArgs - 1 downto 0 do
       idx[i] := TVM (vm).popInteger;

   ar := TArrayObject.Create(idx);
   for i := 0 to length (ar.dataf) - 1 do
       ar.dataf[i] := random ();
   TVM (vm).push (ar);
end;


procedure TBuiltInArray.getRndn (vm : TObject);
var mean, sd : double;
    idx : TIndexArray;
    i : integer;
    nArgs : integer;
    ar : TArrayObject;
begin
   nArgs := TVM (vm).popInteger;
   sd := TVM (vm).popScalar;
   mean := TVM (vm).popScalar;

   setLength (idx, nArgs-2);
   for i := nArgs - 3 downto 0 do
       idx[i] := TVM (vm).popInteger;

   ar := TArrayObject.Create(idx);
   for i := 0 to length (ar.dataf) - 1 do
       ar.dataf[i] := RandG (mean, sd);
   TVM (vm).push (ar);
end;


procedure TBuiltInArray.getRndi (vm : TObject);
var lower, upper : integer;
    n : integer;
    ar : TArrayObject;
    al : TListObject;
    i : integer;
    idx : TIndexArray;
begin
  al := TVM (vm).popList;       // dimensions
  upper := TVM (vm).popInteger;
  lower := TVM (vm).popInteger;
  setLength (idx, al.list.count);
  n := 1;
  for i := 0 to al.list.Count - 1 do
      begin
      n := n * al.list[i].iValue;
      idx[i] := al.list[i].iValue;
      end;

  ar := TArrayObject.Create (idx);

  for i := 0 to (n) - 1 do
      begin
      ar.dataf[i] := RandomRange(lower, upper);
      end;

  TVM (vm).push (ar);
end;


procedure TBuiltInArray.getMake (vm : TObject);
var numberOfElements : integer;
    ar: TArrayObject;
begin
  numberOfElements := TVM (vm).popInteger;

  ar := TArrayObject.Create (numberOfElements);

  TVM (vm).push(ar);
end;


procedure TBuiltInArray.getRange (vm : TObject);
var start, finish : double;
    i, n : integer;
    step : double;
    ar: TArrayObject;
begin
  step := TVM (vm).popScalar;
  if step < 0 then
     TVM (vm).raiseError('Step size must be positive in range function');

  finish := TVM (vm).popScalar;
  start := TVM (vm).popScalar;

  n := Trunc((finish - start) / step) + 1;

  ar := TArrayObject.Create (n);

  i := 0;
  while start <= finish do
    begin
    ar.setValue1D(i, start);
    start := start + step;
    inc(i);
    end;
  TVM (vm).push(ar);
end;


procedure TBuiltInArray.isEqual (vm : TObject);
var a1, a2 : TArrayObject;
begin
  a2 := TVM (vm).popArray;
  a1 := TVM (vm).popArray;

 TVM (vm).push(TArrayObject.isEqualTo(a1, a2));
end;

end.

