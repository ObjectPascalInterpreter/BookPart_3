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
     procedure   isEqual (vm : TObject);
     procedure   getRange (vm : TObject);
     constructor Create;
     destructor  Destroy; override;
  end;


implementation

Uses Math,
     uRhodusTypes,
     uSymbolTable,
     uVM,
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
  addMethod(getRndu,    VARIABLE_ARGS, 'rand',  'Create an array of uniformly random numbers: ar = arrays.rand (4,4,2)');
  addMethod(getRndi,     3, 'randi', 'Create a list of uniformly random integers: ar = arrays.randi (lower, upper, [3,3])');
  addMethod(isEqual,     2, 'equal', 'Return true if the two arrays are equal: arrays.equal (m1,m2)');
  addMethod(isEqual,     2, 'appendrow', 'Return true if the two arrays are equal: arrays.equal (m1,m2)');
  end;


destructor TBuiltInArray.Destroy;
begin
  inherited;
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
    numberOfElements, i : integer;
    hstep : double;
    r : double;
    ar: TArrayObject;
begin
  numberOfElements := TVM (vm).popInteger;
  finish := TVM (vm).popScalar;
  start := TVM (vm).popScalar;

  hstep := (finish-start)/numberOfElements;

  r := start;

  ar := TArrayObject.Create (numberOfElements);

  for i := 0 to numberOfElements - 1 do
     begin
      ar.setValue1D(i, r);
      r := r + hstep;
     end;
  TVM (vm).push(ar);
end;


procedure TBuiltInArray.isEqual (vm : TObject);
var m1, m2 : TArrayObject;
begin
  m2 := TVM (vm).popArray;
  m1 := TVM (vm).popArray;

 TVM (vm).push(TArrayObject.isEqualTo(m1, m2));
end;

end.

