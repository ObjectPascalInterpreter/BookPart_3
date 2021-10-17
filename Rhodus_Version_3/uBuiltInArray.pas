unit uBuiltInArray;


// This source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

Uses SysUtils, Classes, uLibModule;

type
  TBuiltInArray = class (TModuleLib)
     procedure   getRndu (vm : TObject);
     procedure   getRndi (vm : TObject);
     procedure   isEqual (vm : TObject);
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
     uVMExceptions;


constructor TBuiltInArray.Create;
begin
  inherited Create ('arrays', 'Array Module, deals with n-dimensional arrays');

  addMethod(getRndu,     1, 'rand',  'Create an array of uniformly random numbers: ar = arrays.rand ([4,4,2])');
  addMethod(getRndi,     3, 'randi', 'Create a list of uniformly random integers: ar = arrays.randi (lower, upper, [3,3])');
  addMethod(isEqual,     2, 'equal', 'Return true if the two arrays are equal: arrays.equal (m1,m2)');
  end;


destructor TBuiltInArray.Destroy;
begin
  inherited;
end;


procedure TBuiltInArray.getRndu(vm: TObject);
var ar : TArrayObject;
    al : TListObject;
    nArgs : integer;
    i : integer;
    idx : TIndexArray;
begin
   al := TVM (vm).popList;

   setLength (idx, al.list.count);
   for i := 0 to al.list.Count - 1 do
       idx[i] := al.list[i].iValue;

   ar := TArrayObject.Create(idx);
   for i := 0 to length (ar.data) - 1 do
       ar.data[i] := random ();
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
  al := TVM (vm).popList;
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
      ar.data[i] := RandomRange(lower, upper);
      end;

  TVM (vm).push (ar);
end;


procedure TBuiltInArray.isEqual (vm : TObject);
var m1, m2 : TArrayObject;
begin
  m2 := TVM (vm).popArray;
  m1 := TVM (vm).popArray;

 TVM (vm).push(TArrayObject.isEqualTo(m1, m2));
end;

end.

