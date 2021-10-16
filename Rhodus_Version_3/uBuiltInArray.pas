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
     procedure   getIdent (vm : TObject);
     //procedure   getRndi (vm : TObject);
     constructor Create;
     destructor  Destroy; override;
  end;


implementation

Uses Math, uSymbolTable, uVM, uStringObject, uArrayObject, uMachineStack,
     uMemoryManager, uVMExceptions;


constructor TBuiltInArray.Create;
begin
  inherited Create ('arrays', 'Array Module');

  addMethod(getIdent,          1, 'ident', 'Create an identity matrix for a 2D array: m = arrays.ident (4)');
  addMethod(getRndu,          -1, 'rndu', 'Create an array of uniformly random numbers: l = arrays.rndu (4,4)');
  //addMethod(getRndi,         3, 'rndi', 'Create a list if uniformly random integer: l = lists.rndi (lower, upper, number)');
end;


destructor TBuiltInArray.Destroy;
begin
  inherited;
end;


procedure TBuiltInArray.getRndu(vm: TObject);
var ar : TArrayObject;
    nArgs : integer;
    nRows, nCols, i, j : integer;
    idx : TIndexArray;
begin
   nArgs := TVM (vm).popInteger;

   setLength (idx, nArgs);
   for i := nArgs - 1 downto 0 do
       idx[i] := TVM (vm).popInteger;

   ar := TArrayObject.Create(idx);
   for i := 0 to length (ar.data) - 1 do
       ar.data[i] := random ();
   TVM (vm).push (ar);
end;

procedure TBuiltInArray.getIdent (vm : TObject);
var n : integer;
    ar : TArrayObject;
begin
  n := TVM (vm).popInteger;
  ar := TArrayObject.Create([n,n]);
  for var i := 0 to n - 1 do
      ar.setValue2D (i, i, 1.0);
  TVM (vm).push (ar);
end;

//procedure TBuiltInList.getRndi (vm : TObject);
//var n, start, finish : integer;
//    l : TListObject;
//    i : integer;
//begin
//  n := TVM (vm).popInteger;
//  finish := TVM (vm).popInteger;
//  start := TVM (vm).popInteger;
//  l := TListObject.Create (n);
//  for i := 0 to n - 1 do
//      begin
//      l.list[i].iValue := RandomRange(start, finish+1);// + start; // +1 to include end of range
//      l.list[i].itemType := liInteger;
//      end;
//  TVM (vm).push (l);
//end;

end.

