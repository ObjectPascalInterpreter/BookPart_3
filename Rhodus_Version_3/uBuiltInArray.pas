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
begin
   nArgs := TVM (vm).popInteger;
   if nArgs = 1 then
      begin
      nRows := TVM (vm).popInteger;
      ar := TArrayObject.Create([nRows]);
      for i := 0 to nRows - 1 do
          ar.setValue2D(0, i, random());
      TVM (vm).push (ar);
      exit;
      end;
   if nArgs = 2 then
      begin
      nRows := TVM (vm).popInteger;
      nCols := TVM (vm).popInteger;
      ar := TArrayObject.Create([nRows, nCols]);
      for i := 0 to nRows - 1 do
          for j := 0 to nCols - 1 do
              ar.setValue2D(i,j, random());
      TVM (vm).push (ar);
      end;
end;

//
//procedure TBuiltInList.getRndu (vm : TObject);
//var n, range : integer;
//    l : TListObject;
//    i : integer;
//begin
//  n := TVM (vm).popInteger;
//  l := TListObject.Create (n);
//  for i := 0 to n - 1 do
//      begin
//      l.list[i].dValue := random();
//      l.list[i].itemType := liDouble;
//      end;
//  TVM (vm).push (l);
//end;


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

