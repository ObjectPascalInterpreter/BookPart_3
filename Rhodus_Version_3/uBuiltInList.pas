unit uBuiltInList;

{
  Unit:    uBuiltInList.pas
  Author:  Herbert M sauro
  Date:    10/2021
  Purpose: This file implements the List type for the Rhodus interpreter.

  Ths source is distributed under Apache 2.0
  See https://www.apache.org/licenses/LICENSE-2.0.txt for further information

  Copyright (C) 2019-2024 Herbert M Sauro

  Author Contact Information:
  email: hsauro@gmail.com
}

interface

Uses SysUtils, Classes, uLibModule;

type
  TBuiltInList = class (TModuleLib)

     procedure   range (vm : TObject);
     procedure   getRndu (vm : TObject);
     procedure   getRndi (vm : TObject);
     constructor Create;
     destructor  Destroy; override;
  end;


implementation

Uses Math,
     uSymbolTable,
     uVM,
     uStringObject,
     uListObject,
     uMachineStack,
     uMemoryManager,
     uVMExceptions,
     uRhodusTypes;


constructor TBuiltInList.Create;
begin
  inherited Create ('lists', 'List Module');

  addMethod(range,       3, 'range', 'Create a list based on the range arguments: l = lists.range (0, 10, 2)');
  addMethod(getRndu,     1, 'rndu',  'Create a list if uniformly random numbers: l = lists.rndu (10)');
  addMethod(getRndi,     3, 'rndi',  'Create a list if uniformly random integer: l = lists.rndi (lower, upper, number)');
end;


destructor TBuiltInList.Destroy;
begin
  inherited;
end;


procedure TBuiltInList.range (vm : TObject);
var start, finish, step : double;
    n : integer;
    alist : TListObject;
begin
 step := TVM(vm).popScalar;
 finish := TVM(vm).popScalar;
 start := TVM(vm).popScalar;
 n := trunc ((finish - start)/step);
 alist := TListObject.Create(n);
 for var i := 0 to n - 1 do
     begin
     alist.list[i].dValue := start;
     alist.list[i].itemType := liDouble;
     start := start + step
     end;
 TVM (vm).push(alist);
end;


procedure TBuiltInList.getRndu (vm : TObject);
var n : integer;
    l : TListObject;
    i : integer;
begin
  n := TVM (vm).popInteger;
  l := TListObject.Create (n);
  for i := 0 to n - 1 do
      begin
      l.list[i].dValue := random();
      l.list[i].itemType := liDouble;
      end;
  TVM (vm).push (l);
end;


procedure TBuiltInList.getRndi (vm : TObject);
var n, start, finish : integer;
    l : TListObject;
    i : integer;
begin
  n := TVM (vm).popInteger;
  finish := TVM (vm).popInteger;
  start := TVM (vm).popInteger;
  l := TListObject.Create (n);
  for i := 0 to n - 1 do
      begin
      l.list[i].iValue := RandomRange(start, finish+1);// + start; // +1 to include end of range
      l.list[i].itemType := liInteger;
      end;
  TVM (vm).push (l);
end;

end.

