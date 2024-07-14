unit uBuiltInList;

{
  Unit:    uBuiltInList.pas
  Author:  Herbert M sauro
  Date:    10/2021
  Purpose: This file implements the List library for the Rhodus interpreter.

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
     procedure   getRange (vm : TObject);
     procedure   getRndu (vm : TObject);
     procedure   getRndi (vm : TObject);
     constructor Create;
     destructor  Destroy; override;
  end;


implementation

Uses Math,
     uVM,
     uSymbolTable,
     uStringObject,
     uListObject,
     uMachineStack,
     uMemoryManager,
     uVMExceptions,
     uHelpUnit,
     uRhodusTypes;


constructor TBuiltInList.Create;
begin
  inherited Create ('lists');

  addMethod(getRange,    3, 'range'); // l = lists.range (0, 10, 2)');
  addMethod(getRndu,     1, 'rndu'); // l = lists.rndu (10)');
  addMethod(getRndi,     3, 'rndi'); // l = lists.rndi (lower, upper, number)');
end;


destructor TBuiltInList.Destroy;
begin
  inherited;
end;


procedure TBuiltInList.getRange (vm : TObject);
var start, finish, step : double;
    n, i : integer;
    alist : TListObject;
begin
  step := TVM(vm).popScalar;
  if step < 0 then
     TVM (vm).raiseError('Step size must be positive in range function');

  finish := TVM(vm).popScalar;
  start := TVM(vm).popScalar;
  n := Trunc((finish - start) / step) + 1;

  alist := TListObject.Create(n);
  i := 0;
  while start <= finish do
  begin
    alist.list[i].setDouble(start);
    start := start + step;
    Inc(i);
  end;

 TVM (vm).push(alist);
end;


procedure TBuiltInList.getRndu (vm : TObject);
var n : integer;
    l : TListObject;
    i : integer;
begin
  n := TVM (vm).popInteger;
  if n <= 0 then
     TVM (vm).raiseError('argument to randu cannot be zero or negative');

  l := TListObject.Create (n);
  for i := 0 to n - 1 do
      begin
      l.list[i].dValue := random();
      l.list[i].itemType := symDouble;
      end;
  TVM (vm).push (l);
end;


procedure TBuiltInList.getRndi (vm : TObject);
var n, start, finish : integer;
    l : TListObject;
    i : integer;
begin
  n := TVM (vm).popInteger;
  if n <= 0 then
     TVM (vm).raiseError('The number of elements to rndi cannot be zero or negative');

  finish := TVM (vm).popInteger;
  start := TVM (vm).popInteger;
  l := TListObject.Create (n);
  for i := 0 to n - 1 do
      begin
      l.list[i].iValue := RandomRange(start, finish+1);// + start; // +1 to include end of range
      l.list[i].itemType := symInteger;
      end;
  TVM (vm).push (l);
end;

end.

