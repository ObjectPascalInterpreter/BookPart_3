unit uBuiltInList;


// This source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

Uses SysUtils, Classes, uLibModule;

type
  TBuiltInList = class (TModuleLib)

     procedure   getLength (vm : TObject);
     procedure   remove (vm : TObject);
     procedure   removeLastElement (vm : TObject);
     procedure   insert (vm : TObject);
     procedure   append (vm : TObject);
     procedure   range (vm : TObject);
     procedure   getRndu (vm : TObject);
     procedure   getRndi (vm : TObject);
     procedure   getSum (vm : TObject);
     procedure   getMax (vm : TObject);
     procedure   getMin (vm : TObject);
     constructor Create;
     destructor  Destroy; override;
  end;


implementation

Uses Math, uSymbolTable, uVM, uStringObject, uListObject, uMachineStack,
     uMemoryManager, uVMExceptions, uRhodusTypes;


constructor TBuiltInList.Create;
begin
  inherited Create ('lists', 'List Module');

  addMethod(range,             3, 'range', 'Create a list based on the range arguments: l = lists.range (0, 10, 2)');
  addMethod(getRndu,           1, 'rndu', 'Create a list if uniformly random numbers: l = lists.rndu (10)');
  addMethod(getRndi,           3, 'rndi', 'Create a list if uniformly random integer: l = lists.rndi (lower, upper, number)');
end;


destructor TBuiltInList.Destroy;
begin
  inherited;
end;

// Remove an element from a list with a given index
procedure TBuiltInList.remove (vm : TObject);
var s : TListObject;
    index : integer;
begin
  index := TVM (vm).popInteger;
  s := TVM (vm).popList;
  s.remove (index);
  TVM (vm).pushNone;
end;


procedure TBuiltInList.getLength (vm : TObject);
var s : TListObject;
begin
   s := TVM (vm).popList;
   TVM (vm).push(s.list.Count);
end;


procedure TBuiltInList.getSum (vm : TObject);
var s : TListObject;
    sum : double;
begin
   s := TVM (vm).popList;
   sum := 0;
   for var i := 0 to s.list.Count - 1 do
       case s.list[i].itemType of
          liInteger :  sum := sum + s.list[i].iValue;
          liDouble  :  sum := sum + s.list[i].dValue;
       else
          raise ERuntimeException.Create('You can not sum non-numerical values in a list');
       end;
   TVM (vm).push(sum);
end;


procedure TBuiltInList.removeLastElement (vm : TObject);
var s : TListObject;
     r : TListItem;
begin
   s := TVM (vm).popList;
   if s.list.count = 0 then
      begin
      TVM (vm).push  (s);
      exit;
      end;

   r := s.list [s.list.Count - 1];
   case r.itemType of
      liInteger  : TVM (vm).push (r.iValue);
      liDouble   : TVM (vm).push (r.dValue);
      liBoolean  : TVM (vm).push (r.bValue);
      liString   : TVM (vm).push (r.sValue.clone);
      liList     : TVM (vm).push (r.lValue.clone);
      liFunction : TVM (vm).push (TUserFunction (r.fValue));
      liModule   : TVM (vm).pushModule (TModule (r.mValue));
   end;
   s.remove (s.list.Count - 1);
   // We don't push None as with the other methods because
   // the above pushes a value on to the return stack
end;


procedure TBuiltInList.insert (vm : TObject);
var s : TListObject;
    index : integer;
    value : PMachineStackRecord;
begin
   index := TVM (vm).popInteger;
   value := TVM (vm).pop;
   s := TVM (vm).popList;
   if (index < 0) or (index > s.list.Count) then
      raise ERuntimeException.Create('List index out of range');

   case value.stackType of
      stInteger :    s.insert (index, value.iValue);
      stDouble  :    s.insert (index, value.dValue);
      stBoolean :    s.insert (index+1, value.bValue);
      stString  :    s.insert (index+1, value.sValue);
      stList    :    s.insert (index+1, value.lValue);
      stFunction:    s.insertUserFunction (index+1, value.fValue);
      stModule  :    s.insertModule (index+1, value.module)
   else
      raise ERuntimeException.Create('Internal error: unrecognized data type during list insert');
   end;
   TVM (vm).pushNone;
end;


procedure TBuiltInList.append (vm : TObject);
var s : TListObject;
    value : PMachineStackRecord;
    ts : TStringObject;
    ls : TListObject;
    fs : TUserFunction;
begin
   value := TVM (vm).pop;
   s := TVM (vm).popList;

   case value.stackType of
      stInteger :    s.append (value.iValue);
      stDouble  :    s.append (value.dValue);
      stBoolean :    s.append (value.bValue);
      stString  :    begin
                     ts := value.sValue.clone;
                     ts.blockType := btOwned;
                     s.append (ts);
                     end;
      stList    :    begin
                     ls := value.lValue.clone;
                     ls.blockType := btOwned;
                     s.append (ls);
                     end;
      stFunction:    begin
                     fs := value.fValue.clone;
                     fs.blockType := btOwned;
                     s.appendUserFunction (fs);
                     end;
      stModule  :    raise ERuntimeException.Create('Adding modules to a list is not permitted');
   else
      raise ERuntimeException.Create('Internal error: unrecognized data type during list insert');
   end;
   TVM (vm).pushNone;
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


procedure TBuiltInList.getMax (vm : TObject);
var s : TListObject;
    i : integer;
    value : double;
begin
  value := -1E10;
  s := TVM (vm).popList;
  for i := 0 to s.list.Count - 1 do
      case s.list[i].itemType of
         liInteger:
              begin
              if s.list[i].iValue > value then
                 value := s.list[i].iValue;
              end;
         liDouble :
              begin
              if s.list[i].dValue > value then
                 value := s.list[i].dValue;

              end
      else
        raise ERuntimeException.Create('Internal error: unrecognized data type during list insert');
      end;
   TVM (vm).push (double (value));
end;

procedure TBuiltInList.getMin (vm : TObject);
var s : TListObject;
    i : integer;
    value : double;
begin
  value := 1E10;
  s := TVM (vm).popList;
  for i := 0 to s.list.Count - 1 do
      case s.list[i].itemType of
         liInteger:
              begin
              if s.list[i].iValue < value then
                 value := s.list[i].iValue;
              end;
         liDouble :
              begin
              if s.list[i].dValue < value then
                 value := s.list[i].dValue;
              end
      else
        raise ERuntimeException.Create('Can only find the min value for numeric data');
      end;
   TVM (vm).push (double(value));
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

