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
     procedure   getMax (vm : TObject);
     procedure   getMin (vm : TObject);
     constructor Create;
     destructor  Destroy; override;
  end;


implementation

Uses Math, uSymbolTable, uVM, uStringObject, uListObject, uMachineStack,
     uMemoryManager, uVMExceptions;


constructor TBuiltInList.Create;
begin
  inherited Create ('lists', 'List Module');

  addMethod(getLength,         1, 'len', 'Return the length of a list: lists.len (mylist)');
  addMethod(remove,            2, 'remove', 'Remove an element from a list with given index: lists.remove (mylist, 4)');
  addMethod(removeLastElement, 1, 'pop', 'Remove the last element from a list: lists.pop (list)');
  addMethod(insert,            3, 'insert', 'Insert a new element after index: lists.insert (a, 3.14, 1)');
  addMethod(append,            2, 'append', 'Append the element to the list: lists.append (a, 3.14)');
  addMethod(getMax,            1, 'max', 'Find the maximum value is a 1D list of values: lists.max ({1,2,3})');
  addMethod(getMin,            1, 'min', 'Find the minimum value is a 1D list of values: lists.min ({1,2,3})');
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
   // We don't push None as with the other methods because the above
   // pushes a value on to the return stack
end;


procedure TBuiltInList.insert (vm : TObject);
var s : TListObject;
    index : integer;
    value : PMachineStackRecord;
begin
   index := TVM (vm).popInteger;
   value := TVM (vm).pop;
   s := TVM (vm).popList;

   case value.stackType of
      stInteger :    s.insert (index, value.iValue);
      stDouble  :    s.insert (index, value.dValue);
      stBoolean :    s.insert (index, value.bValue);
      stString  :    s.insert (index, value.sValue);
      stList    :    s.insert (index, value.lValue);
      stFunction:    s.insertUserFunction (index, value.fValue);
      stModule  :    s.insertModule (index, value.module)
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

end.

