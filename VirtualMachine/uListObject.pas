// Developed using Delphi for Windows and Mac platforms.

// This source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

unit uListObject;

interface

Uses System.SysUtils, uUtils, System.generics.Collections, uObjectSupport,
     uMemoryManager, uStringObject, uArrayObject, uRhodusObject;

type
  TListItemType = (liInteger, liBoolean, liDouble, liString, liList, liArray, liFunction, liModule);
  TListItem = class;

  TListMethods = class (TMethodsBase)
      procedure getLength (vm : TObject);
      procedure append (vm : TObject);
      procedure remove (vm : TObject);
      procedure getSum (vm : TObject);
      procedure insert (vm : TObject);
      procedure removeLastElement (vm : TObject);
      procedure getMax (vm : TObject);
      procedure getMin (vm : TObject);
      procedure getDims (vm : TObject);
      constructor Create;
      destructor  Destroy; override;
  end;

  TListContainer = class(TList<TListItem>)
    destructor Destroy; override;
  end;

  TListObject = class(TRhodusObject)
  private
  public
    list: TListContainer;          // Contains the data

    class function addLists(list1, list2: TListObject): TListObject;
    class function multiply(value: integer; aList: TListObject): TListObject;
    class function listEquals(list1, list2: TListObject): boolean;

    procedure append(iValue: integer); overload;
    procedure append(bValue: boolean); overload;
    procedure append(dValue: double); overload;
    procedure append(sValue: TStringObject); overload;
    procedure append(lValue: TListObject); overload;
    procedure append(aValue: TArrayObject); overload;
    procedure appendUserFunction(fValue: TObject);
    procedure appendModule(mValue: TObject);

    procedure remove(index: integer);
    procedure insert(index, iValue: integer); overload;
    procedure insert(index: integer; dValue: double); overload;
    procedure insert(index: integer; bValue: boolean); overload;
    procedure insert(index: integer; lValue: TListObject); overload;
    procedure insert(index: integer; sValue: TStringObject); overload;
    procedure insertUserFunction(index: integer; fValue: TObject);
    procedure insertModule(index: integer; mValue: TObject);

    function slice (lower, upper : integer) : TListObject;

    class function sliceList2 (alist : TListObject; lower, upper : integer) : TListObject;

    function clone: TListObject;
    function listToString: string;
    function getsize: integer;
    constructor Create(count: integer);
    destructor  Destroy; override;
  end;

  TListItem = class(TObject)
    itemType: TListItemType;
    iValue: integer;
    bValue: boolean;
    dValue: double;
    sValue: TStringObject;
    lValue: TListObject;
    aValue: TArrayObject;
    fValue: TObject; // User function
    mValue: TObject; // Module
    function elementToString: string;
    class function listEquals(list1: TListItem; list2: TListItem): boolean;
    function getsize(): integer;
    function getScalar : double;

    procedure setInteger (value : integer);
    function  clone : TListItem;

    constructor Create(iValue: integer); overload;
    constructor Create(bValue: boolean); overload;
    constructor Create(dValue: double); overload;
    constructor Create(sValue: TStringObject); overload;
    constructor Create(lValue: TListObject); overload;
    constructor Create(aValue: TArrayObject); overload;
    constructor CreateUserFunction(fValue: TObject);
    constructor CreateModule(mValue: TObject);

    constructor Create(item: TListItem); overload;
    destructor  Destroy; override;
  end;


implementation

Uses Math,
     Rtti,
     uVMExceptions,
     uSymbolTable,
     uRhodusTypes,
     uMachineStack,
     uVM;

type
  TIntArray = array of integer;
  TDoubleArray = array of double;

var listMethods : TListMethods;


constructor TListMethods.Create;
begin
  methodList := TMethodList.Create;

  methodList.Add(TMethodDetails.Create ('len',    0, 'Return the length of a list: var.len (mylist)', getLength));
  methodList.Add(TMethodDetails.Create ('append', 1, 'Append the element to the list: var.append (a, 3.14)', append));
  methodList.Add(TMethodDetails.Create ('remove', 1, 'Remove an element from a list with given index: var.remove (mylist, 4)', remove));
  methodList.Add(TMethodDetails.Create ('sum',    0, 'Find the sum of values in a list. var.sum ()', getSum));
  methodList.Add(TMethodDetails.Create ('pop',    1, 'Remove the last element from a list: var.pop (list)', removeLastElement));
  methodList.Add(TMethodDetails.Create ('max',    1, 'Find the maximum value is a 1D list of values: var.max ({1,2,3})', getMin));
  methodList.Add(TMethodDetails.Create ('min',    1, 'Find the minimum value is a 1D list of values: var.min ({1,2,3})', getMin));
  methodList.Add(TMethodDetails.Create ('dims',   0, 'Get dims: var.min ({1,2,3})', getDims));

  methodList.Add(TMethodDetails.Create ('dir',    0, 'dir of string object methods', dir));
end;


destructor TListMethods.Destroy;
begin
  inherited;
end;


// Object method for the list object type

// In all these methods the order of the stack from the top is:
//  Arguments
//  Object Method
//  Object

procedure TListMethods.getLength (vm : TObject);
var alist : TListObject;
begin
  // No arguments for this method
  TVM (vm).decStackTop; // Dump the object method
  alist := TVM (vm).popList;
  TVM (vm).push (alist.list.Count);
end;


procedure TListMethods.append (vm : TObject);
var s : TListObject;
    value : PMachineStackRecord;
    ts : TStringObject;
    ls : TListObject;
    ar : TArrayObject;
    fs : TUserFunction;
begin
  value := TVM (vm).pop;
  TVM (vm).decStackTop; // Dump the object method
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
      stArray   :    begin
                     ar := value.aValue.clone;
                     ar.blockType := btOwned;
                     s.append (ar);
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


// Remove an element from a list with a given index
procedure TListMethods.remove (vm : TObject);
var s : TListObject;
    index : integer;
begin
  index := TVM (vm).popInteger;
  TVM (vm).decStackTop; // Dump the object method
  s := TVM (vm).popList;
  s.remove (index);
  TVM (vm).pushNone;
end;


procedure TListMethods.getSum (vm : TObject);
var s : TListObject;
    sum : double;
begin
   // No argument to pop off
   TVM (vm).decStackTop; // Dump the object method
   s := TVM (vm).popList;
   sum := 0;
   for var i := 0 to s.list.Count - 1 do
       case s.list[i].itemType of
          liInteger :  sum := sum + s.list[i].iValue;
          liDouble  :  sum := sum + s.list[i].dValue;
       else
          raise ERuntimeException.Create('You cannot sum non-numerical values in a list');
       end;
   TVM (vm).push(sum);
end;


procedure TListMethods.removeLastElement (vm : TObject);
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


procedure TListMethods.insert (vm : TObject);
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


procedure TListMethods.getMax (vm : TObject);
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


procedure TListMethods.getMin (vm : TObject);
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


procedure countItems (s : TListObject; dims : TIntArray; level : integer);
var i : integer;
begin
  for i := 0 to s.list.Count - 1 do
      begin
      if s.list[i].itemType = liInteger then
         inc (dims[level])
      else
         countItems (s.list[i].lValue, dims, level+1);
      writeln (TRttiEnumerationType.GetName(s.list[i].itemType));
      end;
end;


procedure collectData (s : TListObject; data : TDoubleArray; var count : integer);
var i : integer;
begin
  for i := 0 to s.list.Count - 1 do
      begin
      if s.list[i].itemType = liInteger then
         begin
         data[count] := s.list[i].iValue;
         inc (count);
         end;
      if s.list[i].itemType = liList then
         collectData (s.list[i].lValue, data, count);
      end;
end;


procedure TListMethods.getDims (vm : TObject);
var s, r : TListObject;
    i : integer;
    dims: TIntArray;
    data : TDoubleArray;
    count : integer;
begin
  TVM (vm).decStackTop; // Dump the object method
  s := TVM (vm).popList;
  setlength (dims, 10);
  setLength (data, 16);
  r := s;
  count := 0;
  while r.list.Count > 0 do
      begin
      r := r.list[0].lValue;
      inc (count);
      end;
  dims[0] := s.list.Count;
  dims[1] := s.list[0].lValue.list.Count;

  count := 0;
  collectData (s, data, count);
  //level := 0;
  //countItems (s, dims, level);
  for i := 0 to 10 do
      write (data[i]:3:1, ' ');
  writeln;
  TVM (vm).push (double(2.6));
end;



// ---------------------------------------------------------------------------------------------------

destructor TListContainer.Destroy;
begin
  inherited;
end;


constructor TListObject.Create(count: integer);
var
  i: integer;
begin
  inherited Create;

  objectType := symList;
  list := TListContainer.Create;
  methods := listMethods;
  for i := 0 to count - 1 do
    list.add(TListItem.Create(0));
  memoryList.addNode(self); // Add a reference to the memory manager
end;


destructor TListObject.Destroy;
var
  i: integer;
begin
  for i := list.count - 1 downto 0 do
    begin
    list[i].free;
    end;
  list.free;
  inherited;
end;


function TListObject.clone: TListObject;
var
  i: integer;
begin
  result := TListObject.Create(self.list.count);
  for i := 0 to self.list.count - 1 do
    case self.list[i].itemType of
      liInteger:
        begin
          result.list[i].itemType := liInteger;
          result.list[i].iValue := self.list[i].iValue;
        end;
      liBoolean:
        begin
          result.list[i].itemType := liBoolean;
          result.list[i].bValue := self.list[i].bValue;
        end;
      liDouble:
        begin
          result.list[i].itemType := liDouble;
          result.list[i].dValue := self.list[i].dValue;
        end;
      liString:
        begin
          result.list[i].itemType := liString;
          result.list[i].sValue := self.list[i].sValue.clone;
          result.list[i].sValue.blockType := btOwned;
        end;
      liList:
        begin
          result.list[i].itemType := liList;
          result.list[i].lValue := self.list[i].lValue.clone;
          result.list[i].lValue.blockType := btOwned;
        end;
      liFunction:
        begin
          result.list[i].itemType := liFunction;
          result.list[i].fValue := (self.list[i].fValue as TUserFunction).clone;
          (result.list[i].fValue as TUserFunction).blockType := btOwned;
        end;
      liModule:
        begin
          raise ERuntimeException.Create('Copying modules is not permitted');
          // We don't clone modules
        end;
    end;
end;

function TListObject.listToString: string;
var
  i: integer;
begin
  result := '[';
  if self.list.count > 0 then
    result := result + self.list[0].elementToString;

  for i := 1 to self.list.count - 1 do
    begin
      result := result + ',' + self.list[i].elementToString;
    end;
  result := result + ']';
end;

function TListObject.getsize: integer;
var
  i: integer;
begin
  result := getRhodusObjectSize;
  result := result + self.InstanceSize;
  for i := 0 to self.list.count - 1 do
    result := result + self.list[i].getsize();
end;

procedure TListObject.remove(index: integer);
begin
  if (index < 0) or (index > self.list.count - 1) then
    raise ERuntimeException.Create ('out of range while indexing element in list');
  self.list.Delete(index);
end;

procedure TListObject.insert(index, iValue: integer);
begin
  list.insert(index, TListItem.Create(iValue));
end;

procedure TListObject.insert(index: integer; dValue: double);
begin
  list.insert(index, TListItem.Create(dValue));
end;

procedure TListObject.insert(index: integer; bValue: boolean);
begin
  list.insert(index, TListItem.Create(bValue));
end;

procedure TListObject.insert(index: integer; lValue: TListObject);
begin
  list.insert(index, TListItem.Create(lValue));
end;

procedure TListObject.insert(index: integer; sValue: TStringObject);
begin
  list.insert(index, TListItem.Create(sValue));
end;

procedure TListObject.insertUserFunction(index: integer; fValue: TObject);
begin
  list.insert(index, TListItem.CreateUserFunction(fValue));
end;

procedure TListObject.insertModule(index: integer; mValue: TObject);
begin
  list.insert(index, TListItem.CreateModule(mValue));
end;


function TListObject.slice (lower, upper : integer) : TListObject;
var i : integer;
    obj : TListObject;
begin
  // -1 means slice all
  if lower = -1 then
     lower := 0;
  if upper = -1 then
     upper := list.Count;

  if upper < lower then
     obj := TListObject.Create(0)
  else
     begin
     if upper >= list.Count then
        upper := list.Count - 1;

     obj := TListObject.Create (0);
     for i := lower to upper do
         begin
         case list[i].itemType of
            liInteger : obj.append(list[i].iValue);
            liBoolean : obj.append(list[i].bValue);
            liDouble  : obj.append(list[i].dValue);
            liString  : obj.append(list[i].sValue.clone);
            liList    : obj.append(list[i].lValue.clone);
            liArray   : obj.append(list[i].aValue.clone);
            liFunction: obj.appendUserFunction(TUserFunction (list[i].fValue).clone);
            liModule  : obj.appendModule(TModule (list[i].mValue));
         end;
         end;
     end;
  result := obj;
end;

// ---------------------------------------------------------------
// Class methods


class function TListObject.sliceList2 (alist : TListObject; lower, upper : integer) : TListObject;
var i : integer;
    obj : TListObject;
begin
  // -1 means slice all
  if lower = -1 then
     lower := 0;
  if upper = -1 then
     upper := alist.list.Count;

  if upper < lower then
     obj := TListObject.Create(0)
  else
     begin
     if upper >= alist.list.Count then
        upper := alist.list.Count - 1;

     obj := TListObject.Create (0);
     for i := lower to upper do
         begin
         case alist.list[i].itemType of
            liInteger : obj.append(alist.list[i].iValue);
            liBoolean : obj.append(alist.list[i].bValue);
            liDouble  : obj.append(alist.list[i].dValue);
            liString  : obj.append(alist.list[i].sValue.clone);
            liList    : obj.append(alist.list[i].lValue.clone);
            liArray   : obj.append(alist.list[i].aValue.clone);
            liFunction: obj.appendUserFunction(TUserFunction (alist.list[i].fValue).clone);
            liModule  : obj.appendModule(TModule (alist.list[i].mValue));
         end;
         end;
     end;
  result := obj;
end;


class function TListObject.addLists(list1, list2: TListObject): TListObject;
var
  i: integer;
begin
  if list1.blockType = btGarbage then
    result := list1
  else
    result := list1.clone;

  for i := 0 to list2.list.count - 1 do
    result.list.add(TListItem.Create(list2.list[i]));
end;

// Make value copies of aList and combine them into one list
// eg 3*{1} = {1,1,1}
class function TListObject.multiply(value: integer; aList: TListObject)
  : TListObject;
var
  i, j: integer;
  nContents: integer;
  workingCopy: TListObject;
  item: TListItem;
begin
  if aList.isBound then
    workingCopy := aList.clone
  else
    workingCopy := aList;

  nContents := aList.list.count;
  result := TListObject.Create(0);
  for i := 0 to value - 1 do
    if nContents = 0 then
      result.list.add(TListItem.Create(0))
    else
      for j := 0 to nContents - 1 do
        begin
          item := TListItem.Create(workingCopy.list[j]);
          result.list.add(item);
        end;
end;

class function TListObject.listEquals(list1, list2: TListObject): boolean;
var
  i: integer;
begin
  if list1.list.count <> list2.list.count then
    exit(False);

  result := True;
  for i := 0 to list1.list.count - 1 do
    result := result and TListItem.listEquals(list1.list[i], list2.list[i]);
end;

procedure TListObject.append(iValue: integer);
begin
  list.add(TListItem.Create(iValue));
end;

procedure TListObject.append(bValue: boolean);
begin
  list.add(TListItem.Create(bValue));
end;

procedure TListObject.append(dValue: double);
begin
  list.add(TListItem.Create(dValue));
end;

procedure TListObject.append(sValue: TStringObject);
begin
  if sValue.blockType = btGarbage then
    sValue.blockType := btOwned;

  list.add(TListItem.Create(sValue));
end;

procedure TListObject.append(aValue: TArrayObject);
begin
  if aValue.blockType = btGarbage then
    aValue.blockType := btOwned;

  list.add(TListItem.Create(aValue));
end;

procedure TListObject.append(lValue: TListObject);
begin
  if lValue.blockType = btGarbage then
    lValue.blockType := btOwned;

  list.add(TListItem.Create(lValue));
end;

procedure TListObject.appendUserFunction(fValue: TObject);
begin
  if (fValue as TUserFunction).blockType = btBound then
    (fValue as TUserFunction).blockType := btOwned;

  list.add(TListItem.CreateUserFunction(fValue));
end;

procedure TListObject.appendModule(mValue: TObject);
begin
  list.add(TListItem.CreateModule(mValue));
end;


// ----------------------------------------------------------------------

constructor TListItem.Create(item: TListItem);
begin
  inherited Create;
  itemType := item.itemType;
  case itemType of
    liInteger:
      self.iValue := item.iValue;
    liDouble:
      self.dValue := item.dValue;
    liBoolean:
      self.bValue := item.bValue;
    liString:
      begin
        self.sValue := item.sValue.clone;
        self.sValue.blockType := btOwned;
      end;
    liList:
      begin
        self.lValue := item.lValue.clone;
        self.lValue.blockType := btOwned;
      end;
    liFunction:
      begin
        self.fValue := item.fValue
      end;
    liModule:
      begin
        self.mValue := item.mValue
      end;
  else
    ERuntimeException.Create
      ('Internal Error: unknown itemType in ListItem.Create');
  end;
end;

constructor TListItem.Create(iValue: integer);
begin
  itemType := liInteger;
  self.iValue := iValue;
end;

constructor TListItem.Create(bValue: boolean);
begin
  itemType := liBoolean;
  self.bValue := bValue;
end;

constructor TListItem.Create(dValue: double);
begin
  itemType := liDouble;
  self.dValue := dValue;
end;

constructor TListItem.Create(sValue: TStringObject);
begin
  itemType := liString;
  self.sValue := sValue;
end;

constructor TListItem.Create(lValue: TListObject);
begin
  itemType := liList;
  self.lValue := lValue;
end;

constructor TListItem.Create(aValue: TArrayObject);
begin
  itemType := liArray;
  self.aValue := aValue;
end;

constructor TListItem.CreateUserFunction(fValue: TObject);
begin
  itemType := liFunction;
  self.fValue := fValue;
end;

constructor TListItem.CreateModule(mValue: TObject);
begin
  itemType := liModule;
  self.mValue := mValue;
end;

destructor TListItem.Destroy;
begin
  if itemType = liList then
    begin
      if lValue.isOwned then
        lValue.blockType := btGarbage
      else
        lValue.free;
    end;
  if itemType = liString then
    begin
      if sValue.isOwned then
        sValue.blockType := btGarbage
      else
        sValue.free;
    end;
  if itemType = liFunction then
    begin
      if (fValue as TUserFunction).blockType = btOwned then
        (fValue as TUserFunction).blockType := btGarbage
      else
        fValue.free;
    end;
  inherited;
end;


function TListItem.clone : TListItem;
begin
  case itemType of
    liInteger: begin result := TListItem.Create(iValue); end;
    liBoolean: result := TListItem.Create(bValue);
    liDouble: result := TListItem.Create(dValue);
    liString: result := TListItem.Create(sValue.clone);
    liList: begin result := TListItem.Create(lValue.clone); result.lValue.blockType := btOwned; end;
    liArray: result := TListItem.Create(aValue.clone);
    liFunction: result := TListItem.CreateUserFunction(TUSerFunction (fValue).clone);
    liModule: raise ERuntimeException.Create('can''t clone module');
  else
    raise ERuntimeException.Create('Internal Error in ListItem Clone');
  end;
end;


function TListItem.getsize(): integer;
begin
  result := sizeof(TObject);
  result := result + sizeof(self.itemType);
  case itemType of
    liInteger:
      result := result + sizeof(integer);
    liBoolean:
      result := result + sizeof(boolean);
    liDouble:
      result := result + sizeof(double);
    liString:
      result := result + sValue.getsize();
    liList:
      result := result + lValue.getsize();
  end;
end;


function TListItem.getScalar : double;
begin
  if itemType = liInteger then
     exit (iValue);
  if itemType = liDouble then
     exit (dValue);
  exit (0);
end;


procedure TListItem.setInteger (value : integer);
begin
  iValue := value;
  itemType := liInteger;
end;


function boolToString(value: boolean): string;
begin
  if value then
    result := 'True'
  else
    result := 'False';
end;

function TListItem.elementToString: string;
begin
  result := '';
  case self.itemType of
    liInteger:
      result := result + inttostr(self.iValue);
    liBoolean:
      result := result + boolToString(self.bValue);
    liDouble:
      result := result + floattostr(self.dValue);
    liString:
      result := result + '"' + self.sValue.value + '"';
    liList:
      result := result + self.lValue.listToString;
    liFunction:
      result := result + (self.fValue as TUserFunction).name;
    liModule:
      result := result + TModule(self.mValue).name;
  else
    raise ERuntimeException.Create('Unknown type in elementToString');
  end;
end;

class function TListItem.listEquals(list1: TListItem; list2: TListItem)
  : boolean;
begin
  result := False;
  if (list1.itemType = liInteger) and (list2.itemType = liInteger) then
    if list1.iValue = list2.iValue then
      exit(True)
    else
      exit(False);

  if (list1.itemType = liBoolean) and (list2.itemType = liBoolean) then
    if list1.bValue = list2.bValue then
      exit(True)
    else
      exit(False);

  if (list1.itemType = liDouble) and (list2.itemType = liDouble) then
    if list1.dValue = list2.dValue then
      exit(True)
    else
      exit(False);

  if (list1.itemType = liString) and (list2.itemType = liString) then
    if list1.sValue.isEqualTo(list2.sValue) then
      exit(True)
    else
      exit(False);

  if (list1.itemType = liList) and (list2.itemType = liList) then
    exit(TListObject.listEquals(list1.lValue, list2.lValue))
  else
    exit(False);
end;


// -----------------------------------------------------------------------

initialization
   listMethods := TListMethods.Create;
finalization
   listMethods.Free;
end.

