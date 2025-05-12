// Developed using Delphi for Windows and Mac platforms.

// This source is distributed under Apache 2.0

// Copyright (C) 2019-2024 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

unit uListObject;

interface

Uses System.SysUtils, uUtils, System.generics.Collections,
     uDataObjectMethods,
     uMemoryManager,
     uRhodusTypes,
     uStringObject,
     uArrayObject,
     uMatrixObject,
     uValueObject,
     uDataObject;

type
  TListItemType = TElementType;

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
      constructor Create;
      destructor  Destroy; override;
  end;

  TListContainer = class(TList<TListItem>)
    destructor Destroy; override;
  end;

  TListObject = class(TDataObject)
  private
  public
    list: TListContainer;          // Contains the data

    class var listMethods : TListMethods;

    class function addLists(list1, list2: TListObject): TListObject;
    class function multiply(value: integer; aList: TListObject): TListObject;
    class function listEquals(list1, list2: TListObject): boolean;

    //procedure setItem (item : TListItem);
    function  getItem (index : integer) : TListItem;

    procedure setItemToDouble (index : integer; value : double);  // space must exist before you call this

    procedure append(iValue: integer); overload;
    procedure append(bValue: boolean); overload;
    procedure append(dValue: double); overload;
    procedure append(data : TDataObject); overload;
    procedure appendModule(mValue: TObject);

    procedure remove(index: integer);
    procedure insert(index, iValue: integer); overload;
    procedure insert(index: integer; dValue: double); overload;
    procedure insert(index: integer; bValue: boolean); overload;
    procedure insert(index: integer; obj: TDataObject);  overload;
    procedure insertModule(index: integer; mValue: TObject);
    function  findMin : double;
    function  findMax : double;

    function slice (lower, upper : integer) : TListObject;

    class function sliceList2 (alist : TListObject; lower, upper : integer) : TListObject;

    function clone: TDataObject; override;
    function ToString: string; override;
    function getSize: integer;  override;

    property Item[index : Integer]: TListItem read getItem; default; // write setItem; default;

    constructor Create; overload;
    constructor Create(count: integer); overload;
    destructor  Destroy; override;
  end;

  TListItem = class(TObject)
    itemType: TListItemType;
    iValue: integer;
    bValue: boolean;
    dValue: double;
    dataObject : TDataObject;
    moduleValue: TObject; // Module
    function elementToString: string;
    class function listEquals(list1: TListItem; list2: TListItem): boolean;
    function getsize(): integer;
    function getScalar : double;

    procedure setInteger (value : integer);
    procedure setDouble (value : double);

    function  clone : TListItem;

    constructor Create(iValue: integer); overload;
    constructor Create(bValue: boolean); overload;
    constructor Create(dValue: double); overload;
    constructor Create(obj: TDataObject);  overload;
    constructor CreateModule(mValue: TObject);

    constructor Create(item: TListItem); overload;
    destructor  Destroy; override;
  end;


implementation

Uses Math,
     Rtti,
     uVMExceptions,
     uSymbolTable,
     uMachineStack,
     uVM;

type
  TIntArray = array of integer;
  TDoubleArray = array of double;

constructor TListMethods.Create;
begin
  methodList := TMethodList.Create (self);

  methodList.Add(TMethodDetails.Create ('len',    0, 'Return the length of a list: var.len ()', getLength));
  methodList.Add(TMethodDetails.Create ('append', 1, 'Append the element to the list: var.append (a, 3.14)', append));
  methodList.Add(TMethodDetails.Create ('remove', 1, 'Remove an element from a list with given index (returns nothing): var.remove (4)', remove));
  methodList.Add(TMethodDetails.Create ('insert', 2, 'Insert an element into a list at a given index (returns nothing): var.insert (index, 5.4)', insert));
  methodList.Add(TMethodDetails.Create ('sum',    0, 'Find the sum of values in a list. var.sum ()', getSum));
  methodList.Add(TMethodDetails.Create ('pop',    0, 'Remove the last element from a list and return it: el = var.pop ()', removeLastElement));
  methodList.Add(TMethodDetails.Create ('max',    0, 'Find the maximum value is a 1D list of values: var.max ()', getMax));
  methodList.Add(TMethodDetails.Create ('min',    0, 'Find the minimum value is a 1D list of values: var.min ()', getMin));
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
var s : TListObject;
    md : TMethodDetails;
begin
  // No arguments for this method
  md := TVM (vm).popMethodDetails;
  s := TListObject (md.self);
  TVM (vm).push (s.list.Count);
end;


procedure TListMethods.append (vm : TObject);
var s : TListObject;
    value : PMachineStackRecord;
    md : TMethodDetails;
    data : TDataObject;
begin
  value := TVM (vm).pop;
  md := TVM (vm).popMethodDetails;
  s := TListObject (md.self);

   case value.stackType of
      symInteger :    s.append (value.iValue);
      symDouble  :    s.append (value.dValue);
      symBoolean :    s.append (value.bValue);
      symString,
      symList,
      symArray,
      symMatrix,
      symUserFunc:    begin
                      data := value.dataObject.clone;
                      data.blockType := btOwned;
                      s.append (data);
                      end;
      symModule  :    raise ERuntimeException.Create('Adding modules to a list is not permitted');
   else
      raise ERuntimeException.Create('Internal error: unrecognized data type during list insert');
   end;
   TVM (vm).pushNone;
end;


// Remove an element from a list with a given index, retun nothing
procedure TListMethods.remove (vm : TObject);
var s : TListObject;
    index : integer;
    md : TMethodDetails;
begin
  index := TVM (vm).popInteger;
  md := TVM (vm).popMethodDetails;
  s := TListObject (md.self);
  s.remove (index);
  TVM (vm).pushNone;
end;


procedure TListMethods.getSum (vm : TObject);
var s : TListObject;
    sum : double;
    md : TMethodDetails;
begin
  // No argument to pop off
  md := TVM (vm).popMethodDetails;
  s := TListObject (md.self);
  sum := 0;
  for var i := 0 to s.list.Count - 1 do
      case s.list[i].itemType of
         symInteger :  sum := sum + s.list[i].iValue;
         symDouble  :  sum := sum + s.list[i].dValue;
      else
         raise ERuntimeException.Create('You cannot sum non-numerical values in a list');
      end;
  TVM (vm).push(sum);
end;


// Remove an element for the list and push it onto the stack for the caller to retrieve
procedure TListMethods.removeLastElement (vm : TObject);
var s : TListObject;
     r : TListItem;
    md : TMethodDetails;
begin
  md := TVM (vm).popMethodDetails;
  s := TListObject (md.self);
  if s.list.count = 0 then
     begin
     TVM (vm).push  (s);
     exit;
     end;

   r := s.list [s.list.Count - 1];
   case r.itemType of
      symInteger  : TVM (vm).push (r.iValue);
      symDouble   : TVM (vm).push (r.dValue);
      symBoolean  : TVM (vm).push (r.bValue);
      symString   : TVM (vm).push (r.dataObject.clone);
      symList     : TVM (vm).push (r.dataObject.clone);
      symArray    : TVM (vm).push (r.dataObject.clone);
      symMatrix   : TVM (vm).push (r.dataObject.clone);
      symUserFunc : TVM (vm).push (r.dataObject);
      symModule   : TVM (vm).push (TModule (r.moduleValue));
   end;
   s.remove (s.list.Count - 1);
   // We don't push None as with the other methods because
   // the above pushes a value on to the return stack
end;


procedure TListMethods.insert (vm : TObject);
var s : TListObject;
    index : integer;
    value : PMachineStackRecord;
    md : TMethodDetails;
begin
  value := TVM (vm).pop;
  index := TVM (vm).popInteger;
  md := TVM (vm).popMethodDetails;
  s := TListObject (md.self);
  if (index < 0) or (index > s.list.Count) then
     raise ERuntimeException.Create('List index out of range');

   case value.stackType of
      symInteger :    s.insert (index, value.iValue);
      symDouble  :    s.insert (index, value.dValue);
      symBoolean :    s.insert (index, value.bValue);
      symString  :    s.insert (index, value.dataObject);
      symList    :    s.insert (index, value.dataObject);
      symArray   :    s.insert (index, value.dataObject);
      symMatrix  :    s.insert (index, value.dataObject);
      symUserFunc:    s.insert (index, value.dataObject);
      symModule  :    s.insertModule (index, value.module)
   else
      raise ERuntimeException.Create('Internal error: unrecognized data type during list insert');
   end;
   TVM (vm).pushNone;
end;


procedure TListMethods.getMax (vm : TObject);
var s : TListObject;
    md : TMethodDetails;
begin
  md := TVM (vm).popMethodDetails;
  s := TListObject (md.self);
  TVM (vm).push (s.findMax());
end;


procedure TListMethods.getMin (vm : TObject);
var s : TListObject;
    md : TMethodDetails;
begin
  md := TVM (vm).popMethodDetails;
  s := TListObject (md.self);
  TVM (vm).push (s.findMin());
end;


procedure countItems (s : TListObject; dims : TIntArray; level : integer);
var i : integer;
begin
  for i := 0 to s.list.Count - 1 do
      begin
      if s.list[i].itemType = symInteger then
         inc (dims[level])
      else
         countItems (TListObject (s.list[i].dataObject), dims, level+1);
      writeln (TRttiEnumerationType.GetName(s.list[i].itemType));
      end;
end;


procedure collectData (s : TListObject; data : TDoubleArray; var count : integer);
var i : integer;
begin
  for i := 0 to s.list.Count - 1 do
      begin
      if s.list[i].itemType = symInteger then
         begin
         data[count] := s.list[i].iValue;
         inc (count);
         end;
      if s.list[i].itemType = symList then
         collectData (TListObject (s.list[i].dataObject), data, count);
      end;
end;


// -------------------------------------------------------------------------------------------------


destructor TListContainer.Destroy;
begin
  inherited;
end;


// --------------------------------------------------------------------------------------------------
// Create an empty list
constructor TListObject.Create;
begin
  inherited Create;    // Adds object to the memory pool

  objectType := symList;
  list := TListContainer.Create;
  methods := TListObject.listMethods;
end;


constructor TListObject.Create(count: integer);
var
  i: integer;
begin
  Create;

  for i := 0 to count - 1 do
    list.add(TListItem.Create(0));
end;


destructor TListObject.Destroy;
var
  i: integer;
begin
  for i := list.count - 1 downto 0 do
    list[i].free;
  list.free;
  inherited;
end;


function TListObject.clone: TDataObject;
var
  i: integer;
  li : TListObject;
begin
  result := TListObject.Create(self.list.count);
  for i := 0 to self.list.count - 1 do
    begin
    li := result as TListObject;
    case self.list[i].itemType of
      symInteger:
        begin
          li.list[i].itemType := self.list[i].itemType;
          li.list[i].iValue := self.list[i].iValue;
        end;
      symBoolean:
        begin
          li.list[i].itemType := self.list[i].itemType;
          li.list[i].bValue := self.list[i].bValue;
        end;
      symDouble:
        begin
          li.list[i].itemType := self.list[i].itemType;
          li.list[i].dValue := self.list[i].dValue;
        end;
      symString,
      symList,
      symArray,
      symMatrix,
      symUserFunc :
        begin
          li.list[i].itemType := self.list[i].itemType;
          li.list[i].dataObject := self.list[i].dataObject.clone;
          li.list[i].dataObject.blockType := btOwned; // owned by the container list
        end;
      symModule:
        begin
          raise ERuntimeException.Create('Copying modules is not permitted');
          // We don't clone modules
        end;
    end;
    end;
end;


function TListObject.toString: string;
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


function TListObject.getItem (index : integer) : TListItem;
begin
  result := self.list[index];
end;


procedure TListObject.setItemToDouble (index : integer; value : double);
begin
  list[index].dValue := value;
  list[index].itemType := symDouble;
end;


function TListObject.getsize: integer;
var
  i: integer;
begin
  result := self.InstanceSize;
  for i := 0 to self.list.count - 1 do
    result := result + list[i].getsize();
end;


procedure TListObject.remove(index: integer);
begin
  if (index < 0) or (index > list.count - 1) then
    raise ERuntimeException.Create ('out of range while indexing element in list');
  list[index].Free;
  list.Delete(index);
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


procedure TListObject.insert(index : integer; obj: TDataObject);
begin
  list.insert(index, TListItem.Create(obj));
end;


procedure TListObject.insertModule(index: integer; mValue: TObject);
begin
  list.insert(index, TListItem.CreateModule(mValue));
end;


function TListObject.findMin : double;
var i : integer;
begin
  if list.Count = 0 then
     result := 0  // error?
  else
     begin
     case list[0].itemType of
        symInteger : result := list[0].iValue;
        symDouble  : result := list[0].dValue;
     else
         raise ERuntimeException.Create('Can only find the min value for numeric data');
     end;
     for i := 0 to list.Count - 1 do
        case list[i].itemType of
           symInteger:
                begin
                if list[i].iValue < result then
                   result := list[i].iValue;
                end;
           symDouble :
                begin
                if list[i].dValue < result then
                   result := list[i].dValue;
                end
        else
          raise ERuntimeException.Create('Can only find the min value for numeric data');
        end;
     end;
end;

function TListObject.findMax : double;
var i : integer;
begin
  if list.Count = 0 then
     result := 0  // error?
  else
     begin
     case list[0].itemType of
        symInteger : result := list[0].iValue;
        symDouble  : result := list[0].dValue;
     else
         raise ERuntimeException.Create('Can only find the max value for numeric data');
     end;
     for i := 0 to list.Count - 1 do
        case list[i].itemType of
           symInteger:
                begin
                if list[i].iValue > result then
                   result := list[i].iValue;
                end;
           symDouble :
                begin
                if list[i].dValue > result then
                   result := list[i].dValue;
                end
        else
          raise ERuntimeException.Create('Can only find the min value for numeric data');
        end;
     end;
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
            symInteger : obj.append(list[i].iValue);
            symBoolean : obj.append(list[i].bValue);
            symDouble  : obj.append(list[i].dValue);
            symString  : obj.append(list[i].dataObject.clone);
            symList    : obj.append(list[i].dataObject.clone);
            symArray   : obj.append(list[i].dataObject.clone);
            symMatrix  : obj.append(list[i].dataObject.clone);
            symUserFunc: obj.append(list[i].dataObject.clone);
            symModule  : obj.appendModule(TModule (list[i].moduleValue));
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
            symInteger : obj.append(alist.list[i].iValue);
            symBoolean : obj.append(alist.list[i].bValue);
            symDouble  : obj.append(alist.list[i].dValue);
            symString  : obj.append(alist.list[i].dataObject.clone);
            symList    : obj.append(alist.list[i].dataObject.clone);
            symArray   : obj.append(alist.list[i].dataObject.clone);
            symMatrix  : obj.append(alist.list[i].dataObject.clone);
            symUserFunc: obj.append(TUserFunction (alist.list[i].dataObject).clone);
            symModule  : obj.appendModule(TModule (alist.list[i].moduleValue));
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
    result := list1.clone as TListObject;

  for i := 0 to list2.list.count - 1 do
    result.list.add(TListItem.Create(list2.list[i]));
end;


// Make value copies of aList and combine them into one list
// eg 3*{1} = {1,1,1}
class function TListObject.multiply(value: integer; aList: TListObject) : TListObject;
var
  i, j: integer;
  nContents: integer;
  workingCopy: TListObject;
  item: TListItem;
begin
  if aList.isBound then
    workingCopy := aList.clone as TListObject
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


procedure TListObject.append(data : TDataObject);
begin
  if data.blockType = btGarbage then
     data.blockType := btOwned;

  list.add(TListItem.Create(data));
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
    symInteger:
      self.iValue := item.iValue;
    symDouble:
      self.dValue := item.dValue;
    symBoolean:
      self.bValue := item.bValue;
    symString:
      begin
        self.dataObject := item.dataObject.clone;
        self.dataObject.blockType := btOwned;
      end;
    symList:
      begin
        self.dataObject := item.dataObject.clone;
        self.dataObject.blockType := btOwned;
      end;
    symArray:
      begin
        self.dataObject := item.dataObject.clone;
        self.dataObject.blockType := btOwned;
      end;
    symMatrix:
      begin
        self.dataObject := item.dataObject.clone;
        self.dataObject.blockType := btOwned;
      end;
    symUserFunc:
      begin
        self.dataObject := item.dataObject
      end;
    symModule:
      begin
        self.moduleValue := item.moduleValue;
      end;
  else
    ERuntimeException.Create
      ('Internal Error: unknown itemType in ListItem.Create');
  end;
end;


constructor TListItem.Create(iValue: integer);
begin
  itemType := symInteger;
  self.iValue := iValue;
end;


constructor TListItem.Create(bValue: boolean);
begin
  itemType := symBoolean;
  self.bValue := bValue;
end;


constructor TListItem.Create(dValue: double);
begin
  itemType := symDouble;
  self.dValue := dValue;
end;


constructor TListItem.Create(obj: TDataObject);
begin
  itemType := obj.objectType;;
  self.dataObject := obj;
end;


constructor TListItem.CreateModule(mValue: TObject);
begin
  itemType := symModule;
  self.moduleValue := mValue;
end;


// This needs looking at.
destructor TListItem.Destroy;
begin
  case itemType of
     symList,
     symString,
     symMatrix,
     symArray : begin
                if dataObject.isOwned then
                  dataObject.blockType := btGarbage
                else
                  dataObject.free;
                end;
  symUserFunc : begin
                if (dataObject as TUserFunction).blockType = btOwned then
                    (dataObject as TUserFunction).blockType := btGarbage
                else
                 dataObject.free;
                end;
  symInteger, symDouble, symBoolean, symValueObject : begin end
     else
       begin raise ERuntimeException.Create ('Failed to free object in TListItem.Destroy:' + stToStr(itemType)); end;
  end;

  inherited;
end;


function TListItem.clone : TListItem;
begin
  case itemType of
    symInteger:  result := TListItem.Create(iValue);
    symBoolean:  result := TListItem.Create(bValue);
    symDouble:   result := TListItem.Create(dValue);
    symString:   result := TListItem.Create(dataObject.clone);
    symList:     begin result := TListItem.Create(dataObject.clone); result.dataObject.blockType := btOwned; end;
    symArray:    result := TListItem.Create(dataObject.clone);
    symMatrix:   result := TListItem.Create(dataObject.clone);
    symUserFunc: result := TListItem.Create(dataObject.clone);
    symModule:   raise ERuntimeException.Create('can''t clone module');
  else
    raise ERuntimeException.Create('Internal Error in ListItem Clone');
  end;
end;


function TListItem.getsize(): integer;
begin
  result := sizeof(TObject);
  result := result + sizeof(self.itemType);
  case itemType of
    symInteger:
      result := result + sizeof(integer);
    symBoolean:
      result := result + sizeof(boolean);
    symDouble:
      result := result + sizeof(double);
    symString:
      result := result + dataObject.getsize();
    symList:
      result := result + dataObject.getsize();
    symObject:
      result := result + dataObject.getSize();
    symArray:
      result := result + dataObject.getSize();
  else
    raise ERuntimeException.Create('Error in size not implement for this type');
  end;
end;


function TListItem.getScalar : double;
begin
  if itemType = symInteger then
     exit (iValue);
  if itemType = symDouble then
     exit (dValue);
  exit (0);
end;


procedure TListItem.setInteger (value : integer);
begin
  iValue := value;
  itemType := symInteger;
end;


procedure TListItem.setDouble (value : double);
begin
  dValue := value;
  itemType := symDouble;
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
    symInteger:
      result := result + inttostr(self.iValue);
    symBoolean:
      result := result + boolToString(self.bValue);
    symDouble:
      result := result + floattostr(self.dValue);
    symString:
      result := result + '"' + self.dataObject.ToString + '"';
    symList:
      result := result + self.dataObject.ToString;
    symUserFunc:
      result := result + self.dataObject.ToString;
    symModule:
      result := result + TModule(self.moduleValue).moduleName;
    symArray:
      result := result + self.dataObject.ToString;
    symMatrix:
      result := result + self.dataObject.ToString;
    symObject:
      result := result + self.dataObject.ToString;
    symValueObject:
      result := result + self.dataObject.ToString;
  else
    raise ERuntimeException.Create('Unknown type in elementToString: ' + stToStr(self.itemType));
  end;
end;


class function TListItem.listEquals(list1: TListItem; list2: TListItem) : boolean;
begin
  result := False;

  if (list1.itemType = symInteger) and (list2.itemType = symInteger) then
    if list1.iValue = list2.iValue then
      exit(True)
    else
      exit(False);

  if (list1.itemType = symBoolean) and (list2.itemType = symBoolean) then
    if list1.bValue = list2.bValue then
      exit(True)
    else
      exit(False);

  if (list1.itemType = symDouble) and (list2.itemType = symDouble) then
    if list1.dValue = list2.dValue then
      exit(True)
    else
      exit(False);

  if (list1.itemType = symString) and (list2.itemType = symString) then
    if TStringObject(list1.dataObject).isEqualTo(TStringObject(list2.dataObject)) then
      exit(True)
    else
      exit(False);

  if (list1.itemType = symList) and (list2.itemType = symList) then
      if TListObject.listEquals(TListObject (list1.dataObject), TListObject (list2.dataObject)) then
         exit (True)
      else
         exit(False);

  if (list1.itemType = symArray) and (list2.itemType = symArray) then
      if TArrayObject.isEqualTo(TArrayObject(list1.dataObject), TArrayObject(list2.dataObject)) then
         exit (True)
      else
         exit(False);

  if (list1.itemType = symMatrix) and (list2.itemType = symMatrix) then
      if TMatrixObject.isEqualTo(TMatrixObject (list1.dataObject), TMatrixObject (list2.dataObject)) then
         exit (True)
      else
         exit(False);
end;


// -----------------------------------------------------------------------

initialization
   TListObject.listMethods := TListMethods.Create;
finalization
   TListObject.listMethods.Free;
end.

