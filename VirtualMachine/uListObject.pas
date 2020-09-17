// Developed using Delphi for Windows and Mac platforms.

// Ths source is distributed under Apache 2.0

// Copyright (C) 2019-2020 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

unit uListObject;

interface

Uses System.SysUtils, uUtils, System.generics.Collections,
     uMemoryManager, uStringObject;

type
   TListItemType = (liInteger, liBoolean, liDouble, liString, liList);
   TListItem = class;

   TListObject = class (TRhodusObject)
      private
      public
        list : TObjectList<TListItem>;

        class function addLists (list1, list2 : TListObject) : TListObject;
        class function multiply (value : integer; aList : TListObject) : TListObject;
        class function listEquals (list1, list2 : TListObject) : boolean;

        procedure   append (iValue : integer); overload;
        procedure   append (bValue : boolean); overload;
        procedure   append (dValue : double); overload;
        procedure   append (sValue : TStringObject); overload;
        function    clone : TListObject;
        function    listToString : string;
        function    getsize : integer;
        constructor Create (count : integer);
        destructor  Destroy; override;
   end;

   TListItem = class (TObject)
       itemType : TListItemType;
       iValue : integer;
       bValue : boolean;
       dValue : double;
       sValue : TStringObject;
       lValue : TListObject;
       function    elementToString : string;
       class function listEquals (list1 : TListItem; list2 : TListItem) : boolean;
       function    getSize() : integer;

       constructor Create (iValue : integer); overload;
       constructor Create (bValue : boolean); overload;
       constructor Create (dValue : double); overload;
       constructor Create (sValue : TStringObject); overload;
       constructor Create (lValue : TListObject); overload;
       constructor Create(item : TListItem); overload;


       destructor  Destroy; override;
   end;



implementation

Uses uVMExceptions;

constructor TListObject.Create (count : integer);
var i : integer;
begin
  inherited Create;

  list := TObjectList<TListItem>.Create;
  for i  := 0 to count - 1 do
      list.add (TListItem.Create (0));
  memoryList.addNode (self);
end;


destructor TListObject.Destroy;
begin
  list.Free;
  inherited;
end;


function TListObject.clone : TListObject;
var i : integer;
begin
  result := TListObject.Create (self.list.count);
  for i := 0 to self.list.Count - 1 do
      case self.list[i].itemType of
          liInteger : begin result.list[i].itemType := liInteger; result.list[i].iValue := self.list[i].iValue; end;
          liBoolean : begin result.list[i].itemType := liBoolean; result.list[i].bValue := self.list[i].bValue; end;
          liDouble  : begin result.list[i].itemType := liDouble;  result.list[i].dValue := self.list[i].dValue; end;
          liString  : begin
                      result.list[i].itemType := liString;
                      result.list[i].sValue := self.list[i].sValue.clone;
                      result.list[i].sValue.blockType := btOwned;
                      end;
          liList    : begin
                      result.list[i].itemType := liList;
                      result.list[i].lValue := self.list[i].lValue.clone;
                      result.list[i].lValue.blockType := btOwned;
                      end;
      end;
end;


function TListObject.listToString  : string;
var i : integer;
begin
  result := '{';
  if self.list.Count > 0 then
     result := result + self.list[0].elementToString;

  for i := 1 to self.list.Count  - 1 do
      begin
      result := result + ',' + self.list[i].elementToString;
      end;
  result := result + '}';
end;


function TListObject.getsize : integer;
var i : integer;
begin
  result := getRhodusObjectSize;
  result := result + self.InstanceSize;
  for i := 0 to self.list.Count - 1 do
      result := result + self.list[i].getSize();
end;


class function TListObject.addLists (list1, list2 : TListObject) : TListObject;
var i : integer;
begin
  if list1.blockType = btGarbage then
     result := list1
  else
     result := list1.clone;

  for i := 0 to list2.list.Count - 1 do
      result.list.Add (TListItem.Create (list2.list[i]));
end;


// Make value copies of aList and combine them into one list
// eg 3*{1} = {1,1,1}
class function TListObject.multiply (value : integer; aList : TListObject) : TListObject;
var i, j : integer;
    nContents : integer;
    workingCopy : TListObject;
    item : TListItem;
begin
  if aList.isBound then
     workingCopy := alist.clone
  else
     workingCopy := aList;

  nContents := aList.list.Count;
  result := TListObject.Create(0);
  for i := 0 to value - 1 do
      if nContents = 0 then
         result.list.Add(TListItem.Create (0))
      else
         for j := 0 to nContents - 1 do
             begin
             item := TListItem.Create(workingCopy.list[j]);
             result.list.Add(item);
             end;
end;


class function TListObject.listEquals (list1, list2 : TListObject) : boolean;
var i : integer;
begin
  if list1.list.count <> list2.list.count then
     exit (False);

  result := True;
  for i := 0 to list1.list.count - 1 do
      result := result and TListItem.listEquals (list1.list[i], list2.list[i]);
end;


procedure TListObject.append (iValue : integer);
begin
  list.Add (TListItem.Create(iValue));
end;


procedure TListObject.append (bValue : boolean);
begin
  list.Add (TListItem.Create(bValue));
end;


procedure TListObject.append (dValue : double);
begin
  list.Add (TListItem.Create(dValue));
end;


procedure TListObject.append (sValue : TStringObject);
begin
  if sValue.blockType = btGarbage then
     sValue.blockType := btOwned;

  list.Add (TListItem.Create(sValue));
end;


// ----------------------------------------------------------------------

constructor TListItem.Create(item : TListItem);
begin
  inherited Create;
  itemType := item.itemType;
  case itemType of
     liInteger: self.iValue := item.iValue;
     liDouble:  self.dValue := item.dValue;
     liBoolean: self.bValue := item.bValue;
     liString:  begin self.sValue := item.sValue.clone; self.sValue.blockType := btOwned; end;
     liList :   begin self.lValue := item.lValue.clone; self.lValue.blockType := btOwned; end;
  else
     ERuntimeException.Create('Internal Error: unknown itemType in ListItem.Create');
  end;
end;


constructor TListItem.Create (iValue : integer);
begin
  itemType := liInteger;
  self.iValue := iValue;
end;

constructor TListItem.Create (bValue : boolean);
begin
  itemType := liBoolean;
  self.bValue := bValue;
end;

constructor TListItem.Create (dValue : double);
begin
  itemType := liDouble;
  self.dValue := dValue;
end;

constructor TListItem.Create (sValue : TStringObject);
begin
  itemType := liString;
  self.sValue := sValue;
end;

constructor TListItem.Create (lValue : TListObject);
begin
  itemType := liList;
  self.lValue := lValue;
end;


destructor TListItem.Destroy;
begin
  if itemType = liList then
     begin
     if lValue.isOwned then
        lValue.blockType := btGarbage
     else
        lValue.Free;
     end;
  if itemType = liString then
     begin
     if sValue.isOwned then
        sValue.blockType := btGarbage
     else
        sValue.Free;
     end;
  inherited;
end;


function TListItem.getSize() : integer;
begin
  result := sizeof (TObject);
  result := result + sizeof (self.itemType);
  case itemType of
    liInteger : result := result + sizeof (integer);
    liBoolean : result := result + sizeof (boolean);
    liDouble  : result := result + sizeof (double);
    liString  : result := result + sValue.getSize();
    liList    : result := result + lValue.getsize();
  end;
end;


function boolToString (value : boolean) : string;
begin
  if value then
     result := 'True'
  else
     result := 'False';
end;


function TListItem.elementToString : string;
begin
  result := '';
  case self.itemType of
    liInteger : result := result + inttostr (self.iValue);
    liBoolean : result := result + boolToString (self.bValue);
    liDouble  : result := result + floattostr (self.dValue);
    liString  : result := result + self.sValue.value;
    liList : result := result + self.lValue.listToString;
  else
    raise ERuntimeException.Create('Unknown type in elementToString');
  end;
end;


class function TListItem.listEquals (list1 : TListItem; list2 : TListItem) : boolean;
begin
  result := False;
  if (list1.itemType = liInteger) and (list2.itemType = liInteger) then
     if list1.iValue = list2.iValue then
        exit (True) else exit (False);

  if (list1.itemType = liBoolean) and (list2.itemType = liBoolean) then
     if list1.bValue = list2.bValue then
        exit (True) else exit (False);

  if (list1.itemType = liDouble) and (list2.itemType = liDouble) then
     if list1.dValue = list2.dValue then
        exit (True) else exit (False);

  if (list1.itemType = liString) and (list2.itemType = liString) then
     if list1.sValue.isEqualTo (list2.sValue) then
        exit (True) else exit (False);

  if (list1.itemType = liList) and (list2.itemType = liList) then
     exit (TListObject.listEquals (List1.lValue, List2.lValue))
  else
     exit (False);
end;


// -----------------------------------------------------------------------




end.
