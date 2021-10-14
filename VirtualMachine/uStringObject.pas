unit uStringObject;

// This source is distributed under Apache 2.0

// Copyright (C)  2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

uses Classes, uMemoryManager, uObjectSupport;

type
  TStringMethods = class (TMethodsBase)
     procedure   getLength (vm : TObject);
     procedure   find (vm : TObject);
     procedure   toUpper (vm : TObject);
     procedure   toLower (vm : TObject);
     procedure   left (vm : TObject);
     procedure   right (vm : TObject);
     procedure   trim (vm : TObject);
     procedure   mid (vm : TObject);
     procedure   split (vm :TObject);
     constructor Create;
     destructor  Destroy; override;
  end;

  TStringObject = class (TRhodusObject)

     value : string;
     stringMethods : TStringMethods;

     function        isEqualTo (str1 : TStringObject) : boolean;
     class function  add (str1, str2 : TStringObject) : TStringObject;
     function        clone : TStringObject;
     constructor     createConstantObj (value : string);
     function        getSize() : integer;
     constructor     Create (value : string);
     destructor      Destroy; override;
  end;

implementation

Uses SysUtils,
     StrUtils,
     Generics.Collections,
     System.Character,
     uRhodusTypes,
     uVM,
     uMachineStack,
     uListObject,
     uVMExceptions;

var globalStringMethods : TStringMethods;

constructor TStringMethods.Create;
begin
  methodList := TMethodList.Create;

  methodList.Add(TMethodDetails.Create ('len',     'Return the length of a string', 0, getLength));
  methodList.Add(TMethodDetails.Create ('find',    'Finds a substring in string. Returns -1 if it fails: var.find ("CD")', 1, find));
  methodList.Add(TMethodDetails.Create ('toUpper', 'Converts all letters in the string to uppoer case: var.toUpper ()', 0, toUpper));
  methodList.Add(TMethodDetails.Create ('toLower', 'Converts all letters in the string to lower case: var.toUpper ()', 0, toLower));
  methodList.Add(TMethodDetails.Create ('left',    'Returns the left n chars of a string. var.left (5)', 1, left));
  methodList.Add(TMethodDetails.Create ('right',   'Returns the right n chars of a string. var.right (5)', 1, right));
  methodList.Add(TMethodDetails.Create ('mid',     'Returns a substring of string from start to count characters: var.mid (2, 4)', 2, mid));
  methodList.Add(TMethodDetails.Create ('trim',    'Removes any spaces from the start and endof the string: var.trim ()', 0, trim));
  methodList.Add(TMethodDetails.Create ('split',   'Splits at a given character into a list of strings: var.split (",")', 1, split));

  methodList.Add(TMethodDetails.Create ('dir',     'dir of string object methods', 0, dir));
end;


destructor TStringMethods.Destroy;
begin
  for var i := 0 to methodList.Count - 1 do
      methodList[i].Free;
  methodlist.Free;
  inherited;
end;


procedure TStringMethods.getLength (vm : TObject);
var s : TStringObject;
begin
   TVM (vm).decStackTop; // Dump the object method
   s := TVM (vm).popString;
   TVM (vm).push(length (s.value));
end;


procedure TStringMethods.find (vm : TObject);
var s, substr : TStringObject;
    index : integer;
begin
   substr := TVM (vm).popString;
   TVM (vm).decStackTop; // Dump the object method
   s := TVM (vm).popString;
   index := pos (substr.value, s.value);
   TVM (vm).push(index-1);
end;


procedure TStringMethods.toUpper (vm : TObject);
var s : TStringObject;
begin
  TVM (vm).decStackTop; // Dump the object method
  s := TVM (vm).popString;
  s.value := UpperCase (s.value);
  TVM (vm).push(s);
end;


procedure TStringMethods.toLower (vm : TObject);
var s : TStringObject;
begin
  TVM (vm).decStackTop; // Dump the object method
  s := TVM (vm).popString;
  s.value := LowerCase (s.value);
  TVM (vm).push(s);
end;


procedure TStringMethods.left (vm : TObject);
var s : TStringObject;
    index : integer;
begin
  index := TVM (vm).popInteger;
  TVM (vm).decStackTop; // Dump the object method
  s := TVM (vm).popString;
  TVM (vm).push (TStringObject.create (LeftStr (s.value, index)));
end;


procedure TStringMethods.right (vm : TObject);
var s : TStringObject;
    index : integer;
begin
  index := TVM (vm).popInteger;
  TVM (vm).decStackTop; // Dump the object method
  s := TVM (vm).popString;
  TVM (vm).push (TStringObject.create (RightStr (s.value, index)));
end;


procedure TStringMethods.trim (vm : TObject);
var s : TStringObject;
begin
  TVM (vm).decStackTop; // Dump the object method
  s := TVM (vm).popString;
  TVM (vm).push (TStringObject.create (sysutils.trim (s.value)));
end;


procedure TStringMethods.mid (vm : TObject);
var s : TStringObject;
    start, count : integer;
begin
  count := TVM (vm).popInteger;
  start := TVM (vm).popInteger;
  TVM (vm).decStackTop; // Dump the object method
  s := TVM (vm).popString;

  TVM (vm).push (TStringObject.create (MidStr (s.value, start + 1, count)));
end;


procedure TStringMethods.split (vm :TObject);
var  s : TStringObject;
     delimiter : string;
     splitted: TArray<String>;
     alist : TListObject;
     i : integer;
begin
  delimiter := TVM (vm).popString.value;

  TVM (vm).decStackTop; // Dump the object method
  s := TVM (vm).popString;

  splitted := SplitString(s.value, delimiter);
  alist := TListObject.Create (0);
  for i := 0 to length (splitted) - 1 do
      alist.append(TStringObject.create(splitted[i]));
  TVM (vm).push(alist);
end;

// ---------------------------------------------------------------------
function createStringObject (value : string) : TStringObject;
begin
  result := TStringObject.Create (value);
end;


constructor TStringObject.createConstantObj (value : string);
begin
  blockType := btConstant;
  self.value := value;
  stringMethods := globalStringMethods;
end;


constructor TStringObject.Create (value : string);
begin
  blockType := btGarbage;
  objectType := symString;
  self.value := value;
  stringMethods := globalStringMethods;
  memoryList.addNode (self);
end;


destructor TStringObject.Destroy;
begin
  value := '';
  inherited;
end;


function TStringObject.clone : TStringObject;
begin
  result := TStringObject.Create (value);
end;


function TStringObject.getSize() : integer;
begin
  result := self.InstanceSize;
  result := result + Length (value);
end;


function TStringObject.isEqualTo (str1 : TStringObject) : boolean;
begin
  result := self.value = str1.value;
end;


class function TStringObject.add (str1, str2 : TStringObject) : TStringObject;
begin
  result := TStringObject.Create (str1.value + str2.value);
end;


// -----------------------------------------------------------------------

initialization
   globalStringMethods := TStringMethods.Create;
finalization
  globalStringMethods.Free;
end.


