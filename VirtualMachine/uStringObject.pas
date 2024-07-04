unit uStringObject;

// This source is distributed under Apache 2.0

// Copyright (C)  2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

uses Classes, uMemoryManager, uObjectSupport, uRhodusObject;

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

     function        isEqualTo (str1 : TStringObject) : boolean;
     class function  add (str1, str2 : TStringObject) : TStringObject;
     function        clone : TStringObject;
     constructor     createConstantObj (value : string);
     function        getSize() : integer;
     function        slice (lower, upper : integer) : TStringObject;
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

var stringMethods : TStringMethods;

constructor TStringMethods.Create;
begin
  methodList := TMethodList.Create;

  methodList.Add(TMethodDetails.Create ('len',     0, 'Return the length of a string: a.len()', getLength));
  methodList.Add(TMethodDetails.Create ('find',    1, 'Finds a substring in string. Returns -1 if it fails: a.find ("CD")', find));
  methodList.Add(TMethodDetails.Create ('toUpper', 0, 'Converts all letters in the string to upper case: a.toUpper ()', toUpper));
  methodList.Add(TMethodDetails.Create ('toLower', 0, 'Converts all letters in the string to lower case: a.toUpper ()', toLower));
  methodList.Add(TMethodDetails.Create ('left',    1, 'Returns the left n chars of a string: a.left (5)', left));
  methodList.Add(TMethodDetails.Create ('right',   1, 'Returns the right n chars of a string: a.right (5)', right));
  methodList.Add(TMethodDetails.Create ('mid',     2, 'Returns a substring of string from start to count characters: a.mid (2, 4)', mid));
  methodList.Add(TMethodDetails.Create ('trim',    0, 'Removes any spaces from the start and end of the string: a.trim ()', trim));
  methodList.Add(TMethodDetails.Create ('split',   1, 'Splits at a given character into a list of strings: a.split (",")', split));

  methodList.Add(TMethodDetails.Create ('dir',     0, 'dir of string object methods', dir));
end;


destructor TStringMethods.Destroy;
begin
  inherited;
end;


procedure TStringMethods.getLength (vm : TObject);
var s : TStringObject;
    md : TMethodDetails;
begin
   md := TVM (vm).popMethodDetails;
   s := TStringObject (md.self);
   TVM (vm).push(length (s.value));
end;


procedure TStringMethods.find (vm : TObject);
var s, substr : TStringObject;
    index : integer;
    md : TMethodDetails;
begin
   substr := TVM (vm).popString;
   md := TVM (vm).popMethodDetails;
   s := TStringObject (md.self);
   index := pos (substr.value, s.value);
   TVM (vm).push(index-1);
end;


procedure TStringMethods.toUpper (vm : TObject);
var s : TStringObject;
    md : TMethodDetails;
begin
  md := TVM (vm).popMethodDetails;
  s := TStringObject (md.self);
  s.value := UpperCase (s.value);
  TVM (vm).push(s);
end;


procedure TStringMethods.toLower (vm : TObject);
var s : TStringObject;
    md : TMethodDetails;
begin
  md := TVM (vm).popMethodDetails;
  s := TStringObject (md.self);
  s.value := LowerCase (s.value);
  TVM (vm).push(s);
end;


procedure TStringMethods.left (vm : TObject);
var s : TStringObject;
    index : integer;
    md : TMethodDetails;
begin
  index := TVM (vm).popInteger;
  md := TVM (vm).popMethodDetails;
  s := TStringObject (md.self);
  TVM (vm).push (TStringObject.create (LeftStr (s.value, index)));
end;


procedure TStringMethods.right (vm : TObject);
var s : TStringObject;
    index : integer;
    md : TMethodDetails;
begin
  index := TVM (vm).popInteger;
  md := TVM (vm).popMethodDetails;
  s := TStringObject (md.self);
  TVM (vm).push (TStringObject.create (RightStr (s.value, index)));
end;


procedure TStringMethods.trim (vm : TObject);
var s : TStringObject;
    md : TMethodDetails;
begin
  md := TVM (vm).popMethodDetails;
  s := TStringObject (md.self);
  TVM (vm).push (TStringObject.create (sysutils.trim (s.value)));
end;


procedure TStringMethods.mid (vm : TObject);
var s : TStringObject;
    start, count : integer;
    md : TMethodDetails;
begin
  count := TVM (vm).popInteger;
  start := TVM (vm).popInteger;
  md := TVM (vm).popMethodDetails;
  s := TStringObject (md.self);

  TVM (vm).push (TStringObject.create (MidStr (s.value, start + 1, count)));
end;


procedure TStringMethods.split (vm :TObject);
var s : TStringObject;
    delimiter : string;
    splitted: TArray<String>;
    alist : TListObject;
    i : integer;
    md : TMethodDetails;
begin
  delimiter := TVM (vm).popString.value;

  md := TVM (vm).popMethodDetails;
  s := TStringObject (md.self);

  splitted := SplitString(s.value, delimiter);
  alist := TListObject.Create (0);
  for i := 0 to length (splitted) - 1 do
      alist.append(TStringObject.create(splitted[i]));
  TVM (vm).push(alist);
end;

// ---------------------------------------------------------------------------

function createStringObject (value : string) : TStringObject;
begin
  result := TStringObject.Create (value);
end;


constructor TStringObject.createConstantObj (value : string);
begin
  // There is no call to the inherited create which adds an object to
  // the memory pool. Constant strings are handled differently and are
  // freed separately when a module is freed.
  //inherited Create;

  blockType := btConstant;
  self.value := value;
  methods := stringMethods;
end;


constructor TStringObject.Create (value : string);
begin
  inherited Create; // Adds object to the memory pool

  blockType := btGarbage;
  objectType := symString;
  self.value := value;
  methods := stringMethods;
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


function TStringObject.slice (lower, upper : integer) : TStringObject;
begin
  // -1 means slice all
  if lower = -1 then
     lower := 0;
  if upper = -1 then
     upper := length (value);

  if upper < lower then
     result := TStringObject.Create('')
  else
     begin
     // Clamp the length if need be although I think copy does this anyway
     if upper >= length (value) then
        upper := length (value) - 1;
     result := TStringObject.Create(Copy(value, lower+1, upper-lower+1));
     end;
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
  stringMethods := TStringMethods.Create;
finalization
  stringMethods.Free;
end.


