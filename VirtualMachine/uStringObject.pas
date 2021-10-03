unit uStringObject;

// This source is distributed under Apache 2.0

// Copyright (C)  2019-2020 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

uses Classes, uMemoryManager, uObjectSupport;

type
  TStringObject = class (TRhodusObject)

     value : string;

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
     System.Character,
     uRhodusTypes, uVM, uMachineStack, uListObject, uVMExceptions;

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
  end;


var methodListObject : TMethodList;
    stringMethods : TStringMethods;

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


//class procedure TStringMethods.val (vm : TObject);
//var iValue : integer; dValue : double;
//begin
//  case TVM (vm).peek().stackType of
//    stInteger :
//        begin
//        iValue := TVM (vm).popInteger;
//        TVM (vm).decStackTop; // Dump the object method
//        TVM (vm).push (TStringObject.create (inttostr (iValue)));
//        end;
//    stDouble :
//        begin
//        dValue := TVM (vm).popScalar;
//        TVM (vm).decStackTop; // Dump the object method
//        TVM (vm).push (TStringObject.create (floattostr (dValue)));
//        end
//  else
//     raise ERuntimeException.Create('can only convert integers or floats to a string form');
//  end;
//end;

//class procedure TStringMethods.str (vm : TObject);
//var s : TStringObject;
//    iValue, iCode : integer;
//    ch : Char;
//begin
//  s := TVM (vm).popString;
//  TVM (vm).decStackTop; // Dump the object method
//
//  for ch in s.value do
//      if not TCharacter.IsNumber(ch) then
//         begin
//         // Float
//         TVM (vm).push (strtofloat (s.value));
//         exit;
//         end;
//
//  // Must be an integer
//  system.Val (s.value, iValue, iCode);
//  if iCode = 0 then
//     TVM (vm).push (iValue)
//  else
//     raise ERuntimeException.Create('Internal error in val(), expecting an integer');
//end;

//class procedure TStringMethods.formatStr (vm : TObject);
//begin
//
//end;

// ---------------------------------------------------------------------
function createStringObject (value : string) : TStringObject;
begin
  result := TStringObject.Create (value);
end;


constructor TStringObject.createConstantObj (value : string);
begin
  blockType := btConstant;
  self.value := value;
  methodList := methodListObject;
  stringMethods.methodList := methodList;
end;


constructor TStringObject.Create (value : string);
begin
  blockType := btGarbage;
  objectType := symString;
  self.value := value;
  methodList := methodListObject;
  stringMethods.methodList := methodList;
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
  methodListObject := TMethodList.Create;
  stringMethods := TStringMethods.Create;

  methodListObject.Add(TMethodDetails.Create ('len',     'Return the length of a string', 0, stringMethods.getLength));
  methodListObject.Add(TMethodDetails.Create ('find',    'Finds a substring in string. Returns -1 if it fails: var.find ("CD")', 1, stringMethods.find));
  methodListObject.Add(TMethodDetails.Create ('toUpper', 'Converts all letters in the string to uppoer case: var.toUpper ()', 0, stringMethods.toUpper));
  methodListObject.Add(TMethodDetails.Create ('toLower', 'Converts all letters in the string to lower case: var.toUpper ()', 0, stringMethods.toLower));
  methodListObject.Add(TMethodDetails.Create ('left',    'Returns the left n chars of a string. var.left (5)', 1, stringMethods.left));
  methodListObject.Add(TMethodDetails.Create ('right',   'Returns the right n chars of a string. var.right (5)', 1, stringMethods.right));
  methodListObject.Add(TMethodDetails.Create ('mid',     'Returns a substring of string from start to count characters: var.mid (2, 4)', 2, stringMethods.mid));
  methodListObject.Add(TMethodDetails.Create ('trim',    'Removes any spaces from the start and endof the string: var.trim ()', 0, stringMethods.trim));
  methodListObject.Add(TMethodDetails.Create ('split',   'Splits at a given character into a list of strings: var.split (",")', 1, stringMethods.split));

  methodListObject.Add(TMethodDetails.Create ('dir',     'dir of string object methods', 0, stringMethods.dir));

finalization
  for var i := 0 to methodListObject.Count - 1 do
      methodListObject[i].Free;
  methodListObject.Free;
  stringMethods.Free;
end.


