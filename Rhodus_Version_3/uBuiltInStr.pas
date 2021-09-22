unit uBuiltInStr;


// This source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com


interface

Uses SysUtils, Classes, StrUtils, uLibModule;

type
  TBuiltInStr = class (TModuleLib)

     procedure   getLength (vm : TObject);
     procedure   find (vm : TObject);
     procedure   toUpper (vm : TObject);
     procedure   toLower (vm : TObject);
     procedure   left (vm : TObject);
     procedure   right (vm : TObject);
     procedure   trim (vm : TObject);
     procedure   mid (vm : TObject);
     procedure   split (vm :TObject);
     procedure   val (vm : TObject);
     procedure   str (vm : TObject);
     constructor Create;
  end;


implementation

Uses System.Character, Math, uSymbolTable, uVM, uStringObject,
     uListObject, uMemoryManager, uVMExceptions, uMachineStack;


constructor TBuiltInStr.Create;
begin
  inherited Create ('strings', 'String Module');

  addMethod(getLength,   1, 'len', 'Return the length of a string');
  addMethod(find,        2, 'find', 'Finds a substring in string. Returns -1 if it fails: strings.find ("ABCDEFG", "CD"');
  addMethod(toUpper,     1, 'toUpper', 'Converts all letters in the string to uppoer case: strings.toUpper ("AcdefGH"');
  addMethod(toLower,     1, 'toLower', 'Converts all letters in the string to lower case: strings.toUpper ("AcdefGH"');
  addMethod(left,        2, 'left', 'Returns the left n chars of a string. strings.left ("AcdefGH", 5');
  addMethod(right,       2, 'right', 'Returns the right n chars of a string. strings.right ("AcdefGH", 5');
  addMethod(mid,         3, 'mid', 'Returns a substring of string from start to count characters: strings.mid ("AcdefGH", 2, 4');
  addMethod(trim,        1, 'trim', 'Removes ny spaces from the start and endof the string: strings.trim ("  AcdefGH ")');
  addMethod(split,       2, 'split', 'Splits at a given character into a list of strings: strings.split ("AB CD DE", " ")');
  addMethod(str,         1, 'str', 'Converts a string into a number: strings.str ("1.23"');
  addMethod(val,         1, 'val', 'Converts a number into a string: strings.val (1.23)');
end;


procedure TBuiltInStr.find (vm : TObject);
var s, substr : TStringObject;
    index : integer;
begin
   substr := TVM (vm).popString;
   s := TVM (vm).popString;
   index := pos (substr.value, s.value);
   TVM (vm).push(index-1);
end;


procedure TBuiltInStr.split (vm :TObject);
var  s : TStringObject;
     delimiter : string;
     splitted: TArray<String>;
     alist : TListObject;
     i : integer;
begin
  delimiter := TVM (vm).popString.value;
  s := TVM (vm).popString;
  splitted := SplitString(s.value, delimiter);
  alist := TListObject.Create (0);
  for i := 0 to length (splitted) - 1 do
      alist.append(TStringObject.create(splitted[i]));
  TVM (vm).push(alist);
end;


procedure TBuiltInStr.getLength (vm : TObject);
var s : TStringObject;
begin
   s := TVM (vm).popString;
   TVM (vm).push(length (s.value));
end;


procedure TBuiltInStr.toUpper (vm : TObject);
var s : TStringObject;
begin
  s := TVM (vm).popString;
  s.value := UpperCase (s.value);
  TVM (vm).push(s);
end;


procedure TBuiltInStr.toLower (vm : TObject);
var s : TStringObject;
begin
  s := TVM (vm).popString;
  s.value := LowerCase (s.value);
  TVM (vm).push(s);
end;


procedure TBuiltInStr.left (vm : TObject);
var s : TStringObject;
    index : integer;
begin
  index := TVM (vm).popInteger;
  s := TVM (vm).popString;
  TVM (vm).push (TStringObject.create (LeftStr (s.value, index)));

end;


procedure TBuiltInStr.right (vm : TObject);
var s : TStringObject;
    index : integer;
begin
  index := TVM (vm).popInteger;
  s := TVM (vm).popString;
  TVM (vm).push (TStringObject.create (RightStr (s.value, index)));
end;


procedure TBuiltInStr.mid (vm : TObject);
var s : TStringObject;
    start, count : integer;
begin
  count := TVM (vm).popInteger;
  start := TVM (vm).popInteger;
  s := TVM (vm).popString;
  TVM (vm).push (TStringObject.create (MidStr (s.value, start + 1, count)));
end;


procedure TBuiltInStr.trim (vm : TObject);
var s : TStringObject;
begin
  s := TVM (vm).popString;
  TVM (vm).push (TStringObject.create (sysutils.trim (s.value)));
end;


procedure TBuiltInStr.str (vm : TObject);
var s : TStringObject;
    iValue, iCode : integer;
    ch : Char;
begin
  s := TVM (vm).popString;

  for ch in s.value do
      if not TCharacter.IsNumber(ch) then
         begin
         // Float
         TVM (vm).push (strtofloat (s.value));
         exit;
         end;

  // Must be an integer
  system.Val (s.value, iValue, iCode);
  if iCode = 0 then
     TVM (vm).push (iValue)
  else
     raise ERuntimeException.Create('Internal error in val(), expecting an integer');
end;


procedure TBuiltInStr.val (vm : TObject);
var iValue : integer; dValue : double;
begin
  case TVM (vm).peek().stackType of
    stInteger :
        begin
        iValue := TVM (vm).popInteger;
        TVM (vm).push (TStringObject.create (inttostr (iValue)));
        end;
    stDouble :
        begin
        dValue := TVM (vm).popScalar;
        TVM (vm).push (TStringObject.create (floattostr (dValue)));
        end
  else
     raise ERuntimeException.Create('can only convert integers or floats to a string form');
  end;


end;


end.
