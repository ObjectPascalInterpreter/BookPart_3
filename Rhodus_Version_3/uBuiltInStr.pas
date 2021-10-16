unit uBuiltInStr;


// This source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com


interface

Uses SysUtils, Classes, StrUtils, uLibModule;

type
  TBuiltInStr = class (TModuleLib)
     procedure   val (vm : TObject);
     procedure   str (vm : TObject);
     procedure   formatStr (vm : TObject);
     constructor Create;
  end;


implementation

Uses System.Character, Math, uSymbolTable, uVM, uStringObject,
     uListObject, uMemoryManager, uVMExceptions, uMachineStack;


constructor TBuiltInStr.Create;
begin
  inherited Create ('strings', 'String Module');

  addMethod(str,         1, 'str', 'Converts a string into a number: strings.str ("1.23"');
  addMethod(val,         1, 'val', 'Converts a number into a string: strings.val (1.23)');
  addMethod(formatStr,   2, 'format', 'Formats a number and returns a string, eg strings.format (2.3456, "%3.2f")');
end;


procedure TBuiltInStr.formatStr (vm : TObject);
var m : PMachineStackRecord;
    fstr: string;
begin
   fstr := TVM (vm).popString.value;
   m := TVM (vm).pop;
   case m.stackType of
      stInteger :
           TVM (vm).push(TStringObject.create(format (fstr, [m.iValue])));
      stDouble :
           TVM (vm).push(TStringObject.create(format (fstr, [m.dValue])));
      stString :
           TVM (vm).push(TStringObject.create(format (fstr, [m.sValue.value])));
   else
      raise ERuntimeException.Create('You can only use integers, floats amd strings in the format method.');
   end;
end;


procedure TBuiltInStr.str (vm : TObject);
var s : TStringObject;
    iValue, iCode : integer;
    ch : Char;
begin
  s := TVM (vm).popString;

  for ch in s.value do
      if not ch.IsDigit() then
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
