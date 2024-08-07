unit uBuiltInStr;

{
  Unit:    uBuiltInStr.pas
  Author:  Herbert M sauro
  Date:    10/2021
  Purpose: This file implements the string library for the Rhodus interpreter.

  Ths source is distributed under Apache 2.0
  See https://www.apache.org/licenses/LICENSE-2.0.txt for further information

  Copyright (C)  2019-2024 Herbert M Sauro

  Author Contact Information:
  email: hsauro@gmail.com
}


interface

Uses SysUtils,
     Classes,
     StrUtils,
     uLibModule,
     uHelpUnit;

type
  TBuiltInStr = class (TModuleLib)
     procedure   val (vm : TObject);
     procedure   str (vm : TObject);
     procedure   formatStr (vm : TObject);
     procedure   randomStr (vm : TObject);
     procedure   reverseStr (vm : TObject);
     constructor Create;
  end;


implementation

Uses System.Character, Math,
     uRhodusTypes,
     uSymbolTable,
     uVM,
     uStringObject,
     uListObject,
     uMemoryManager,
     uVMExceptions,
     uMachineStack;


constructor TBuiltInStr.Create;
begin
  inherited Create ('strings');

  addStringValue('asciiLower',  'abcdefghijklmnopqrstuv', true);//, 'Returns the lower ascii characters as a string', true);
  addStringValue('asciiLpper', 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', true);// 'Returns the upper ascii characters', true);
  addStringValue('digits', '0123456789', true);

  addMethod(str,         1, 'str');     // convert string to number
  addMethod(val,         1, 'val');     // convert number to string
  addMethod(formatStr,   2, 'format');  // eg strings.format (2.3456, "%3.2f")
  addMethod(randomStr,   1, 'random');  // Generate random string
  addMethod(reverseStr,  1, 'reverse');  // reverse a string
end;


procedure TBuiltInStr.reverseStr (vm : TObject);
var s, r : TStringObject;
begin
  s := TVM (vm).popString;
  r := s.clone as TStringObject;
  s.value := reverseString (r.value);
  TVM (vm).push(r);
end;

procedure TBuiltInStr.randomStr (vm : TObject);
var n : integer;
    astr : string;
    seqlen, ch : integer;
    result : string;
begin
  n := TVM (vm).popInteger;

  astr := 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890';

  seqlen := Length(astr);
  SetLength(result, n);
  Randomize;

  for Ch := Low(Result) to High(Result) do
    result[Ch] := astr.Chars[Random(seqlen)];

  TVM (vm).push(TStringObject.Create(result));
end;


procedure TBuiltInStr.formatStr (vm : TObject);
var m : PMachineStackRecord;
    fstr: string;
begin
   fstr := TVM (vm).popString.value;
   m := TVM (vm).pop;
   case m.stackType of
      symInteger :
           TVM (vm).push(TStringObject.create(format (fstr, [m.iValue])));
      symDouble :
           TVM (vm).push(TStringObject.create(format (fstr, [m.dValue])));
      symString :
           TVM (vm).push(TStringObject.create(format (fstr, [TStringObject(m.dataObject).value])));
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
    symInteger :
        begin
        iValue := TVM (vm).popInteger;
        TVM (vm).push (TStringObject.create (inttostr (iValue)));
        end;
    symDouble :
        begin
        dValue := TVM (vm).popScalar;
        TVM (vm).push (TStringObject.create (floattostr (dValue)));
        end
  else
     raise ERuntimeException.Create('can only convert integers or floats to a string form');
  end;


end;


end.
