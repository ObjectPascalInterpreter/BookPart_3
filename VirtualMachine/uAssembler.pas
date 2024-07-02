unit uAssembler;

// Developed using Delphi for Windows and Mac platforms.

// *** This source is distributed under Apache 2.0 ***

// Copyright (C) 2019-2020 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

Uses Classes, System.SysUtils, System.StrUtils, System.Types, IOUtils, uSymbolTable, uOpCodes, uProgramCode;

function assembleCode (const srcCode : string) : TProgram;
function dissassemble (module : TModule; aProgram : TProgram) : string;

implementation

Uses uConstantTable;

var labels : TStringList;

// From stackoverflow (35158485) Alex James
function RemoveDupSpaces(const Input : String) : String;
var
  P : Integer;
begin
  Result := Input;
  repeat
    P := Pos('  ', Result);  // that's two spaces
    if P > 0 then
      Delete(Result, P + 1, 1);
  until P = 0;
end;


procedure collectLabels (srcCode : string);
var sl  : TStringList;
    i : integer;
    s, alabel : string;
    splitted: TArray<string>;
    instCounter : integer;
begin
   labels := TStringList.Create;
   sl := TStringList.Create;
   sl.text := srcCode;
   instCounter := 0;
   for i := 0 to sl.Count - 1 do
       begin
       s := sl[i].TrimLeft;

       // Ignore comment lines and empty lines
       if (s <> '') then
          if (s[1] <> '#') then
             begin
             s := RemoveDupSpaces(s);
             splitted := s.Split([' ']);
             if rightStr(splitted[0], 1) = ':' then
                begin
                // get label
                alabel := leftStr (splitted[0], length (splitted[0])-1);
                labels.AddObject(alabel, TObject (instCounter));
                end;
             inc (instCounter);
             end;
       end;
end;


procedure tokenize (srcStr : string; var dest1, dest2, dest3 : string);
var index : integer;
begin
  dest1 := ''; dest2 := ''; dest3 := '';
  index := pos (':', srcStr);
  if index <> 0 then
     begin
     dest1 := leftstr (srcStr, index-1);
     srcStr := trim (rightStr (srcStr, length (srcStr) - index ));
     end;
  index := pos (' ', srcStr);
  if index <> 0 then
     begin
     dest2 := leftStr (srcStr, index-1);
     dest3 := rightStr (srcStr, length (srcStr) - index);
     end
  else
     dest2 := srcStr;
  dest1 := trim (dest1);
  dest2 := trim (dest2);
  dest3 := trim (dest3);
end;


function assembleCode (const srcCode : string) : TProgram;
var sl  : TStringList;
    i : integer;
    s, alabel, astr : string;
    instCounter, labelInt, index : integer;
    opCodeStr, opCodeArgument : string;
begin
   collectLabels (srcCode);

   result := TProgram.Create;
   sl := TStringList.Create;
   try
     sl.text := srcCode;
     instCounter := 0;
     for i := 0 to sl.Count - 1 do
         begin
         s := sl[i].TrimLeft;

         // Ignore comment lines and empty lines
         if (s <> '') then
            if (s[1] <> '#') then
               begin
               s := RemoveDupSpaces(s);
               tokenize(s, alabel, opcodeStr, opCodeArgument);

               if opCodeStr = opCodeNames[oNop]     then begin result.addByteCode (oNop, 0);     inc (instCounter); continue end;
               if opCodeStr = opCodeNames[oAdd]     then begin result.addByteCode (oAdd, 0);     inc (instCounter); continue end;
               if opCodeStr = opCodeNames[oSub]     then begin result.addByteCode (oSub, 0);     inc (instCounter); continue end;
               if opCodeStr = opcodeNames[oMult]    then begin result.addByteCode (oMult, 0);    inc (instCounter); continue end;
               if opCodeStr = opCodeNames[oDivide]  then begin result.addByteCode (oDivide, 0);  inc (instCounter); continue end;
               if opCodeStr = opCodeNames[oMod]     then begin result.addByteCode (oMod, 0);     inc (instCounter); continue end;
               if opCodeStr = opCodeNames[oDivi]    then begin result.addByteCode (oDivi, 0);    inc (instCounter); continue end;

               if opCodeStr = opCodeNames[oInc]     then begin result.addByteCode (oInc, 0);     inc (instCounter); continue end;
               if opCodeStr = opCodeNames[oDec]     then begin result.addByteCode (oDec, 0);     inc (instCounter); continue end;
               if opCodeStr = opCodeNames[oLocalInc] then begin result.addByteCode (oLocalInc, 0);  inc (instCounter); continue end;
               if opCodeStr = opCodeNames[oLocalDec] then begin result.addByteCode (oLocalDec, 0);  inc (instCounter); continue end;
               if opCodeStr = opCodeNames[oNot]     then begin result.addByteCode (oNot, 0);     inc (instCounter); continue end;
               if opCodeStr = opCodeNames[oAnd]     then begin result.addByteCode (oAnd, 0);     inc (instCounter); continue end;
               if opCodeStr = opCodeNames[oOr]      then begin result.addByteCode (oOr, 0);      inc (instCounter); continue end;
               if opCodeStr = opCodeNames[oXor]     then begin result.addByteCode (oXor, 0);     inc (instCounter); continue end;
               if opCodeStr = opCodeNames[oPop]     then begin result.addByteCode (oPop, 0);     inc (instCounter); continue end;
               if opCodeStr = opCodeNames[oIsGt]    then begin result.addByteCode (oIsGt, 0);    inc (instCounter); continue end;
               if opCodeStr = opCodeNames[oIsGte]   then begin result.addByteCode (oIsGte, 0);   inc (instCounter); continue end;
               if opCodeStr = opCodeNames[oIsLt]    then begin result.addByteCode (oIsLt, 0);    inc (instCounter); continue end;
               if opCodeStr = opCodeNames[oIsLte]   then begin result.addByteCode (oIsLte, 0);   inc (instCounter); continue end;
               if opCodeStr = opCodeNames[oIsEq]    then begin result.addByteCode (oIsEq, 0);    inc (instCounter); continue end;
               if opCodeStr = opCodeNames[oIsNotEq] then begin result.addByteCode (oIsNotEq, 0); inc (instCounter); continue end;
               if opCodeStr = opCodeNames[oPrint]   then begin result.addByteCode (oPrint, 0);   inc (instCounter); continue end;
               if opCodeStr = opCodeNames[oPrintln] then begin result.addByteCode (oPrintln, 0); inc (instCounter); continue end;
               if opCodeStr = opCodeNames[oHalt]    then begin result.addByteCode (oHalt, 0);    inc (instCounter); continue end;
               if opCodeStr = opCodeNames[oRet]     then begin result.addByteCode (oRet, 0);     inc (instCounter); continue end;
               if opCodeStr = opCodeNames[oLvecIdx] then begin result.addByteCode (oLvecIdx, 0); inc (instCounter); continue end;
               if opCodeStr = opCodeNames[oSvecIdx] then begin result.addByteCode (oSvecIdx, 0); inc (instCounter); continue end;
               if opCodeStr = opCodeNames[oLocalLvecIdx] then begin result.addByteCode (oLocalLvecIdx, 0); inc (instCounter); continue end;
               if opCodeStr = opCodeNames[oLocalSvecIdx] then begin result.addByteCode (oLocalSvecIdx, 0); inc (instCounter); continue end;
               if opCodeStr = opCodeNames[oCall]    then begin result.addByteCode (oCall, 0);    inc (instCounter); continue end;
               if opCodeStr = opCodeNames[oBuiltin] then begin result.addByteCode (oBuiltin, 0); inc (instCounter); continue end;
               //if opCodeStr = opCodeNames[oLoadModuleSymbol] then
               //   begin
              //    result.addByteCode (oLoadModuleSymbol, strtoint (opCodeArgument));
               //   inc (instCounter);
               //   continue;
               //   end;
               if opCodeStr = opCodeNames[oStoreSymbol] then begin
                  result.addByteCode (oStoreSymbol, strtoint (opCodeArgument), 0);
                  inc (instCounter);
                  continue;
                  end;
                if opCodeStr = opCodeNames[oCreateList] then
                  begin
                  result.addByteCode (oCreateList, strtoint (opCodeArgument), 0);
                  inc (instCounter);
                  continue
                  end;
               if opCodeStr = opCodeNames[oPushi] then
                  begin
                  result.addByteCode (oPushi, strtoint (opCodeArgument), 0);
                  inc (instCounter);
                  continue;
                  end;
               if opCodeStr = opCodeNames[oPushd] then
                  begin
                  raise Exception.Create('Note implemented Error Message');
//                  result.addByteCode (oPushd, currentModule.constantValueTable.Add (TConstantValueElement.Create(strtofloat (opCodeArgument))));
                  inc (instCounter);
                  continue;
                  end;
               if opCodeStr = opCodeNames[oPushb] then
                  begin
                  if opCodeArgument = 'true' then
                     result.addByteCode (oPushb, true, 0);
                  if opCodeArgument = 'false' then
                    result.addByteCode (oPushb, false, 0);
                  if (opCodeArgument <> 'false') and (opCodeArgument <> 'true') then
                    raise Exception.Create('Expecting boolean true or false in pushb');
                  inc (instCounter);
                  continue;
                  end;
               if opCodeStr = opCodeNames[oPushs] then
                  begin
                  astr := opCodeArgument;
                  // Check for double quotes at start and end of string
                  if (astr[1] = '"') and (astr[length (astr)] = '"') then
                     begin
                     // Strip double quotes from the string
                     astr := astr.Substring (0, astr.Length - 1);
                     delete (astr, 1, 1);
                     raise Exception.Create('Not implemented Error Message');
                     //result.addByteCode (oPUSHs, constantValueTable.Add (TConstantValueElement.Create (astr)));
                     inc (instCounter);
                     continue;
                     end
                  else
                     raise Exception.Create('Expecting string as argument to pushs');
                  end;
                if opCodeStr = opCodeNames[oJmp]   then
                  begin
                  index := labels.IndexOf (opCodeArgument);
                  if index <> -1 then
                     begin
                     labelInt := integer (labels.Objects[index]);
                     result.addByteCode (oJMP, labelInt - instCounter, 0);
                     end
                  else
                     raise Exception.Create ('Unable to locate label specificed in jmp opcode');
                  inc (instCounter);
                  continue;
                  end;
               if opCodeStr = opCodeNames[oJmpIfTrue]   then
                  begin
                  index := labels.IndexOf (opCodeArgument);
                  if index <> -1 then
                     begin
                     labelInt := integer (labels.Objects[index]);
                     result.addByteCode (oJmpIfTrue, labelInt - instCounter, 0);
                     end
                  else
                     raise Exception.Create ('Unable to locate label specificed in jmpIfTrue opcode');

                  inc (instCounter);
                  continue;
                  end;
               if opCodeStr = opCodeNames[oJmpIfFalse] then
                  begin
                  index := labels.IndexOf (opCodeArgument);
                  if index <> -1 then
                     begin
                     labelInt := integer (labels.Objects[index]);
                     result.addByteCode (oJmpIfFalse, labelInt - instCounter, 0);
                     end
                  else
                     raise Exception.Create ('Unable to locate label specificed in jmpIfFalse opcode');
                  inc (instCounter);
                  continue;
                  end;

                 raise Exception.Create ('Error: Unknown op code in program: ' + opCodeStr);
                 end;
              end;
   finally
     sl.Free;
     labels.Free;
   end;
end;


function dissassemble (module : TModule; aProgram : TProgram) : string;
var i : integer;
begin
  result := '';
  for i := 0 to aProgram.count - 1 do
      begin
      result := result + format ('%3d', [i]);
      case aProgram.code[i].opCode of
        oNop        : result := result + '  ' + opCodeNames[oNop]    + ' ' + sLineBreak;
        oHalt       : result := result + '  ' + opCodeNames[oHalt]   + sLineBreak;

        oPushi      : result := result + '  ' + opCodeNames[oPushi] + ' ' + inttostr (aProgram.code[i].index) + sLineBreak;
        oPushb      : result := result + '  ' + opCodeNames[oPushb] + ' ' + inttostr (aProgram.code[i].index) + sLineBreak;
        oPushd      : result := result + '  ' + opCodeNames[oPushd] + ' ' + floattostr (module.moduleProgram.constantValueTable[aProgram.code[i].index].dValue) + sLineBreak;
        oPushs      : result := result + '  ' + opCodeNames[oPushs] + ' "' + module.moduleProgram.constantValueTable[aProgram.code[i].index].sValue.value + '"' + sLineBreak;
        oPushNone   : result := result + '  ' + opCodeNames[oPushNone] + sLineBreak;
        oDup        : result := result + '  ' + opCodeNames[oDup] + sLineBreak;
        oPopDup     : result := result + '  ' + opCodeNames[oPopDup] + sLineBreak;

        oAdd        : result := result + '  ' + opCodeNames[oAdd]    + sLineBreak;
        oSub        : result := result + '  ' + opCodeNames[oSub]    + sLineBreak;
        oMult       : result := result + '  ' + opCodeNames[oMult]   + sLineBreak;
        oDivide     : result := result + '  ' + opCodeNames[oDivide] + sLineBreak;
        oDivi       : result := result + '  ' + opCodeNames[oDivI]   + sLineBreak;
        oMod        : result := result + '  ' + opCodeNames[oMod]    + sLineBreak;
        oPower      : result := result + '  ' + opCodeNames[oPower]  + sLineBreak;
        oUmi        : result := result + '  ' + opCodeNames[oUmi]    + sLineBreak;
        oInc        : result := result + '  ' + opCodeNames[oInc]    + ' ' + floattostr (aProgram.code[i].float) + sLineBreak;
        oDec        : result := result + '  ' + opCodeNames[oDec]    + ' ' + inttostr (aProgram.code[i].index) + ', ' + floattostr (aProgram.code[i].float) + sLineBreak;
        oLocalInc   : result := result + '  ' + opCodeNames[oLocalInc] + sLineBreak;
        oLocalDec   : result := result + '  ' + opCodeNames[oLocalDec] + sLineBreak;
        oOr         : result := result + '  ' + opCodeNames[oOr]     + sLineBreak;
        oAnd        : result := result + '  ' + opCodeNames[oAnd]    + sLineBreak;
        oXor        : result := result + '  ' + opCodeNames[oXor]    + sLineBreak;
        oNot        : result := result + '  ' + opCodeNames[oNot]    + sLineBreak;
        oPop        : result := result + '  ' + opCodeNames[oPop]    + sLineBreak;

        oIsGt       : result := result + '  ' + opCodeNames[oIsGt]   + sLineBreak;
        oIsLt       : result := result + '  ' + opCodeNames[oIsLt]   + sLineBreak;
        oIsGte      : result := result + '  ' + opCodeNames[oIsGte]   + sLineBreak;
        oIsLte      : result := result + '  ' + opCodeNames[oIsLte]   + sLineBreak;
        oIsEq       : result := result + '  ' + opCodeNames[oIsEq]   + sLineBreak;
        oIsNotEq    : result := result + '  ' + opCodeNames[oIsNotEq]   + sLineBreak;

        oPrint      : result := result + '  ' + opCodeNames[oPrint]  + sLineBreak;
        oPrintln    : result := result + '  ' + opCodeNames[oPrintln]  + sLineBreak;
        oAssertTrue : result := result + '  ' + opCodeNames[oAssertTrue]  + sLineBreak;
        oAssertFalse: result := result + '  ' + opCodeNames[oAssertFalse]  + sLineBreak;
        oSetColor   : result := result + '  ' + opCodeNames[oSetColor]  + sLineBreak;

        oJmp        : result := result + '  ' + opCodeNames[oJmp]   + ' ' + inttostr (aProgram.code[i].index) + sLineBreak;
        oJmpIfTrue  : result := result + '  ' + opCodeNames[ojmpIfTrue] + ' ' + inttostr (aProgram.code[i].index) + sLineBreak;
        oJmpIfFalse : result := result + '  ' + opCodeNames[oJmpIfFalse] + ' ' + inttostr (aProgram.code[i].index) + sLineBreak;

        oStoreSymbol  : result := result + '  ' + opCodeNAmes[oStoreSymbol] + ' ' + aProgram.code[i].symbolName + sLineBreak;
         oLoadSymbol  : result := result + '  ' + opCodeNames[oLoadSymbol]  + ' ' + aProgram.code[i].symbolName + sLineBreak;
          oLoadAttr   : result := result + '  ' + opCodeNames[oLoadAttr] + ' ' + aProgram.code[i].symbolName + sLineBreak;
         oStoreAttr   : result := result + '  ' + opCodeNames[oStoreAttr] + ' ' + aProgram.code[i].symbolName + sLineBreak;
        oStoreLocal   : result := result + '  ' + opCodeNAmes[oStoreLocal] + ' ' + inttostr (aProgram.code[i].index) + sLineBreak;
        oLoadLocal    : result := result + '  ' + opCodeNames[oLoadLocal]  + ' ' + inttostr (aProgram.code[i].index) + sLineBreak;
        oBuildSlice   : result := result + '  ' + opCodeNames[oBuildSlice] + sLineBreak;
        oSliceAll     : result := result + '  ' + opCodeNames[oSliceAll] + sLineBreak;

        //oCreateArray  : result := result + '  ' + opCodeNames[oCreateArray] + ' ' + inttostr (aProgram.code[i].index) + sLineBreak;
        oCall       : result := result + '  ' + opCodeNames[oCall]  + sLineBreak;
        oBuiltin    : result := result + '  ' + opCodeNames[oBuiltin] + ' ' + inttostr (aProgram.code[i].index) + sLineBreak;
        oRet        : result := result + '  ' + opCodeNames[oRet]     + ' ' + sLineBreak;
        oLvecIdx    : result := result + '  ' + opCodeNames[oLvecIdx] + sLineBreak;
        oSvecIdx    : result := result + '  ' + opCodeNames[oSvecIdx] + sLineBreak;
        oLocalLvecIdx    : result := result + '  ' + opCodeNames[oLocalLvecIdx] + ' ' + inttostr (aProgram.code[i].index) + sLineBreak;
        oLocalSvecIdx    : result := result + '  ' + opCodeNames[oLocalSvecIdx] + ' ' + inttostr (aProgram.code[i].index) + sLineBreak;
        oCreateList : result := result + '  ' + opCodeNames[oCreateList] + ' ' + inttostr (aProgram.code[i].index) + sLineBreak;
        oImportModule : result := result + '  ' + opCodeNames[oImportModule] + ' ' + aProgram.code[i].moduleName + sLineBreak;
      else
        writeln ('Unknown opcode during dissassembly: ', aProgram.code[i].opCode);
      end;
      end;
end;

end.

