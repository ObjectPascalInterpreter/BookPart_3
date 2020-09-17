unit uRunCode;

// Ths source is distributed under Apache 2.0

// Copyright (C)  2019-2020 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

Uses uVM,
     uMachineStack,
     uScanner,
     uModule,
     uSyntaxAnalysis,
     uAST;


type
  TRunFramework = class (TObject)
     sc : TScanner;
     mainModule : TModule;
     sy : TSyntaxAnalysis;
     vm : TVM;

     function  compileCode (const src : string;  var moduleIndex : integer; bolShowAssembler : boolean) : boolean;
     procedure runCode (const src : string; bolShowAssembler : boolean);
     procedure getAllocatedSymbols;

     destructor Destroy; override;
  end;

  TPrintClass = class (TObject)
      procedure print (st : PMachineStackRecord);
      procedure println (st : PMachineStackRecord);
  end;

implementation

uses uCommands, SysUtils, StrUtils, uAssembler, uSymbolTable;

// Print methods to support output from the Vm.
// -------------------------------------------------------------------------------
procedure TPrintClass.print (st : PMachineStackRecord);
begin
  if st <> nil then
     case st.stackType of
          stNone    : begin end; //write ('undefined value'); end;
          stInteger : write (st.iValue);
          stDouble  : write (Format('%g', [st.dValue]));
          stString  : write (st.sValue.value);
          stBoolean : if st.bValue = True then
                         write ('True')
                      else
                         write ('False');
          stList    : begin
                      write (st.lValue.listToString);
                      end;
     else
        writeln ('Unrecognized value from print');
     end;
end;


procedure TPrintClass.println (st : PMachineStackRecord);
begin
  print (st);
  writeln;
end;


// ------------------------------------------------------------------------------

destructor TRunFramework.Destroy;
begin
  sc.Free;
  sy.Free;
  inherited;
end;


function TRunFramework.compileCode (const src : string; var moduleIndex : integer; bolShowAssembler : boolean) : boolean;
var i : integer;
    module : TModule;
    root : TASTNode;
begin
  result := True;
  mainModule.clearCode;
  sc.scanString(src);
  sc.nextToken;
  try
    if sc.token = TTokenCode.tModule then
       moduleIndex := sy.parseModule
    else
       begin
       moduleIndex := 0;
       root := sy.mainProgram(mainModule);
       writeln (displayAST (root));
       if bolShowAssembler then
          begin
          for i := 0 to mainModule.symbolTable.Count - 1 do
              if mainModule.symbolTable[i].symbolType = symUserFunc then
                 writeln (dissassemble(mainModule.symbolTable[i].fValue.funcCode));
          writeln (dissassemble(mainModule.code));
         end;
       end;
  except
    on e:exception do
       begin
       setGreen;
       writeln ('ERROR ' + '[line ' + inttostr (sc.tokenElement.lineNumber) + ', column: ' + inttostr (sc.tokenElement.columnNumber) + '] ' + e.Message);
       setWhite;
       result := False;
       end;
  end;
end;


procedure TRunFramework.runCode (const src : string; bolShowAssembler : boolean);
var st :PMachineStackRecord;
    printObj : TPrintClass;
    moduleIndex : integer;
begin
   if compileCode (src, moduleIndex, bolShowAssembler) then
      begin
      printObj := TPrintClass.Create;
      try
        vm := TVM.Create;
        registerRuntimeWithConsole (self);
        vm.registerPrintCallBack(printObj.print);
        vm.registerPrintlnCallBack(printObj.println);
        vm.runModule (moduleList[moduleIndex]);
        while vm.stackHasEntry do
             begin
             st := vm.pop;
             case st.stackType of
              stNone    : begin end;
              stInteger : writeln (st.iValue);
              stBoolean : writeln (BoolToStr(st.bValue, True));
              stDouble  : writeln (Format('%g', [st.dValue]));
              stString  : writeln (st.sValue.value);
              stList    : writeln (st.lValue.listToString());
              stModule  : writeln ('Module: ' + st.mValue.name + ', ' + st.mValue.helpStr);
              stFunction: writeln ('Function');
             else
               writeln ('Unrecognized type of value returned from virtual machine');
             end;
             end;
      finally
        printObj.Free;
        FreeAndNil (vm);
      end;
      end;
end;


procedure TRunFrameWork.getAllocatedSymbols;
var i, len : integer; f : TUserFunction; astr: string;
begin
  writeln (Format('%-7s%-14s%-12s%-22s%-10s', ['Index', 'Name', 'Type', 'Value', 'Size']));
  // Write out all the symbols
  for i := 0 to mainModule.symbolTable.Count - 1 do
      begin
      if mainModule.symbolTable[i].symbolType <> symUndefined then
         begin
         write (Format ('%-7d', [i]), Format ('%-14s', [mainModule.symbolTable[i].symbolName]));
         case mainModule.symbolTable[i].symbolType of
              symInteger: writeln (Format ('%-12s%-22d%-6d',  ['int', mainModule.symbolTable[i].iValue, sizeof (integer)]));
              symBoolean: writeln (Format ('%-12s%-22s%-6d',  ['boolean', boolToStr (mainModule.symbolTable[i].bValue), sizeof (Boolean)]));
              symDouble : writeln (Format ('%-12s%-22g%-6d%', ['float', mainModule.symbolTable[i].dValue, sizeof (double)]));
              symString : begin
                          astr :=  '"' + leftStr (mainModule.symbolTable[i].sValue.value, 50);
                          if length (mainModule.symbolTable[i].sValue.value) > 50 then
                             astr := astr + '...."'
                          else
                             astr := astr + '"';

                          write   (Format ('%-12s', ['string']),  Format ('%-22s', [astr]));
                          len := length (mainModule.symbolTable[i].sValue.value);
                          writeln (Format ('%-12d', [len]));
                          end;
              symList   : writeln (Format ('%-12s%-22s%-6d%', ['list', mainModule.symbolTable[i].lValue.listToString, mainModule.symbolTable[i].lValue.getsize()]));
              symUserFunc: begin
                           f := mainModule.symbolTable[i].fValue;
                           writeln (Format ('%-12s%-22s%-12d', ['ufunc', 'NA', f.getSize()]));
                           end;
              symUndefined : continue;
         else
             raise Exception.Create('This type not yet supported in getMemoryUsed');
         end;
         end;
      end;
end;

end.
