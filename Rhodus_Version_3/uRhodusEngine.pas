unit uRhodusEngine;

// This source is distributed under Apache 2.0

// Copyright (C)  2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

Uses Generics.Collections,
     IOUtils,
     uVM,
     uSymbolTable,
     uMachineStack,
     uScanner,
     uScannerTypes,
     uConstructAST,
     uAST,
     uCompile,
     uLibModule,
     uBuiltInGlobal,
     uBuiltInMath,
     uBuiltInRandom,
     uBuiltInArray,
     uBuiltInStr,
     uBuiltinList,
     uBuiltInTurtle,
     uMemoryManager,
     uObjectSupport,
     uSyntaxParser;

type
  TCallBackFunction = procedure (st : PMachineStackRecord);
  TIntCallBackFunction = procedure (x : integer);

  TRhodus = class (TObject)
    private
      vmMemory : integer;
      sc : TScanner;
      syntaxParser : TSyntaxParser;
      ast : TConstructAST;
      root : TASTNode;
      vm : TVM;
      printCallBack : TCallBackFunction;
      printLnCallBack : TCallBackFunction;
      setColorCallBack : TCallBackFunction;
      function memAllocatedByVm : integer;
    public
      class var bolShowTree : boolean;
      class var bolShowByteCode : boolean;

      procedure setPrintCallBack (printcallBack : TCallBackFunction);
      procedure setPrintLnCallBack (printLnCallBack : TCallBackFunction);
      procedure setSetColorCallBack (setColorCallBack : TCallBackFunction);

      function  getVM : TVM;
      function  getVersion : string;
      function  compileToAST (sourceCode : string; var syntaxError : TSyntaxError) : boolean;
      function  generateByteCode (interactive : boolean; var compileError : TCompilerError) : boolean;
      function  compileCode (const src : string;  var module : TModuleLib; interactive : boolean) : boolean;
      procedure compileAndRun (const src : string; interactive : boolean);
      procedure getAllocatedSymbols (argument : string);
      procedure showByteCodeMethod (module : TModule);
      function  runCode (module : TModule; interactive : boolean; printcallBack : TCallBackFunction) : boolean;

      constructor Create;
      destructor  Destroy; override;
  end;


implementation

uses uCommands,
     SysUtils,
     StrUtils,
     uAssembler,
     uOpCodes,
     uBuiltInOS,
     uBuiltInConfig,
     uTerminal,
     uRhodusTypes,
     uEnvironment,
     uVMExceptions;


// ------------------------------------------------------------------------------


constructor TRhodus.Create;
var astr : string;
begin
  mainModule := TModuleLib.Create (TSymbol.mainModuleId, 'Main Module');  // mainModule is declared in uModule

  addGlobalMethodsToModule (mainModule);

  addAllBuiltInLibraries (mainModule);

  sc  := TScanner.Create;             // Create the lexical scanner
  syntaxParser := TSyntaxParser.Create (sc);  // This only does a syntax analysis
  ast := TConstructAst.Create (syntaxParser.tokenVector);   // Create the parser that will generate the AST

  vmMemory := memAllocatedByVm;

  if FileExists (launchEnvironment.moduleDir + '\\startup.rh') then
     begin
     astr := TFile.ReadAllText(launchEnvironment.moduleDir + '\\startup.rh');
     try
       if not compileCode(astr, mainModule, False) then
          begin
          writeln ('Errors while compiling startup script, startup.rh (correct the startup script). Type any key to continue.');
          readln;
          end
       else
          begin
          if not runCode(mainModule, False, nil) then
             begin
             writeln ('Error when executing startup script, startup.rh. Correct the startup script. Type any key to continue.');
             readln;
             end;
          end;
     except

     end;
     end;
     end;


destructor TRhodus.Destroy;
begin
  ast.Free;
  sc.Free;
  syntaxParser.Free;
  mainModule.free;
  memoryList.freeGarbage;
  inherited;
end;


function  TRhodus.getVersion : string;
begin
  result := uBuiltInConfig.RHODUS_VERSION;
end;


function TRhodus.getVM : TVM;
begin
  result := vm;
end;


function TRhodus.compileToAST (sourceCode : string; var syntaxError : TSyntaxError) : boolean;
begin
  try
    result := True;
    sc.scanString(sourceCode);
    if syntaxParser.syntaxCheck(syntaxError) then
       begin
       root := ast.constructAST ();
       if bolShowTree then
          writeln (displayAST (root));
       end
    else
      result := False;
  except
    on e: exception do
       begin
       syntaxError.errorMsg := 'Internal Error: ' + e.Message;
       result := False;
       end;
  end;
end;


function TRhodus.generateByteCode (interactive : boolean; var compileError : TCompilerError) : boolean;
var compiler : TCompiler;
    errMsg : string;
    error : TSyntaxError;
    compilerError : TCompilerError;
begin
  result := True;
  // Note we don't clear the symboltables because the next script
  // may need to refer to entries in the symbol table.
  mainModule.clearCode;
  try
    try
     // The compiler will generate vm byte code and store it in module
     compiler := TCompiler.Create (mainModule);
     try
        compiler.interactive := interactive;
        if compiler.startCompilation (mainModule, root, compileError) then
           mainModule.code.addByteCode(oHalt)
        else
          result := False;
     finally
       compiler.Free;
     end;
  finally
    root.freeAST();
  end;
    except
      on e:exception do
         begin
         setGreen;
         compileError.errorMsg := 'ERROR ' + '[line ' + inttostr (sc.tokenElement.lineNumber) + ', column: ' + inttostr (sc.tokenElement.columnNumber) + '] ' + e.Message;
         setWhite;
         result := False;
         end;
    end;
end;

// Carries out the following:
//   Does an initial syntax check
//   If succssful it Creates the AST
//   Compiles to bytecode

function TRhodus.compileCode (const src : string; var module : TModuleLib; interactive : boolean) : boolean;
var root : TASTNode;
    compiler : TCompiler;
    errMsg : string;
    error : TSyntaxError;
    compilerError : TCompilerError;
begin
  result := True;
  // Note we don't clear the symboltables because the next script
  // may need to refer to entries in the symbol table.
  module.clearCode;
  sc.scanString(src);
  try
   // The compiler will generate vm byte code and store it in module
   compiler := TCompiler.Create (module);
   try
      compiler.interactive := interactive;
      try
          if not syntaxParser.syntaxCheck (error) then
             begin
             writeln ('ERROR ' + '[line ' + inttostr (error.lineNumber) + ', column: ' + inttostr (error.columnNumber) + '] ' + error.errorMsg);
             result := False;
             exit;
             end;
        root := ast.constructAST;

        if bolShowByteCode  then
           writeln (displayAST (root));
        try
          if not compiler.startCompilation (module, root, compilerError) then
             begin
             setGreen;
             writeln ('ERROR ' + '[line ' + inttostr (compilerError.lineNumber) + ', column: ' + inttostr (compilerError.columnNumber) + '] ' + compilerError.errorMsg);
             setWhite;
             result := False;
             exit;
             end;

          mainModule.code.addByteCode(oHalt);
        except
          on e: ERuntimeException do
             begin
             setGreen;
             writeln ('ERROR: ' + e.Message);
             setWhite;
             result := False;
             end;
        end;
      finally
        root.freeAST();
      end;
   finally
     compiler.Free;
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


function TRhodus.memAllocatedByVm : integer;
var vm : TVM;
    start : integer;
begin
  start := getMemoryAllocated();
  vm := TVM.Create;
  result := getMemoryAllocated() - start;
  vm.Free;
end;


procedure TRhodus.showByteCodeMethod (module : TModule);
var key : string;
begin
  for key in mainModule.symbolTable.keys do
      if mainModule.symbolTable.items[key] <> nil then
         if mainModule.symbolTable.Items[key].symbolType = symUserFunc then
            begin
            if not mainModule.symbolTable.items[key].fValue.isbuiltInFunction then
               writeln (dissassemble(mainModule, mainModule.symbolTable.items[key].fValue.funcCode));
            end;
  writeln (dissassemble(mainModule, mainModule.code));
end;


procedure TRhodus.setPrintCallBack (printCallBack : TCallBackFunction);
begin
  self.printCallBack := printCallBack;
end;

procedure TRhodus.setPrintLnCallBack (printLnCallBack : TCallBackFunction);
begin
  self.printLnCallBack := printLnCallBack;
end;

procedure TRhodus.setSetColorCallBack (setColorCallBack : TCallBackFunction);
begin
  self.setColorCallBack := setColorCallBack;
end;

function TRhodus.runCode (module : TModule; interactive : boolean; printcallBack : TCallBackFunction) : boolean;
var st :PMachineStackRecord;
    key : string;
begin
  result := True;
  try
        vm := TVM.Create;
        vm.interactive := interactive;

        registerRuntimeWithConsole (self);

        vm.registerPrintCallBack(printcallBack);
        vm.registerPrintlnCallBack(printlnCallBack);
        vm.registerSetColorcallBack (setColorCallBack);

        try
          vm.runModule (module);

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
                stArray   : writeln (st.aValue.arrayToString());
                stModule  : writeln ('Module: ' + st.module.name + ' ' + st.module.helpStr);
                stFunction: writeln ('Function: ' + st.fValue.moduleRef.name + '.' + st.fValue.name);
                stObjectMethod : begin writeln ('Object: ' + st.oValue.name); vm.pop(); end;   // pop the operand
               else
                 writeln ('Unrecognized type of value returned from virtual machine');
               end;
               end;
          except
          on e:exception do
             begin
              setGreen;
              writeln ('ERROR: ' + e.Message);
              setWhite;
              result := False;
              end;
        end;
  finally
    FreeAndNil (vm);
  end;
end;


procedure TRhodus.compileAndRun (const src : string; interactive : boolean);
var st :PMachineStackRecord;
    key : string;
    initialMem : integer;
begin
   initialMem := getMemoryAllocated();
   if compileCode (src, mainModule, interactive) then
      begin
      try
        vm := TVM.Create;
        vm.interactive := interactive;

        registerRuntimeWithConsole (self);
 
        vm.registerPrintCallBack(printcallBack);
        vm.registerPrintlnCallBack(printlnCallBack);
        vm.registerSetColorcallBack (setColorCallBack);

        try
         // if bolShowByteCode then
        //     showByteCodeMethod(mainModule);

          vm.runModule (mainModule);

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
                stArray   : writeln (st.aValue.arrayToString());
                stModule  : writeln ('Module: ' + st.module.name + ' ' + st.module.helpStr);
                stFunction: writeln ('Function: ' + st.fValue.moduleRef.name + '.' + st.fValue.name);
               else
                 writeln ('Unrecognized type of value returned from virtual machine');
               end;
               end;
          if bolShowByteCode then
              begin
              for key in mainModule.symbolTable.keys do
                  if mainModule.symbolTable.items[key] <> nil then
                     if mainModule.symbolTable.Items[key].symbolType = symUserFunc then
                        begin
                        if mainModule.symbolTable.items[key].fValue.isbuiltInFunction then
                           writeln ('No code for builtin function')
                        else
                           writeln (dissassemble(mainModule, mainModule.symbolTable.items[key].fValue.funcCode));
                        end;
              writeln (dissassemble(mainModule, mainModule.code));
             end;

        except
          on e:exception do
             begin
              setGreen;
              writeln ('ERROR: ' + e.Message);
              setWhite;
              end;
        end;

      finally
        FreeAndNil (vm);
      end;
      end;
end;


procedure TRhodus.getAllocatedSymbols (argument : string);
var i, len : integer; f : TUserFunction; astr: string;
    module : TModule;
begin
  //if argument <> '' then
  //   begin
  //   if not moduleList.find (argument, module) then
  //      raise Exception.Create('Module: ' + argument + ' does not exist');
  //   end
  //else
   //  module := nil;

//  module := moduleList[moduleIndex];
//  writeln (Format('%-7s%-14s%-12s%-22s%-10s', ['Index', 'Name', 'Type', 'Value', 'Size']));
//  // Write out all the symbols
//  for i := 0 to module.symbolTable.Count - 1 do
//      begin
//      if module.symbolTable[i] = nil then
//         continue;
//
//      if moduleList[moduleIndex].symbolTable[i].symbolType <> symUndefined then
//         begin
//         write (Format ('%-7d', [i]), Format ('%-14s', [module.symbolTable[i].symbolName]));
//         case module.symbolTable[i].symbolType of
//              symInteger: writeln (Format ('%-12s%-22d%-6d',  ['int', module.symbolTable[i].iValue, sizeof (integer)]));
//              symBoolean: writeln (Format ('%-12s%-22s%-6d',  ['boolean', boolToStr (module.symbolTable[i].bValue), sizeof (Boolean)]));
//              symDouble : writeln (Format ('%-12s%-22g%-6d%', ['float', module.symbolTable[i].dValue, sizeof (double)]));
//              symString : begin
//                          astr :=  '"' + leftStr (module.symbolTable[i].sValue.value, 50);
//                          if length (module.symbolTable[i].sValue.value) > 50 then
//                             astr := astr + '...."'
//                          else
//                             astr := astr + '"';
//
//                          write   (Format ('%-12s', ['string']),  Format ('%-22s', [astr]));
//                          len := length (module.symbolTable[i].sValue.value);
//                          writeln (Format ('%-12d', [len]));
//                          end;
//              symList   : writeln (Format ('%-12s%-22s%-6d%', ['list', module.symbolTable[i].lValue.listToString, module.symbolTable[i].lValue.getsize()]));
//              symUserFunc: begin
//                           f := module.symbolTable[i].fValue;
//                           writeln (Format ('%-12s%-22s%-12d', ['ufunc', 'NA', f.getSize()]));
//                           end;
//              symUndefined : continue;
//         else
//             raise Exception.Create('This type not yet supported in getMemoryUsed');
//         end;
//         end;
//      end;
end;

end.
