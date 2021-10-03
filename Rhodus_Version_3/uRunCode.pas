unit uRunCode;

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
     uConstructAST,
     uAST,
     uCompile,
     uLibModule,
     uBuiltInGlobal,
     uBuiltInMath,
     uBuiltInRandom,
     uBuiltInStr,
     uBuiltinList,
     uBuiltInTurtle,
     uMemoryManager,
     uObjectSupport;

type
  TPrintClass = class (TObject)
      currentColor : string;
      procedure print (st : PMachineStackRecord);
      procedure println (st : PMachineStackRecord);
      procedure setColor (st : PMachineStackRecord);
  end;

  TRunFramework = class (TObject)
    private
      vmMemory : integer;
      function memAllocatedByVm : integer;
    public
      sc : TScanner;
      ast : TConstructAST;
      vm : TVM;
      showAssembler : boolean;
      printObj : TPrintClass;

      function  getVersion : string;
      function  compileCode (const src : string;  var module : TModuleLib; interactive : boolean) : boolean;
      procedure compileAndRun (const src : string; interactive : boolean);
      procedure getAllocatedSymbols (argument : string);
      procedure showByteCode (module : TModule);
      procedure runCode (module : TModule; interactive : boolean);

      constructor Create;
      destructor Destroy; override;
  end;


implementation

uses uCommands,
     SysUtils,
     StrUtils,
     uAssembler,
     uOpCodes,
     uBuiltInOS,
     uTerminal,
     uRhodusTypes,
     uEnvironment;


// Print methods to support output from the VM
// -------------------------------------------------------------------------------
procedure TPrintClass.print (st : PMachineStackRecord);
begin
  uTerminal.setColor (currentColor);

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
          stModule  : begin
                      write (st.module.name);
                      end;
          stFunction: begin
                      write (st.fValue.name);
                      end
     else
        writeln ('Unrecognized value from print');
     end;
end;


procedure TPrintClass.println (st : PMachineStackRecord);
begin
  print (st);
  writeln;
end;


procedure TPrintClass.setColor (st : PMachineStackRecord);
begin
  if st.stackType <> stString then
     writeln ('Expecting a string in setColor: white, red, green, blue, yellow, purple, aqua')
  else
     currentColor := st.sValue.value;
  uTerminal.setColor(currentColor);
end;

// ------------------------------------------------------------------------------

constructor TRunFramework.Create;
var astr : string;
begin
  showAssembler := False;
  mainModule := TModuleLib.Create (TSymbol.mainModuleId, 'Main Module');  // mainModule is declared in uModule

  addMethodsToModule (mainModule);

  addAllBuiltInLibraries (mainModule);

  printObj := TPrintClass.Create;     // Print services that the VM can use
  sc  := TScanner.Create;             // Create the lexical scanner
  ast := TConstructAst.Create (sc);   // Create the parser that will generate the AST

  vmMemory := memAllocatedByVm;

  if FileExists (launchEnvironment.moduleDir + '\\startup.rh') then
     begin
     astr := TFile.ReadAllText(launchEnvironment.moduleDir + '\\startup.rh');
     if not compileCode(astr, mainModule, False) then
        writeln ('Errors in startup script')
     else
        runCode(mainModule, False);
     end;
end;


destructor TRunFramework.Destroy;
begin
  ast.Free;
  sc.Free;
  printObj.Free;
  mainModule.free;
  memoryList.freeGarbage;
  inherited;
end;


function  TRunFramework.getVersion : string;
begin
  result := uBuiltInOS.RHODUS_VERSION;
end;


function TRunFramework.compileCode (const src : string; var module : TModuleLib; interactive : boolean) : boolean;
var root : TASTNode;
    compiler : TCompiler;
begin
  result := True;
  // Note we don't clear the symboltables because the next script
  // may need to refer to entries in the symbol table.
  module.clearCode;
  sc.scanString(src);
  sc.nextToken;
  try
   // The compiler will generate vm byte code and store it in module
   compiler := TCompiler.Create (module);
   try
      compiler.interactive := interactive;
      try
        root := ast.parseProgram;

        if bolShowAssembler then
           writeln (displayAST (root));
        try
          compiler.startCompilation (module, root);
          mainModule.code.addByteCode(oHalt);
        except
          on e: ESyntaxException do
             begin
             setGreen;
             writeln ('ERROR ' + '[line ' + inttostr (e.lineNumber) + ', column: ' + inttostr (e.columnNumber) + '] ' + e.errorMsg);
             setWhite;
             result := False;
             end;
        end;
      finally
        //freePool;
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


function TRunFramework.memAllocatedByVm : integer;
var vm : TVM;
    start : integer;
begin
  start := getMemoryAllocated();
  vm := TVM.Create;
  result := getMemoryAllocated() - start;
  vm.Free;
end;


procedure TRunFramework.showByteCode (module : TModule);
var key : string;
begin
  for key in mainModule.symbolTable.keys do
      if mainModule.symbolTable.items[key] <> nil then
         if mainModule.symbolTable.Items[key].symbolType = symUserFunc then
            begin
            if mainModule.symbolTable.items[key].fValue.isbuiltInFunction then
               continue
               //writeln ('No code for builtin function')
            else
               writeln (dissassemble(mainModule, mainModule.symbolTable.items[key].fValue.funcCode));
            end;
  writeln (dissassemble(mainModule, mainModule.code));
end;


procedure TRunFramework.runCode (module : TModule; interactive : boolean);
var st :PMachineStackRecord;
    key : string;
begin
      try
        vm := TVM.Create;
        vm.interactive := interactive;

        registerRuntimeWithConsole (self);
        vm.registerPrintCallBack(printObj.print);
        vm.registerPrintlnCallBack(printObj.println);
        vm.registerSetColorcallBack (printObj.setColor);

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
                stModule  : writeln ('Module: ' + st.module.name + ' ' + st.module.helpStr);
                stFunction: writeln ('Function: ' + st.fValue.moduleRef.name + '.' + st.fValue.name);
                stObjectMethod : begin writeln ('Object: ' + st.oValue.name); vm.pop(); end;   // pop the operand
               else
                 writeln ('Unrecognized type of value returned from virtual machine');
               end;
               end;
          if bolShowAssembler then
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


procedure TRunFramework.compileAndRun (const src : string; interactive : boolean);
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
        vm.registerPrintCallBack(printObj.print);
        vm.registerPrintlnCallBack(printObj.println);
        vm.registerSetColorcallBack (printObj.setColor);

        try
          //showByteCode(mainModule);
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
                stModule  : writeln ('Module: ' + st.module.name + ' ' + st.module.helpStr);
                stFunction: writeln ('Function: ' + st.fValue.moduleRef.name + '.' + st.fValue.name);
               else
                 writeln ('Unrecognized type of value returned from virtual machine');
               end;
               end;
          if bolShowAssembler then
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


procedure TRunFrameWork.getAllocatedSymbols (argument : string);
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
