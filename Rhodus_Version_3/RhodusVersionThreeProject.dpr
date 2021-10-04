program RhodusVersionThreeProject;

{$OVERFLOWCHECKS ON}
{$RANGECHECKS ON}

{$WARN SYMBOL_PLATFORM OFF}  // Stops Win32Check warnings


// Source code for Writing an Interpreter in Object Pascal: Part III

// Rhodus langage Version 3

// Developed under Delphi for Windows and Mac platforms.
// *** This source is distributed under Apache 2.0 ***

// Copyright (C) 2018-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

// This is not a Delphi builtin console application, instead we allocate our own console. This
// is to prevent flashing when we move the console window to the center of the screen

//{$  APPTYPE CONSOLE}

{$R *.res}

{$define LogErrorsToFile}
{$define LogMemoryLeakDetailToFile}

uses
  {$IFDEF DEBUG}
  {$ENDIF }
  Windows,
  ShellAPI,
  System.SysUtils,
  System.StrUtils,
  Math,
  IOUtils,
  Classes,
  Vcl.GraphUtil,
  Vcl.Graphics,
  System.Types,
  System.Generics.Defaults,
  System.Generics.Collections,
  uScanner in 'uScanner.pas',
  FastMM4Messages in '..\..\Library\FastMM\FastMM4Messages.pas',
  uFindWindows in 'uFindWindows.pas',
  uAssembler in '..\VirtualMachine\uAssembler.pas',
  uConstantTable in '..\VirtualMachine\uConstantTable.pas',
  uListObject in '..\VirtualMachine\uListObject.pas',
  uMachineStack in '..\VirtualMachine\uMachineStack.pas',
  uMemoryManager in '..\VirtualMachine\uMemoryManager.pas',
  uOpCodes in '..\VirtualMachine\uOpCodes.pas',
  uRhodusObject in '..\VirtualMachine\uRhodusObject.pas',
  uStringObject in '..\VirtualMachine\uStringObject.pas',
  uSymbolTable in '..\VirtualMachine\uSymbolTable.pas',
  uUtils in '..\VirtualMachine\uUtils.pas',
  uVM in '..\VirtualMachine\uVM.pas',
  uVMExceptions in '..\VirtualMachine\uVMExceptions.pas',
  uInitialize in 'uInitialize.pas',
  uCommands in 'uCommands.pas',
  uRunCode in 'uRunCode.pas',
  uBuiltInOS in 'uBuiltInOS.pas',
  uBuiltInMath in 'uBuiltInMath.pas',
  uLibModule in 'uLibModule.pas',
  uBuiltInRandom in 'uBuiltInRandom.pas',
  uBuiltInStr in 'uBuiltInStr.pas',
  uBuiltInList in 'uBuiltInList.pas',
  uBuiltInTurtle in 'uBuiltInTurtle.pas',
  uTurtleInterface in 'uTurtleInterface.pas',
  uAST in 'uAST.pas',
  uASTNodeType in 'uASTNodeType.pas',
  uConstructAST in 'uConstructAST.pas',
  uCompile in 'uCompile.pas',
  uGlobal in 'uGlobal.pas',
  uBuiltInGlobal in 'uBuiltInGlobal.pas',
  uTerminal in 'uTerminal.pas',
  uBuiltInFile in 'uBuiltInFile.pas',
  uProgramCode in 'uProgramCode.pas',
  uRhodusTypes in '..\VirtualMachine\uRhodusTypes.pas',
  uEnvironment in 'uEnvironment.pas',
  uObjectSupport in 'uObjectSupport.pas',
  uBuiltInConfig in 'uBuiltInConfig.pas',
  uBuiltInSys in 'uBuiltInSys.pas',
  uListOfBuiltIns in 'uListOfBuiltIns.pas',
  uSyntaxParser in 'uSyntaxParser.pas';

var sourceCode : string;
    fragment : string;
    runFramework : TRunFramework;
    color : TColor;


function searchHelp (const helpStr : string) : string;
var i, j, index : integer;
    astr : TArray<string>;
    symbol1, symbol2: TSymbol;
begin
  // Search places for any help for things of the form X or X.Y
  astr := SplitString(helpStr, '.');
  if length (astr) = 1 then  // then its just X
     begin
     // Check first if its in _main_
     // Its either in the global space or its a module name
     if mainModule.symbolTable.find(astr[0], symbol1) then
        begin
        if symbol1.symbolType = symModule then
           result := symbol1.mValue.helpStr
        else
           begin
           result := symbol1.toString;
           end;
        end
     else
        begin
        // Check the global space
        if mainModule.symbolTable.find(TSymbol.globalId, symbol1) then
           begin
           if symbol1.mValue.symbolTable.find(astr[0], symbol2) then
              begin
              result := symbol2.fValue.helpStr;
              end
           else
              result := 'Unable to locate symbol';
           end
        else
           result := 'Internal error: global space could not be located';
         end;
     end
  else
     begin
     if length (astr) = 2 then
        begin
        if mainModule.symbolTable.find(astr[0], symbol1) then
           begin
           if symbol1.symbolType = symModule then
              begin
              if symbol1.mValue.symbolTable.find (astr[1], symbol2)  then
                 case symbol2.symbolType of
                     symUserFunc : result := symbol2.fValue.helpStr;
                 else
                    result := symbol2.helpStr
                 end;
              end;
           end
        else
           result := 'Unable to locate the module: ' + astr[0];
        end
     else
        result := 'Don''t know how to search for what you indicated: "' + helpstr + '"';
     end;
end;


// Extract string up to first space, if no space found return entire string
function getCommand (src : string) : string;
var index : integer;
begin
  index := pos(' ', src);
  if index = 0 then
     result := src
  else
     result := copy(src, 1, index-1);
end;


// Get string to the right of the first space, if no space return empty string
function getArgument (src : string) : string;
var index : integer;
begin
  index := pos(' ', src);
  if index = 0 then
     result := ''
  else
     result := rightStr (src, length (src) - index);
end;


function executeCommand (src : string) : boolean;
var index : integer;
    helpStr : string;
begin
   result := False;

   if listOfCommands.find (getCommand (src), index) then
      begin
      listOfCommands[index].fcn (getArgument (src));
      exit (True);
      end;

   if leftstr (src, 1) = '?' then
      begin
      helpStr := rightStr (src, length (src) - 1);
      writeln (searchHelp (helpStr));
      exit (True);
      end;

   if leftStr (src, 5) = 'debug' then
      begin
      bolShowAssembler := not bolShowAssembler;
      if bolShowAssembler then
         writeln ('Debug ON')
      else writeln ('Debug OFF');
      exit (True);
      end;

   exit (False);

   // not a command, see if is a file name, if so run it
   if ExtractFileExt(src) <> '.rh' then
      src := src + '.rh';

   if FileExists (getCurrentDir + '\' + src) then
      begin
      //
      computeBaseLineMemoryAllocated;
      runFramework.compileAndRun (TFile.ReadAllText (getCurrentDir + '\' + src), False);  // False = not interactive
      exit (True)
      end;
end;


begin
  ReportMemoryLeaksOnShutdown := True;
  setUpConsole;
  setExtendedConsoleMode; // To get more colors
  setUpEnvironment (ParamStr (0));


  try
    runFramework := TRunFrameWork.Create;
    registerRuntimeWithConsole (runFramework);

    try
      computeBaseLineMemoryAllocated;
      displayWelcome;
      while True do
          begin
          try
            displayPrompt;
            readln (sourceCode);
            if sourceCode = 'quit' then
               break;

            if executeCommand (sourceCode) then
                continue;

            // Not a command therefore it could be code
            runFramework.showAssembler := bolShowAssembler;

            if sourceCode = '#p' then
               begin
               writeln ('Type q or return to exit and run program');
               write ('... ');
               sourceCode := '';
               readln (fragment);
               while (fragment <> 'q') and (fragment <> '') do
                   begin
                   if sourceCode = '' then
                      sourceCode := fragment
                   else
                      sourceCode := sourceCode + sLineBreak + fragment;
                   write ('... ');
                   readln (fragment)
                   end;
               end;
            try
              runFramework.syntaxCheck(sourceCode);
            except
               on e: ESyntaxException do
                  writeln ('ERROR ' + '[line ' + inttostr (e.lineNumber) + ', column: ' + inttostr (e.columnNumber) + '] ' + e.errorMsg);
            end;
//            if runFramework.compileCode (sourceCode, mainModule, True) then
//               begin
//               if TRunFramework.showByteCode then
//                  runFramework.showByteCodeMethod (mainModule);
//               runFrameWork.runCode (mainModule, True);
//               end;

          except
              on e:exception do
                 writeln (e.Message);
          end;
      end;
    finally

      runFramework.Free;

      writeln ('Current Memory Map:'#13#10, '[', memoryList.mapMemory, ']');
      memoryList.free;  // <- there is a small leak that needs to be tracked down
      writeln ('Total number of tests = ', nTotalTests);
      writeln ('Press any key to exit');
      readln;
      shutDownConsole;
    end;
  except
    on e:exception do
       writeln ('Internal Error: ' + e.message);
  end;
end.
