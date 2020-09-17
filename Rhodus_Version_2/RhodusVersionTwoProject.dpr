program RhodusVersionTwoProject;

{$OVERFLOWCHECKS ON}
{$RANGECHECKS ON}

{$WARN SYMBOL_PLATFORM OFF}  // Stops Win32Check warnings


// Source code for Writing an Interpreter in Object Pascal: Part II

// Rhodus langage Version 1

// Developed under Delphi for Windows and Mac platforms.
// *** Ths source is distributed under Apache 2.0 ***

// Copyright (C) 2018-2020 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

// This is not a Delphi builtin console application, instead we allocate our own console. This
// is to prevent flashing when we move the console window to the center of the screen

//{$  APPTYPE CONSOLE}

{$R *.res}

uses
  {$IFDEF DEBUG}
  FastMM4 in '..\..\Library\FastMM\FastMM4.pas',
  {$ENDIF }
  Windows,
  ShellAPI,
  System.SysUtils,
  System.StrUtils,
  Math,
  IOUtils,
  Classes,
  System.Types,
  System.Generics.Defaults,
  System.Generics.Collections,
  uScanner in 'uScanner.pas',
  uSyntaxAnalysis in 'uSyntaxAnalysis.pas',
  FastMM4Messages in '..\..\Library\FastMM\FastMM4Messages.pas',
  uFindWindows in 'uFindWindows.pas',
  uAssembler in '..\VirtualMachine\uAssembler.pas',
  uBuiltinFunctions in '..\VirtualMachine\uBuiltinFunctions.pas',
  uConstantTable in '..\VirtualMachine\uConstantTable.pas',
  uListObject in '..\VirtualMachine\uListObject.pas',
  uMachineStack in '..\VirtualMachine\uMachineStack.pas',
  uMemoryManager in '..\VirtualMachine\uMemoryManager.pas',
  uModule in '..\VirtualMachine\uModule.pas',
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
  uBuiltInModule in 'uBuiltInModule.pas',
  uBuiltInRandom in 'uBuiltInRandom.pas',
  uBuiltInStr in 'uBuiltInStr.pas',
  uBuiltInList in 'uBuiltInList.pas',
  uBuiltInTurtle in 'uBuiltInTurtle.pas',
  uTurtleInterface in 'uTurtleInterface.pas',
  uAST in 'uAST.pas',
  uASTNodeType in 'uASTNodeType.pas';

var sourceCode : string;
    bolShowAssembler : boolean;
    runFramework : TRunFramework;
    bt : TObjectList<TBuiltInModule>;


procedure displayWelcome;
begin
  writeln ('Welcome to Rhodus Language I Console, Version ', RHODUS_VERSION);
  writeln ('Data and Time: ', dateToStr (Date), ', ', timeToStr (Time));
  displayHelp;
end;


procedure displayPrompt;
begin
  setRed;
  write ('>> ');
  setWhite;
end;


function searchHelp (const helpStr : string) : string;
var i, index : integer;
begin
  if helpstr = '?' then
     begin
     result := 'Builtin functions:' + sLineBreak;
     result := builtinList[0].name;
     for i := 1 to builtinList.Count - 1 do
         result := result + ', ' + builtinList[i].name;
     exit;
     end;

  // Search places for any help
  if builtinList.find(helpStr, index) then
     result := builtinList[index].helpStr
  else
     result := 'No help found';
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


function runCommand (src : string) : boolean;
var fileName : string;
    index : integer;
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

   //writeln ('Unknown command');
   exit (False);

   // not a command, see if is a file name, if so run it
   if ExtractFileExt(src) <> '.rh' then
      src := src + '.rh';
   if FileExists (getCurrentDir + '\' + src) then
      begin
      runFramework.runCode (TFile.ReadAllText (getCurrentDir + '\' + src), bolShowAssembler);
      exit (True)
      end;
end;


begin
  ReportMemoryLeaksOnShutdown := True;
  setUpConsole;
  setUpEnvironment (ParamStr (0));

  try
    bolShowAssembler := False;

    moduleList.add (TModule.Create ('_main_'));

    bt := TObjectList <TBuiltInModule>.Create();
    bt.Add (TBuiltInOS.Create);
    bt.Add (TBuiltInMath.Create);
    bt.Add (TBuiltInRandom.Create);
    bt.Add (TBuiltInStr.Create);
    bt.Add (TBuiltinList.Create);
    bt.Add (TBuiltInTurtle.Create);

    runFramework := TRunFrameWork.Create;
    runFramework.mainModule := moduleList[0];
    runFramework.sc := TScanner.Create;
    runFramework.sy := TSyntaxAnalysis.Create (runFramework.sc);
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

             if runCommand (sourceCode) then
                continue;

             runFramework.runCode (sourceCode, bolShowAssembler);

          except
              on e:exception do
                 writeln (e.Message);
          end;
      end;
    finally
      runFramework.Free;

      writeln ('Current Memory Map:'#13#10, '[', memoryList.mapMemory, ']');
      memoryList.freeGarbage;
      //writeln ('Garbage Freed:'#13#10, '[' + memoryList.mapMemory + ']');
      memoryList.free;
      writeln ('Total number of tests = ', nTotalTests);
      bt.Free;
      writeln ('Press any key to exit');
      readln;
      shutDownConsole;
    end;
  except
    on Exception do
       writeln ('Error');
  end;
end.