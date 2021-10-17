  program RhodusVersionThreeProject;

{$OVERFLOWCHECKS ON}
{$RANGECHECKS ON}
{$WARN DUPLICATE_CTOR_DTOR OFF}

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
  FastMM4 in '..\..\Library\FastMM\FastMM4.pas',
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
  uRhodusEngine in 'uRhodusEngine.pas',
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
  uSyntaxParser in 'uSyntaxParser.pas',
  uTokenVector in 'uTokenVector.pas',
  uScannerTypes in 'uScannerTypes.pas',
  uEmbeddAPI in 'uEmbeddAPI.pas',
  uRepl in 'uRepl.pas',
  uArrayObject in '..\VirtualMachine\uArrayObject.pas',
  uBuiltInArray in 'uBuiltInArray.pas',
  uBuiltInMatrix in 'uBuiltInMatrix.pas';

var config : TRhodusConfig;


begin
  ReportMemoryLeaksOnShutdown := True;
  setUpConsole;
  setExtendedConsoleMode; // To get more colors
  setUpEnvironment (ParamStr (0));

  // Doesn't do anything as yet.
  rhodus_initialize(config);

  startRepl();
end.
