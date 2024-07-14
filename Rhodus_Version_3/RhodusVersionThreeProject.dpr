  program RhodusVersionThreeProject;

{$OVERFLOWCHECKS ON}
{$RANGECHECKS ON}
{$WARN DUPLICATE_CTOR_DTOR OFF}

{$WARN SYMBOL_PLATFORM OFF}  // Stops Win32Check warnings


// Source code for Writing an Interpreter in Object Pascal: Part III

// Rhodus language Version 3

// Developed under Delphi for Windows and Mac platforms.
// *** This source is distributed under Apache 2.0 ***

// Copyright (C) 2018-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

// This is not a Delphi builtin console application, instead we allocate our own console. This
// is to prevent flashing when we move the console window to the center of the screen

// The following is therefore commented out
//{$  APPTYPE CONSOLE}

{$R *.res}

{$define LogErrorsToFile}
{$define LogMemoryLeakDetailToFile}

{$R *.dres}

uses
  //{$IFDEF DEBUG}
//FastMM5,
  //{$ENDIF }
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
  uFindWindows in 'uFindWindows.pas',
  uAssembler in '..\VirtualMachine\uAssembler.pas',
  uConstantTable in '..\VirtualMachine\uConstantTable.pas',
  uListObject in '..\VirtualMachine\uListObject.pas',
  uMachineStack in '..\VirtualMachine\uMachineStack.pas',
  uMemoryManager in '..\VirtualMachine\uMemoryManager.pas',
  uOpCodes in '..\VirtualMachine\uOpCodes.pas',
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
  uBuiltInConfig in 'uBuiltInConfig.pas',
  uBuiltInSys in 'uBuiltInSys.pas',
  uListOfBuiltIns in 'uListOfBuiltIns.pas',
  uSyntaxParser in 'uSyntaxParser.pas',
  uTokenVector in 'uTokenVector.pas',
  uScannerTypes in 'uScannerTypes.pas',
  uRepl in 'uRepl.pas',
  uArrayObject in '..\VirtualMachine\uArrayObject.pas',
  uBuiltInArray in 'uBuiltInArray.pas',
  uBuiltInMatrix in 'uBuiltInMatrix.pas',
  uMath in 'uMath.pas',
  uBuiltInGraphics in 'uBuiltInGraphics.pas',
  uRhodusLibTypes in '..\librhodus\uRhodusLibTypes.pas',
  uIntStack in '..\VirtualMachine\uIntStack.pas',
  uJumpTables in '..\VirtualMachine\uJumpTables.pas',
  uMatrixObject in '..\VirtualMachine\uMatrixObject.pas',
  uVectorObject in '..\VirtualMachine\uVectorObject.pas',
  uMatrixFunctions in 'uMatrixFunctions.pas',
  uValueObject in '..\VirtualMachine\uValueObject.pas',
  uBuiltInPlotter in 'uBuiltInPlotter.pas',
  uHelpUnit in '..\Common\uHelpUnit.pas',
  uDataObjectMethods in '..\VirtualMachine\uDataObjectMethods.pas',
  uDataObject in '..\VirtualMachine\uDataObject.pas',
  uBuiltInASCIIPlot in 'uBuiltInASCIIPlot.pas';

begin
  //FastMM_SetEventLogFilename('D:\log.txt');
  //FastMM_EnterDebugMode;
  ReportMemoryLeaksOnShutdown := True;
  setUpConsole;
  setExtendedConsoleMode; // To get more colors
  setUpEnvironment (ParamStr (0));

  startRepl();
end.
