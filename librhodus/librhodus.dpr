library librhodus;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  System.SysUtils,
  System.Classes,
  uEmbeddAPI in 'uEmbeddAPI.pas',
  uArrayObject in '..\VirtualMachine\uArrayObject.pas',
  uAssembler in '..\VirtualMachine\uAssembler.pas',
  uBuiltinFunctions in '..\VirtualMachine\uBuiltinFunctions.pas',
  uConstantTable in '..\VirtualMachine\uConstantTable.pas',
  uListObject in '..\VirtualMachine\uListObject.pas',
  uMachineStack in '..\VirtualMachine\uMachineStack.pas',
  uMemoryManager in '..\VirtualMachine\uMemoryManager.pas',
  uOpCodes in '..\VirtualMachine\uOpCodes.pas',
  uRhodusObject in '..\VirtualMachine\uRhodusObject.pas',
  uRhodusTypes in '..\VirtualMachine\uRhodusTypes.pas',
  uStringObject in '..\VirtualMachine\uStringObject.pas',
  uSymbolTable in '..\VirtualMachine\uSymbolTable.pas',
  uUtils in '..\VirtualMachine\uUtils.pas',
  uVM in '..\VirtualMachine\uVM.pas',
  uVMExceptions in '..\VirtualMachine\uVMExceptions.pas',
  uAST in '..\Rhodus_Version_3\uAST.pas',
  uASTNodeType in '..\Rhodus_Version_3\uASTNodeType.pas',
  uASTType in '..\Rhodus_Version_3\uASTType.pas',
  uBuiltInArray in '..\Rhodus_Version_3\uBuiltInArray.pas',
  uBuiltInConfig in '..\Rhodus_Version_3\uBuiltInConfig.pas',
  uBuiltInFile in '..\Rhodus_Version_3\uBuiltInFile.pas',
  uBuiltInGlobal in '..\Rhodus_Version_3\uBuiltInGlobal.pas',
  uBuiltInList in '..\Rhodus_Version_3\uBuiltInList.pas',
  uBuiltInMath in '..\Rhodus_Version_3\uBuiltInMath.pas',
  uBuiltInMatrix in '..\Rhodus_Version_3\uBuiltInMatrix.pas',
  uBuiltInOS in '..\Rhodus_Version_3\uBuiltInOS.pas',
  uBuiltInRandom in '..\Rhodus_Version_3\uBuiltInRandom.pas',
  uBuiltInStr in '..\Rhodus_Version_3\uBuiltInStr.pas',
  uBuiltInSys in '..\Rhodus_Version_3\uBuiltInSys.pas',
  uBuiltInTurtle in '..\Rhodus_Version_3\uBuiltInTurtle.pas',
  uCommands in '..\Rhodus_Version_3\uCommands.pas',
  uCompile in '..\Rhodus_Version_3\uCompile.pas',
  uConstructAST in '..\Rhodus_Version_3\uConstructAST.pas',
  uEnvironment in '..\Rhodus_Version_3\uEnvironment.pas',
  uFindWindows in '..\Rhodus_Version_3\uFindWindows.pas',
  uGlobal in '..\Rhodus_Version_3\uGlobal.pas',
  uInitialize in '..\Rhodus_Version_3\uInitialize.pas',
  uLibModule in '..\Rhodus_Version_3\uLibModule.pas',
  uListOfBuiltIns in '..\Rhodus_Version_3\uListOfBuiltIns.pas',
  uMath in '..\Rhodus_Version_3\uMath.pas',
  uMemory in '..\Rhodus_Version_3\uMemory.pas',
  uObjectSupport in '..\Rhodus_Version_3\uObjectSupport.pas',
  uProgramCode in '..\Rhodus_Version_3\uProgramCode.pas',
  uRepl in '..\Rhodus_Version_3\uRepl.pas',
  uRhodusEngine in '..\Rhodus_Version_3\uRhodusEngine.pas',
  uScanner in '..\Rhodus_Version_3\uScanner.pas',
  uScannerTypes in '..\Rhodus_Version_3\uScannerTypes.pas',
  uSyntaxParser in '..\Rhodus_Version_3\uSyntaxParser.pas',
  uTerminal in '..\Rhodus_Version_3\uTerminal.pas',
  uTokenVector in '..\Rhodus_Version_3\uTokenVector.pas',
  uTurtleInterface in '..\Rhodus_Version_3\uTurtleInterface.pas',
  uBuiltInGraphics in '..\Rhodus_Version_3\uBuiltInGraphics.pas',
  uIntStack in '..\VirtualMachine\uIntStack.pas';

{$R *.res}

exports
    rhodus_initialize,
    rhodus_terminate,
    rhodus_run,
    rhodus_getLastError,
    rhodus_getSettings;

begin
end.
