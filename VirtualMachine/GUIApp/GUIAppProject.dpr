program GUIAppProject;

uses
  Vcl.Forms,
  ufMain in 'ufMain.pas' {frmMain},
  uAssembler in '..\uAssembler.pas',
  uOpCodes in '..\uOpCodes.pas',
  uVM in '..\uVM.pas',
  uConstantTable in '..\uConstantTable.pas',
  uUtils in '..\uUtils.pas',
  uBuiltinFunctions in '..\uBuiltinFunctions.pas',
  uMachineStack in '..\uMachineStack.pas',
  uListObject in '..\uListObject.pas',
  uMemoryManager in '..\uMemoryManager.pas',
  uStringObject in '..\uStringObject.pas',
  uVMExceptions in '..\uVMExceptions.pas',
  uSyntaxAnalysis in '..\..\Rhodus_Version_1\uSyntaxAnalysis.pas',
  uScanner in '..\..\Rhodus_Version_1\uScanner.pas',
  uModule in '..\uModule.pas',
  uSymbolTable in '..\uSymbolTable.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
