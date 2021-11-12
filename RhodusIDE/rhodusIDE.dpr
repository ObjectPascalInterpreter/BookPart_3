program rhodusIDE;

uses
  Vcl.Forms,
  ufMain in 'ufMain.pas' {frmMain},
  ulibTypes in '..\librhodus\ulibTypes.pas' {$R *.res},
  Vcl.Themes,
  Vcl.Styles,
  uExamples in 'uExamples.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Calypso SE');
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
