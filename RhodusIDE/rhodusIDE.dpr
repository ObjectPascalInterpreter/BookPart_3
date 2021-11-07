program rhodusIDE;

uses
  Vcl.Forms,
  ufMain in 'ufMain.pas' {frmMain},
  ulibTypes in '..\librhodus\ulibTypes.pas',
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Calypso SE');
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
