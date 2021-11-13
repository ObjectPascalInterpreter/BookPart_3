program rhodusIDE;

uses
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  ulibTypes in '..\librhodus\ulibTypes.pas',
  ufMain in 'ufMain.pas' {frmMain},
  uExamples in 'uExamples.pas',
  ufAbout in 'ufAbout.pas' {frmAbout};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Calypso SE');
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmAbout, frmAbout);
  Application.Run;
end.
