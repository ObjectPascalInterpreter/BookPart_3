program rhodusIDE;

uses
  Vcl.Forms,
  ufMain in 'ufMain.pas' {frmMain},
  ulibTypes in '..\librhodus\ulibTypes.pas' {$R *.res},
  Vcl.Themes,
  Vcl.Styles,
  ufInputDialog in 'ufInputDialog.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Calypso SE');
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
