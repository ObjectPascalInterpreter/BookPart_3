program rhodusIDE;

uses
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  uRhodusLibTypes in '..\librhodus\uRhodusLibTypes.pas',
  ufMain in 'ufMain.pas' {frmMain},
  uExamples in 'uExamples.pas',
  ufAbout in 'ufAbout.pas' {frmAbout},
  Img32.Clipper in 'Image32\source\Img32.Clipper.pas',
  Img32.CQ in 'Image32\source\Img32.CQ.pas',
  Img32.Draw in 'Image32\source\Img32.Draw.pas',
  Img32.Extra in 'Image32\source\Img32.Extra.pas',
  Img32.Fmt.BMP in 'Image32\source\Img32.Fmt.BMP.pas',
  Img32.Fmt.GIF in 'Image32\source\Img32.Fmt.GIF.pas',
  Img32.Fmt.JPG in 'Image32\source\Img32.Fmt.JPG.pas',
  Img32.Fmt.PNG in 'Image32\source\Img32.Fmt.PNG.pas',
  Img32.Fmt.SVG in 'Image32\source\Img32.Fmt.SVG.pas',
  Img32.Layers in 'Image32\source\Img32.Layers.pas',
  Img32.Panels in 'Image32\source\Img32.Panels.pas',
  Img32 in 'Image32\source\Img32.pas',
  Img32.Resamplers in 'Image32\source\Img32.Resamplers.pas',
  Img32.SVG.Core in 'Image32\source\Img32.SVG.Core.pas',
  Img32.SVG.Path in 'Image32\source\Img32.SVG.Path.pas',
  Img32.SVG.PathDesign in 'Image32\source\Img32.SVG.PathDesign.pas',
  Img32.SVG.Reader in 'Image32\source\Img32.SVG.Reader.pas',
  Img32.SVG.Writer in 'Image32\source\Img32.SVG.Writer.pas',
  Img32.Text in 'Image32\source\Img32.Text.pas',
  Img32.Transform in 'Image32\source\Img32.Transform.pas',
  Img32.Vector in 'Image32\source\Img32.Vector.pas',
  Clipper in 'Image32\source\Clipper\Clipper.pas',
  ClipperCore in 'Image32\source\Clipper\ClipperCore.pas',
  ClipperOffset in 'Image32\source\Clipper\ClipperOffset.pas',
  uLoadRhodus in 'uLoadRhodus.pas',
  ufPreferences in 'ufPreferences.pas' {frmPreferences},
  uPreferences in 'uPreferences.pas',
  VCLStyleSynEdit in 'VCLStyleSynEdit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Calypso SE');
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
