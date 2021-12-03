(*
VCLStyleSynEdit By SMP3 .
My Blog : DelphiNews.WordPress.Com .
-----------------------------------------------------------
This small unit add the capability to SynEdit to support
VCL Style !
-----------------------------------------------------------

*)
unit VCLStyleSynEdit;

interface

uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
 Vcl.Controls, Vcl.Forms, Vcl.Dialogs, SynEdit,SynEditMiscClasses,Vcl.Themes,Vcl.Styles;
type TSynEdit = Class(SynEdit.TSynEdit )
 private
 LStyle:TCustomStyleServices;
 protected
 procedure CMSTYLECHANGED(var Message:TMessage);message CM_STYLECHANGED;
 procedure Loaded; override;
 public
 procedure UpdateStyle;
 Constructor Create(AOwner: TComponent); override;

End;
implementation

{ TSynEdit }

procedure TSynEdit.CMSTYLECHANGED(var Message: TMessage);
begin
 UpdateStyle;
 Invalidate;
end;

constructor TSynEdit.Create(AOwner: TComponent);
begin
 inherited;
Gutter.Gradient:=True;
Gutter.BorderStyle :=gbsRight;
UpdateStyle;

end;

procedure TSynEdit.Loaded;
begin
 inherited;
 UpdateStyle;
end;

procedure TSynEdit.UpdateStyle;
begin

LStyle := TStyleManager.ActiveStyle;
if not LStyle.IsSystemStyle then begin

Color := LStyle.GetStyleColor(scEdit);
with Gutter do begin
GradientEndColor :=LStyle.GetSystemColor(clBtnFace);
GradientStartColor :=LStyle.GetSystemColor(clWindow);
Font.Color :=LStyle.GetSystemColor(clWindowText );
BorderColor :=LStyle.GetStyleColor(scSplitter);
end;
ActiveLineColor :=LStyle.GetSystemColor(clHighlight);

end;
end;

initialization
TStyleManager.Engine.RegisterStyleHook(TSynEdit,TScrollBoxStyleHook);
finalization
TStyleManager.Engine.UnRegisterStyleHook(TSynEdit,TScrollBoxStyleHook);

end.
