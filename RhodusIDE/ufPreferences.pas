unit ufPreferences;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Samples.Spin,
  uPreferences;

type
  TfrmPreferences = class(TForm)
    pnlBottom: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
    cboFontlist: TComboBox;
    spFontSize: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    lbThemes: TListBox;
    procedure btnOkClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure spFontSizeChange(Sender: TObject);
    procedure cboFontlistClick(Sender: TObject);
    procedure lbThemesClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure setFontName (name : string);
    procedure setTheme (name : string);
  end;

var
  frmPreferences: TfrmPreferences;

implementation

{$R *.dfm}

Uses Vcl.Themes;

// The following two methods are called
// to set up the preferences.
procedure  TfrmPreferences.setTheme (name : string);
var i : integer;
begin
  for i := 0 to lbThemes.Count - 1 do
      if lbThemes.Items[i] = name then
         begin
         lbThemes.ItemIndex := i;
         exit;
         end;
end;


procedure TfrmPreferences.setFontName (name : string);
var i : integer;
begin
  for i := 0 to cboFontlist.Items.Count - 1 do
      if cboFontlist.Items[i] = name then
         begin
         cboFontList.ItemIndex := i;
         exit;
         end;
end;


procedure TfrmPreferences.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;


procedure TfrmPreferences.btnOkClick(Sender: TObject);
begin
  preferences.editor.fontSize := spFontSize.Value;
  preferences.editor.fontName := cboFontlist.Items[cboFontlist.itemIndex];

  ModalResult := mrOk;
end;

procedure TfrmPreferences.cboFontlistClick(Sender: TObject);
begin
  preferences.editor.fontName := cboFontlist.Items[cboFontlist.ItemIndex];
end;

procedure TfrmPreferences.FormCreate(Sender: TObject);
var i : Integer;
    stylename: string;
begin
  for i := 0 to Screen.Fonts.Count - 1 do
      if Screen.fonts.Strings[i][1] <> '@' then
        cboFontList.Items.Add(Screen.Fonts.Strings[i]);
  cboFontList.itemIndex := 0;

  lbThemes.Clear;
  // retrieve all the styles linked in the executable
  for stylename in TStyleManager.StyleNames do
      lbThemes.Items.Add(stylename);
end;

procedure TfrmPreferences.lbThemesClick(Sender: TObject);
begin
  preferences.IDE.theme := lbThemes.Items[lbThemes.ItemIndex];
end;

procedure TfrmPreferences.spFontSizeChange(Sender: TObject);
begin
  preferences.editor.fontSize := spFontSize.Value;
end;

end.
