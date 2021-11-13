unit ufMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, ulibTypes,
  Vcl.ExtCtrls, Vcl.FileCtrl, Vcl.ComCtrls,
  uExamples, Vcl.Menus;

const RHODUS_VERSION = 0.5;

type
  TfrmMain = class(TForm)
    Panel1: TPanel;
    btnRun: TButton;
    Panel2: TPanel;
    Panel3: TPanel;
    moutput: TMemo;
    Splitter1: TSplitter;
    editor: TMemo;
    btnLoad: TButton;
    OpenDialog: TOpenDialog;
    lblVersion: TLabel;
    DirectoryListBox1: TDirectoryListBox;
    FileListBox1: TFileListBox;
    btnClear: TButton;
    cboExamples: TComboBox;
    Label1: TLabel;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    File2: TMenuItem;
    Run1: TMenuItem;
    Quit1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    About1: TMenuItem;
    procedure btnRunClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure DirectoryListBox1Change(Sender: TObject);
    procedure FileListBox1Click(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure cboExamplesChange(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure N2Click(Sender: TObject);
    procedure Quit1Click(Sender: TObject);
  private
    { Private declarations }
    examples : TExamples;
  public
    { Public declarations }
    handle: THandle;
    procedure loadScript;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

Uses IOUtils, ufAbout;

type
  TRhodusInitialise = function (var config : TRhodusConfig) : THandle; stdcall;
  TRhodusRun = function  (handle : THandle; code : AnsiString) : integer; stdcall;
  TRhodusTerminate = procedure (handle : THandle); stdcall;
  TRhodusGetLastError = function (handle : THandle) : PRhodusError; stdcall;
  TRhodusGetSettings = function (handle : THandle) : PRhodusSettings; stdcall;

var config : TRhodusConfig;
    rhodus_initialize : TRhodusInitialise;
    rhodus_run : TRhodusRun;
    rhodus_terminate : TRhodusTerminate;
    rhodus_getLastError : TRhodusGetLastError;
    rhodus_getSettings : TRhodusGetSettings;
    rhodus : THandle;


procedure rhodus_print (astr : AnsiString);
begin
  if astr = sLineBreak then
     frmMain.moutput.Lines[frmMain.moutput.CaretPos.Y] := frmMain.moutput.Lines[frmMain.moutput.CaretPos.y] + astr
  else
     frmMain.moutput.Lines[frmMain.moutput.CaretPos.Y] := frmMain.moutput.Lines[frmMain.moutput.CaretPos.y] + astr;
end;


procedure rhodus_println (astr : AnsiString);
begin
  frmMain.moutput.Lines.Append (astr);
end;


function rhodus_readString (const prompt : AnsiString) : PAnsiChar;
begin
  result := PAnsiChar (AnsiString (InputBox('Read String:', prompt, '')));
end;


procedure TfrmMain.About1Click(Sender: TObject);
var frm : TfrmAbout;
begin
  try
    frm := TfrmAbout.Create (nil);
    frm.lblMessage.Caption := 'Rhodus IDE Version: ' + floattostr (RHODUS_VERSION);
    frm.ShowModal;
  finally
    frm.free;
  end;
end;

procedure TfrmMain.btnClearClick(Sender: TObject);
begin
  moutput.Clear;
end;

procedure TfrmMain.loadScript;
begin
 if OpenDialog.Execute then
     begin
     editor.Lines.LoadFromFile(opendialog.FileName);
     end;
end;


procedure TfrmMain.btnLoadClick(Sender: TObject);
begin
  loadScript;
end;

procedure TfrmMain.btnRunClick(Sender: TObject);
var errorId : integer;
    pt : PRhodusError;
begin
  errorId := rhodus_run (rhodus, editor.text);
  if errorId < 0 then
     begin
     pt := rhodus_getLastError (rhodus);
     moutput.Lines.Add(AnsiString (pt.errorMsg));
     end;
end;


procedure TfrmMain.cboExamplesChange(Sender: TObject);
begin
  editor.Lines.Text :=
     (cboExamples.items.Objects[cboExamples.ItemIndex] as TExample).src;
end;

procedure TfrmMain.DirectoryListBox1Change(Sender: TObject);
begin
  FileListBox1.Directory := DirectoryListBox1.Directory;
end;


procedure TfrmMain.FileListBox1Click(Sender: TObject);
var fileName : string;
begin
  fileName := FileListBox1.FileName;
  if TPath.GetExtension(fileName) = '.rh' then
     editor.Lines.Text := TFile.ReadAllText(filename);
end;


procedure TfrmMain.FormCreate(Sender: TObject);
var i : integer;
begin
  handle := LoadLibrary('librhodus.dll');
  @rhodus_initialize := GetProcAddress(Handle, 'rhodus_initialize');
  @rhodus_run := GetProcAddress(Handle, 'rhodus_run');
  @rhodus_terminate := GetProcAddress(Handle, 'rhodus_terminate');
  @rhodus_getLastError := GetProcAddress(Handle, 'rhodus_getLastError');
  @rhodus_getSettings := GetProcAddress(Handle, 'rhodus_getSettings');

  config.printPtr := rhodus_print;
  config.printlnPtr := rhodus_println;
  config.readStringPtr := rhodus_readString;

  rhodus := rhodus_initialize (config);
  lblVersion.caption := 'Running Version: ' + AnsiString (rhodus_getSettings(rhodus).versionStr);

  examples := TExamples.Create;
  for i := 0 to examples.Count - 1 do
      cboExamples.AddItem(examples[i].name, examples[i]);
  cboExamples.ItemIndex := -1;
end;

procedure TfrmMain.N2Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmMain.Quit1Click(Sender: TObject);
begin
  loadScript;
end;

end.
