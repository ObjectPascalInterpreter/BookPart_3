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
    pnlRight: TPanel;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    btnNew: TButton;
    pnlInfo: TPanel;
    mnuNew: TMenuItem;
    pnlImage: TPanel;
    pnlDrawing: TImage;
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
    procedure pnlRightResize(Sender: TObject);
    procedure mnuNewClick(Sender: TObject);
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
  TRhodusGetSettings = function (handle : THandle) : PRhodusSettings; cdecl;

var config : TRhodusConfig;
    graphicsMethods : TGraphicsMethods;
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

// --------------------------------------------------------------
procedure gClear;
begin
  frmMain.pnlDrawing.Canvas.Pen.Color := clWhite;
  frmMain.pnlDrawing.Canvas.Brush.Color := clWebOldLace;
  frmMain.pnlDrawing.canvas.rectangle(0, 0, frmMain.pnlDrawing.Width-1, frmMain.pnlDrawing.Height-1);
end;

function gGetCanvasSize : TRhodusPoint;
begin
  result.w := frmMain.pnlDrawing.Width;
  result.h := frmMain.pnlDrawing.Height;
end;

procedure gMoveTo (x, y : double);
begin
  frmMain.pnlDrawing.canvas.moveTo (trunc (x), trunc (y));
end;

procedure gLineTo (x, y : double);
begin
  frmMain.pnlDrawing.Canvas.Pen.Color := clRed;
  frmMain.pnlDrawing.Canvas.Pen.Width := 2;
  frmMain.pnlDrawing.canvas.lineTo (trunc (x), trunc (y));
end;


procedure gRefresh;
begin
end;


// ----------------------------------------------------------------

procedure TfrmMain.loadScript;
begin
 if OpenDialog.Execute then
    editor.Lines.LoadFromFile(opendialog.FileName);
end;

procedure TfrmMain.mnuNewClick(Sender: TObject);
begin
  editor.Clear;
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
  try
  handle := LoadLibrary('librhodus.dll');
  @rhodus_initialize := GetProcAddress(Handle, 'rhodus_initialize');
  @rhodus_run := GetProcAddress(Handle, 'rhodus_run');
  @rhodus_terminate := GetProcAddress(Handle, 'rhodus_terminate');
  @rhodus_getLastError := GetProcAddress(Handle, 'rhodus_getLastError');
  @rhodus_getSettings := GetProcAddress(Handle, 'rhodus_getSettings');

  config.printPtr := rhodus_print;
  config.printlnPtr := rhodus_println;
  config.readStringPtr := rhodus_readString;

  graphicsMethods.redrawRequest := gRefresh;
  graphicsMethods.clear := gClear;
  graphicsMethods.getCanvasSize := gGetCanvasSize;
  graphicsMethods.moveTo := gMoveTo;
  graphicsMethods.lineTo := gLineTo;
  config.graphicsHandlerPtr := @graphicsMethods;

  rhodus := rhodus_initialize (config);
  lblVersion.caption := 'Running Version: ' + AnsiString (rhodus_getSettings(rhodus).versionStr);

  examples := TExamples.Create;
  for i := 0 to examples.Count - 1 do
      cboExamples.AddItem(examples[i].name, examples[i]);
  cboExamples.ItemIndex := -1;

  gClear;
  except
    showmessage ('Error');
  end;
end;

procedure TfrmMain.N2Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmMain.pnlRightResize(Sender: TObject);
begin
  pnlDrawing.Canvas.Pen.Color := clWhite;
  pnlDrawing.Canvas.Brush.Color := clWebOldLace;
  pnlDrawing.Picture.Bitmap.Width := pnlDrawing.Width;
  pnlDrawing.Picture.Bitmap.Height := pnlDrawing.Height;
  pnlDrawing.canvas.rectangle(0, 0, pnlDrawing.Width-1, pnlDrawing.Height-1);
end;

procedure TfrmMain.Quit1Click(Sender: TObject);
begin
  loadScript;
end;

end.
