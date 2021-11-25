unit ufMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, ulibTypes,
  Vcl.ExtCtrls, Vcl.FileCtrl, Vcl.ComCtrls, Vcl.Themes,
  uExamples, Vcl.Menus, SyncObjs, SynEdit, SynEditHighlighter, SynEditCodeFolding, SynHighlighterPython, SynHighlighterGeneral,
  SynEditMiscClasses, SynEditSearch, StrUtils;

const RHODUS_VERSION = 0.5;

type
  TRGBTriple = packed record
    rgbtBlue: Byte;
    rgbtGreen: Byte;
    rgbtRed: Byte;
  end;
  TRGBTripleArray = ARRAY[Word] of TRGBTriple;
  pRGBTripleArray = ^TRGBTripleArray;

  TfrmMain = class(TForm)
    Panel1: TPanel;
    btnRun: TButton;
    Panel2: TPanel;
    Panel3: TPanel;
    moutput: TMemo;
    Splitter1: TSplitter;
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
    editor: TSynEdit;
    SynGeneralSyn1: TSynGeneralSyn;
    SynEditSearch1: TSynEditSearch;
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
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    examples : TExamples;
    bmp : TBitmap;
    Line: array of pRGBTripleArray;
    procedure defaultColors;
  public
    { Public declarations }
    handle: THandle;
    Lock: TCriticalSection;
    procedure loadScript;
  end;

  TRhodusWorker = class (TThread)
    private
      FLock : TCriticalSection;
    protected
      procedure Execute; override;
      constructor Create(ALock: TCriticalSection);
  end;

var
  frmMain: TfrmMain;
  rt : TRhodusWorker;

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

    pen_r, pen_g, pen_b : integer;
    brush_r, brush_g, brush_b : integer;
    penWidth : double;


constructor TRhodusWorker.Create(ALock: TCriticalSection);
begin
  self.FLock := ALock;
  inherited Create (False);
end;

procedure TRhodusWorker.Execute;
var errorId : integer;
    pt : PRhodusError;
begin
  FLock.Acquire;
  errorId := rhodus_run (rhodus, frmMain.editor.text);
  if errorId < 0 then
     begin
     pt := rhodus_getLastError (rhodus);
     frmMain.moutput.Lines.Add(AnsiString (pt.errorMsg));
     end;
  FLock.Release;
end;


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
  TThread.Synchronize(nil,
    procedure
    begin
    frmMain.pnlDrawing.Canvas.Pen.Color := RGB (brush_r, brush_g, brush_b);
    frmMain.pnlDrawing.Canvas.Brush.Style := TBrushStyle.bsSolid;
    frmMain.pnlDrawing.Canvas.Brush.Color := RGB (brush_r, brush_g, brush_b);

    frmMain.pnlDrawing.canvas.rectangle(0, 0, frmMain.pnlDrawing.Width, frmMain.pnlDrawing.Height);
    end
  );
end;


procedure gSetPenColor (r, g, b : integer);
begin
  pen_r := r; pen_g := g; pen_b := b;
end;


procedure gSetPenWidth (w : double);
begin
  penWidth := w;
end;


procedure gSetBrushColor (r, g, b : integer);
begin
  brush_r := r; brush_g := g; brush_b := b;
end;


procedure gSetPixel (x, y : integer);
var c : TRGBTriple;
    bigP : pRGBTripleArray;
begin
  TThread.Synchronize(nil,
    procedure
    begin
    bigP := frmMain.Line[x];
    bigP[y].rgbtRed := pen_r;// := RGB (pen_r, pen_g, pen_b);
    bigP[y].rgbtGreen := pen_g;
    bigP[y].rgbtBlue := pen_b;
    Application.ProcessMessages;
    frmMain.pnlDrawing.Invalidate;
    end
  );
end;


procedure gDrawRect (x, y, w, h : double);
begin
  TThread.Synchronize(nil,
    procedure
    begin
    frmMain.pnlDrawing.Canvas.Pen.Width := trunc (penWidth);
    frmMain.pnlDrawing.Canvas.Pen.Color := RGB (pen_r, pen_g, pen_b);
    frmMain.pnlDrawing.Canvas.Rectangle(trunc (x), trunc (y), trunc (x + w), trunc(y + h));
    end
  );
end;

procedure gDrawFilledRect (x, y, w, h : double);
begin
  TThread.Synchronize(nil,
    procedure
    begin
    frmMain.pnlDrawing.Canvas.Pen.Width := trunc (penWidth);
    frmMain.pnlDrawing.Canvas.Pen.Color := RGB (pen_r, pen_g, pen_b);
    frmMain.pnlDrawing.Canvas.Brush.Style := TBrushStyle.bsSolid;
    frmMain.pnlDrawing.Canvas.Brush.Color := RGB (brush_r, brush_g, brush_b);
    frmMain.pnlDrawing.Canvas.Rectangle(trunc (x), trunc (y), trunc (x + w), trunc(y + h));
    frmMain.pnlDrawing.Refresh;
    end
  );
end;

procedure gDrawEllipse (x1, y1, x2, y2 : double);
begin
  TThread.Synchronize(nil,
    procedure
    begin
    frmMain.pnlDrawing.Canvas.Pen.Width := trunc (penWidth);
    frmMain.pnlDrawing.Canvas.Pen.Color := RGB (pen_r, pen_g, pen_b);
    frmMain.pnlDrawing.Canvas.Brush.Style := TBrushStyle.bsClear;
    frmMain.pnlDrawing.Canvas.Ellipse(trunc (x1), trunc (y1), trunc (x2), trunc(y2));
    end
  );
end;

procedure gDrawFilledEllipse (x1, y1, x2, y2 : double);
begin
  TThread.Synchronize(nil,
    procedure
    begin
    frmMain.pnlDrawing.Canvas.Pen.Width := trunc (penWidth);
    frmMain.pnlDrawing.Canvas.Brush.Style := TBrushStyle.bsSolid;
    frmMain.pnlDrawing.Canvas.Brush.Color := RGB (brush_r, brush_g, brush_b);
    frmMain.pnlDrawing.Canvas.Pen.Color := RGB (pen_r, pen_g, pen_b);
    frmMain.pnlDrawing.Canvas.Ellipse(trunc (x1), trunc (y1), trunc (x2), trunc(y2));
    end
  );
end;


function gGetCanvasSize : TRhodusPoint;
begin
  result.w := frmMain.pnlDrawing.Width;
  result.h := frmMain.pnlDrawing.Height;
end;


procedure gMoveTo (x, y : double);
begin
  TThread.Synchronize(nil,
    procedure
    begin
    frmMain.pnlDrawing.canvas.moveTo (trunc (x), trunc (y));
    end
  );
end;

procedure gLineTo (x, y : double);
begin
  TThread.Synchronize(nil,
    procedure
    begin
    frmMain.pnlDrawing.Canvas.Pen.Color := RGB (pen_r, pen_g, pen_b);
    frmMain.pnlDrawing.Canvas.Pen.Width := trunc (penWidth);
    frmMain.pnlDrawing.canvas.lineTo (trunc (x), trunc (y));
    end
  );
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
begin
   defaultColors;
   rt := TRhodusWorker.Create (Lock);
end;

procedure TfrmMain.cboExamplesChange(Sender: TObject);
begin
  editor.Lines.Text := (cboExamples.items.Objects[cboExamples.ItemIndex] as TExample).src;
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

procedure TfrmMain.defaultColors;
begin
  // clWebOldLace
  brush_r := 253; brush_g := 245;  brush_b := 230;
  pen_r := 255; pen_g := 0; pen_b := 0;
  penwidth := 1;
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
  graphicsMethods.setPixel := gSetPixel;
  graphicsMethods.moveTo := gMoveTo;
  graphicsMethods.lineTo := gLineTo;
  graphicsMethods.setPenColor := gSetPenColor;
  graphicsMethods.setPenWidth := gSetPenWidth;
  graphicsMethods.setBrushColor := gSetBrushColor;
  graphicsMethods.drawRectangle := gDrawRect;
  graphicsMethods.drawFilledRectangle := gDrawFilledRect;
  graphicsMethods.drawEllipse := gDrawEllipse;
  graphicsMethods.drawFilledEllipse := gDrawFilledEllipse;

  config.graphicsHandlerPtr := @graphicsMethods;

  rhodus := rhodus_initialize (config);
  lblVersion.caption := 'Running Version: ' + AnsiString (rhodus_getSettings(rhodus).versionStr);

  examples := TExamples.Create;
  for i := 0 to examples.Count - 1 do
      cboExamples.AddItem(examples[i].name, examples[i]);
  cboExamples.ItemIndex := -1;

  defaultColors;
  Lock := TCriticalSection.Create;
 
  Bmp := pnlDrawing.Picture.Bitmap;
  Bmp.PixelFormat := pf24bit;
  Bmp.Width := pnlDrawing.Width;
  Bmp.Height := pnlDrawing.Height;
  setlength (line, Bmp.Height);
  for i := 0 to Bmp.Height - 1 do
      line[i] :=  Bmp.ScanLine [i];
  gClear;
  editor.Color := $614326;
  editor.Font.Color := $F0FBFF;
  editor.Gutter.Color :=  $614326;
  editor.Gutter.Font.Color := $838383;
  SynGeneralSyn1.KeyAttri.Foreground := $0099FF;
  except
    on e: exception do
      showmessage ('Error in FormCreate: ' + e.message);
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  lock.Free;
end;

procedure TfrmMain.N2Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmMain.pnlRightResize(Sender: TObject);
begin
  pnlDrawing.Canvas.Brush.Color := RGB (brush_r, brush_g, brush_b);
  pnlDrawing.Picture.Bitmap.Width := pnlDrawing.Width;
  pnlDrawing.Picture.Bitmap.Height := pnlDrawing.Height;
  pnlDrawing.canvas.rectangle(0, 0, pnlDrawing.Width-1, pnlDrawing.Height-1);

  Bmp := pnlDrawing.Picture.Bitmap;
  Bmp.PixelFormat := pf24bit;
  Bmp.Width := pnlDrawing.Width;
  Bmp.Height := pnlDrawing.Height;
  setlength (line, Bmp.Height);
  for var i := 0 to Bmp.Height - 1 do
      line[i] :=  Bmp.ScanLine [i];
end;

procedure TfrmMain.Quit1Click(Sender: TObject);
begin
  loadScript;
end;

initialization
  TStyleManager.Engine.RegisterStyleHook(TSynEdit, TScrollBoxStyleHook);
end.
