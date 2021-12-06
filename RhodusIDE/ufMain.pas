unit ufMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Types,
  Vcl.ExtCtrls, Vcl.FileCtrl, Vcl.ComCtrls, Vcl.Themes, System.UIConsts, System.UITypes,
  uExamples, Vcl.Menus, SyncObjs, SynEdit, SynEditHighlighter, SynEditCodeFolding, SynHighlighterPython, SynHighlighterGeneral,
  SynEditMiscClasses, SynEditSearch, StrUtils, IniFiles, //Skia, Skia.VCL,
  Img32, Img32.Fmt.PNG, Img32.vector, Img32.Draw, Vcl.Buttons, uRhodusLibTypes;

const RHODUS_IDE_VERSION = 0.5;

type
  TRGBTriple = packed record
    rgbtBlue: Byte;
    rgbtGreen: Byte;
    rgbtRed: Byte;
  end;
  TRGBTripleArray = ARRAY[Word] of TRGBTriple;
  pRGBTripleArray = ^TRGBTripleArray;

  TfrmMain = class(TForm)
    pnlTop: TPanel;
    btnRun: TButton;
    pnlLeftPanel: TPanel;
    pnlEditor: TPanel;
    btnLoad: TButton;
    OpenDialog: TOpenDialog;
    lblVersion: TLabel;
    DirectoryListBox1: TDirectoryListBox;
    FileListBox1: TFileListBox;
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
    SynGeneralSyn1: TSynGeneralSyn;
    SynEditSearch1: TSynEditSearch;
    mnuSave: TMenuItem;
    btnSave: TButton;
    SaveDialog: TSaveDialog;
    pblBottomBase: TPanel;
    Splitter1: TSplitter;
    memoOutput: TMemo;
    pnlMemoButtons: TPanel;
    btnClear: TButton;
    pnlSideControls: TPanel;
    btnCloseSidePanel: TSpeedButton;
    Splitter4: TSplitter;
    mnuSettings: TMenuItem;
    mnuPreferences: TMenuItem;
    SynEditor: TSynEdit;
    procedure btnRunClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure DirectoryListBox1Change(Sender: TObject);
    procedure FileListBox1Click(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure cboExamplesChange(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure N2Click(Sender: TObject);
    procedure pnlRightResize(Sender: TObject);
    procedure mnuNewClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mnuSaveClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnCloseSidePanelClick(Sender: TObject);
    procedure mnuPreferencesClick(Sender: TObject);
  private
    { Private declarations }
    examples : TExamples;
    bmp : TBitmap;
    Line: array of pRGBTripleArray;
    previous_x, previous_y : double;  // Used to implementmoveto, lineto in image32
    procedure setUpDefaultColors;
    procedure writeControlPlacement;
    procedure readControlPlacement;
    function  getContrastingColor(Color: TColor): TColor;
  public
    { Public declarations }
    Lock: TCriticalSection;
    //LPaint: ISkPaint;
    //LSurface : ISkSurface;
    //LBitmap : TBitmap;
    img : TImage32;

    procedure loadScript;
    procedure saveScript;
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

Uses IOUtils,
     VCL.GraphUtil,
     ufAbout,
     uLoadRhodus,
     uPreferences,
     ufPreferences;

var rhodus : THandle;

    pen_r, pen_g, pen_b : integer;
    brush_r, brush_g, brush_b : integer;
    penWidth : double;
    updateStatus : boolean;


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
  errorId := rhodus_run (rhodus, frmMain.synEditor.text);
  if errorId < 0 then
     begin
     pt := rhodus_getLastError (rhodus);
     frmMain.memoOutput.Lines.Add(AnsiString (pt.errorMsg));
     end;
  FLock.Release;
end;


procedure rhodus_print (const astr : AnsiString);
begin
  if astr = sLineBreak then
     frmMain.memoOutput.Lines[frmMain.memoOutput.CaretPos.Y] := frmMain.memoOutput.Lines[frmMain.memoOutput.CaretPos.y] + astr
  else
     frmMain.memoOutput.Lines[frmMain.memoOutput.CaretPos.Y] := frmMain.memoOutput.Lines[frmMain.memoOutput.CaretPos.y] + astr;
end;


procedure rhodus_println (const astr : AnsiString);
begin
  frmMain.memoOutput.Lines.Append (astr);
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
    frm.lblMessage.Caption := 'Rhodus IDE Version: ' + floattostr (RHODUS_IDE_VERSION);
    frm.ShowModal;
  finally
    frm.free;
  end;
end;


procedure TfrmMain.btnClearClick(Sender: TObject);
begin
  memoOutput.Clear;
end;

procedure TfrmMain.btnCloseSidePanelClick(Sender: TObject);
begin
  if pnlLeftPanel.Width < 120 then
     begin
     DirectoryListBox1.Visible := true;
     FileListBox1.Visible := true;
     pnlLeftPanel.Width := 300;
     end
  else
    begin
    DirectoryListBox1.Visible := false;
    FileListBox1.Visible := false;
    pnlLeftPanel.Width := 60;
    end;
end;

// --------------------------------------------------------------


procedure updateCanvas;
begin
  TThread.Synchronize(nil,
    procedure
    begin
    frmMain.img.CopyToDc(frmMain.pnlDrawing.Picture.Bitmap.Canvas.Handle);
    frmMain.pnlDrawing.Invalidate;
    end);
end;

procedure gBeginUpdate;
begin
  updateStatus := True;
end;


procedure gEndUpdate;
begin
  if updateStatus = True then
     updateCanvas;

  updateStatus := False;
end;


procedure gClearColor (r, g, b : integer);
begin
  TThread.Synchronize(nil,
    procedure
    begin
//    frmMain.LBitmap.SkiaDraw(
//      procedure (const ACanvas: ISkCanvas)
//      begin
//        frmMain.LPaint.Color := TAlphaColor($FF000000 or makeColor (brush_r, brush_g, brush_b));
//        ACanvas.DrawRect(TRectF.Create(0, 0, frmMain.pnlDrawing.Width, frmMain.pnlDrawing.Height), frmMain.LPaint);
//      end);
//    frmMain.pnlDrawing.Picture.Assign(frmMain.LBitmap);

    brush_r := r; brush_g := g; brush_b := b;
    frmMain.img.clear (Img32.Color32($FF, byte (brush_r), byte (brush_g), byte (brush_b)));
    //frmMain.img.CopyToDc(frmMain.pnlDrawing.Picture.Bitmap.Canvas.Handle);
    //updateCanvas;

    //frmMain.pnlDrawing.Canvas.Pen.Color := RGB (brush_r, brush_g, brush_b);
    //frmMain.pnlDrawing.Canvas.Brush.Style := TBrushStyle.bsSolid;
    //frmMain.pnlDrawing.Canvas.Brush.Color := RGB (brush_r, brush_g, brush_b);
    //frmMain.pnlDrawing.canvas.rectangle(0, 0, frmMain.pnlDrawing.Width, frmMain.pnlDrawing.Height);
    end);
end;


procedure gRefresh;
begin
  TThread.Synchronize(nil,
    procedure
    begin
    updateCanvas;
    end);
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
    frmMain.img.pixel[x, y] := Color32 ($FF, pen_r, pen_g, pen_b);
    if not updateStatus then
       frmMain.img.CopyToDc(frmMain.pnlDrawing.Picture.Bitmap.Canvas.Handle);

    //bigP := frmMain.Line[x];
    //bigP[y].rgbtRed := pen_r;// := RGB (pen_r, pen_g, pen_b);
    //bigP[y].rgbtGreen := pen_g;
    //bigP[y].rgbtBlue := pen_b;
    Application.ProcessMessages;
    if not updateStatus then
       frmMain.pnlDrawing.Invalidate;
    end
  );
end;


procedure gDrawRect (x, y, w, h : double);
var path : TPathD;
begin
  TThread.Synchronize(nil,
    procedure
    begin
//    frmMain.LBitmap.SkiaDraw(
//      procedure (const ACanvas: ISkCanvas)
//      begin
//        frmMain.LPaint.Color := TAlphaColor($FF000000 or makeColor (pen_r, pen_g, pen_b));
//        ACanvas.DrawRect(TRectF.Create(trunc (x), trunc (y), trunc (x + w), trunc (y + h)), frmMain.LPaint);
//      end);
//    frmMain.pnlDrawing.Picture.Assign(frmMain.LBitmap);

    path := Rectangle(RectD(x, y, x + w, y + h));
    DrawLine(frmMain.img, path, penWidth, Color32($FF, pen_r, pen_g, pen_b), esPolygon);

    if not updateStatus then
       updateCanvas

    //frmMain.pnlDrawing.Canvas.Pen.Width := trunc (penWidth);
    //frmMain.pnlDrawing.Canvas.Pen.Color := RGB (pen_r, pen_g, pen_b);
    //frmMain.pnlDrawing.Canvas.Rectangle(trunc (x), trunc (y), trunc (x + w), trunc(y + h));
    end
  );
end;

procedure gDrawFilledRect (x, y, w, h : double);
var path : TPathD;
begin
  TThread.Synchronize(nil,
    procedure
    begin
    path := Rectangle(RectD(x, y, x + w, y + h));
    DrawLine(frmMain.img, path, penWidth, Color32 ($FF, pen_r, pen_g, pen_b), esPolygon);
    DrawPolygon(frmMain.img, path, frNonZero, Color32 ($FF, brush_r, brush_g, brush_b));

    if not updateStatus then
       updateCanvas


    //frmMain.pnlDrawing.Canvas.Pen.Width := trunc (penWidth);
    //frmMain.pnlDrawing.Canvas.Pen.Color := RGB (pen_r, pen_g, pen_b);
    //frmMain.pnlDrawing.Canvas.Brush.Style := TBrushStyle.bsSolid;
    //frmMain.pnlDrawing.Canvas.Brush.Color := RGB (brush_r, brush_g, brush_b);
    //frmMain.pnlDrawing.Canvas.Rectangle(trunc (x), trunc (y), trunc (x + w), trunc(y + h));
    //frmMain.pnlDrawing.Refresh;
    end
  );
end;

procedure gDrawEllipse (x1, y1, x2, y2 : double);
var
  //LOval: ISkRoundRect;
  //LRect: TRectF;
  path : TPathD;
begin
  TThread.Synchronize(nil,
    procedure
    begin
    //frmMain.LBitmap.SkiaDraw(
    //  procedure (const ACanvas: ISkCanvas)
    //  begin
    //    LRect := TRectF.Create(TPointF.Create(trunc (x1), trunc (y1)), trunc (x2), trunc (y2));
    //    frmMain.LPaint.Color := TAlphaColor($FF000000 or makeColor (pen_r, pen_g, pen_b));
    //    LOval := TSkRoundRect.Create;
    //    LOval.SetOval(LRect);
    //    ACanvas.DrawRoundRect(LOval, frmMain.LPaint);
    //  end);
    //frmMain.pnlDrawing.Picture.Assign(frmMain.LBitmap);

    path := Img32.Vector.Ellipse(RectD (x1, y1, x2, y2));
    DrawLine(frmMain.img, path, penWidth, Color32 ($FF, pen_r, pen_g, pen_b), esPolygon);

    if not updateStatus then
       updateCanvas;

    //frmMain.pnlDrawing.Canvas.Pen.Width := trunc (penWidth);
    //frmMain.pnlDrawing.Canvas.Pen.Color := RGB (pen_r, pen_g, pen_b);
    //frmMain.pnlDrawing.Canvas.Brush.Style := TBrushStyle.bsClear;
    //frmMain.pnlDrawing.Canvas.Ellipse(trunc (x1), trunc (y1), trunc (x2), trunc(y2));
    end
  );
end;

procedure gDrawFilledEllipse (x1, y1, x2, y2 : double);
var path : TPathD;
begin
  TThread.Synchronize(nil,
    procedure
    begin
    path := Img32.Vector.Ellipse(RectD (x1, y1, x2, y2));
    DrawLine(frmMain.img, path, penWidth, Color32 ($FF, pen_r, pen_g, pen_b), esPolygon);
    DrawPolygon(frmMain.img, path, frNonZero, Color32 ($FF, brush_r, brush_g, brush_b));

    if not updateStatus then
       updateCanvas

    //frmMain.pnlDrawing.Canvas.Pen.Width := trunc (penWidth);
    //frmMain.pnlDrawing.Canvas.Brush.Style := TBrushStyle.bsSolid;
    //frmMain.pnlDrawing.Canvas.Brush.Color := RGB (brush_r, brush_g, brush_b);
    //frmMain.pnlDrawing.Canvas.Pen.Color := RGB (pen_r, pen_g, pen_b);
    //frmMain.pnlDrawing.Canvas.Ellipse(trunc (x1), trunc (y1), trunc (x2), trunc(y2));
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
    //frmMain.pnlDrawing.canvas.moveTo (trunc (x), trunc (y));
    frmMain.previous_x := x; frmMain.previous_y := y;
    end
  );
end;

procedure gLineTo (x, y : double);
begin
  TThread.Synchronize(nil,
    procedure
    begin
    DrawLine (frmMain.img, PointD(frmMain.previous_x, frmMain.previous_y),
               PointD (x, y), penWidth, Color32 ($FF, pen_r, pen_g, pen_b));
    frmMain.previous_x := x; frmMain.previous_y := y;

    if not updateStatus then
       updateCanvas

    //frmMain.pnlDrawing.Canvas.Pen.Color := RGB (pen_r, pen_g, pen_b);
    //frmMain.pnlDrawing.Canvas.Pen.Width := trunc (penWidth);
    //frmMain.pnlDrawing.canvas.lineTo (trunc (x), trunc (y));
    end
  );
end;

procedure gLineWithColor (penColor : AnsiString; x1, y1, x2, y2 : double);
var aColor : TAlphaColorRec;
begin
  TThread.Synchronize(nil,
    procedure
    begin
    if penColor = '' then
       DrawLine (frmMain.img, PointD(x1, y1), PointD (x2, y2), penWidth, Color32 ($FF, pen_r, pen_g, pen_b))
    else
       begin
       acolor := TAlphaColorRec (StringToAlphaColor (penColor));
       DrawLine (frmMain.img, PointD(x1, y1), PointD (x2, y2), penWidth, Color32 ($FF, acolor.R, acolor.G, acolor.B));
       end;

    if not updateStatus then
       updateCanvas
    end
  );
end;

// ----------------------------------------------------------------

procedure TfrmMain.loadScript;
begin
 if OpenDialog.Execute then
    synEditor.Lines.LoadFromFile(opendialog.FileName);
end;


procedure TfrmMain.saveScript;
begin
  if SaveDialog.Execute then
     synEditor.Lines.SaveToFile(saveDialog.FileName);
end;


procedure TfrmMain.mnuNewClick(Sender: TObject);
begin
  synEditor.Clear;
end;

procedure TfrmMain.mnuPreferencesClick(Sender: TObject);
begin
  frmPreferences := TfrmPreferences.Create (nil);
  try
    frmPreferences.spFontSize.Value := preferences.editor.fontSize;
    frmPreferences.setFontName (preferences.editor.fontName);
    frmPreferences.setTheme (preferences.IDE.theme);
    if frmPreferences.ShowModal = mrOk then
       begin
       synEditor.Font.Size := preferences.editor.fontSize;
       synEditor.Font.Name := preferences.editor.fontName;
       TStyleManager.SetStyle(preferences.IDE.theme);
       synEditor.Color := TStyleManager.ActiveStyle.GetSystemColor(clWindow);
       synEditor.Font.Color := getContrastingColor(synEditor.Color);
       synEditor.Gutter.Color := TStyleManager.ActiveStyle.GetSystemColor(clBtnFace);
       synEditor.Gutter.Font.Color := getContrastingColor(synEditor.Color);
       end;
  finally
    frmPreferences.Free;
  end;
end;

procedure TfrmMain.btnLoadClick(Sender: TObject);
begin
  loadScript;
end;

procedure TfrmMain.btnRunClick(Sender: TObject);
begin
   setUpDefaultColors;
   rt := TRhodusWorker.Create (Lock);
end;

procedure TfrmMain.btnSaveClick(Sender: TObject);
begin
  saveScript;
end;

procedure TfrmMain.cboExamplesChange(Sender: TObject);
begin
  synEditor.Lines.Text := (cboExamples.items.Objects[cboExamples.ItemIndex] as TExample).src;
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
     synEditor.Lines.Text := TFile.ReadAllText(filename);
end;


procedure TfrmMain.setUpDefaultColors;
var acolor : TAlphaColor;
begin
  acolor := StringToAlphaColor('OldLace');
  brush_r := TAlphaColorRec(acolor).R;
  brush_g := TAlphaColorRec(acolor).G;
  brush_b := TAlphaColorRec(acolor).B;
  pen_r := 255; pen_g := 0; pen_b := 0;
  penwidth := 1;
end;

procedure TfrmMain.writeControlPlacement;
var
   iniFile  : TIniFile;
   idx : integer;
   ctrl : TControl;
begin
   iniFile := TIniFile.Create(ChangeFileExt(Application.ExeName,'.ini')) ;
   try
    savePreferences (iniFile);

    iniFile.WriteInteger('MainForm', 'Top', self.Top);
    iniFile.WriteInteger('MainForm', 'Left', self.Left);
    iniFile.WriteInteger('MainForm', 'Height', self.Height);
    iniFile.WriteInteger('MainForm', 'Width', self.Width);

     for idx := 0 to -1 + Self.ComponentCount do
     begin
       if Components[idx] is TControl then
       begin
         ctrl := TControl(Components[idx]);
         if MatchText(ctrl.name, ['pnlLeftPanel', 'memoOutput', 'synEditor', 'pnlInfo', 'pnlDrawing', 'pnlRight']) then
            begin
            iniFile.WriteInteger(ctrl.Name,'Top',ctrl.Top) ;
            iniFile.WriteInteger(ctrl.Name,'Left',ctrl.Left) ;
            iniFile.WriteInteger(ctrl.Name,'Width',ctrl.Width) ;
            iniFile.WriteInteger(ctrl.Name,'Height',ctrl.Height) ;
            end;
       end;
     end;
   finally
     FreeAndNil(iniFile) ;
   end;
end;

procedure TfrmMain.readControlPlacement;
var
   iniFile  : TIniFile;
   idx : integer;
   ctrl : TControl;
begin
   iniFile := TIniFile.Create(ChangeFileExt(Application.ExeName,'.ini')) ;
   try
     loadPreferences (iniFile);

     Top := iniFile.ReadInteger('MainForm', 'Top', 100);
     Left := iniFile.ReadInteger('MainForm', 'Left', 100);
     Height := iniFile.ReadInteger('MainForm', 'Height', 960);
     Width := iniFile.ReadInteger('MainForm', 'Width', 1400);

     for idx := 0 to -1 + Self.ComponentCount do
     begin
       if Components[idx] is TControl then
       begin
         ctrl := TControl(Components[idx]) ;
         ctrl.Top := iniFile.ReadInteger(ctrl.Name,'Top',ctrl.Top) ;
         ctrl.Left := iniFile.ReadInteger(ctrl.Name,'Left',ctrl.Left) ;
         ctrl.Width := iniFile.ReadInteger(ctrl.Name,'Width',ctrl.Width) ;
         ctrl.Height := iniFile.ReadInteger(ctrl.Name,'Height',ctrl.Height) ;
       end;
     end;
   finally
     FreeAndNil(iniFile) ;
   end;
end;


procedure TfrmMain.mnuSaveClick(Sender: TObject);
begin
  saveScript;
end;


function TfrmMain.getContrastingColor(Color: TColor): TColor;
var r,g,b:double;i:integer;
begin
Color := ColorToRGB(Color);
r:=GetRValue(Color) ;
g:=GetGValue(Color) ;
b:=GetBValue(Color) ;
i:=round( Sqrt(
      r * r * 0.241 +
      g * g * 0.691 +
      b * b * 0.068));
if (i > 128) then   // treshold seems good in wide range
  Result := clBlack
else
  Result := clWhite;
end;


procedure TfrmMain.FormCreate(Sender: TObject);
var i : integer;
begin
  readControlPlacement;
  if not loadRhodusDll then
     begin
     showmessage ('Failed to locate and load the rhodus runtime library');
     exit;
     end;
  try
    // set up all the callbacks
    config.printPtr := rhodus_print;
    config.printlnPtr := rhodus_println;
    config.readStringPtr := rhodus_readString;

    graphicsMethods.refresh := gRefresh;
    graphicsMethods.clearColor := gClearColor;
    graphicsMethods.getCanvasSize := gGetCanvasSize;
    graphicsMethods.setPixel := gSetPixel;
    graphicsMethods.moveTo := gMoveTo;
    graphicsMethods.lineTo := gLineTo;
    graphicsMethods.lineWithColor := gLineWithColor;
    graphicsMethods.setPenColor := gSetPenColor;
    graphicsMethods.setPenWidth := gSetPenWidth;
    graphicsMethods.setBrushColor := gSetBrushColor;
    graphicsMethods.drawRectangle := gDrawRect;
    graphicsMethods.drawFilledRectangle := gDrawFilledRect;
    graphicsMethods.drawEllipse := gDrawEllipse;
    graphicsMethods.drawFilledEllipse := gDrawFilledEllipse;
    graphicsMethods.beginupdate := gBeginUpdate;
    graphicsMethods.endupdate := gEndUpdate;

    // Assign the graphics handler list
    config.graphicsHandlerPtr := @graphicsMethods;

    // Use to control whether after a drwing command the screen is updated
    updateStatus := False;

    rhodus := rhodus_initialize (config);
    lblVersion.caption := 'Running Version: ' + AnsiString (rhodus_getSettings(rhodus).versionStr);

    // Load in the exmpel scripts
    examples := TExamples.Create;
    for i := 0 to examples.Count - 1 do
        cboExamples.AddItem(examples[i].name, examples[i]);
    cboExamples.ItemIndex := -1;

    setUpDefaultColors;
    Lock := TCriticalSection.Create;

//    Bmp := pnlDrawing.Picture.Bitmap;
//    Bmp.PixelFormat := pf24bit;
//    Bmp.Width := pnlDrawing.Width;
//    Bmp.Height := pnlDrawing.Height;
//    setlength (line, Bmp.Height);
//    for i := 0 to Bmp.Height - 1 do
//        line[i] :=  Bmp.ScanLine [i];

    //synEditor.Color := $614326;
    //synEditor.Font.Color := $F0FBFF;
    //synEditor.Gutter.Color :=  $614326;
    //synEditor.Gutter.Font.Color := $838383;
    //SynGeneralSyn1.KeyAttri.Foreground := $0099FF;

    synEditor.Font.Size := preferences.editor.fontSize;
    synEditor.Font.Name := preferences.editor.fontName;
    TStyleManager.SetStyle(preferences.IDE.theme);

    synEditor.Color := TStyleManager.ActiveStyle.GetSystemColor(clWindow);
    synEditor.Font.Color := getContrastingColor(synEditor.Color);

    synEditor.Gutter.Color := TStyleManager.ActiveStyle.GetSystemColor(clBtnFace);
    synEditor.Gutter.Font.Color := getContrastingColor(synEditor.Color);

    // Old skia code
    //LSurface := TSkSurface.MakeRaster(pnlDrawing.Width, pnlDrawing.Height);
    //LPaint := TSkPaint.Create;
    //LPaint.setAntiAlias(true);
    //LPaint.StrokeWidth := 1;
    //LPaint.Style := TSkPaintStyle.Stroke;
    //LBitmap := TBitmap.Create (pnlDrawing.Width, pnlDrawing.Height);

    img := TImage32.Create(pnlDrawing.Width, pnlDrawing.Height);
    pnlDrawing.Picture.Bitmap.SetSize(img.Width, img.Height);

    gClearColor (brush_r, brush_g, brush_b);
    gRefresh;
  except
    on e: exception do
      showmessage ('Error in FormCreate: ' + e.message);
  end;
end;


procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  writeControlPlacement;
  lock.Free;
end;

procedure TfrmMain.N2Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmMain.pnlRightResize(Sender: TObject);
begin
  img := TImage32.Create(pnlDrawing.Width, pnlDrawing.Height);
  pnlDrawing.Picture.Bitmap.SetSize(img.Width, img.Height);

//  pnlDrawing.Canvas.Brush.Color := RGB (brush_r, brush_g, brush_b);
//  pnlDrawing.Picture.Bitmap.Width := pnlDrawing.Width;
//  pnlDrawing.Picture.Bitmap.Height := pnlDrawing.Height;
//  pnlDrawing.canvas.rectangle(0, 0, pnlDrawing.Width-1, pnlDrawing.Height-1);
//
//  Bmp := pnlDrawing.Picture.Bitmap;
//  Bmp.PixelFormat := pf24bit;
//  Bmp.Width := pnlDrawing.Width;
//  Bmp.Height := pnlDrawing.Height;
//  setlength (line, Bmp.Height);
//  for var i := 0 to Bmp.Height - 1 do
//      line[i] :=  Bmp.ScanLine [i];
end;


initialization
  // Ensures that the TSybEdit is corectly themed
  TStyleManager.Engine.RegisterStyleHook(TCustomSynEdit, TScrollBoxStyleHook);
end.
