unit uPlotPanel;

interface

uses SysUtils, Classes, System.Types, System.UIConsts, System.UITypes,
     skia, Vcl.skia, Vcl.Graphics;

type
  TNiceScale = class (TObject)
    private
      minPoint, maxPoint : double;
      range : double;
     procedure calculate;
    public
     niceMin, niceMax : double;
     maxTicks : double;
     tickSpacing : double;

     function niceNum(range : double; round : boolean) : double;
     constructor Create (amin, amax : double);
     procedure setMaxTicks(maxTicks : double);
     procedure setMinMaxPoints(minPoint, maxPoint : double);
  end;

  TDataColumn = record
                 visible : boolean;
                 x : array of double;
                 color : TAlphaColor;
                end;

  TDataSeries = class (TObject)
        XColumnIndex : integer;
        columns : array of TDataColumn;
        nRows, nColumns : integer;

        // i,j = row,column
        procedure   setData (i, j : integer; value : double);
        function    getData (i, j : integer) : double;
        procedure   createSpace  (nRows, nColumns : integer);
        constructor Create (nRows, nColumns : integer);
  end;

  TPlotPanel = class (TSkCustomControl)
   private
     x_vmax, y_vmax, x_vmin, y_vmin : double;

     typeface : ISkTypeface;
     font : ISkFont;
     myADest : TRectF;

     procedure setViewPort;
     procedure drawToCanvas (ACanvas : ISkCanvas; ADest : TRectF);
     procedure draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single); override;
     function  worldtoScreen(x_w, y_w : double) : TPointF;
     procedure writeText (ACanvas : ISkCanvas; value, x, y : double);
   public
     data : TDataSeries;
     backgroundColor : TAlphaColor;
     x_wmax, y_wmax, x_wmin, y_wmin : double;
     procedure resize; override;
     //procedure SkMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
     procedure exportToPdf (fileName : string);
     procedure exportToPng (fileName : string);
     procedure clearSpace();
     procedure allocateSpace (nRows, nColumns : integer);
     procedure setXAxisColumn (index : integer);
     procedure setDataColumnVisibility (index : integer; visible : boolean);
     procedure setColumnColor (index : integer; color : TAlphaColor);
     function  getColumnColor (index : integer) : TAlphaColor;
     procedure setData (i, j : integer; value : double);
     procedure clear;

     constructor Create (AOwner: TComponent); override;
   published
     property Top;
     property Left;
     //property OnMouseMove;
  end;



implementation

uses Math;

type
  TColors = array of TAlphaColor;

var
  colors: TColors;

// I got this from
// https://stackoverflow.com/questions/8506881/nice-label-algorithm-for-charts-with-minimum-ticks
// and translated it into Delphi.
// I think it orginially came from Graphics Gems (1990)
constructor TNiceScale.Create (amin, amax : double);
begin
  minPoint := amin;
  maxPoint := amax;
  maxTicks := 10;
  calculate();
end;


procedure TNiceScale.calculate;
begin
  range := niceNum(maxPoint - minPoint, false);
  tickSpacing := niceNum(range / (maxTicks - 1), true);
  niceMin := Math.floor(minPoint / tickSpacing) * tickSpacing;
  niceMax := Math.ceil(maxPoint / tickSpacing) * tickSpacing;
end;


function TNiceScale.niceNum(range : double; round : boolean) : double;
var
  exponent : double; // exponent of range
  fraction  : double; // fractional part of range
  niceFraction : double; // nice, rounded fraction
begin
  exponent := Math.floor(Math.log10(range));
  fraction := range / Math.power(10, exponent);

  if (round) then
     begin
     if (fraction < 1.5) then
         niceFraction := 1
     else if (fraction < 3) then
          niceFraction := 2
     else if (fraction < 7) then
          niceFraction := 5
     else
          niceFraction := 10;
     end
 else
     begin
     if (fraction <= 1) then
        niceFraction := 1
      else if (fraction <= 2) then
        niceFraction := 2
      else if (fraction <= 5) then
        niceFraction := 5
      else
        niceFraction := 10;
    end;

   result := niceFraction * Math.power(10, exponent);
end;


procedure TNiceScale.setMaxTicks(maxTicks : double);
begin
    maxTicks := maxTicks;
    calculate();
end;


procedure TNiceScale.setMinMaxPoints(minPoint, maxPoint : double);
begin
  minPoint := minPoint;
  maxPoint := maxPoint;
  calculate();
end;

// -------------------------------------------------------------------


constructor TDataSeries.Create (nRows, nColumns : integer);
begin
  XColumnIndex := 0;
  createSpace(nRows, nColumns);
end;


procedure TDataSeries.createSpace  (nRows, nColumns : integer);
var i : integer;
begin
  setLength (columns, nColumns);
  for i := 0 to nColumns - 1 do
      begin
      setLength (columns[i].x, nRows);
      columns[i].color := colors[i];
      columns[i].visible := True;
      end;
  self.nRows := nRows; self.nColumns := nColumns;
end;


procedure TDataSeries.setData (i, j : integer; value : double);
begin
  try
  columns[j].x[i] := value; // Data is stored as teh transpose
  except
    i := 1;
  end;
end;


function TDataSeries.getData (i, j : integer) : double;
begin
  result := columns[j].x[i];
end;

// -------------------------------------------------------------------


// --------------------------------------------------------------------

constructor TPlotPanel.Create (AOwner: TComponent);
begin
  inherited;
  setViewPort;

  x_wmax := 10; y_wmax := 10;
  x_wmin := 0;  y_wmin := 0;
  typeface := TSkTypeface.MakeFromName('Arial', TSkFontStyle.Normal);
  font := TSkFont.Create(typeface, 18, 1);
  data := TDataSeries.Create(0, 0);
  //HitTest := True; // to make it respond to mouse events
  backgroundColor := claWhite;
end;


procedure TPlotPanel.resize;
begin
  inherited;
  setViewPort;
end;


procedure TPlotPanel.clearSpace;
begin
  data.createSpace (0, 0);
end;

procedure TPlotPanel.allocateSpace(nRows: Integer; nColumns: Integer);
begin
  data.createSpace (nRows, nColumns);
end;


procedure TPlotPanel.setViewPort;
begin
  x_vmax := myADest.Width - 45;
  y_vmax := myADest.Height - 70;;
  x_vmin := 80; y_vmin := 60;
end;


procedure TPlotPanel.setColumnColor (index : integer; color : TAlphaColor);
begin
  data.columns[index].color := color;
end;


function TPlotPanel.getColumnColor (index : integer) : TAlphaColor;
begin
  result := data.columns[index].color;
end;


procedure TPlotPanel.setData (i, j : integer; value : double);
begin
  data.setdata (i, j, value);
end;


procedure TPlotPanel.clear;
begin
  clearSpace();
  allocateSpace(100, 2);
  redraw;
end;


// columns in the data series have a visibility flag.
// If set to false, a given column will not be plotted.
procedure TPlotPanel.setDataColumnVisibility (index : integer; visible : boolean);
begin
  data.columns[index].visible := visible;
end;


// We can set the x axis to whatever column we want in the dataseries
procedure TPlotPanel.setXAxisColumn (index : integer);
begin
  data.XColumnIndex := index;
end;


// Write text at pixel coordinates;
procedure TPlotPanel.writeText (ACanvas : ISkCanvas; value, x, y : double);
var LBlob: ISkTextBlob;
    LPaint : ISkPaint;
    ABounds : TRectF;
    tx, ty : single;
    astr: string;
    pt : TPoint;
begin
  LPaint := TSkPaint.Create;
  LPaint.Color := TAlphaColors.Black;
  LPaint.Style := TSkPaintStyle.Fill;

  astr := value.ToString;
  font.MeasureText(astr, ABounds, LPaint);
  font.Hinting := TSkFontHinting.Full;

  tx := x - ABounds.Width/2;
  ty := y + ABounds.Height/2;

  LBlob := TSkTextBlob.MakeFromText(astr, font);
  ACanvas.DrawTextBlob(LBlob, tx, ty, LPaint);
end;


procedure TPlotPanel.drawToCanvas (ACanvas : ISkCanvas; ADest : TRectF);
var LPaint: ISkPaint;
    LPath : ISkPathBuilder;
    Path : ISKPath;
    pt : TPointF;
    i, k : integer;
    nu : TNiceScale;
    maxTicks : integer;
    x, y : double;
    CDest : TRectF;
    topCorner, bottomRightcorner : TPointF;
    pt1, pt2 : TPointF;
begin
  if data.nRows < 2 then
     exit;

  x_vmax := ADest.Width - 45;
  y_vmax := ADest.Height - 70;;
  x_vmin := 80; y_vmin := 60;

  // Create clipping rectangle for the plotting area
  ACanvas.Save;
  try
    topCorner := worldtoScreen(x_wmin, y_wmax);
    bottomRightcorner := worldtoScreen(x_wmax, y_wmin);
    CDest.Left := topCorner.x;
    CDest.Top := topCorner.y;
    CDest.Width := bottomRightcorner.x - topCorner.x;
    CDest.Height := bottomRightcorner.y - topCorner.y;

    ACanvas.ClipRect(CDest);

    LPaint := TSkPaint.Create(TSkPaintStyle.Stroke);
    LPaint.StrokeJoin := TSkStrokeJoin.Round;
    LPaint.AntiAlias := True;
    LPaint.StrokeWidth := 2.5;
    LPaint.Color := claBlue;
    for k := 0 to data.nColumns - 1 do
        begin
        LPaint.Color := data.columns[k].color;
        if k <> data.XColumnIndex then
           begin
           if data.columns[k].visible then
              begin
              pt := worldtoScreen(data.getData(0,data.XColumnIndex), data.getData(0,k));
              LPath := TSkPathBuilder.Create;
              LPath.MoveTo (pt);
              for i := 1 to data.nRows - 1 do
                  begin
                     pt := worldtoScreen(data.getData(i,data.XColumnIndex), data.getData(i,k));
                     LPath.LineTo (pt);
                  end;

              path := LPath.Detach;
              ACanvas.DrawPath(path, LPaint);
              end;
           end;
        end;

  finally
    ACanvas.Restore;
  end;

  // Original clipping rectangle restored so that we can now to draw outside the plotting area.

  // Draw axes lines
  LPaint.StrokeWidth := 1;
  LPaint.Color := claBlack;

  // Draw tick marks
  nu := TNiceScale.Create (x_wmin, x_wmax);
  pt1 := worldtoScreen(nu.niceMin, 0);
  pt2 := worldtoScreen(nu.niceMax, 0);
  ACanvas.DrawLine (pt1, pt2, LPaint);
  try
    // Draw x-axis ticks
    maxTicks := trunc (nu.maxTicks);
    x := nu.nicemin;

    for i := 0 to maxTicks  - 2 do
        begin
        pt := worldtoScreen(x, 0);
        if x <> 0 then
           ACanvas.DrawLine(PointF(pt.x, pt.y), PointF(pt.x, pt.y+10), LPaint);
        if x = 0 then
           writeText (ACanvas, x, pt.x + 12, pt.y + 16)
        else
           writeText (ACanvas, x, pt.x, pt.y + 20);
        x := x + nu.tickSpacing;
        end;
  finally
    nu.free;
  end;

  // Draw axes lines
  LPaint.StrokeWidth := 1;
  LPaint.Color := claBlack;

  nu := TNiceScale.Create (y_wmin, y_wmax);
  ACanvas.DrawLine (worldtoScreen(0, nu.nicemin), worldtoScreen(0, nu.niceMax), LPaint);

  try
    // Draw y-axis ticks
    maxTicks := trunc (nu.maxTicks);
    y := nu.nicemin;

    for i := 0 to maxTicks - 2 do
        begin
        pt := worldtoScreen(0, y);
        if y <> 0 then
           ACanvas.DrawLine(PointF(pt.x, pt.y), PointF(pt.x-10, pt.y), LPaint);
        if y <> 0 then
           writeText (ACanvas, y, pt.x - 22, pt.y);
        y := y + nu.tickSpacing;
        end;
  finally
    nu.free;
  end;
end;


procedure TPlotPanel.Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
begin
  myADest := ADest;
  ACanvas.Save;
  try
    ACanvas.ClipRect(ADest);
    ACanvas.Clear (backgroundColor);
    drawToCanvas(ACanvas, ADest);
  finally
    ACanvas.Restore;
  end;
end;


function TPlotPanel.worldtoScreen(x_w, y_w : double) : TPointF;
var sx, sy : double;
begin
  // point on screen
  // calculating Sx and Sy
  sx := (x_vmax - x_vmin) / (x_wmax - x_wmin);
  sy := (y_vmax - y_vmin) / (y_wmax - y_wmin);

  // calculating the point on viewport
  result.x := (x_vmin + ((x_w - x_wmin) * sx));
  result.y := myADest.Height - (y_vmin + ((y_w - y_wmin) * sy));
end;


procedure TPlotPanel.exportToPdf (fileName : string);
var LPDFStream: TStream;
    LDocument: ISkDocument;
    LCanvas : ISkCanvas;
    ADest : TRectF;
begin
  LPDFStream := TFileStream.Create(fileName, fmCreate);
  try
    LDocument := TSkDocument.MakePDF(LPDFStream);
    try
      LCanvas := LDocument.BeginPage(Width, Height);
      try
        LCanvas.Save;
        try
          ADest.Top := 0; ADest.Right := 0;
          ADest.Width := Width; ADest.Height := Height;
          LCanvas.Clear (backgroundColor);
          drawToCanvas(LCanvas, ADest);
       finally
         LCanvas.Restore;
       end;
      finally
        LDocument.EndPage;
      end;
    finally
      LDocument.Close;
    end;
  finally
    LPDFStream.Free;
  end;
end;


procedure TPlotPanel.exportToPng (fileName : string);
var LSurface: ISkSurface;
    ADest : TRectF;
begin
  ADest.Top := 0; ADest.Right := 0;
  ADest.Width := Width; ADest.Height := Height;

  LSurface := TSkSurface.MakeRaster(trunc (Width), trunc (Height));
  LSurface.Canvas.Clear(backgroundColor);
  drawToCanvas(LSurface.Canvas, ADest);
  LSurface.MakeImageSnapshot.EncodeToFile(fileName);
end;


initialization
  setLength (colors, 7);
  colors[0] := claBrown;
  colors[1] := claRed;
  colors[2] := claBlue;
  colors[3] := claOrange;
  colors[4] := claGreen;
  colors[5] := claPurple;
  colors[6] := claLavender;
end.

