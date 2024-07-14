unit uBuiltInASCIIPlot;

{
  Unit:    uBuiltInFile.pas
  Author:  Herbert M sauro
  Date:    10/2021
  Purpose: This file implements ASCII plotting for the Rhodus interpreter.

  Ths source is distributed under Apache 2.0
  See https://www.apache.org/licenses/LICENSE-2.0.txt for further information

  Copyright (C)  2019-2024 Herbert M Sauro

  Author Contact Information:
  email: hsauro@gmail.com
}


interface

Uses SysUtils, Classes, Generics.Collections, uLibModule;


type



  TBuiltInASCIIPlot = class (TModuleLib)
     private
        fileHandleCounter : integer;
     public

       procedure barplot (vm : TObject);
       procedure setSymbol (vm : TObject);
       procedure drawLinePLot (vm : TObject);

       procedure prettyPrintMatrix (vm : TObject);

       constructor Create;
       destructor  Destroy; override;
  end;

implementation

Uses Math,
     uSymbolTable,
     uVMExceptions,
     uVM,
     uHelpUnit,
     uStringObject,
     uListObject,
     uMemoryManager,
     IOUtils,
     uArrayObject,
     uMatrixObject,
     uMachineStack;

const
  MAX_HEIGHT = 25;
  BAR_CHAR = '.';
  SPACE_CHAR = ' ';
  LABEL_COUNT = 5;

  // Sizes of line plot
  WIDTH = 60;
  HEIGHT = 20;

type
  TDataArray = TArray<Double>;
  TOrientation = (Vertical, Horizontal);

  TPoint = record
    x: Double;
    y: Double;
  end;

  TFloatArray = TArray<double>;
  TStringRow = TArray<string>;
  TStringArray = TArray<TStringRow>;
  TIntArray = TArray<Integer>;

var
  MaxHeightBars : integer = MAX_HEIGHT;
  barCHar : string = BAR_CHAR;

constructor TBuiltInASCIIPlot.Create;
begin
  inherited Create ('txtplot');

  addMethod (barplot,   2, 'bar',   'Draw an ASCII bar plot: txtplot.bar (array)');
  addmethod (setSymbol, 1, 'setSymbol', 'Set symbol, txtplot.setSymbol ("x")');
  addMethod (drawlineplot,  2, 'line', 'Draw an ASCII line plot, txtplot.lineplot (x, y)');

  addMethod (prettyPrintMatrix, 3, 'tabulate', 'Pretty print a matrix');

   //addValue ('pi', Pi,      'The value of pi');
end;


destructor TBuiltInASCIIPlot.Destroy;
begin
  inherited;
end;


procedure DrawBarGraph(const Data: TDataArray; includeData : boolean);
var
  i, j, BarHeight, BarWidth: Integer;
  MaxValue, ScaledValue: Double;
  GraphWidth: Integer;
  LabelValue : double;
  LabelPosition, PrevPosition : integer;
begin
  // Find the maximum value in the data array
  MaxValue := 0;
  for i := 0 to High(Data) do
      if Data[i] > MaxValue then
         MaxValue := Data[i];

  GraphWidth := MaxHeightBars * 2;

  for i := 0 to High(Data) do
      begin
      BarWidth := Round((Data[i] * GraphWidth) / (MaxValue));
      Write(Format('%2d |', [i + 1]));
      for j := 1 to BarWidth do
          Write(barChar);
      if includeData then
         WriteLn(Format(' %.2f', [Data[i]]))
      else
         writeln;
      end;

      // Draw the x-axis
      Write('   +');
      for i := 1 to GraphWidth do
          Write('-');
      WriteLn;

      // Draw the x-axis labels
      Write('    ');
      for i := 0 to LABEL_COUNT - 1 do
          begin
          LabelValue := i * MaxValue / (LABEL_COUNT - 1);
          LabelPosition := Round((LabelValue * GraphWidth) / MaxValue);
          if i > 0 then
             Write(StringOfChar(SPACE_CHAR, LabelPosition - PrevPosition - 1));
          Write(Format('%.2f', [LabelValue]));
          PrevPosition := LabelPosition + Length(Format('%.2f', [LabelValue])) - 1;
          end;
  WriteLn;
end;


procedure PlotData(const data: TArray<TPoint>; draw_box: Boolean);
var
  graph: array[0..HEIGHT-1, 0..WIDTH-1] of Char;
  i, j: Integer;
  x_min, x_max, y_min, y_max: Double;
  x_scale, y_scale: Double;
  x, y: Integer;
  x_axis, y_axis: Integer;
  dataSize : integer;
begin
  // Initialize graph with spaces
  for i := 0 to HEIGHT - 1 do
      for j := 0 to WIDTH - 1 do
        graph[i, j] := ' ';

  dataSize := length(data);
  // Find min and max values
  x_min := data[0].x;
  x_max := data[0].x;
  y_min := data[0].y;
  y_max := data[0].y;
  for i := 1 to dataSize - 1 do
      begin
      if data[i].x < x_min then x_min := data[i].x;
      if data[i].x > x_max then x_max := data[i].x;
      if data[i].y < y_min then y_min := data[i].y;
      if data[i].y > y_max then y_max := data[i].y;
      end;

  if x_min = x_max then
     raise ERuntimeException.Create('X data is not valid, only contains zeros');

  // Calculate scaling factors
  x_scale := (WIDTH - 1) / (x_max - x_min);
  y_scale := (HEIGHT - 1) / (y_max - y_min);

  // Plot the data points
  for i := 0 to dataSize - 1 do
      begin
      x := Round((data[i].x - x_min) * x_scale);
      y := Round((HEIGHT - 1) - (data[i].y - y_min) * y_scale);
      if (x >= 0) and (x < WIDTH) and (y >= 0) and (y < HEIGHT) then
        graph[y, x] := barChar[1];
      end;

  // Draw x and y axes
  x_axis := Round((0 - y_min) * y_scale);
  y_axis := Round((0 - x_min) * x_scale);
  if (x_axis >= 0) and (x_axis < HEIGHT) then
    for j := 0 to WIDTH - 1 do
      if graph[x_axis, j] = ' ' then graph[x_axis, j] := '-';

  if (y_axis >= 0) and (y_axis < WIDTH) then
    for i := 0 to HEIGHT - 1 do
      if graph[i, y_axis] = ' ' then graph[i, y_axis] := '|';
  if (x_axis >= 0) and (x_axis < HEIGHT) and (y_axis >= 0) and (y_axis < WIDTH) then
    graph[x_axis, y_axis] := '+';

  // Draw box if requested
  if draw_box then
    begin
    for i := 0 to HEIGHT - 1 do
      begin
      if graph[i, 0] = ' ' then graph[i, 0] := '|';
      if graph[i, WIDTH - 1] = ' ' then graph[i, WIDTH - 1] := '|';
      end;
    for j := 0 to WIDTH - 1 do
      begin
      if graph[0, j] = ' ' then graph[0, j] := '-';
      if graph[HEIGHT - 1, j] = ' ' then graph[HEIGHT - 1, j] := '-';
      end;
    graph[0, 0] := '+';
    graph[0, WIDTH - 1] := '+';
    graph[HEIGHT - 1, 0] := '+';
    graph[HEIGHT - 1, WIDTH - 1] := '+';
    end;

  // Print the graph
  for i := 0 to HEIGHT - 1 do
    begin
    for j := 0 to WIDTH - 1 do
      Write(graph[i, j]);
    WriteLn;
    end;

  // Print scale information
  WriteLn(Format('X-axis: %.2f to %.2f', [x_min, x_max]));
  WriteLn(Format('Y-axis: %.2f to %.2f', [y_min, y_max]));
end;


procedure PrettyPrint2DArray(const arr: TMatrixObject; with_borders: Boolean; decimal_places: Integer);
var
  cell_content: TStringArray;
  col_widths: TIntArray;
  i, j, k: Integer;
  format: String;
  len: Integer;
  rows, cols : integer;
  value : double;
begin
  rows := arr.numRows;
  cols := arr.numCols;

  SetLength(cell_content, rows, cols);
  SetLength(col_widths, cols);

  // Initialize column widths to zero
  for j := 0 to cols - 1 do
    col_widths[j] := 0;

  // Create format string
  format := '%.' + IntToStr(decimal_places) + 'f';

  // Convert floats to strings and find max width for each column
  for i := 0 to rows - 1 do
    for j := 0 to cols - 1 do
    begin
      value := arr[i,j];
      cell_content[i][j] := SysUtils.Format(format, [value]);
      len := Length(cell_content[i][j]);
      if len > col_widths[j] then
        col_widths[j] := len;
    end;

  if with_borders then
  begin
    // Print top border
    Write('+');
    for j := 0 to cols - 1 do
    begin
      for k := 0 to col_widths[j] + 1 do
        Write('-');
      Write('+');
    end;
    WriteLn;
  end;

  // Print rows
  for i := 0 to rows - 1 do
  begin
    if with_borders then Write('|');
    for j := 0 to cols - 1 do
    begin
      if with_borders then write (' ') else write ('');

      //Write(IfThen(with_borders, ' ', ''));
      Write(cell_content[i][j]:col_widths[j]);
      if with_borders then write(' |') else
         if j < cols - 1 then write (' ') else write ('');
      //Write(IfThen(with_borders, ' |', IfThen(j < cols - 1, '  ', '')));
    end;
    WriteLn;

    // Print row separator if not the last row
    if with_borders and (i < rows - 1) then
    begin
      Write('+');
      for j := 0 to cols - 1 do
      begin
        for k := 0 to col_widths[j] + 1 do
          Write('-');
        Write('+');
      end;
      WriteLn;
    end;
  end;

  if with_borders then
  begin
    // Print bottom border
    Write('+');
    for j := 0 to cols - 1 do
    begin
      for k := 0 to col_widths[j] + 1 do
        Write('-');
      Write('+');
    end;
    WriteLn;
  end;
end;


procedure TBuiltInASCIIPlot.prettyPrintMatrix (vm : TObject);
var m : TMatrixObject;
    dplaces : integer;
    drawBorder : boolean;
begin
  dplaces := TVM (vm).popInteger;
  drawBorder := TVM (vm).popBoolean;
  m := TVM (vm).popMatrix;

  PrettyPrint2DArray(m, drawBorder, dPlaces);
  TVM (vm).pushNone;
end;


procedure TBuiltInASCIIPlot.setSymbol (vm : TObject);
begin
  barChar := TVM (vm).popString.value;
  TVM (vm).pushNone;
end;


procedure TBuiltInASCIIPlot.barplot (vm : TObject);
var adata : TArrayObject;
    data : TDataArray;
    i : integer;
    includeData : boolean;
begin
   includeData := TVM (vm).popBoolean;
   adata := TVM (vm).popArray;
   if adata.ndims = 1 then
      begin
      maxHeightBars := 15;
      setlength (data, adata.dim[0]);
      for i:= 0 to adata.dim[0] - 1 do
          data[i] := adata.getValue(i);
      drawBarGraph(data, includeData)
      end
   else
      raise ERuntimeException.Create('Array must be one dimensional');
   TVM (vm).pushNone;
end;


procedure TBuiltInASCIIPlot.drawLinePlot (vm : TObject);
var x, y : TArrayObject;
    data : TArray<TPoint>;
    i : integer;
begin
   y := TVM (vm).popArray;
   x := TVM (vm).popArray;
   if (x.ndims = 1) and (y.ndims = 1) then
      begin
      if x.dim[0] = y.dim[0] then
         begin
         setlength (data, x.dim[0]);
         for i := 0 to x.dim[0] - 1 do
             begin
             data[i].x := x.getValue(i);
             data[i].y := y.getValue(i);
             end;
         plotData(data, true);
         end
      else
         raise  ERuntimeException.Create('x and y arrays must be one dimensional');
      end
   else
     raise  ERuntimeException.Create('x and y arrays must be one dimensional');
   TVM (vm).pushNone;
end;

end.


