unit uTerminal;

// This source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com


interface

Uses uRhodusEngine;

procedure displayPrompt;
procedure displayWelcome;
procedure setColor (color: string);
procedure setRed;
procedure setGreen;
procedure setBlue;
procedure setYellow;
procedure setWhite;
procedure setAqua;
procedure setUpConsole;
procedure shutDownConsole;
procedure clearConsoleScreen;
procedure registerRuntimeWithConsole (runtime : TRhodus);
function  getRhodus : TRhodus;
procedure SetExtendedConsoleMode;
procedure writeText (str :string);
procedure setCurrentColors;

implementation

Uses Windows, Classes, SysUtils, StrUtils, System.UITypes, System.UIConsts, uCommands, uRepl;

type
  COORD = record
    X, Y: smallint;
  end;

  TCONSOLE_FONT_INFOEX = record
    cbSize: cardinal;
    nFont: longword;
    dwFontSize: COORD;
    FontFamily: cardinal;
    FontWeight: cardinal;
    FaceName: array [0 .. LF_FACESIZE - 1] of WideChar;
  end;

  PCONSOLE_FONT_INFOEX = ^TCONSOLE_FONT_INFOEX;


function GetConsoleWindow: HWND; stdcall; external kernel32 name 'GetConsoleWindow';

function SetCurrentConsoleFontEx(ConsoleOutput: THandle; MaximumWindow: BOOL;
  ConsoleInfo: PCONSOLE_FONT_INFOEX): BOOL; stdcall;
  external kernel32 name 'SetCurrentConsoleFontEx';
function GetCurrentConsoleFontEx(ConsoleOutput: THandle; MaximumWindow: BOOL;
  ConsoleInfo: PCONSOLE_FONT_INFOEX): BOOL; stdcall;
  external kernel32 name 'GetCurrentConsoleFontEx';

var
   BufInfo: TConsoleScreenBufferInfo;
   conOut : THandle;
   refRhodus : TRhodus;

   consoleMode : boolean;

   // RGB
   currentColor : array[0..2] of byte;


procedure ColorToRGB(const Color: Integer; out R, G, B: Byte);
begin
  R := Color and $FF;
  G := (Color shr 8) and $FF;
  B := (Color shr 16) and $FF;
end;


procedure registerRuntimeWithConsole (runtime : TRhodus);
begin
  refRhodus := runtime;
end;

function getRGB (r, g, b : byte) : string;
begin
  result := inttostr(r) + ';' + inttostr(g) + ';' + inttostr (b);
end;


procedure setForeGround (r, g, b : byte);
begin
  if consoleMode then
     write (#27'[38;2;' + getRGB (r, g, b) + 'm');
end;


procedure setBackGround (r, g, b : byte);
begin
  if consoleMode then
     write (#27'[48;2;' + getRGB (r, g, b) + 'm');
end;


function getRhodus : TRhodus;
begin
  result := refRhodus;
end;


procedure setBlue;
begin
  currentColor[0] := 0;  currentColor[1] := 0;  currentColor[2] := 255;
  setCurrentColors;
end;


procedure setYellow;
begin
  currentColor[0] := 255;  currentColor[1] := 255;  currentColor[2] := 0;
  setCurrentColors;
end;


procedure setAqua;
begin
  currentColor[0] := 0;  currentColor[1] := 255;  currentColor[2] := 255;
  setCurrentColors;
end;


procedure setPurple;
begin
  currentColor[0] := 158;  currentColor[1] := 0;  currentColor[2] := 211;
  setCurrentColors;
end;

procedure setWhite;
begin
  currentColor[0] := 204;
  currentColor[1] := 204;
  currentColor[2] := 204;

  setCurrentColors;
end;


procedure setGreen;
begin
  currentColor[0] := 0;
  currentColor[1] := 255;
  currentColor[2] := 0;
  setCurrentColors;
end;


procedure setOrange;
begin
  currentColor[0] := 255;
  currentColor[1] := 165;
  currentColor[2] := 0;
  setCurrentColors;
end;


procedure setRed;
begin
  currentColor[0] := 255;
  currentColor[1] := 0;
  currentColor[2] := 0;

  setCurrentColors;
end;


procedure setCurrentColors;
begin
  if consoleMode then
     begin
     setForeGround (currentColor[0], currentColor[1], currentColor[2]);
     write (#27'[48;2;' + '1' + ';' + '43' + ';' + '54' + 'm');
     end;
end;


procedure writeText (str :string);
begin
  if consoleMode then
     setCurrentColors;
  write (str);
end;


procedure SetExtendedConsoleMode;
var mode : DWord;
    stdout: THandle;
begin
  stdout := GetStdHandle(STD_OUTPUT_HANDLE);
  GetConsoleMode (stdout, mode);
  mode := mode or 4;
  SetConsoleMode (stdout, mode);
end;


// Color can be some set colors like red, green, blue, aqua, yellow, white, or purple
// or it can be one of the named web colors
procedure setColor (color: string);
var acolor : TAlphaColor;
begin
  if color = '' then
     begin setWhite; exit; end;
  if color = 'white' then
     begin setWhite; exit; end;
  if color = 'red' then
     begin setRed; exit; end;
  if color = 'green' then
     begin setGreen; exit; end;
  if color = 'blue' then
     begin setBlue; exit; end;
  if color = 'aqua' then
     begin setAqua; exit; end;
  if color = 'yellow' then
     begin setYellow; exit; end;
  if color = 'purple' then
     begin setPurple; exit; end;

  acolor := StringToAlphaColor(color);//  WebColorNameToColor('clWeb' + color);
  currentColor[0] := TAlphaColorRec(acolor).R;
  currentColor[1] := TAlphaColorRec(acolor).G;
  currentColor[2] := TAlphaColorRec(acolor).B;

  setCurrentColors;
end;


procedure displayWelcome;
begin
  uTerminal.setWhite;
  clearConsoleScreen;
  write ('Welcome to Rhodus Language III Console. Version: ');
  setOrange;
  writeln (getRhodus().getVersion());
  setWhite;
  writeln ('Data and Time: ', dateToStr (Date), ', ', timeToStr (Time));
  displayHelp;
end;


procedure displayPrompt;
begin
  setRed;
  write ('>> ');
  setWhite;
end;


// Provided by David Heffernan on stackoverflow
// https://stackoverflow.com/questions/29794559/delphi-console-xe7-clearscreen
procedure clearConsoleScreen;
var
  stdout: THandle;
  csbi: TConsoleScreenBufferInfo;
  ConsoleSize: DWORD;
  NumWritten: DWORD;
  Origin: TCoord;
begin
  stdout := GetStdHandle(STD_OUTPUT_HANDLE);
  Win32Check(stdout<>INVALID_HANDLE_VALUE);
  Win32Check(GetConsoleScreenBufferInfo(stdout, csbi));
  ConsoleSize := csbi.dwSize.X * csbi.dwSize.Y;
  Origin.X := 0;
  Origin.Y := 0;
  Win32Check(FillConsoleOutputCharacter(stdout, ' ', ConsoleSize, Origin, NumWritten));
  Win32Check(FillConsoleOutputAttribute(stdout, csbi.wAttributes, ConsoleSize, Origin,  NumWritten));
  Win32Check(SetConsoleCursorPosition(stdout, Origin));
end;


// Ignore ctrl-c
// Concept from https://stackoverflow.com/questions/1000223/
function consoleHandler (dwCtrlType: DWORD) : BOOL; stdcall;
begin
  // Avoid terminating with Ctrl+C
  if (CTRL_C_EVENT = dwCtrlType) then
    begin
    if refRhodus <> nil then
       refRhodus.getVM().stopVm();
    result := TRUE
    end
  else
    result := FALSE;
end;


// Modified from https://stackoverflow.com/questions/24762413/how-to-set-console-font
procedure setConsoleFont(const AFontSize: word);
var
  ci: TCONSOLE_FONT_INFOEX;
  ch: THandle;
begin
  FillChar(ci, SizeOf(TCONSOLE_FONT_INFOEX), 0);
  ci.cbSize := SizeOf(TCONSOLE_FONT_INFOEX);

  ch := GetStdHandle(STD_OUTPUT_HANDLE);
  GetCurrentConsoleFontEx(ch, FALSE, @ci);

  ci.FontFamily := FF_DONTCARE;
  ci.FaceName := 'Consolas';
  ci.dwFontSize.X := 0;
  ci.dwFontSize.Y := AFontSize;
  ci.FontWeight := FW_NORMAL;
  SetCurrentConsoleFontEx(ch, FALSE, @ci);
end;


procedure setConsoleWindowPosition;
var
  ConsoleHwnd: HWND;
  stdOutHandle : THandle;
  R: TRect;
  Rect: TSmallRect;
  Coord: TCoord;
  x, y : integer;
begin
  ConsoleHwnd := GetConsoleWindow;
  stdOuthandle := GetStdHandle(STD_OUTPUT_HANDLE);

  Rect.Left := 1;
  Rect.Top := 1;
  Rect.Right := 110;
  Rect.Bottom := 40;
  Coord.X := (Rect.Right + 1 - Rect.Left);
  Coord.y := Rect.Bottom + 1 - Rect.Top;
  SetConsoleScreenBufferSize(GetStdHandle(STD_OUTPUT_HANDLE), Coord);
  SetConsoleWindowInfo(GetStdHandle(STD_OUTPUT_HANDLE), True, Rect);

  GetWindowRect(ConsoleHwnd, R);
  x := (GetSystemMetrics(SM_CXSCREEN) - (R.Right - R.Left)) div 2;
  y := (GetSystemMetrics(SM_CYSCREEN) - (R.Bottom - R.Top)) div 2 - 100;
  // Center the console window
  SetWindowPos(ConsoleHwnd, 0,  x, y, 0, 0, SWP_NOSIZE);

  clearConsoleScreen;  // This will position the horizonal scroll bar as well.
  //ShowScrollBar(ConsoleHwnd, SB_VERT, True);
end;


procedure setUpConsole;
begin
  // used to avoid window flashing.
  AllocConsole;
  setConsoleFont (22);
  setConsoleWindowPosition;
  conOut := getStdHandle(STD_OUTPUT_HANDLE);
  getConsoleScreenBufferInfo(ConOut, BufInfo);
  setConsoleCtrlHandler(@consoleHandler, True);  // Ignore Ctrl-C
  currentColor[0] := 204;
  currentColor[1] := 204;
  currentColor[2] := 204;
  consoleMode := True;
end;


procedure shutDownConsole;
begin
  FreeConsole;
end;


initialization
  consoleMode := False;
end.
