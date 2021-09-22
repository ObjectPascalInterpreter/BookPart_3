unit uTerminal;


// This source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com


interface

Uses uRunCode;

procedure displayPrompt;
procedure displayWelcome;
procedure setColor (color: string);
procedure setRed;
procedure setGreen;
procedure setBlue;
procedure setYellow;
procedure setWhite;
procedure setUpConsole;
procedure shutDownConsole;
procedure clearConsoleScreen;
procedure registerRuntimeWithConsole (runtime : TRunFramework);
function  getRunTime : TRunFramework;

implementation

Uses Windows, Classes, SysUtils, StrUtils, uVM, uCommands;

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
   refRuntime : TRunFramework;


procedure registerRuntimeWithConsole (runtime : TRunFramework);
begin
  refRuntime := runtime;
end;

function getRunTime : TRunFramework;
begin
  result := refRuntime;
end;

procedure setBlue;
begin
  setConsoleTextAttribute(TTextRec(Output).Handle, FOREGROUND_INTENSITY or FOREGROUND_BLUE);
end;

procedure setYellow;
begin
  setConsoleTextAttribute(TTextRec(Output).Handle, FOREGROUND_INTENSITY or FOREGROUND_GREEN or FOREGROUND_RED);
end;

procedure setAqua;
begin
  setConsoleTextAttribute(TTextRec(Output).Handle, FOREGROUND_INTENSITY or FOREGROUND_GREEN or FOREGROUND_BLUE);
end;

procedure setPurple;
begin
  setConsoleTextAttribute(TTextRec(Output).Handle, FOREGROUND_INTENSITY or FOREGROUND_RED or FOREGROUND_BLUE);
end;

procedure setRed;
begin
  setConsoleTextAttribute(TTextRec(Output).Handle, FOREGROUND_INTENSITY or FOREGROUND_RED);
end;

procedure setWhite;
begin
  setConsoleTextAttribute(conOut, FOREGROUND_INTENSITY or FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE);
end;

procedure setGreen;
begin
  setConsoleTextAttribute(conOut, FOREGROUND_INTENSITY or FOREGROUND_GREEN);
end;

procedure setColor (color: string);
begin
  setWhite (); // default if we don't recongize any color

  if color = 'white' then
     setWhite;
  if color = 'red' then
     setRed;
  if color = 'green' then
     setGreen;
  if color = 'blue' then
     setBlue;
  if color = 'aqua' then
     setAqua;
  if color = 'yellow' then
     setYellow;
  if color = 'purple' then
     setpurple;
end;


procedure displayWelcome;
begin
  writeln ('Welcome to Rhodus Language III Console, Version ', getRunTime().getVersion());
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
    if refRuntime <> nil then
       refRuntime.vm.stopVm();
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
  R: TRect;
  Rect: TSmallRect;
  Coord: TCoord;
  x, y : integer;
begin
  ConsoleHwnd := GetConsoleWindow;

  Coord.X := 120; // Width
  Coord.y := 512;  // Height
  SetConsoleScreenBufferSize(GetStdHandle(STD_OUTPUT_HANDLE), Coord);

  Rect.Left := 0;   //  must be zero
  Rect.Top := 0;
  Rect.Right := Coord.X - (Rect.Left + 1);
  Rect.Bottom := Coord.y - (Rect.Top + 1);
  SetConsoleWindowInfo(GetStdHandle(STD_OUTPUT_HANDLE), True, Rect);

  GetWindowRect(ConsoleHwnd, R);
  x := (GetSystemMetrics(SM_CXSCREEN) - (R.Right - R.Left)) div 2;
  y := (GetSystemMetrics(SM_CYSCREEN) - (R.Bottom - R.Top)) div 2;
  // Center the console window
  SetWindowPos(ConsoleHwnd, 0,  x, y, 0, 0, SWP_NOSIZE);

  ShowScrollBar(ConsoleHwnd, SB_VERT, True);
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
end;


procedure shutDownConsole;
begin
  FreeConsole;
end;



end.
