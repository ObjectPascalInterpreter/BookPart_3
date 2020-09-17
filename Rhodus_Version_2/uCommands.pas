unit uCommands;

// Ths source is distributed under Apache 2.0

// Copyright (C)  2019-2020 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

// All console commands are hendled here

interface

Uses
{$IFDEF DEBUG}
     FastMM4,
{$ENDIF}
     Windows, Classes, SysUtils, StrUtils, Generics.Collections, uVM, uRunCode;

type
  TCallCommand = function (argument : string) : boolean;

  TCommand = class (TObject)
    name : string;
    argument : string;
    fcn : TCallCommand;
    constructor Create (name : string; fcn : TCallCommand);
  end;

  TListOfCommand = class (TObjectList<TCommand>)
    public
       function find (name : string; var index : integer) : boolean;
  end;


var
  listOfCommands : TListOfCommand;

  procedure displayHelp;
  procedure clearConsoleScreen;
  procedure registerRuntimeWithConsole (runtime : TRunFramework);
  procedure setRed;
  procedure setGreen;
  procedure setWhite;
  procedure setUpConsole;
  procedure shutDownConsole;
  function  getMemoryAllocated : UInt64;
  procedure computeBaseLineMemoryAllocated;


implementation

Uses System.Types,
     System.Generics.Defaults,
     IOUtils,
     uInitialize,
     uMemoryManager,
     ShellAPI;

function GetConsoleWindow: HWND; stdcall; external kernel32 name 'GetConsoleWindow';


var
   BufInfo: TConsoleScreenBufferInfo;
   conOut : THandle;
   refRuntime : TRunFramework; // console needs a reference inorder to interrupt vm
   baseLineMemoryAllocated : UInt64;
   winList : TStringList;


procedure registerRuntimeWithConsole (runtime : TRunFramework);
begin
  refRuntime := runtime;
end;


constructor TCommand.Create(name: string; fcn: TCallCommand);
begin
  self.name := name;
  self.fcn := fcn;
end;


function TListOfCommand.find (name : string; var index : integer) : boolean;
var i : integer;
begin
  index := -1;
  for i  := 0 to count - 1 do
      if self[i].name = name then
         begin
         index := i;
         exit (True);
         end;
  result := False;
end;

// -------------------------------------------------------------------

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
  setConsoleTextAttribute(conOUt, FOREGROUND_INTENSITY or FOREGROUND_GREEN);
end;


procedure displayHelp;
begin
  writeln;
  writeln ('quit'#9#9'Quit the application');
  writeln ('list fileName'#9'List the contents of a file');
  writeln ('run fileName'#9'Run the code in the specified file');
  writeln ('edit fileName'#9'Start notepad to edit the file');
  writeln ('del filenName'#9#9'Delete a file');
  writeln ('dir'#9#9'List the directory of .rh files');
  writeln ('pwd'#9#9'Print the current working directory');
  writeln ('cd path'#9#9'Change working directory, eg cd ., cd .\myfiles');
  writeln ('samples'#9#9'Change to samples directory');
  writeln ('rhoduspath'#9'Print out the Rhodus path');
  writeln ('cls'#9#9'Clear the screen');
  writeln ('debug'#9#9'Turn debugging on and off (displays bytecode)');
  writeln ('mem'#9#9'Report on current memory usage');
  writeln ('free'#9#9'Run the memory garbage collector');
  writeln ('tests'#9#9'Run the tests');
  writeln ('');
  writeln ('?name'#9#9'Get help about object name');
  writeln ('??'#9#9'List help topics');
  writeln;
  writeln ('To run a script, just type its filename. Note: there is no need to specify the .rh extension');
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


procedure setConsoleWindowPosition;
var
  ConsoleHwnd: HWND;
  R: TRect;
begin
  ConsoleHwnd := GetConsoleWindow;
  // Center the console window
  GetWindowRect(ConsoleHwnd, R);
  SetWindowPos(ConsoleHwnd, 0,
    (GetSystemMetrics(SM_CXSCREEN) - (R.Right - R.Left)) div 2,
    (GetSystemMetrics(SM_CYSCREEN) - (R.Bottom - R.Top)) div 2,
    0, 0, SWP_NOSIZE);
end;


// Ignore ctrol-c
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


procedure setUpConsole;
begin
  // used to avoid window flashing.
  AllocConsole;
  setConsoleWindowPosition;
  conOut := getStdHandle(STD_OUTPUT_HANDLE);
  getConsoleScreenBufferInfo(ConOut, BufInfo);
  setConsoleCtrlHandler(@consoleHandler, True);  // Ignore Ctrl-C
end;


procedure shutDownConsole;
begin
  FreeConsole;
end;


function pwdCommand (argument : string) : boolean;
begin
  writeln (GetCurrentDir);
  result := True;
end;


function helpCommand (argument : string) : boolean;
begin
  displayHelp;
  result := True;
end;


function clearCommand (argument : string) : boolean;
begin
  clearConsoleScreen;
  result := True;
end;

function rhodusPathCommand (argument : string) : boolean;
begin
  writeln (RHODUSPATH);
  result := True;
end;


function samplesCommand (argument : string) : boolean;
begin
  setCurrentDir (RHODUSPATH + '\' + 'SampleScripts');
  result := True;
end;


function cdCommand (argument : string) : boolean;
begin
   if not setCurrentDir(argument) then
      writeln ('FAILED to change directory');
   result := True;
end;


function freeCommand (argument : string) : boolean;
begin
  memoryList.freeGarbage;
  result := True;
end;


function getMemoryAllocated : UInt64;
var st: TMemoryManagerState; sb: TSmallBlockTypeState;
begin
  GetMemoryManagerState(st);
  result :=  st.TotalAllocatedMediumBlockSize + st.TotalAllocatedLargeBlockSize;
  for sb in st.SmallBlockTypeStates do
      result := result + sb.UseableBlockSize * sb.AllocatedBlockCount;
end;


procedure computeBaseLineMemoryAllocated;
begin
  baseLineMemoryAllocated := getMemoryAllocated;
end;


function memoryUsedCommand (argument : string) : boolean;
var  r : TMemorySummary;
     mstr : string;
begin
  r := memoryList.getNumberAllocatedNodes();
  write ('[Dynamic Memory] all: ', r.totalAllocated);
  write ('; garbage: ', r.numGarbage);
  write ('; owned: ', r.numOwned);
  writeln ('; bound: ', r.numBound);
  writeln ('; constant: ', r.numConstant);
  mstr := memoryList.mapMemory;
  if mstr <> '' then
     writeln ('Memory Map: ' + sLineBreak + mstr);
  writeln ('Memory allocated: ', getMemoryAllocated - baseLineMemoryAllocated);
end;


function dirCommand (argument : string) : boolean;
var count : integer; fileName : string;
    dirlist : TStringDynArray;
begin
  count := 0;
  dirList := TDirectory.GetFiles(GetCurrentDir, '*.rh');
  // Sort the file names using this awkward code
  TArray.Sort<string>(dirList, TDelegatedComparer<string>.Construct(
        function(const Left, Right: string): Integer
        begin
          result := CompareText(Left, Right);
        end));

  for fileName in dirList do
      begin
      write (Format('%-20s', [extractFileName (getCurrentDir + '\' + fileName)]));
      if count mod 4 = 0 then
         writeln;
      inc (count);
      end;
  writeln;
  exit (True);
end;


function EnumWindowsProc(WHandle: HWND; LParM: LParam): LongBool;StdCall;Export;
var Title,ClassName:array[0..128] of char;
    sTitle,sClass,Linia:STRING ;

begin
 Result:=True;

 GetWindowText(wHandle, Title,128);
 GetClassName(wHandle, ClassName,128);

 sTitle:=Title;
 sClass:=ClassName;

 if IsWindowVisible(wHandle) then
    begin
    if rightStr (sTitle, 10) = ' - Notepad' then
       winlist.addObject (sTitle, TObject (WHandle));
    Linia := sTitle + '        '+sClass+'       '+IntToHex(wHandle,4);
    end;
end;


function editCommand (fileName : string) : boolean;
var index : integer;
begin
  if ExtractFileExt(fileName) <> '.rh' then
     fileName := fileName + '.rh';

  // The following is to ensure that if the nodepad window is already
  // open, we don't reopen it into a new window
  EnumWindows(@EnumWindowsProc, 0);
  if winList.Find (fileName + ' - Notepad', index) then
     SetForeGroundWindow(Cardinal (winList.Objects[index]))
  else
     ShellExecute(0, nil, PChar('notepad.exe'), PChar(RHODUSPAth + '\' + fileName), nil, SW_SHOWNORMAL);
  exit (True);
end;


function listCommand (fileName : string) : boolean;
begin
  if ExtractFileExt(fileName) <> '.rh' then
     fileName := fileName + '.rh';

  if TFile.Exists (getCurrentDir + '\' + fileName) then
     writeln (TFile.ReadAllText(getCurrentDir + '\' + fileName))
  else
     writeln ('list: No such file');
  exit (True);
end;


function symbolsCommnd (argument : string) : boolean;
begin
  if refRuntime <> nil then
     refRuntime.getAllocatedSymbols;
  exit (True);
end;


function getTestScriptsDir : string;
begin
   result := RHODUSPATH + '\TestScripts';
end;


function testsCommand (fileName : string) : boolean;
var tdir : string;
begin
  tdir := getTestScriptsDir + '\';
  writeln ('Running tests....');
  if ExtractFileExt(fileName) <> '.rh' then
     fileName := fileName + '.rh';

  for fileName in TDirectory.GetFiles(tdir, '*.rh') do
      begin
      writeln ('Test File: ' + Format('%-20s', [extractFileName (tdir + fileName)]));

      refRuntime.runCode (TFile.ReadAllText(fileName), False);
      writeln;
      end;
  writeln;

  exit (True);
end;


function runCommand (fileName : string) : boolean;
begin
  if ExtractFileExt(fileName) <> '.rh' then
     fileName := fileName + '.rh';
  if TFile.Exists (getCurrentDir + '\' + fileName) then
     begin
     if refRuntime <> nil then
        refRuntime.runCode (TFile.ReadAllText(getCurrentDir + '\' + fileName), False)
     else
        begin
        writeln ('Internal Error: Runtime system not available');
        exit (True);
        end;
     exit (True);
     end
  else
     begin
     writeln ('File not found');
     exit (True);
     end;
end;


function delCommand (fileName : string) : boolean;
begin
  if ExtractFileExt(fileName) <> '.rh' then
     fileName := fileName + '.rh';
  if FileExists (getCurrentDir + '\' + fileName) then
     begin
     TFile.Delete(getCurrentDir + '\' + fileName);
     exit (True);
     end
  else
     begin
     writeln ('File not found');
     exit (True);
     end;
end;


initialization
   listOfCommands := TListOfCommand.Create;
   listOfCommands.Add (TCommand.Create ('pwd',        pwdCommand));
   listOfCommands.Add (TCommand.Create ('help',       helpCommand));
   listOfCommands.Add (TCommand.Create ('cls',        clearCommand));
   listOfCommands.Add (TCommand.Create ('rhoduspath', rhodusPathCommand));
   listOfCommands.Add (TCommand.Create ('samples',    samplesCommand));
   listOfCommands.Add (TCommand.Create ('cd',         cdCommand));
   listOfCommands.Add (TCommand.Create ('free',       freeCommand));
   listOfCommands.Add (TCommand.Create ('mem',        memoryUsedCommand));
   listOfCommands.Add (TCommand.Create ('edit',       editCommand));
   listOfCommands.Add (TCommand.Create ('dir',        dirCommand));
   //listOfCommands.Add (TCommand.Create ('list',       listCommand));
   listOfCommands.Add (TCommand.Create ('tests',      testsCommand));
   listOfCommands.Add (TCommand.Create ('run',        runCommand));
   listOfCommands.Add (TCommand.Create ('symbols',    symbolsCommnd));
   listOfCommands.Add (TCommand.Create ('del',        delCommand));

   winList := TStringList.Create;
finalization
   listOfCommands.Free;
   winList.Free;
end.
