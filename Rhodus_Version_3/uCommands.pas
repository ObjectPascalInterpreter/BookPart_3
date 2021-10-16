unit uCommands;

// This source is distributed under Apache 2.0

// Copyright (C)  2019-2020 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

// All console commands are hendled here

interface

Uses Windows, Classes, SysUtils, StrUtils, Generics.Collections, uVM, uRhodusEngine;

type
  TCallCommand = function (argument : string) : boolean;

  TCommand = class (TObject)
    name : string;
    helpStr : string;
    argument : string;
    fcn : TCallCommand;
    constructor Create (const name, helpStr: string; fcn : TCallCommand);
  end;

  TListOfCommand = class (TObjectList<TCommand>)
    public
       function find (name : string; var index : integer) : boolean;
  end;


var
  listOfCommands : TListOfCommand;

  bolShowByteCode : boolean;
  bolShowTree : boolean;

  procedure displayHelp;
  function  getMemoryAllocated : UInt64;


implementation

Uses System.Types,
     System.Generics.Defaults,
     IOUtils,
     uInitialize,
     uMemoryManager,
     ShellAPI,
     uTerminal,
     uBuiltInGlobal,
     uConstantTable,
     uEnvironment,
     uArrayObject;

var
   winList : TStringList;


constructor TCommand.Create(const name, helpStr: string; fcn: TCallCommand);
begin
  self.name := name;
  self.helpStr := helpStr;
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

procedure displayHelp;
var i : integer;
begin
  writeln;
  writeln ('quit'#9#9'Quit the application, really really quit');
  for i := 0 to listOfCommands.Count - 1 do
      if length (listOfCommands[i].name) > 7 then
         writeln (listOfCommands[i].name + #9 + listOfCommands[i].helpStr)
      else
         writeln (listOfCommands[i].name + #9#9 + listOfCommands[i].helpStr);

  writeln ('symbols'#9#9'Display symbols in main module');
  writeln ('#p'#9#9'Start a multi-line program (q or return to finish)');
  writeln ('');
  writeln ('?X or ?M.X'#9'Get help about a symbol, X, or a symbol in a module, M');
  writeln ('Type modules() to get a list of loaded modules');
  writeln ('Type dir() on any module to get the list of methods, eg math.dir()');
  writeln;

  writeln ('To run a script, type run followed by its filename. Note: there is no need to specify the .rh extension');
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

function startPathCommand (argument : string) : boolean;
begin
  writeln (launchEnvironment.basePath);
  result := True;
end;


function samplesCommand (argument : string) : boolean;
begin
  setCurrentDir (launchEnvironment.basePath + '\\' + 'SampleScripts');
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


function memoryUsedCommand (argument : string) : boolean;
var  r : TMemorySummary;
     mstr : string;
begin
  r := memoryList.getNumberAllocatedNodes();
  write ('[Dynamic Memory] all: ', r.totalAllocated);
  write ('; garbage: ', r.numGarbage);
  write ('; owned: ', r.numOwned);
  write ('; bound: ', r.numBound);
  writeln ('; constant: ', r.numConstant);
  mstr := memoryList.mapMemory;
  if mstr <> '' then
     writeln ('Memory Map: ' + sLineBreak + mstr);
  writeln ('Memory allocated: ', getMemoryAllocated - baseLineMemoryAllocated);
  writeln ('Instance size for main module: ', mainModule.getSize());
  writeln ('Constant table size : ', mainModule.code.constantValueTable.getSize);
  result := true;
end;


function detailedMemoryUsageCommand (argument : string) : boolean;
var mstr : string;
begin
  mstr := memoryList.mapMemoryDetailed;
  writeln (mstr);
  result := true;
end;


function dirCommand (argument : string) : boolean;
var count : integer; fileName : string;
    dirlist, filelist, totalList : TStringDynArray;
begin
  dirlist := TDirectory.GetDirectories (GetCurrentDir);
  fileList := TDirectory.GetFiles(GetCurrentDir, '*.rh');
  // Sort the file names using this looking awkward code
  TArray.Sort<string>(fileList, TDelegatedComparer<string>.Construct(
        function(const Left, Right: string): Integer
        begin
          result := CompareText(Left, Right);
        end));

  totalList := dirList + fileList;
  count := 1;
  for fileName in totalList do
      begin
      write (Format('%-20s', [extractFileName (getCurrentDir + '\' + fileName)]));
      if count mod 4 = 0 then
         writeln;
      inc (count);
      end;
  writeln;
  exit (True);
end;


// look for notepad if its already running
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
     begin
     if FileExists (fileName) then
        ShellExecute(0, nil, PChar('notepad.exe'), PChar (fileName), nil, SW_SHOWNORMAL)
     else
        ShellExecute(0, nil, PChar('notepad.exe'), PChar(launchEnvironment.basePath + '\\' + fileName), nil, SW_SHOWNORMAL);
     end;
  exit (True);
end;


function showTreeCommand (command : string) : boolean;
begin
  bolShowTree := not bolShowTree;
  if bolShowTree then
     writeln ('Show tree ON')
  else writeln ('Show tree OFF');
  result := False;
end;

function showCodeCommand (command : string) : boolean;
begin
  bolShowByteCode := not bolShowByteCode;
  if bolShowByteCode then
     writeln ('Show code ON')
  else writeln ('Show code OFF');
  result := False;
end;


function listCommand (fileName : string) : boolean;
begin
  if ExtractFileExt(fileName) <> '.rh' then
     fileName := fileName + '.rh';

  if TFile.Exists (getCurrentDir + '\' + fileName) then
     writeln (TFile.ReadAllText(getCurrentDir + '\' + fileName))
  else
     writeln ('cat: No such file');
  exit (True);
end;


function symbolsCommnd (argument : string) : boolean;
begin
  if getRunTime <> nil then
     getRunTime.getAllocatedSymbols (argument);
  exit (True);
end;


function getTestScriptsDir : string;
begin
   result := launchEnvironment.basePath + '\\TestScripts';
end;


function testsCommand (fileName : string) : boolean;
var tdir : string;
begin
  tdir := getTestScriptsDir + '\';
  cdCommand (tdir);
  setGreen;
  writeln ('Running tests....');
  setWhite;
  if ExtractFileExt(fileName) <> '.rh' then
     fileName := fileName + '.rh';

  for fileName in TDirectory.GetFiles(tdir, '*.rh') do
      begin
      setRed;
      writeln ('Test File: ' + Format('%-20s', [extractFileName (tdir + fileName)]));
      setWhite;

      TRhodus.bolShowByteCode := False;
      getRuntime().compileAndRun (TFile.ReadAllText(fileName), False);  // false = not interactive
      writeln;
      end;
  writeln;

  exit (True);
end;


function runCommand (fileName : string) : boolean;
var cwd : string;
begin
  if ExtractFileExt(fileName) <> '.rh' then
     fileName := fileName + '.rh';
  cwd := getCurrentDir();
  if TFile.Exists (cwd + '\' + fileName) then
     begin
     if getRuntime() <> nil then
        getRuntime().compileAndRun (TFile.ReadAllText(cwd + '\' + fileName), False) // True = interactive
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
   listOfCommands.Add (TCommand.Create ('pwd',        'Print the current working directory', pwdCommand));
   listOfCommands.Add (TCommand.Create ('help',       'Print out the help screen', helpCommand));
   listOfCommands.Add (TCommand.Create ('cls',        'Clear the screen', clearCommand));
   listOfCommands.Add (TCommand.Create ('startPath',  'Print out the startup path', startPathCommand));
   listOfCommands.Add (TCommand.Create ('samples',    'cd to the samples directory', samplesCommand));
   listOfCommands.Add (TCommand.Create ('cd',         'Change working directory, eg cd .\myfiles', cdCommand));
   listOfCommands.Add (TCommand.Create ('free',       'Run the memory garbage collector', freeCommand));
   listOfCommands.Add (TCommand.Create ('mem',        'Report on current memory usage', memoryUsedCommand));
   listOfCommands.Add (TCommand.Create ('MEM',        'Detailed report on current memory usage', detailedMemoryUsageCommand));
   listOfCommands.Add (TCommand.Create ('edit',       'Start notepad to edit the file, eg edit myfile', editCommand));
   listOfCommands.Add (TCommand.Create ('dir',        'List the directory of .rh files', dirCommand));
   listOfCommands.Add (TCommand.Create ('cat',        'List the contents of a file', listCommand));
   listOfCommands.Add (TCommand.Create ('tests',      'Run the test files', testsCommand));
   listOfCommands.Add (TCommand.Create ('run',        'Run the code in the specified file, eg run myfile ', runCommand));
   listOfCommands.Add (TCommand.Create ('#del',       'Delete a file, eg #del myfile', delCommand));
   listOfCommands.Add (TCommand.Create ('showtree',   'Toggle show/hide abstract syntax tree', showTreeCommand));
   listOfCommands.Add (TCommand.Create ('showcode',   'Toggle show/hide generated bytecode', showCodeCommand));

   winList := TStringList.Create;
finalization
   listOfCommands.Free;
   winList.Free;
end.
