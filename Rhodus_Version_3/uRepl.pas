unit uRepl;

// This source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com


interface

Uses SysUtils, uTerminal, uRhodusEngine;

procedure startRepl;

implementation

Uses StrUtils,
     uMemoryManager,
     uBuiltInGlobal,
     uBuiltInSys,
     uSyntaxParser,
     uRhodusTypes,
     uSymbolTable,
     uCommands,
     IOUtils,
     uCompile,
     uMachineStack;


var rhodus : TRhodus;
    sourceCode : string;
    syntaxError : TSyntaxError;
    compilerError : TCompilerError;


// Print methods to support output from the VM
// -------------------------------------------------------------------------------
procedure print (const astr : AnsiString);
begin
  write (astr);
end;


procedure println (const astr: AnsiString);
begin
  print (astr);
  writeln;
end;


function myRead (const prompt : AnsiString) : PAnsiChar;
var astr : string;
begin
  write (prompt);
  readln (astr);
  result := PAnsiChar (AnsiString (astr));
end;


procedure setColor (const acolor : AnsiString);
begin
  uTerminal.setColor(string (aColor));
end;


// Extract string up to first space, if no space found return entire string
function getCommand (src : string) : string;
var index : integer;
begin
  index := pos(' ', src);
  if index = 0 then
     result := src
  else
     result := copy(src, 1, index-1);
end;


// Get string to the right of the first space, if no space return empty string
function getArgument (src : string) : string;
var index : integer;
begin
  index := pos(' ', src);
  if index = 0 then
     result := ''
  else
     result := rightStr (src, length (src) - index);
end;


function executeCommand (src : string) : boolean;
var index : integer;
begin
  if listOfCommands.find (getCommand (src), index) then
     begin
     listOfCommands[index].fcn (getArgument (src));
     exit (True);
     end;
  exit (False);
end;



function readBlockOfCode : string;
var fragment: string;
begin
  writeln ('Type q or return to exit and run program');
  write ('... ');
  result := '';
  readln (fragment);
  while (fragment <> 'q') and (fragment <> '') do
         begin
         if result = '' then
            result := fragment
         else
            result := result + sLineBreak + fragment;
         write ('... ');
         readln (fragment)
         end;
end;


procedure number (x : integer);
begin
  writeln (x);
end;


procedure executeCode (const sourceCode : string);
begin
  if rhodus.compileToAST (sourceCode, syntaxError) then
     begin
     if rhodus.generateByteCode (True, compilerError) then
        begin
        if TRhodus.bolShowByteCode then
           rhodus.showByteCodeMethod (mainModule);
        rhodus.runCode (mainModule, True);
        end
     else
       begin
       setGreen;
       writeln ('ERROR ' + '[line ' + inttostr (compilerError.lineNumber) + ', column: ' + inttostr (compilerError.columnNumber) + '] ' + compilerError.errorMsg);
       setWhite;
       end;
     end
  else
     begin
     setGreen;
     writeln ('ERROR ' + '[line ' + inttostr (syntaxError.lineNumber) + ', column: ' + inttostr (syntaxError.columnNumber) + '] ' + syntaxError.errorMsg);
     setWhite;
     end;
end;


procedure startUpRhodus;
begin
  rhodus := TRhodus.Create;

  registerRuntimeWithConsole (rhodus);

  rhodus.setPrintCallBack(print);
  rhodus.setPrintLnCallBack(println);
  rhodus.setReadStringCallBack(myRead);
  rhodus.setSetColorCallBack (setColor);
end;


procedure startRepl;
begin
  startUpRhodus;

  try
    displayWelcome;

    try
    while True do
          begin
          TRhodus.bolShowByteCode := bolShowByteCode;
          TRhodus.bolShowTree := bolShowTree;

          displayPrompt;
          readln (sourceCode);

          if sourceCode = 'quit' then
             break;

          if sourceCode = 'restart' then
             begin
             writeln ('Restarting kernel.....');
             rhodus.Free;
             startUpRhodus;
             end;

          if sourceCode = '#p' then
             sourceCode := readBlockOfcode;

          if not executeCommand(sourceCode) then
             executeCode (sourceCode);
          end;
    except
       on e:Exception do
          writeln ('Internal error in Repl Loop: ' + e.message);
     end;

  finally
    rhodus.Free;

    writeln ('Current Memory Map:'#13#10, '[', memoryList.mapMemory, ']');
    writeln ('Press any key to exit');
    readln;
    shutDownConsole;
  end;
end;


end.
