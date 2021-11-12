unit uRepl;

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
    currentColor : string;


// Print methods to support output from the VM
// -------------------------------------------------------------------------------
procedure print (astr : AnsiString);
begin
  uTerminal.setColor (currentColor);
  write (astr);
end;


procedure println (astr: AnsiString);
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


procedure setColor (acolor : AnsiString);
begin
  currentColor := acolor;
  uTerminal.setColor(currentColor);
end;


function searchHelp (const helpStr : string) : string;
var astr : TArray<string>;
    symbol1, symbol2: TSymbol;
begin
  // Search places for any help for things of the form X or X.Y
  astr := SplitString(helpStr, '.');
  if length (astr) = 1 then  // then its just X
     begin
     // Check first if its in _main_
     // Its either in the global space or its a module name
     if mainModule.symbolTable.find(astr[0], symbol1) then
        begin
        if symbol1.symbolType = symModule then
           result := symbol1.mValue.helpStr
        else
           begin
           result := symbol1.toString;
           end;
        end
     else
        begin
        // Check the global space
        if mainModule.symbolTable.find(TSymbol.globalId, symbol1) then
           begin
           if symbol1.mValue.symbolTable.find(astr[0], symbol2) then
              begin
              result := symbol2.fValue.helpStr;
              end
           else
              result := 'Unable to locate symbol';
           end
        else
           result := 'Unable to locate symbol';
         end;
     end
  else
     begin
     if length (astr) = 2 then
        begin
        if mainModule.symbolTable.find(astr[0], symbol1) then
           begin
           if symbol1.symbolType = symModule then
              begin
              if symbol1.mValue.symbolTable.find (astr[1], symbol2)  then
                 case symbol2.symbolType of
                     symUserFunc : result := symbol2.fValue.helpStr;
                 else
                    result := symbol2.helpStr
                 end;
              end;
           end
        else
           result := 'Unable to locate the module: ' + astr[0];
        end
     else
        result := 'Don''t know how to search for what you indicated: "' + helpstr + '"';
     end;
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
    helpStr : string;
begin
   result := False;

   if listOfCommands.find (getCommand (src), index) then
      begin
      listOfCommands[index].fcn (getArgument (src));
      exit (True);
      end;

   if leftstr (src, 1) = '?' then
      begin
      helpStr := rightStr (src, length (src) - 1);
      writeln (searchHelp (helpStr));
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
     writeln ('ERROR ' + '[line ' + inttostr (syntaxError.lineNumber) + ', column: ' + inttostr (syntaxError.columnNumber) + '] ' + syntaxError.errorMsg);
end;


procedure startRepl;
begin
  rhodus := TRhodus.Create;

  registerRuntimeWithConsole (rhodus);

  rhodus.setPrintCallBack(print);
  rhodus.setPrintLnCallBack(println);
  rhodus.setReadStringCallBack(myRead);
  rhodus.setSetColorCallBack (setColor);


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
