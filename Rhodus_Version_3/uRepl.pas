unit uRepl;

interface

Uses SysUtils, uTerminal, uRhodusEngine;

procedure startRepl;

implementation

Uses StrUtils, 
     uMemoryManager, 
     uBuiltInGlobal, 
     uSyntaxParser, 
     uRhodusTypes, 
     uSymbolTable,
     uCommands, 
     IOUtils,
     uCompile,
     uMachineStack;
 

var runFramework : TRhodus;
    sourceCode : string;
    syntaxError : TSyntaxError;
    compilerError : TCompilerError;
    currentColor : string;
    
// Print methods to support output from the VM
// -------------------------------------------------------------------------------
procedure print (st : PMachineStackRecord);
begin
  uTerminal.setColor (currentColor);

  if st <> nil then
     case st.stackType of
          stNone    : begin end; //write ('undefined value'); end;
          stInteger : write (st.iValue);
          stDouble  : write (Format('%g', [st.dValue]));
          stString  : write (st.sValue.value);
          stBoolean : if st.bValue = True then
                         write ('True')
                      else
                         write ('False');
          stList    : begin
                      write (st.lValue.listToString);
                      end;
          stModule  : begin
                      write (st.module.name);
                      end;
          stFunction: begin
                      write (st.fValue.name);
                      end
     else
        writeln ('Unrecognized value from print');
     end;
end;


procedure println (st : PMachineStackRecord);
begin
  print (st);
  writeln;
end;


procedure setColor (st : PMachineStackRecord);
begin
  if st.stackType <> stString then
     writeln ('Expecting a string in setColor: white, red, green, blue, yellow, purple, aqua')
  else
     currentColor := st.sValue.value;
  uTerminal.setColor(currentColor);
end;



function searchHelp (const helpStr : string) : string;
var i, j, index : integer;
    astr : TArray<string>;
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
           result := 'Internal error: global space could not be located';
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

   if leftStr (src, 5) = 'debug' then
      begin
      bolShowAssembler := not bolShowAssembler;
      if bolShowAssembler then
         writeln ('Debug ON')
      else writeln ('Debug OFF');
      exit (True);
      end;

   exit (False);

   // not a command, see if is a file name, if so run it
   if ExtractFileExt(src) <> '.rh' then
      src := src + '.rh';

   if FileExists (getCurrentDir + '\' + src) then
      begin
      //
      computeBaseLineMemoryAllocated;
      runFramework.compileAndRun (TFile.ReadAllText (getCurrentDir + '\' + src), False);  // False = not interactive
      exit (True)
      end;
end;



function readBlockOfCode : string;
var fragment: string;
begin
  writeln ('Type q or return to exit and run program');
  write ('... ');
  sourceCode := '';
  readln (fragment);
  while (fragment <> 'q') and (fragment <> '') do
         begin
         if sourceCode = '' then
            sourceCode := fragment
         else
            sourceCode := sourceCode + sLineBreak + fragment;
         write ('... ');
         readln (fragment)
         end;
end;


procedure number (x : integer);
begin
  writeln (x);
end;


procedure startRepl;
var errMsg : string;
begin
  runFramework := TRhodus.Create;

  registerRuntimeWithConsole (runFramework);
  
  runFramework.setPrintCallBack(print);
  runFramework.setPrintLnCallBack(println);
  runFramework.setSetColorCallBack (setColor);
  
  // Note we don't clear the symboltables because the next script
  // may need to refer to entries in the symbol table.
  //module.clearCode;
    
  try
    displayWelcome;

    while True do
          begin
          displayPrompt;
          readln (sourceCode);
          if sourceCode = 'quit' then
             break;

          if executeCommand (sourceCode) then
             continue;
                
          if sourceCode = '#p' then
             sourceCode := readBlockOfcode;
          
          if runFramework.compileToAST (sourceCode, syntaxError) then
             begin
             if runFramework.generateByteCode (True, compilerError) then
                begin
                if TRhodus.showByteCode then       
                   runFramework.showByteCodeMethod (mainModule);
                runFrameWork.runCode (mainModule, True, print);                
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
  finally
    runFramework.Free;

    writeln ('Current Memory Map:'#13#10, '[', memoryList.mapMemory, ']');
    writeln ('Press any key to exit');
    readln;
    shutDownConsole;
  end;
end;


end.
