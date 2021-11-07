unit uEmbeddAPI;

interface

Uses Classes, SysUtils, uRhodusEngine, uRhodusTypes, ulibTypes;


function  rhodus_initialize (var config : TRhodusConfig) : THandle;  stdcall;
function  rhodus_run (handle : THandle; code : AnsiString) : integer; stdcall;
procedure rhodus_terminate (handle : THandle); stdcall;
function  rhodus_getLastError (handle : THandle) : PAnsiChar; stdcall;
function  rhodus_getSettings (handle : THandle) : PRhodusSettings; stdcall;

implementation

Uses uSyntaxParser,
     uCompile,
     uMachineStack,
     uStringObject,
     uListObject,
     uBuiltInGlobal;

var lastErrorStr : AnsiString;

function rhodus_initialize (var config : TRhodusConfig) : THandle; stdcall
var rhodus : TRhodus;
begin
  rhodus := TRhodus.Create;
  result := THandle (rhodus);

  rhodus.setPrintCallBack(config.printPtr);
  rhodus.setPrintLnCallBack(config.printlnPtr);
end;


function rhodus_getSettings (handle : THandle) : PRhodusSettings; stdcall;
var rhodus : TRhodus;
begin
  rhodus := TRhodus (handle);
  result.versionStr := PansiChar (AnsiString (rhodus.getVersion()));
end;


function rhodus_run (handle : THandle; code : AnsiString) : integer; stdcall;
var rhodus : TRhodus;
    syntaxError : TSyntaxError;
    compilerError : TCompilerError;
begin
  result := 0;
  rhodus := TRhodus (handle);
  if rhodus.compileToAST (code, syntaxError) then
     begin
     if rhodus.generateByteCode (True, compilerError) then
        begin
        //if TRhodus.bolShowByteCode then
        //   rhodus.showByteCodeMethod (mainModule);
        rhodus.runCode (mainModule, True);
        end
     else
       begin
       //setGreen;
       lastErrorStr := 'ERROR ' + '[line ' + inttostr (compilerError.lineNumber) + ', column: ' + inttostr (compilerError.columnNumber) + '] ' + compilerError.errorMsg;
       result := -1;
       //setWhite;
       end;
     end
  else
     begin
     lastErrorStr := 'ERROR ' + '[line ' + inttostr (syntaxError.lineNumber) + ', column: ' + inttostr (syntaxError.columnNumber) + '] ' + syntaxError.errorMsg;
     result := -1;
     end;
end;


procedure rhodus_terminate (handle : THandle); stdcall;
var rhodus : TRhodus;
begin
  rhodus := TRhodus (handle);
  rhodus.free;
end;


function rhodus_getLastError (handle : THandle) : PAnsiChar; stdcall;
begin
  result := PAnsiChar (lastErrorStr);
end;

end.
