unit uEmbeddAPI;

interface

Uses Classes, SysUtils, uRhodusEngine, uRhodusTypes, ulibTypes;


function  rhodus_initialize (var config : TRhodusConfig) : NativeInt;  stdcall;
function  rhodus_run (handle : NativeInt; code : AnsiString) : integer; stdcall;
function  rhodus_terminate (handle : NativeInt) : integer; stdcall;
function  rhodus_getLastError (handle : NativeInt) : PRhodusError; stdcall;
function  rhodus_getSettings (handle : NativeInt) : PRhodusSettings; stdcall;

implementation

Uses uSyntaxParser,
     uCompile,
     uMachineStack,
     uStringObject,
     uListObject,
     uBuiltInGlobal;

var lastError : TRhodusError;
    settings : TRhodusSettings;

function rhodus_initialize (var config : TRhodusConfig) : NativeInt; stdcall
var rhodus : TRhodus;
begin
  try
    rhodus := TRhodus.Create;
    result := NativeInt (rhodus);

    rhodus.setPrintCallBack(config.printPtr);
    rhodus.setPrintLnCallBack(config.printlnPtr);
    rhodus.setReadStringCallBack(config.readStringPtr);
    if config.graphicsHandlerPtr <> nil then
       rhodus.setGraphicsMethodCallBack (config.graphicsHandlerPtr);
  except
    on e: Exception do
      begin
      lastError.errorMsg := PAnsiChar (AnsiString (e.Message));
      result := -1;
      end;
  end;
end;


var s : AnsiString;
function rhodus_getSettings (handle : NativeInt) : PRhodusSettings; stdcall;
var rhodus : TRhodus;
    p : PAnsiChar;
    s: AnsiString;
begin
  result := @settings;
  rhodus := TRhodus (handle);
  s := AnsiString (rhodus.getVersion());
  result.versionStr := PAnsiChar (s);
end;


function rhodus_run (handle : NativeInt; code : AnsiString) : integer; stdcall;
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
        rhodus.runCode (mainModule, True);
        end
     else
       begin
       lastError.errorMsg := PAnsiChar (AnsiString('ERROR ' + '[line ' + inttostr (compilerError.lineNumber) + ', column: ' + inttostr (compilerError.columnNumber) + '] ' + compilerError.errorMsg));
       result := -1;
       end;
     end
  else
     begin
     lastError.errorMsg := PAnsiChar (AnsiString ('ERROR ' + '[line ' + inttostr (syntaxError.lineNumber) + ', column: ' + inttostr (syntaxError.columnNumber) + '] ' + syntaxError.errorMsg));
     result := -1;
     end;
end;


function rhodus_terminate (handle : NativeInt) : integer; stdcall;
var rhodus : TRhodus;
begin
  try
    rhodus := TRhodus (handle);
    rhodus.free;
  except
    on e: Exception do
      begin
      lastError.errorMsg := PAnsiChar (AnsiString (e.Message));
      result := -1;
      end;
  end;
end;


function rhodus_getLastError (handle : NativeInt) : PRhodusError; stdcall;
begin
  result := @LastError;
end;

end.
