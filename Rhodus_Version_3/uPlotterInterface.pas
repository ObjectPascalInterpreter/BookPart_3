unit uPlotterInterface;


// This source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

Uses SysUtils, Classes, System.Types,
  IdGlobal,
  IdBaseComponent,
  IdComponent,
  IdCustomTCPServer,
  IdTCPClient,
  IdContext,
  uArrayObject;

type
  TCmd = packed record
     cmd : string[32];
     iValue : integer;
     dValue : double;
     x, y : double;
  end;

  TPlotter = class (TObject)

      private

         function sendCmdInt (cmd : string; iValue : integer): TCmd;
         function sendCmd (cmd : string): TCmd;  overload;
         function sendCmd (cmd : string; x, y : double): TCmd;  overload;
         function sendCmd (cmd : string; dValue : double): TCmd;  overload;

      public
         client : TIdTCPClient;

         //procedure resetPosition;
         procedure sayHello;
         procedure plotSineWave;
         procedure clear;
         procedure plot (x, y : TArrayObject);

//         procedure penUp;
//         procedure penDown;
//         procedure turnLeft (angle : double);
//         procedure turnRight (angle : double);
//
//         procedure goForward (d : double);
//         procedure goBack (d : double);
//         procedure setPosition (x, y : double);
//         function  getPosition : TPointF;
//         function  getWidth : integer;
//         function  getHeight : integer;
//         procedure setPenColor (c : integer);
//         procedure setPenWidth (w : integer);
//         procedure setBackGroundColor (c : integer);
         function  connect : boolean;

         constructor Create;
         destructor  Destroy; override;

  end;

implementation

constructor TPlotter.Create;
begin
  client := nil;
end;

destructor TPlotter.Destroy;
begin
  client.Free;
end;



function TPlotter.connect : boolean;
begin
   result := True;
  try
   if client = nil then
      client := TIdTCPClient.Create (nil);
   client.Port := 1050;
   client.host := 'localhost';
   client.Connect;
  except
    on E: Exception do
      begin
      writeln ('Unable to connect to server: ' + e.Message);
      result := False;
      end;
  end;
end;


procedure TPlotter.sayHello;
begin
  sendCmd ('sayhello');
end;


procedure TPlotter.plotSineWave;
begin
  sendCmd ('sw');
end;


procedure TPlotter.clear;
begin
  sendCmd ('clear');
end;


procedure TPlotter.plot (x, y : TArrayObject);
begin
  sendCmd ('plot');
end;


//
//
//procedure TTurtle.goBack (d : double);
//begin
//   sendCmd ('bk', -d);
//end;
//
//
//function TTurtle.getPosition : TPointF;
//var data : TCmd;
//begin
//   data := sendCmd ('pos');
//   result.x := data.x; result.y := data.y;
//end;
//
//
//procedure TTurtle.setPosition (x, y : double);
//begin
//  sendCmd ('setpos', x, y);
//end;
//
//
//function TTurtle.getWidth : integer;
//var data : TCmd;
//begin
//  data := sendCmd ('width');
//  result := data.iValue;
//end;
//
//
//function TTurtle.getHeight : integer;
//var data : TCmd;
//begin
//  data := sendCmd ('height');
//  result := data.iValue;
//end;
//
//
//procedure TTurtle.clearScreen;
//begin
//  sendCmd ('clear');
//end;
//
//
//procedure TTurtle.resetPosition;
//begin
//  sendCmd ('reset');
//end;
//
//
//procedure TTurtle.penUp;
//begin
//  sendCmd ('penup');
//end;
//
//
//procedure TTurtle.penDown;
//begin
//  sendCmd ('pendown');
//end;
//
//
//procedure TTurtle.setPenColor (c : integer);
//begin
//  sendCmdInt ('pencolor', c);
//end;
//
//
//procedure TTurtle.setPenWidth (w : integer);
//begin
//  sendCmdInt ('penwidth', w);
//end;
//
//
//procedure TTurtle.setBackGroundColor (c : integer);
//begin
//  sendCmdInt ('bcolor', c);
//end;
//
//
//procedure TTurtle.turnLeft (angle : double);
//begin
//  sendCmd ('lt', angle);
//end;


//procedure TTurtle.turnRight (angle : double);
//begin
//  sendCmd ('rt', angle);
//end;


function TPlotter.sendCmd (cmd : string; dValue : double): TCmd;
var msRecInfo: TMemoryStream;
    buffer : TIdBytes;
    si : integer;
begin
  result.cmd := AnsiString (cmd);
  result.iValue := 0;
  result.dValue := dValue;

  msRecInfo := TMemoryStream.Create;
  try
    msRecInfo.Write (result, SizeOf(result));

    // writes the stream size then writes the stream data
    Client.IOHandler.Write(msRecInfo, 0, True);

    si := sizeof (TCmd);
    client.IOHandler.ReadBytes(buffer, si);
    BytesToRaw(Buffer, result, si);

  finally
    msRecInfo.Free;
  end;
end;


function TPlotter.sendCmdInt (cmd : string; iValue : integer): TCmd;
var msRecInfo: TMemoryStream;
    buffer : TIdBytes;
    si : integer;
begin
  result.cmd := AnsiString (cmd);
  result.iValue := 0;
  result.iValue := iValue;

  msRecInfo := TMemoryStream.Create;
  try
    msRecInfo.Write (result, SizeOf(result));

    // writes the stream size then writes the stream data
    Client.IOHandler.Write(msRecInfo, 0, True);

    si := sizeof (TCmd);
    client.IOHandler.ReadBytes(buffer, si);
    BytesToRaw(Buffer, result, si);

  finally
    msRecInfo.Free;
  end;
end;


function TPlotter.sendCmd (cmd : string; x, y : double): TCmd;
var msRecInfo: TMemoryStream;
    buffer : TIdBytes;
    si : integer;
begin
  result.cmd := AnsiString (cmd);
  result.iValue := 0;
  result.dValue := 0;
  result.x := x;
  result.y := y;

  msRecInfo := TMemoryStream.Create;
  try
    msRecInfo.Write (result, SizeOf(result));

    // writes the stream size then writes the stream data
    Client.IOHandler.Write(msRecInfo, 0, True);

    si := sizeof (TCmd);
    client.IOHandler.ReadBytes(buffer, si);
    BytesToRaw(Buffer, result, si);

  finally
    msRecInfo.Free;
  end;
end;


function TPlotter.sendCmd (cmd : string): TCmd;
var msRecInfo: TMemoryStream;
    buffer : TIdBytes;
    si : integer;
begin
  result.cmd := AnsiString (cmd);
  result.iValue := 0;
  result.dValue := 0;

  msRecInfo := TMemoryStream.Create;
  try
    msRecInfo.Write (result, SizeOf(result));

    // writes the stream size then writes the stream data
    Client.IOHandler.Write(msRecInfo, 0, True);

    si := sizeof (TCmd);
    client.IOHandler.ReadBytes(buffer, si);
    BytesToRaw(Buffer, result, si);

  finally
      msRecInfo.Free;
  end;
end;


end.
