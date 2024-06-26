unit uBuiltInFile;

{
  Unit:    uBuiltInFile.pas
  Author:  Herbert M sauro
  Date:    10/2021
  Purpose: This file implements the file type for the Rhodus interpreter.

  Ths source is distributed under Apache 2.0
  See https://www.apache.org/licenses/LICENSE-2.0.txt for further information

  Copyright (C)  2019-2024 Herbert M Sauro

  Author Contact Information:
  email: hsauro@gmail.com
}


interface

Uses SysUtils, Classes, Generics.Collections, uLibModule;

type
  TFileRecord = record
     name : string;
     mode : string;
     streamHandle : pointer;
  end;

  
  TBuiltInFile = class (TModuleLib)
     private
        fileHandleCounter : integer;
        fileHandlesDict : TDictionary<integer,TFileRecord>;
        procedure closeStream (fr : TFileRecord);
     public

       procedure readAllText (vm : TObject);
       procedure writeAllText (vm : TObject);
       procedure fileExists (vm : TObject);
       procedure openFile (vm : TObject);
       procedure closeFile (vm : TObject);
       procedure readLine (vm : TObject);
       procedure eof1 (vm : TObject);
       procedure writeString (vm : TObject);

       constructor Create;
       destructor  Destroy; override;
  end;

implementation

Uses Math,
     uSymbolTable,
     uVMExceptions,
     uVM,
     uStringObject,
     uListObject, uMemoryManager, IOUtils, uMachineStack;

    
constructor TBuiltInFile.Create;
begin
  inherited Create ('file', 'File IO Module');

  addMethod (readAllText,   1, 'readAllText',   'Opens a text file, reads all the text in the file into a string, and then closes the file.');
  addMethod (writeAllText,  2, 'writeAllText',  'file.writeAllText ("myfile.txt", variable) Creates a new file, write the contents to the file, and then closes the file. If the target file already exists, it is overwritten.');
  addMethod (fileExists,    1, 'fileExists',    'Returns true if the specified file exists, else returns false.');

  addMethod (openFile,      2, 'openFile',      'Returns a handle to an opened file: f = file.openFile ("myfile.txt", "r")');
  addMethod (closeFile,     1, 'closeFile',     'Close a file: file.closeFile (f).');
  addMethod (readLine,      1, 'readLine',      'Read a single line from a text file: s = file.readLine (f).');
  addMethod (eof1,          1, 'eof',           'Returns true if at end of file, otherwise false: if file.eof (f) ...');
  addMethod (writeString,   2, 'writeString',   'Write a string to a text file: file.writeString (f, "A string").');

   //addValue ('pi', Pi,      'The value of pi');
   fileHandlesDict := TDictionary<integer,TFileRecord>.Create;
   fileHandleCounter := 0;
end;


destructor TBuiltInFile.Destroy;
begin
  fileHandlesDict.Free;
  inherited;
end;

procedure TBuiltInFile.readAllText (vm : TObject);
var fileName, data : string;
begin
   fileName := TVM (vm).popString.value;
   data := TFile.ReadAllText (fileName);
   TVM (vm).push(TStringObject.Create (data));
end;


procedure TBuiltInFile.writeAllText (vm : TObject);
var fileName, data : string;
begin
   data := TVM (vm).popString.value;
   fileName := TVM (vm).popString.value;

   TFile.WriteAllText (fileName, data);
end;


procedure TBuiltInFile.fileExists (vm : TObject);
var fileName : string;
begin
   fileName := TVM (vm).popString.value;
   if TFile.Exists (fileName) then
      TVM (vm).push(True)
   else
      TVM (vm).push(False)
end;


procedure TBuiltInFile.closeStream (fr : TFileRecord);
begin
  if fr.mode = 'r' then
     begin
     TStreamReader (fr.streamHandle).Close;
     TStreamReader (fr.streamHandle).Free;    
     end;
  if fr.mode = 'w' then
     begin
     TStreamWriter (fr.streamHandle).Close;
     TStreamWriter (fr.streamHandle).Free;     
     end; 
end;
      

procedure TBuiltInFile.openFile (vm : TObject);
var fileName : string;
    reader : TStreamReader;
    writer : TStreamWriter;
    fr : TFileRecord;
begin
   try
     fr.mode := TVM (vm).popString.value;
     if fr.mode = 'r' then
        begin
        fileName := TVM (vm).popString.value;
        reader := TStreamReader.Create(fileName);
        fr.name := fileName;
        fr.streamHandle := pointer (reader);
        end;
     if fr.mode = 'w' then
        begin 
        fileName := TVM (vm).popString.value;
        writer := TStreamWriter.Create(fileName);
        fr.name := fileName;
        fr.streamHandle := pointer (writer);              
        end;
     inc (fileHandleCounter);
     fileHandlesDict.Add(fileHandleCounter, fr);
     TVM (vm).push(integer (fileHandleCounter));          
   except
     on e:Exception do
       raise ERuntimeException.Create(e.message);
   end;
end;


procedure TBuiltInFile.closeFile (vm : TObject);
var fileHandle : integer;
    fr : TFileRecord;
begin
   fileHandle := TVM (vm).popInteger;
   if fileHandlesDict.TryGetValue(fileHandle, fr) then
      begin
      closeStream(fr);
      fileHandlesDict.Remove(fileHandle);
      end
   else
      raise ERuntimeException.Create('Cannot find file handle:' + inttostr (fileHandle));
   TVM (vm).pushNone;
end;


procedure TBuiltInFile.readLine (vm : TObject);
var fileHandle : integer;
    fr : TFileRecord;    
    astr : string;
begin
   fileHandle := TVM (vm).popInteger;
   if fileHandlesDict.TryGetValue(fileHandle, fr) then
      begin
      if fr.mode = 'r' then    
         astr := TStreamReader(fr.streamHandle).ReadLine
      else
         begin
         closeStream(fr);
         raise ERuntimeException.Create('File was opened for writing not reading');
         end;
      end
   else
      raise ERuntimeException.Create('Cannot find file handle:' + inttostr (fileHandle));
   TVM (vm).push (TStringObject.Create (astr));
end;


procedure TBuiltInFile.writeString (vm : TObject);
var fileHandle : integer;
    fr : TFileRecord;    
    astr : string;
begin
   astr := TVM (vm).popString.value;
   fileHandle := TVM (vm).popInteger;
   if fileHandlesDict.TryGetValue(fileHandle, fr) then
      begin
      if fr.mode = 'w' then    
         TStreamWriter(fr.streamHandle).Write (astr)
      else
         begin
         closeStream(fr);      
         raise ERuntimeException.Create('File was opened for reading not writing');      
         end;
      end
   else
      raise ERuntimeException.Create('Cannot find file handle:' + inttostr (fileHandle));
   TVM (vm).push (TStringObject.Create (astr));
end;



procedure TBuiltInFile.eof1 (vm : TObject);
var fileHandle : integer;
    fr : TFileRecord; 
    atEnd: boolean;
begin
   fileHandle := TVM (vm).popInteger;
   if fileHandlesDict.TryGetValue(fileHandle, fr) then
      begin
      if fr.mode = 'r' then
         atEnd := TStreamReader (fr.streamHandle).EndOfStream
      else
         begin
         closeStream(fr);      
         raise ERuntimeException.Create('File was opened for writing not reading');          
         end;
      
      if atEnd then
         TVM (vm).push(True)
      else
         TVM (vm).push(False)
      end
   else
      raise ERuntimeException.Create('Cannot find file handle:' + inttostr (fileHandle));
end;


end.

