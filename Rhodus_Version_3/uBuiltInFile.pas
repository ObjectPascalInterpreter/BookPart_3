unit uBuiltInFile;


// This source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com


interface

Uses SysUtils, Classes, uLibModule;

type
  TBuiltInFile = class (TModuleLib)

     procedure readAllText (vm : TObject);
     procedure writeAllText (vm : TObject);
     procedure fileExists (vm : TObject);

     constructor Create;
  end;

implementation

Uses Math, uSymbolTable, uVM, uStringObject, uListObject, uMemoryManager, IOUtils, uMachineStack;


constructor TBuiltInFile.Create;
begin
  inherited Create ('file', 'File IO Module');

  addMethod (readAllText,   1, 'readAllText',   'Opens a text file, reads all the text in the file into a string, and then closes the file.');
  addMethod (writeAllText,  2, 'writeAllText',  'file.writeAllText ("myfile.txt", variable) Creates a new file, write the contents to the file, and then closes the file. If the target file already exists, it is overwritten.');
  addMethod (fileExists,    1, 'fileExists',    'Returns true if the specified file exists, else returns false.');

   //addValue ('pi', Pi,      'The value of pi');
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



end.
