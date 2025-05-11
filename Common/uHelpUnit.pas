unit uHelpUnit;

{
  Unit:    uHelpUnit.pas
  Author:  Herbert M sauro
  Date:    10/2024
  Purpose: This file implements part of the help system

  Ths source is distributed under Apache 2.0
  See https://www.apache.org/licenses/LICENSE-2.0.txt for further information

  Copyright (C)  2019-2024 Herbert M Sauro

  Author Contact Information:
  email: hsauro@gmail.com
}

interface

Uses System.Classes,
     System.SysUtils,
     Generics.Collections,
     System.TypInfo,
     System.JSON,
     IOUtils,
     System.Types;

type
  // Used to load help into methods that are part of dataobject
  THelpRecord = record
     m : string;  // method name
     d : string;  // description
     s : string;  // signature
     e : TArray<string>; // examples
  end;


  THMethodType = (hmtMethod, hmtValue);//, hmtDataObject);
  TRecordType = (rtModule, rtDataObject);
  THRecord = record
    methodType : THMethodType;
    methodName: string;
    signature: string;
    description: string;
    examples: TArray<string>;
  end;

  THCommonHelp = record
    recordType : TRecordType;
    name: string;
    description: string;
    methods: TArray<THRecord>;
  end;

  // This temporarily stores help info on all the different
  // types as we read it in from the simple text file.
  THArrayOfHelp = TArray<THCommonHelp>;

  THelpBase = class (TObject)
      description : string;
      methodName : string;
      signature : string;
      examples : TArray<string>;
      function getHelp : string; virtual; abstract;
  end;

  THelpType = (htMethod, htModule, htValue);

  TDataObjectHelp = class (THelpBase)
      dataObjectName : string;
      function getHelp : string;  override;
      constructor CreateObjectMethodHelp(dataObjectName : string; methodName : string);
  end;

  THelpModule = class (THelpBase)
    private
      helpType : THelpType;
    public
      moduleName : string;
      valueName : string;

      function getHelp : string;
      function clone : THelpModule;

      function toLatex : string;
      function toMarkdown : string;

      constructor Create;  overload;

      constructor Create (description : string); overload;

      constructor CreateMethod (moduleName, methodName : string);
      constructor CreateValue (moduleName, valueName : string);
      constructor CreateModule (moduleName : string); overload;

      constructor Create (moduleName, name, description : string; examples : TArray<string>); overload;
      constructor Create (moduleName, name, signature, description : string; examples : TArray<string>);  overload;
      destructor  Destroy; override;
  end;


  THelpDb = class (TObject)
    private
      alist : TArray<string>;
      position : integer;
      numModules, numMethods, numDataObjects : integer;

      arrayOfHelp: THArrayOfHelp;

      function loadFromResource : string;

      procedure parseModule  (var generics : THArrayOfHelp);
      procedure parseMethod (var generics : THArrayOfHelp);
      procedure parseDataObject (var generics : THArrayOfHelp);
      procedure parseDataObjectMethod (var generics : THArrayOfHelp);

      function findModule (name : string) : THCommonHelp;

      function findMethodInModule (moduleName, methodName : string; var mth : THRecord) : boolean;
      function findValueInModule  (moduleName, ValueName : string; var mth : THRecord)  : boolean;
      function findMethodInDataObject (dataObjectName: string; methodName : string; var mth : THRecord) : boolean;

    public
      function  readTextFile : THArrayOfHelp;
      procedure loadHelpDatabase;
  end;

  procedure buildHelpDb;

implementation

Uses StrUtils;

var helpDb : THelpDb;

procedure buildHelpDb;
begin
  helpDb := THelpDb.Create;
  helpDB.loadHelpDatabase
end;


// ------------------------------------------------------------------------

constructor TDataObjectHelp.CreateObjectMethodHelp (dataObjectName : string; methodName : string);
var mth : THRecord;
begin
  self.dataObjectName := dataObjectName;

  if HelpDb.findMethodInDataObject (dataObjectName, methodName, mth) then
     begin
     self.methodName := mth.methodName;
     self.description := mth.description;
     self.signature := mth.signature;
     self.examples := mth.examples;
     end;
end;


function TDataObjectHelp.getHelp : string;
var i : integer;
begin
   if methodName <> '' then
      result := result + 'Method: ' + methodName + sLineBreak;
   if signature <> ''  then
      result := result + '' + signature + sLineBreak;
   result := result + '   ' + description;
   if length (examples) > 0 then
      begin
      result := result + sLineBreak + sLineBreak + 'Examples:';
      for i := 0 to length (examples) - 1 do
      result :=  result + sLineBreak + '  ' + examples[i];
      end;
end;


// ------------------------------------------------------------------------


constructor THelpModule.Create;
begin
  inherited;
end;

// Used by legacy help system
constructor THelpModule.Create (description : string);
begin
  Create;
  self.helpType := htMethod;
  self.moduleName := '';
  self.methodName := '';
  self.signature := '';
  self.description := description;
end;


// Used to add help at the module level
constructor THelpModule.CreateModule (moduleName : string);
var i : integer;
begin
  Create;
  self.helpType := htModule;
  self.moduleName := moduleName;
  self.methodName := '';
  self.signature := '';
  self.description := 'No description available';
  for i := 0 to length (helpDb.arrayOfHelp) - 1 do
      if helpDb.arrayOfHelp[i].Name = moduleName then
         self.description := helpDb.arrayOfHelp[i].Description;
end;


// Used to add help for a value, eg math.pi
constructor THelpModule.CreateValue (moduleName, valueName : string);
var mth : THRecord;
begin
  Create;
  if HelpDb.findValueInModule (moduleName, valueName, mth) then
     begin
     self.helpType := htValue;
     self.moduleName := moduleName;
     self.valueName := valueName;
     self.signature := mth.signature;
     self.description := mth.description;
     self.examples := mth.examples;
     end
  else
     begin
     self.helpType := htValue;
     self.moduleName := moduleName;
     self.methodName := methodName;
     self.signature := '';
     self.description := 'Failed to find help on this module and value';
     end;
end;


// Used to add help for a method, eg math.sin()
constructor THelpModule.CreateMethod (moduleName, methodName : string);
var mth : THRecord;
begin
  Create;
  if HelpDb.findMethodInModule (moduleName, methodName, mth) then
     begin
     self.helpType := htMethod;
     self.moduleName := moduleName;
     self.methodName := methodName;
     self.signature := mth.signature;
     self.description := mth.description;
     self.examples := mth.examples;
     end
  else
     begin
     self.helpType := htMethod;
     self.moduleName := moduleName;
     self.methodName := methodName;
     self.signature := '';
     self.description := 'Failed to find help on this module and method';
     end;
end;

// Legacy create
constructor THelpModule.Create (moduleName, name, signature, description : string; examples : TArray<string>);
begin
  Create;
  self.helpType := htMethod;
  self.moduleName := moduleName;
  self.methodName := name;
  self.signature := signature;
  self.description := description;
  self.examples := examples;
end;


// Legacy, Used to add help for a value, eg math.pi
constructor THelpModule.Create (moduleName, name, description : string; examples : TArray<string>);
begin
  Create;
  self.helpType := htValue;
  self.moduleName := moduleName;
  self.methodName := name;
  self.signature := 'NA';
  self.description := description;
  self.examples := examples;
end;


destructor THelpModule.Destroy;
begin
  self.moduleName := '';
  inherited;
end;


function THelpModule.getHelp : string;
var i : integer;
begin
  result := '';
  case helpType of
     htModule :
       begin
       result := 'Module: ' + moduleName + sLineBreak;
       result := result + '   ' + description;
       end;
     htMethod:
       begin
       if methodName <> '' then
          result := result + 'Method: ' + methodName;
       if moduleName <> '' then
       result := result + ' in module: ' + moduleName + sLineBreak + sLineBreak;
       if signature <> ''  then
          result := result + '' + signature + sLineBreak;
       result := result + '   ' + description;
       if length (examples) > 0 then
          begin
          result := result + sLineBreak + sLineBreak + 'Examples:';
          for i := 0 to length (examples) - 1 do
              result :=  result + sLineBreak + '  ' + examples[i];
       end;
       end;
     htValue:
       begin
       result := 'Value in module: ' + moduleName + sLineBreak + sLineBreak;
       result := result + '   ' + description;
       result := result + sLineBreak + sLineBreak + 'Examples:';
       for i := 0 to length (examples) - 1 do
           result :=  result + sLineBreak + '  ' + examples[i];

       end;
  end;
end;


function THelpModule.toMarkdown : string;
var i : integer;
begin
  result := '';
  case helpType of
     htModule :
       begin
       result := '**Module: ' + moduleName + '**' + sLineBreak;
       result := result + '   ' + description;
       end;
     htMethod:
       begin
       if methodName <> '' then
          begin
          result := result + '**Method: ' + methodName + '**';
          result := result + sLineBreak;
          end;

       if signature <> ''  then
          result := result + '' + '`' + signature + '`' + sLineBreak;

       result := result + '   ' + description;
       if length (examples) > 0 then
          begin
          result := result + sLineBreak + 'Examples:' + sLineBreak;
          for i := 0 to length (examples) - 1 do
              result :=  result + '* ' + '`' + examples[i] + '`' + sLineBreak;
       end;
       end;
     htValue:
       begin
       result := result + '**Value: ' + valueName;
       result := result + '**' + sLineBreak;
       result := result + '   ' + description;
       if length (examples) > 0 then
          begin
          result := result + sLineBreak + 'Examples:' + sLineBreak;
          for i := 0 to length (examples) - 1 do
              result :=  result + '* ' + '`' + examples[i] + '`' + sLineBreak;
       end;
       end;
  end;
end;


function THelpModule.toLatex : string;
var i : integer;
begin
  result := '';
  case helpType of
     htModule :
       begin
       result := 'Module: ' + moduleName + sLineBreak;
       result := result + '   ' + description;
       end;
     htMethod:
       begin
       if methodName <> '' then
          begin
          result := result + '{\bfseries\textsf{Method: ' + methodName;
          result := result + '}}' + sLineBreak + sLineBreak;
          end;

       if signature <> ''  then
          result := result + '' + '{\tt ' + replacetext(signature, '|', '$\vert$') + '}' + sLineBreak;

       result := result + sLinebreak + '   ' + description;
       if length (examples) > 0 then
          begin
          result := result + sLineBreak + sLineBreak + 'Examples:' + sLineBreak;
          result := result + '\vspace{-5pt}' + sLineBreak;
          result := result + '\begin{itemize}' + sLineBreak;
          result := result + '\itemsep -4pt' + sLineBreak;
          for i := 0 to length (examples) - 1 do
              result :=  result + '\item[] ' + '{\tt ' + examples[i] + '}' + sLineBreak;
          result := result + '\end{itemize}' + sLineBreak;
       end;
       end;
     htValue:
       begin
       result := result + '{\bfseries\textsf{Value: ' + valueName;
       result := result + '}}' + sLineBreak + sLineBreak;
       result := result + '   ' + description;
       if length (examples) > 0 then
          begin
          result := result + sLineBreak + sLineBreak + 'Examples:' + sLineBreak;
          result := result + '\vspace{-5pt}' + sLineBreak;
          result := result + '\begin{itemize}' + sLineBreak;
          result := result + '\itemsep -4pt' + sLineBreak;
          for i := 0 to length (examples) - 1 do
              result :=  result + '\item[] ' + '{\tt ' + examples[i] + '}' + sLineBreak;
          result := result + '\end{itemize}' + sLineBreak;
       end;
       end;
  end;
end;


function THelpModule.clone : THelpModule;
begin
  result := THelpModule.Create(self.moduleName, self.methodName, self.signature, self.description, self.examples);
end;


procedure PrintModules(const Modules: THArrayOfHelp);
begin
  for var i := 0 to Length(Modules) - 1 do
  begin
    Writeln('Module Name: ', Modules[i].Name);
    Writeln('Description: ', Modules[i].Description);
    for var j := 0 to Length(Modules[i].Methods) - 1 do
    begin
      Writeln('  Method Name: ', Modules[i].Methods[j].methodName);
      Writeln('  Signature: ', Modules[i].Methods[j].Signature);
      Writeln('  Description: ', Modules[i].Methods[j].Description);
      Writeln('  Examples: ');
      for var k := 0 to Length(Modules[i].Methods[j].Examples) - 1 do
      begin
        Writeln('    ', Modules[i].Methods[j].Examples[k]);
      end;
    end;
    Writeln;
  end;
end;


// --------------------------------------------------------------------------
// THelpDb

function THelpDb.findMethodInModule (moduleName, methodName : string; var mth : THRecord) : boolean;
var m : THCommonHelp;
    i : integer;
begin
  try
    m := findModule(moduleName);
   for i := 0 to length (m.methods) - 1 do
       begin
       if m.methods[i].methodName = methodName then
          begin
          mth := m.methods[i];
          exit (True);
          end;
       end;
 except
    on e:exception do
      begin
      exit (False);
      end;
  end;
  exit (False);
end;


function THelpDb.findValueInModule (moduleName, valueName : string; var mth : THRecord)  : boolean;
var m : THCommonHelp;
    i : integer;
begin
  try
    m := findModule(moduleName);
    for i := 0 to length (m.methods) - 1 do
        begin
        if m.methods[i].methodName = valueName then
           begin
           mth := m.methods[i];
           exit (True);
           end;
        end;
  except
    on e:exception do
      begin
      exit (False);
      end;
  end;
  exit (False);
end;


function THelpDb.findMethodInDataObject (dataObjectName: string; methodName : string; var mth : THRecord) : boolean;
var i, j : integer;
    m : THCommonHelp;
begin
  try
   for i := 0 to length (arrayOfHelp) - 1 do
      begin
      if arrayOfHelp[i].Name = dataObjectName then
         begin
         m := arrayOfHelp[i];
         for j := 0 to length (m.methods) - 1 do
             begin
             if m.methods[j].methodName = methodName then
                begin
                mth := m.methods[j];
                exit (True);
                end;
             end;
         end;
      end;
  except
    on E: exception do
       exit (False);
  end;
  exit (False);
end;


function THelpDb.findModule (name : string) : THCommonHelp;
var i : integer;
begin
  for i := 0 to length (arrayOfHelp) - 1 do
      begin
      if arrayOfHelp[i].Name = name then
         exit (arrayOfHelp[i])
      end;
  raise Exception.Create('Unable to locate module: ' + name);
end;


// --------------------------------------------------------------------


 procedure THelpDb.parseDataObject (var generics : THArrayOfHelp);
 begin
   inc (position);
   setlength (generics, numModules);
  generics[numModules-1].recordType := rtDataObject;
   generics[numModules-1].Name := alist[position];
   inc(position);
   generics[numModules-1].Description := alist[position];
   inc(position);
   numMethods := 1;
   while alist[position] = 'startMethod' do
      parseDataObjectMethod (generics);
 end;


procedure THelpDb.parseDataObjectMethod (var generics : THArrayOfHelp);
var methodName : string;
    method : THRecord;
    ecount : integer;
begin
  inc(position);
  method.methodname := alist[position];
  method.methodType := hmtMethod;
  inc(position);
  method.signature := alist[position];
  inc(position);
  method.description := alist[position];
  inc(position);
  ecount := 0;
  while alist[position] <> '##########' do
        begin
        setlength (method.examples, ecount+1);
        method.examples[ecount] := alist[position];
        inc (ecount); inc (position);
        end;
  setlength (generics[numModules-1].methods, numMethods);
  generics[numModules-1].methods[numMethods-1] := Method;
  inc (numMethods);
  inc (position);
end;


procedure THelpDb.parseModule (var generics : THArrayOfHelp);
begin
  inc (position);
  setlength (generics, numModules);
  generics[numModules-1].recordType := rtModule;
  generics[numModules-1].Name := alist[position];
  inc(position);
  generics[numModules-1].Description := alist[position];
  inc(position);
  numMethods := 1;
  while alist[position] = 'startMethod' do
     parseMethod (generics);
end;


procedure THelpDb.parseMethod (var generics : THArrayOfHelp);
var atype : string;
    method : THRecord;
    ecount : integer;
begin
  inc(position);
  atype := alist[position];
  if atype = 'method' then
     begin
     method.MethodType := hmtMethod;
     inc(position);
     method.methodName := alist[position];
     inc(position);
     method.signature := alist[position];
     inc(position);
     method.description := alist[position];
     inc(position);
     ecount := 0;
     while alist[position] <> '##########' do
         begin
         setlength (method.examples, ecount+1);
         method.examples[ecount] := alist[position];
         inc (ecount); inc (position);
         end;
     setlength (generics[numModules-1].methods, numMethods);
     generics[numModules-1].methods[numMethods-1] := Method;
     end
  else
     begin
     method.MethodType := hmtValue;
     inc(position);
     method.methodName := alist[position];
     inc(position);
     method.description := alist[position];
     inc(position);
     ecount := 0;
     while alist[position] <> '##########' do
         begin
         setlength (method.examples, ecount+1);
         method.examples[ecount] := alist[position];
         inc (ecount); inc (position);
         end;
     setlength (generics[numModules-1].methods, numMethods);
     generics[numModules-1].methods[numMethods-1] := Method;
     end;
  inc (numMethods);
  inc (position);
end;


function THelpDb.readTextFile : THArrayOfHelp;
var moduleName : string;
begin
  alist := TFile.ReadAllLines('db.txt');
  moduleName := alist[0];
  position := 0; numModules := 1;
  while True do
     begin
     if alist[position] = '---------- module' then
        begin
        parseModule (result);
        inc (numModules);
        end;
     if alist[position] = '---------- dataobject' then
        begin
        parseDataObject (result);
        inc (numModules);
        end;
     if alist[position] = 'end' then
        break;
     end;
end;


function THelpDb.loadFromResource : string;
var LStream : TResourceStream;
    List : TStringList;
begin
  try
  LStream := TResourceStream.Create(HInstance, 'HelpDb', RT_RCDATA);
  try
    List := TStringList.Create;
    try
      List.LoadFromStream(LStream);
      result := List.Text;
    finally
      List.Free;
    end;
  finally
    LStream.free;
  end;
  except
    on Exception do
       result := '';
  end;
end;


// ------------------------------------------------------------------------------

function StringToMethodType(const Str: string): THMethodType;
begin
  if Str = 'hmtMethod' then
    Result := hmtMethod
  else if Str = 'hmtValue' then
    Result := hmtValue
  else
    raise Exception.CreateFmt('Unknown method type: %s', [Str]);
end;


function StringToRecordType(const Str: string): TRecordType;
begin
  if Str = 'rtModule' then
    Result := rtModule
  else if Str = 'rtDataObject' then
    Result := rtDataObject
  else
    raise Exception.CreateFmt('Unknown record type: %s', [Str]);
end;


procedure ImportFromJSON(const FileName: string; out HelpArray: THArrayOfHelp);
var
  JSONObject: TJSONObject;
  JSONArray: TJSONArray;
  MethodArray: TJSONArray;
  MethodObject: TJSONObject;
  HelpObject: TJSONObject;
  HelpItem: THCommonHelp;
  MethodItem: THRecord;
  ExampleArray: TJSONArray;
  JSONReader: TStreamReader;
  JSONValue: TJSONValue;
  i, j: Integer;
begin
  JSONReader := TStreamReader.Create(FileName, TEncoding.UTF8);
  try
    JSONValue := TJSONObject.ParseJSONValue(JSONReader.ReadToEnd);
    try
      if JSONValue is TJSONObject then
      begin
        JSONObject := TJSONObject(JSONValue);
        JSONArray := JSONObject.GetValue<TJSONArray>('listOfHelpEntries');
        SetLength(HelpArray, JSONArray.Count);
        for i := 0 to JSONArray.Count - 1 do
        begin
          HelpObject := JSONArray.Items[i] as TJSONObject;
          HelpItem.recordType := StringToRecordType(HelpObject.GetValue<string>('recordType'));
          HelpItem.name := HelpObject.GetValue<string>('moduleName');
          HelpItem.description := HelpObject.GetValue<string>('description');
          MethodArray := HelpObject.GetValue<TJSONArray>('methods');
          SetLength(HelpItem.methods, MethodArray.Count);
          for j := 0 to MethodArray.Count - 1 do
          begin
            MethodObject := MethodArray.Items[j] as TJSONObject;
            MethodItem.methodType := StringToMethodType(MethodObject.GetValue<string>('methodType'));
            MethodItem.methodName := MethodObject.GetValue<string>('methodName');
            MethodItem.signature := MethodObject.GetValue<string>('signature');
            MethodItem.description := MethodObject.GetValue<string>('description');
            ExampleArray := MethodObject.GetValue<TJSONArray>('examples');
            SetLength(MethodItem.examples, ExampleArray.Count);
            for var k := 0 to ExampleArray.Count - 1 do
            begin
              MethodItem.examples[k] := ExampleArray.Items[k].Value;
            end;
            HelpItem.methods[j] := MethodItem;
          end;
          HelpArray[i] := HelpItem;
        end;
      end
      else
        raise Exception.Create('Invalid JSON format');
    finally
      JSONValue.Free;
    end;
  finally
    JSONReader.Free;
  end;
end;


// ------------------------------------------------------------------------------

function MethodTypeToString(mt: THMethodType): string;
begin
  case mt of
    hmtMethod: Result := 'hmtMethod';
    hmtValue: Result := 'hmtValue';
  else
    Result := 'Unknown';
  end;
end;


function RecordTypeToString(rt: TRecordType): string;
begin
  case rt of
    rtModule: Result := 'rtModule';
    rtDataObject: Result := 'rtDataObject';
  else
    Result := 'Unknown';
  end;
end;


procedure ExportToJSON(const HelpArray: THArrayOfHelp; const FileName: string);
var
  JSONObject: TJSONObject;
  JSONArray: TJSONArray;
  MethodArray: TJSONArray;
  MethodObject: TJSONObject;
  HelpObject: TJSONObject;
  HelpItem: THCommonHelp;
  MethodItem: THRecord;
  ExampleArray: TJSONArray;
  i, j: Integer;
  JSONWriter: TStreamWriter;
begin
  JSONArray := TJSONArray.Create;
  try
    for i := 0 to Length(HelpArray) - 1 do
    begin
      HelpItem := HelpArray[i];
      HelpObject := TJSONObject.Create;
      HelpObject.AddPair('recordType', RecordTypeToString(HelpItem.recordType));
      HelpObject.AddPair('moduleName', HelpItem.name);
      HelpObject.AddPair('description', HelpItem.description);
      MethodArray := TJSONArray.Create;
      for j := 0 to Length(HelpItem.methods) - 1 do
      begin
        MethodItem := HelpItem.methods[j];
        MethodObject := TJSONObject.Create;
        MethodObject.AddPair('methodType', MethodTypeToString(MethodItem.methodType));
        MethodObject.AddPair('methodName', MethodItem.methodName);
        MethodObject.AddPair('signature', MethodItem.signature);
        MethodObject.AddPair('description', MethodItem.description);
        ExampleArray := TJSONArray.Create;
        for var example in MethodItem.examples do
        begin
          ExampleArray.Add(example);
        end;
        MethodObject.AddPair('examples', ExampleArray);
        MethodArray.AddElement(MethodObject);
      end;
      HelpObject.AddPair('methods', MethodArray);
      JSONArray.AddElement(HelpObject);
    end;
    JSONObject := TJSONObject.Create;
    JSONObject.AddPair('listOfHelpEntries', JSONArray);
    JSONWriter := TStreamWriter.Create(FileName, False, TEncoding.UTF8);
    try
      JSONWriter.Write(JSONObject.ToString);
    finally
      JSONWriter.Free;
    end;
  finally
    JSONArray.Free;
  end;
end;



procedure THelpDb.loadHelpDatabase;
var jsontext : string;
begin
  // Order of reading is the text file, followed by the json file followed by the resource.
  // db.txt is a simple text file that is easy to edit.
  // claud.json is generated from db.txt and is a json format.
  // These are only used by the developer to help create the help file HelpDb.json
  // because its easier to manually edit a text file such as db.txt than the json file.
  // Once a new copy of the help files developerCopy.json work, copy it over to helpDb.json

  // HelpDb.json is the same as developerCopy.json but is never updated by the software
  // If all else fails we load, what is likely to be, an old version of help in the resources

  // When deployed you only need to copy the HelpDb.json file
  if TFile.Exists('db.txt') then
     begin
     arrayOfHelp := readTextFile;
     ExportToJSON(arrayOfHelp, 'developerCopy.json');
     end
  else
  if TFile.Exists('HelpDb.json') then
     ImportFromJSON('HelpDb.json', arrayOfHelp)
  else
     jsontext := loadFromResource;
end;


initialization
finalization
  if helpDb <> nil then
     helpDb.Free;
end.


//---------- module
//math
//This is the math module
//startMethod
//method
//sin
//sin (a)
//It works out the sin
//x = math.sin (1.2)
//x = math.sin (math.pi)
//x = math.sin ([1,2,3])
//##########
//startMethod
//method
//cos
//cos (a)
//It works out the cos
//x = math.cos (1.2)
//x = math.cos (math.pi)
//x = math.cos ([1,2,3])
//##########
//startMethod
//value
//pi
//Returns the value of pi
//x = math.pi
//##########
//---------- module
//random
//This is the random module
//startMethod
//method
//seed
//seed()
//Set the seed for the random number generator
//seed (23)
//##########
//startMethod
//method
//random
//random()
//Return a uniformly distributed random number:
//random()
//##########
//---------- dataobject
//MatrixObject
//This is the matrix object
//startMethod
//rows
//m.rows ()
//Get the number of rows in the matrix: var.rows()
//n = m.rows()
//println (p.rows())
//##########
//startMethod
//cols
//m.cols ()
//Get the number of cols in the matrix: var.cols()
//n = m.cols()
//println (p.cols())
##########
//end

