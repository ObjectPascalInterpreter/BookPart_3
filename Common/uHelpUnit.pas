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
     System.JSON, IOUtils, System.Types;

type
  THMethodType = (hmtMethod, hmtValue);
  THMethod = record
    methodType : THMethodType;
    methodName: string;
    signature: string;
    description: string;
    examples: TArray<string>;
  end;

  THModule = record
    name: string;
    description: string;
    methods: TArray<THMethod>;
  end;

  THModules = TArray<THModule>;

  THelpType = (htMethod, htModule, htValue);
  THelp = class (TObject)
    private
      helpType : THelpType;
    public
      moduleName : string;
      methodName : string;
      signature : string;
      description : string;
      examples : TArray<string>;

      function getHelp : string;
      function clone : THelp;

      constructor Create (description : string); overload;

      constructor CreateMethod (moduleName, methodName : string);
      constructor CreateValue (moduleName, valueName : string);

      constructor CreateModule (moduleName : string); overload;
      constructor Create (moduleName, name, description : string; examples : TArray<string>); overload;
      constructor Create (moduleName, name, signature, description : string; examples : TArray<string>);  overload;
  end;

  THelpDb = class (TObject)
    private
      alist : TArray<string>;
      position : integer;
      numModules, numMethods : integer;

      modules: THModules;

      function loadFromResource : string;

      function WriteModulesToJson(const Modules: THModules): string;

      procedure parseModule  (var modules : THModules);
      procedure parseMethod (modules : THModules);

      function findModule (name : string) : THModule;

      function findMethodInModule (moduleName, methodName : string; var mth : THMethod) : boolean;
      function findValueInModule  (moduleName, ValueName : string; var mth : THMethod)  : boolean;

    public
      function  readTextFile : THModules;
      procedure loadHelpDatabase;
  end;

  procedure buildHelpDb;

implementation

var helpDb : THelpDb;

procedure buildHelpDb;
begin
  helpDb := THelpDb.Create;
  helpDB.loadHelpDatabase
end;


// Used by legacy help system
constructor THelp.Create (description : string);
begin
  self.helpType := htMethod;
  self.moduleName := '';
  self.methodName := '';
  self.signature := '';
  self.description := description;
end;


// Used to add help at the module level
constructor THelp.CreateModule (moduleName : string);
var i : integer;
begin
  self.helpType := htModule;
  self.moduleName := moduleName;
  self.methodName := '';
  self.signature := '';
  self.description := 'No description available';
  for i := 0 to length (helpDb.modules) - 1 do
      if helpDb.modules[i].Name = moduleName then
         self.description := helpDb.modules[i].Description;
end;



// Used to add help for a value, eg math.pi
constructor THelp.CreateValue (moduleName, valueName : string);
var mth : THMethod;
begin
  if HelpDb.findValueInModule (moduleName, valueName, mth) then
     begin
     self.helpType := htValue;
     self.moduleName := moduleName;
     self.methodName := methodName;
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
constructor THelp.CreateMethod (moduleName, methodName : string);
var mth : THMethod;
begin
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
constructor THelp.Create (moduleName, name, signature, description : string; examples : TArray<string>);
begin
  self.helpType := htMethod;
  self.moduleName := moduleName;
  self.methodName := name;
  self.signature := signature;
  self.description := description;
  self.examples := examples;
end;


// Legacy, Used to add help for a value, eg math.pi
constructor THelp.Create (moduleName, name, description : string; examples : TArray<string>);
begin
  self.helpType := htValue;
  self.moduleName := moduleName;
  self.methodName := name;
  self.signature := 'NA';
  self.description := description;
  self.examples := examples;
end;


function THelp.getHelp : string;
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
       result := result + ' in module ' + moduleName + sLineBreak + sLineBreak;
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
       result := 'Value: ' + methodName + ' in module ' + moduleName + sLineBreak + sLineBreak;
       result := result + '   ' + description;
       result := result + sLineBreak + sLineBreak + 'Examples:';
       for i := 0 to length (examples) - 1 do
           result :=  result + sLineBreak + '  ' + examples[i];

       end;
  end;
end;


function THelp.clone : THelp;
begin
  result := THelp.Create(self.moduleName, self.methodName, self.signature, self.description, self.examples);
end;


function ParseJson(const JsonText: string): THModules;
var
  JsonData: TJSONObject;
  ModulesArray: TJSONArray;
  ModuleObj, MethodObj: TJSONObject;
  Module: THModule;
  Method: THMethod;
  ExamplesArray: TJSONArray;
  i, j, numMethods : Integer;
  methodTypeStr : string;
begin
  JsonData := TJSONObject.ParseJSONValue(JsonText) as TJSONObject;
  try
    ModulesArray := JsonData.GetValue<TJSONArray>('modules');
    SetLength(Result, ModulesArray.Count);

    for i := 0 to ModulesArray.Count - 1 do
    begin
      ModuleObj := ModulesArray.Items[i] as TJSONObject;
      Module.Name := ModuleObj.GetValue<string>('name');
      Module.Description := ModuleObj.GetValue<string>('description');

      numMethods := ModuleObj.GetValue<TJSONArray>('methods').Count;

      SetLength(Module.Methods, numMethods);

      for j := 0 to ModuleObj.GetValue<TJSONArray>('methods').Count - 1 do
        begin
        MethodObj := ModuleObj.GetValue<TJSONArray>('methods').Items[j] as TJSONObject;
        methodTypeStr := MethodObj.GetValue<string>('methodType');
        if methodTypeStr = 'method' then
           method.methodType := hmtMethod
        else method.methodType := hmtValue;

        if method.methodType = hmtMethod then
           begin
           Method.methodName := MethodObj.GetValue<string>('methodName');
           Method.Signature := MethodObj.GetValue<string>('signature');
           Method.Description := MethodObj.GetValue<string>('description');

           ExamplesArray := MethodObj.GetValue<TJSONArray>('examples');
          SetLength(Method.Examples, ExamplesArray.Count);
           for var k := 0 to ExamplesArray.Count - 1 do
           begin
             Method.Examples[k] := ExamplesArray.Items[k].Value;
           end;
           end
        else
           begin
           method.methodName := MethodObj.GetValue<string>('valueName');
          Method.Description := MethodObj.GetValue<string>('description');

          ExamplesArray := MethodObj.GetValue<TJSONArray>('examples');
          SetLength(Method.Examples, ExamplesArray.Count);
          for var k := 0 to ExamplesArray.Count - 1 do
              begin
              Method.Examples[k] := ExamplesArray.Items[k].Value;
              end;
             end;

        Module.Methods[j] := Method;
        end;

      Result[i] := Module;
    end;
  finally
    JsonData.Free;
  end;
end;


procedure PrintModules(const Modules: THModules);
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

function THelpDb.findMethodInModule (moduleName, methodName : string; var mth : THMethod) : boolean;
var m : THModule;
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


function THelpDb.findValueInModule (moduleName, valueName : string; var mth : THMethod)  : boolean;
var m : THModule;
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


function THelpDb.findModule (name : string) : THModule;
var i : integer;
begin
  for i := 0 to length (modules) - 1 do
      begin
      if modules[i].Name = name then
         exit (modules[i])
      end;
  raise Exception.Create('Unable to locate module: ' + name);
end;




procedure THelpDb.parseModule (var modules : THModules);
begin
  inc (position);
  setlength (modules, numModules);
  modules[numModules-1].Name := alist[position];
  inc(position);
  modules[numModules-1].Description := alist[position];
  inc(position);
  numMethods := 1;
  while alist[position] = 'startMethod' do
     parseMethod (modules);
end;


procedure THelpDb.parseMethod (modules : THModules);
var atype : string;
    method : THMethod;
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
     setlength (modules[numModules-1].methods, numMethods);
     modules[numModules-1].methods[numMethods-1] := Method;
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
     setlength (modules[numModules-1].methods, numMethods);
     modules[numModules-1].methods[numMethods-1] := Method;
     end;
  inc (numMethods);
  inc (position);

end;


function THelpDb.readTextFile : THModules;
var moduleName : string;
begin
  alist := TFile.ReadAllLines('db.txt');
  moduleName := alist[0];
  position := 0; numModules := 1;
  while True do
     begin
     if alist[position] = 'module' then
        begin
        parseModule (result);
        inc (numModules);
        end;
     if alist[position] = 'end' then
        break;
     end;
end;


function THelpDb.WriteModulesToJson(const Modules: THModules): string;
var
  JsonRootObject: TJSONObject;
  JsonModulesArray: TJSONArray;
  JsonModuleObject: TJSONObject;
  JsonMethodsArray: TJSONArray;
  JsonMethodObject: TJSONObject;
  JsonExamplesArray: TJSONArray;
  Module: THModule;
  Method: THMethod;
  Example: string;
begin
  JsonRootObject := TJSONObject.Create;
  JsonModulesArray := TJSONArray.Create;
  try
    for Module in Modules do
    begin
      JsonModuleObject := TJSONObject.Create;
      JsonModuleObject.AddPair('name', Module.Name);
      JsonModuleObject.AddPair('description', Module.Description);

      JsonMethodsArray := TJSONArray.Create;
      for Method in Module.Methods do
      begin
        JsonMethodObject := TJSONObject.Create;
        case Method.MethodType of
          hmtMethod: JsonMethodObject.AddPair('methodType', 'method');
          hmtValue: JsonMethodObject.AddPair('methodType', 'value');
        end;
        if Method.MethodType = hmtMethod then
           begin
           JsonMethodObject.AddPair('methodName', Method.methodName);
           JsonMethodObject.AddPair('signature', Method.Signature);
           JsonMethodObject.AddPair('description', Method.Description);
           end
        else
           begin
           JsonMethodObject.AddPair('valueName', Method.methodName);
           JsonMethodObject.AddPair('description', Method.Description);
           end;

        JsonExamplesArray := TJSONArray.Create;
        for Example in Method.Examples do
        begin
          JsonExamplesArray.Add(Example);
        end;
        JsonMethodObject.AddPair('examples', JsonExamplesArray);

        JsonMethodsArray.AddElement(JsonMethodObject);
      end;
      JsonModuleObject.AddPair('methods', JsonMethodsArray);

      JsonModulesArray.AddElement(JsonModuleObject);
    end;

    JsonRootObject.AddPair('modules', JsonModulesArray);
    Result := JsonRootObject.ToString;
  finally
    JsonRootObject.Free;
  end;
end;


function THelpDb.loadFromResource : string;
var LStream : TResourceStream;
    List : TStringList;
begin
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
end;


procedure THelpDb.loadHelpDatabase;
var jsontext : string;
begin
  if TFile.Exists('xHelpDb.json') then
     begin
     Jsontext := TFile.ReadAllText('HelpDb.json');
     Modules := ParseJson(JsonText);
     end
  else
    if TFile.Exists('db.txt') then
       begin
       modules := readTextFile;
       jsontext := WriteModulesToJson(modules);
       TFile.WriteAllText('rhodus.json', jsonText);
     end
  else
     jsontext := loadFromResource;
end;


initialization
finalization
  if helpDb <> nil then
     helpDb.Free;
end.


//module
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
//#
//startMethod
//method
//cos
//cos (a)
//It works out the cos
//x = math.cos (1.2)
//x = math.cos (math.pi)
//x = math.cos ([1,2,3])
//#
//startMethod
//value
//pi
//Returns the value of pi
//x = math.pi
//#
//module
//random
//This is the random module
//startMethod
//method
//seed
//seed()
//Set the seed for the random number generator
//seed (23)
//#
//startMethod
//method
//random
//random()
//Return a uniformly distributed random number:
//random()
//#
//end
//

