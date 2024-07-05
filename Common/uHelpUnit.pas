unit uHelpUnit;

interface

Uses SysUtils, Classes;

type

  THelpType = (htMethod, htModule);
  THelp = class (TObject)
      helpType : THelpType;
      moduleName : string;
      name : string;
      signature : string;
      description : string;
      examples : TArray<string>;

      function getHelp : string;
      function clone : THelp;

      constructor Create (description : string); overload;
      constructor Create (moduleName, description : string); overload;
      constructor Create (moduleName, name, signature, description : string; examples : TArray<string>);  overload;
end;

implementation


constructor THelp.Create (description : string);
begin
  self.helpType := htMethod;
  self.moduleName := 'NA';
  self.name := 'NA';
  self.signature := 'NA';
  self.description := description;
  //self.examples := examples;
end;

constructor THelp.Create (moduleName, description : string);
begin
  self.helpType := htModule;
  self.moduleName := moduleName;
  self.name := 'NA';
  self.signature := 'NA';
  self.description := description;
  //self.examples := examples;
end;


constructor THelp.Create (moduleName, name, signature, description : string; examples : TArray<string>);
begin
  self.helpType := htMethod;
  self.moduleName := moduleName;
  self.name := name;
  self.signature := signature;
  self.description := description;
  self.examples := examples;
end;


function THelp.getHelp : string;
var i : integer;
begin
  case helpType of
     htModule :
       begin
       result := 'Module: ' + moduleName + sLineBreak;
       result := result + '   ' + description;
       end;
     htMethod:
       begin
       result := 'Module: ' + moduleName + sLineBreak;
       result := result + 'Method Name: ' + name + sLineBreak;
       result := result + 'Signature: ' + signature + sLineBreak;
       result := result + '   ' + description;
       result := result + sLineBreak + 'Examples:';
       for i := 0 to length (examples) - 1 do
           result :=  result + sLineBreak + '  ' + examples[i];
       end;
  end;
end;


function THelp.clone : THelp;
begin
  result := THelp.Create(self.moduleName,self.name, self.signature, self.description, self.examples);
end;

end.
