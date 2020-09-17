unit uModule;

// Ths source is distributed under Apache 2.0

// Copyright (C)  2019-2020 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com


// A module defines a collection of zero or more user defined functions and code.
// eg
// function add (a, b) return a+b end;
// println (add (4,5));

interface

Uses Classes, SysUtils, Generics.Collections, uSymbolTable;

type
   TModule = class (TObject)
       name : string;
       code : TProgram;
       symbolTable : TSymbolTable;
       helpStr : string;

       procedure   clearCode;
       constructor Create (name : string);
       destructor  Destroy; override;
   end;

   TModuleList = class (TObjectList<TModule>)
        function find (name : string; var index : integer) : boolean;
   end;

   var moduleList : TModuleList;

implementation


function TModuleList.find (name : string; var index : integer) : boolean;
var i : integer;
begin
  result := False;
  for i := 0 to moduleList.Count - 1 do
      begin
      if moduleList[i].name = name then
         begin
         index := i;
         result := True;
         exit
         end;
      end;
end;


// -----------------------------------------------------------------------------------

constructor TModule.Create (name : string);
begin
  self.name := name;
  code := TProgram.Create;
  symbolTable := TSymbolTable.Create;
end;


destructor  TModule.Destroy;
begin
  code.Free;
  symbolTable.Free;
  inherited;
end;


procedure TModule.clearCode;
begin
  code.Clear;
end;


initialization
  moduleList := TModuleList.Create;
finalization
  moduleList.Free;
end.
