unit uDataObject;

// This source is distributed under Apache 2.0

// All data objects, such as lists, functions, etc descend from this.

// Copyright (C)  2019-2024 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

Uses Classes, SysUtils,
     uHelpUnit,
     uRhodusTypes,
     uDataObjectMethods;

type
  TDataObject = class (TObject)

      blockType : TBlockType;
      objectType : TElementType;

      methodCount : integer;

      methods : TMethodsBase;
      help : THelpModule;

      procedure  getHelp (vm : TObject);
      function   getSize : integer; virtual; abstract;
      function   clone : TDataObject; virtual; abstract;
      //function   toString : string; virtual;   <- This already exists in the base class

      function isConstant : boolean;
      function isBound : boolean;
      function isOwned : boolean;
      function isGarbage : boolean;

      constructor Create;
      destructor  Destroy; override;
  end;


implementation

Uses uStringObject,
     uMemoryManager,
     uMachineStack,
     uVM;


constructor TDataObject.Create;
begin
  inherited;
  help := nil;
  blockType := btGarbage;
  memoryList.addNode(self);
end;


destructor TDataObject.destroy;
begin
  help.Free;
  inherited
end;


function TDataObject.isConstant : boolean;
begin
  result := (blockType = btConstant);
end;


function TDataObject.isBound : boolean;
begin
  result := (blockType = btBound);
end;


function TDataObject.isOwned : boolean;
begin
  result := (blockType = btOwned);
end;


function TDataObject.isGarbage : boolean;
begin
  result := (blockType = btGarbage);
end;


procedure TDataObject.getHelp (vm : TObject);
var m : TMethodDetails;
    obj : TDataObject;
begin
  m := TVM (vm).popMethodDetails();
  obj := TDataObject (m.self);

  if obj.help <> nil then
     begin
      TVM (vm).push(TStringObject.Create(obj.help.getHelp()));
     end
  else
     TVM (vm).push (TStringObject.Create('No help'));
end;


end.
