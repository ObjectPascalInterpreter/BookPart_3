unit uRhodusObject;

// This source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

// Base class for all data objects like string objects, lists objects etc.


interface

Uses Classes,
     SysUtils,
     uRhodusTypes,
     uHelpUnit,
     uObjectSupport;

type
   TRhodusObject = class (TObject)
      blockType : TBlockType;
      objectType : TSymbolElementType;
      methods : TMethodsBase;
      help : THelp;

      function isConstant : boolean;
      function isBound : boolean;
      function isOwned : boolean;
      function isGarbage : boolean;

      function    getRhodusObjectSize : integer;
      procedure   getHelp (vm : TObject);
      constructor Create;
      destructor  Destroy; override;
   end;

implementation

Uses uMemoryManager,
     uStringObject,
     uVM;

constructor TRhodusObject.Create;
begin
  inherited;
  help := nil;
  blockType := btGarbage;
  memoryList.addNode(self);
end;

destructor TRhodusObject.destroy;
begin
  help.Free;
  inherited
end;

function TRhodusObject.getRhodusObjectSize : integer;
begin
  result := sizeof (blockType);
end;


procedure TRhodusObject.getHelp (vm : TObject);
var m : TMethodDetails;
    obj : TRhodusObject;
begin
  m := TVM (vm).popMethodDetails();
  obj := TRhodusObject (m.self);

  if obj.help <> nil then
     begin
      TVM (vm).push(TStringObject.Create(obj.help.getHelp()));
     end
  else
     TVM (vm).push (TStringObject.Create('No help'));
end;



function TRhodusObject.isConstant : boolean;
begin
  result := (blockType = btConstant);
end;


function TRhodusObject.isBound : boolean;
begin
  result := (blockType = btBound);
end;


function TRhodusObject.isOwned : boolean;
begin
  result := (blockType = btOwned);
end;


function TRhodusObject.isGarbage : boolean;
begin
  result := (blockType = btGarbage);
end;


end.
