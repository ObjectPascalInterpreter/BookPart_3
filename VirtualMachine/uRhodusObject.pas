unit uRhodusObject;

// This source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

// Base class for all data objects like string objects, lists objects etc.


interface

Uses Classes, SysUtils, uRhodusTypes, uObjectSupport;

type
   TRhodusObject = class (TObject)
      blockType : TBlockType;
      objectType : TSymbolElementType;
      methods : TMethodsBase;

      function isConstant : boolean;
      function isBound : boolean;
      function isOwned : boolean;
      function isGarbage : boolean;

      function    getRhodusObjectSize : integer;
      constructor Create;
      destructor  Destroy; override;
   end;

implementation

Uses uMemoryManager;

constructor TRhodusObject.Create;
begin
  inherited;
  blockType := btGarbage;
  memoryList.addNode(self);
end;

destructor TRhodusObject.destroy;
begin
  inherited
end;

function TRhodusObject.getRhodusObjectSize : integer;
begin
  result := sizeof (blockType);
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
