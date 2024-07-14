unit uRhodusTypes;


// This source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

Uses SysUtils;

const
   SLICE_ALL   = -999999; //-1;   // Used to indicate :, ie all rows or all columns
   SLICE_EQUAL = -888888; //-2;   // Used to indicate when a slices specifies a singlerow or column, eg m[:,2]

   VARIABLE_ARGS = -1;

type
   // symNonExistant: Requests for symbols that aren't even in the symbol table
   TSymbolElementType = (
          symNonExistant = 0,
          symUndefined,
          symInteger,
          symDouble,
          symBoolean,
          symString,
          symList,
          symArray,
          symVector, // Not actually exposed to the user, used internally
          symMatrix,
          symValueObject,  // Builtin constants such as math.pi
          symObject,       // Used to to mark objects (list, string etc) on the stack
          symUserFunc,
          symModule,
          symSymbol,       // Not exposed to the user
          symLocalSymbol,  // Not exposed to the user
          symSliceObject,  // Not exposed to the user
          symObjectMethod, // Methods attached to string, lists etc, eg a.len()
          symEndObject);  // Useful for making a for loop through all elements

   TSetOfDataObjects = array of TSymbolElementType;

   TIntArray = array of integer;
   TIndexArray = array of integer;
   TDoubleArray = array of double;

   TIntList = TArray<integer>;
   TIntLists = TArray<TIntList>;


   TBlockType = (btGarbage, btBound, btOwned, btConstant, btTemporary);

   TSliceObject = class
     lower, upper : integer;
     constructor Create (lower, upper : integer);
     destructor Destroy; override;
  end;
  TSliceObjectList = array of TSliceObject;

  var
   setOfDataObjects : TSetOfDataObjects;

  function isDataObject (d : TSymbolElementType) : boolean;

implementation

constructor TSliceObject.Create (lower, upper : integer);
begin
  inherited Create;
  self.lower := lower;
  self.upper := upper;
end;


destructor TSliceObject.Destroy;
begin
  inherited;
end;

function isDataObject (d : TSymbolElementType) : boolean;
var i : integer;
begin
  for i := 0 to High (setOfDataObjects) do
      if setOfDataObjects[i] = d then
         exit (True);
  exit (False);
end;


initialization
  setLength (setOfDataObjects, 4);
  setOfDataObjects[0] := symString;
  setOfDataObjects[1] := symList;
  setOfDataObjects[2] := symMatrix;
  setOfDataObjects[3] := symArray;
end.
