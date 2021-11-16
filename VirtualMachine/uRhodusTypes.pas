unit uRhodusTypes;


// This source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

Uses SysUtils;

const
   SLICE_ALL = -1;
   SLICE_EQUAL = -2;

   VARIABLE_ARGS = -1;

type
   // symNonExistant: Requests for symbols that aren't even in the symbol table
   TSymbolElementType = (
          symNonExistant = -1,
          symUndefined,
          symInteger,
          symDouble,
          symBoolean,
          symString,
          symList,
          symArray,
          symUserFunc,
          symModule);

   TIntArray = array of integer;
   TIndexArray = array of integer;
   TDoubleArray = array of double;

   TIntList = TArray<integer>;
   TIntLists = TArray<TIntList>;


   TBlockType = (btGarbage, btBound, btOwned, btConstant, btTemporary);

   TSliceObject = class
     lower, upper : integer;
     constructor Create (lower, upper : integer);
  end;
  TSliceObjectList = array of TSliceObject;


implementation

constructor TSliceObject.Create (lower, upper : integer);
begin
  self.lower := lower;
  self.upper := upper;
end;


end.
