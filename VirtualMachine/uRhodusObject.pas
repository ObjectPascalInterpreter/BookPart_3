unit uRhodusObject;

// This source is distributed under Apache 2.0

// Copyright (C) 2019-2024 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

// Base class for all data objects that need ememory management like strings lists, functions, etc.


interface

Uses Classes,
     SysUtils,
     uRhodusTypes,
     uHelpUnit;

type
   TRhodusObject = class (TObject)
      constructor Create;
      destructor  Destroy; override;
   end;

implementation

Uses uMemoryManager,
     uStringObject,
     uDataObjectMethods,
     uVM;

constructor TRhodusObject.Create;
begin
  inherited;
end;

destructor TRhodusObject.destroy;
begin
  inherited
end;


end.
