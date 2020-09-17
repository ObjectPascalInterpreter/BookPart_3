unit uConstantTable;

// Ths source is distributed under Apache 2.0

// Copyright (C) 2019-2020 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

Uses Generics.Collections, uStringObject;

type
   TConstantValueType = (cpDouble, cpString);

   TConstantValueElement = class (TObject)
       valueType : TConstantValueType;
       dValue    : double;
       sValue    : TStringObject;
       constructor Create (value : double); overload;
       constructor Create (value : string); overload;
       destructor Destroy; override;
   end;
   TConstantValueTable = TObjectList<TConstantValueElement>;

var
  constantValueTable : TConstantValueTable;


implementation

Uses uMemoryManager;

constructor TConstantValueElement.Create (value : double);
begin
  inherited Create;
  dValue := value;
  valueType := cpDouble;
end;


constructor TConstantValueElement.Create (value : string);
begin
  inherited Create;
  sValue := TStringObject.createConstantObj (value);
  valueType := cpString;
end;


destructor TConstantValueElement.Destroy;
begin
  sValue.Free;
  inherited;
end;


initialization
  constantValueTable := TObjectList<TConstantValueElement>.Create;
finalization
  constantValueTable.Free;
end.
