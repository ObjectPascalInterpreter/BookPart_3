unit uConstantTable;

// Ths source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

Uses Classes, SysUtils, Generics.Collections, uStringObject;

type
   TConstantValueType = (cpDouble, cpString);

   // Lists are not stored here because they are constucted at runtime.
   TConstantValueElement = class (TObject)
       valueType : TConstantValueType;
       dValue    : double;
       sValue    : TStringObject;
       constructor Create (value : double); overload;
       constructor Create (value : string); overload;
       destructor  Destroy; override;
   end;

   TConstantValueTable = class (TObjectList<TConstantValueElement>)
         function getSize : integer;
         function clone : TConstantValueTable;
   end;


implementation

Uses uMemoryManager, uSymbolTable;

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
  case valueType of
    cpString : sValue.Free;
    cpDouble : begin end;

  else
    raise Exception.Create('Don''t know how to free :' + inttostr (integer (valuetype)) + ' in ConstantValueElement');
  end;
  inherited;
end;


function TConstantValueTable.getSize : integer;
var element : TConstantValueElement;
begin
  result := 0;
  for element in self do
      result := result + element.InstanceSize;
end;


function TConstantValueTable.clone : TConstantValueTable;
var i : integer;
begin
  result := TConstantValueTable.Create;
  for i := 0 to self.Count - 1 do
      case self[i].valueType of
           cpString :
               result.Add(TConstantValueElement.Create(self[i].sValue.value));
           cpDouble :
               result.Add(TConstantValueElement.Create(self[i].dValue));

               //result.Add(TConstantValueElement.Create(self[i].lValue.value));
      end;
end;

initialization
//  constantValueTable := TConstantValueTable.Create;
finalization
//  constantValueTable.Free;
end.

