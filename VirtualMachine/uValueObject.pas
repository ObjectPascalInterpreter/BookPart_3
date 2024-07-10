unit uValueObject;

// This source is distributed under Apache 2.0

// This file implement a object type that is used
// to represent built in values such as math.pi

// Copyright (C) 2019-2024 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

uses Classes, SysUtils, uMemoryManager,
     uDataObjectMethods,
     uDataObject;

type
  TValueObjectType = (vtInteger, vtDouble);

  TValueObject = class (TDataObject)
      valueType : TValueObjectType;
      iValue : integer;
      dValue : double;

      class function getValue(value : TValueObject) : double;

      function  isEqualTo (value : double) : boolean;
      function  valueToString : string;

      function  clone : TValueObject;
      function  getScalar : double;
      function  getSize : integer; override;

      constructor     Create; overload;
      constructor     Create (value : double); overload;
      destructor      Destroy; override;
  end;

  TValueMethods = class (TMethodsBase)
     constructor Create;
     destructor  Destroy; override;
  end;


implementation

Uses Math, uBuiltInGlobal, uVM,
     uVMExceptions,
     uStringObject,
     uRhodusEngine,
     uRhodusTypes,
     uSymbolTable;

var valueMethods : TValueMethods;


constructor TValueMethods.Create;
begin
  methodList := TMethodList.Create (self);
end;


destructor TValueMethods.Destroy;
begin
  inherited;
end;

// -------------------------------------------------------------------------------

constructor TValueObject.Create;
begin
  inherited Create;  // Adds object to the memory pool
  // Don't let the garbage collector claim these
  self.blockType := btConstant;
  self.objectType := symValueObject;
  methods := valueMethods;
end;


constructor TValueObject.Create (value : double);
begin
  Create;

  valueType := vtDouble;
  dValue := value;
end;


destructor TValueObject.Destroy;
begin
  inherited;
end;


function TValueObject.clone : TValueObject;
begin
  result := TValueObject.Create;
  result.valueType := self.valueType;
  result.iValue := self.iValue;
  result.dValue := self.dValue;
end;


function TValueObject.getScalar : double;
begin
  result := 0;
  case valueType of
    vtInteger : result := iValue;
    vtDouble : result := dValue;
  else
   raiseInternalError('Internal error in getScalar (uValueObject), missing type: ' + inttostr (integer (valueType)));
  end;
end;


class function TValueObject.getValue(value : TValueObject) : double;
begin
  result := 0;
  case value.valueType of
    vtInteger : result := value.iValue;
    vtDouble : result := value.dValue;
  else
   raiseInternalError('Internal error in getValue (uValueObject), missing type: ' + inttostr (integer (value.valueType)));
  end;
end;


function TValueObject.isEqualTo (value : double) : boolean;
var epsSymbol : TSymbol;
begin
  epsSymbol := mainModule.find('math', 'eps');

  if sameValue (dValue, value, epsSymbol.dValue)then
     result := True
  else result := False;
end;


function TValueObject.valueToString : string;
begin
  case valueType of
     vtInteger : result := inttostr (iValue);
     vtDouble : result := floattostr (dValue);
  else
   raiseInternalError('Internal error in valueToString (uValueObject), missing type');
  end;
end;


function TValueObject.getSize : integer;
begin
  result := 0;
  case valueType of
     vtInteger : result := sizeof (iValue);
     vtDouble : result := sizeof (dValue);
  else
   raiseInternalError('Internal error in getSize (uValueObject), unknown type');
  end;
end;


initialization
  valueMethods := TValueMethods.Create;
finalization
  valueMethods.Free;
end.
