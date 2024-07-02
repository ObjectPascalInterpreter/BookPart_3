unit uValueObject;

// This source is distributed under Apache 2.0

// This file implement a object type that is used
// to represent built in values such as math.pi

// Copyright (C) 2019-2024 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

uses Classes, SysUtils, uMemoryManager, uObjectSupport, uRhodusObject;

type
  TValueObjectType = (vtInteger, vtDouble);

  TValueObject = class (TRhodusObject)
      valueType : TValueObjectType;
      iValue : integer;
      dValue : double;
      helpStr : String;

      class function getValue(value : TValueObject) : double;

      function  isEqualTo (value : double) : boolean;
      function  valueToString : string;

      function  clone : TValueObject;

      constructor     Create; overload;
      constructor     Create (value : double); overload;
      destructor      Destroy; override;
  end;

  TValueMethods = class (TMethodsBase)
     procedure   getHelp (vm : TObject);
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
  methodList := TMethodList.Create;

  methodList.Add(TMethodDetails.Create ('help',   0, 'Returns the help associated with this symbol', getHelp));
  methodList.Add(TMethodDetails.Create ('dir',    0, 'dir of string object methods', dir));
end;


destructor TValueMethods.Destroy;
begin
  inherited;
end;


procedure TValueMethods.getHelp (vm : TObject);
var vo : TValueObject;
    m : TMethodDetails;
begin
  // No arguments for this method
  m := TVM (vm).popMethodDetails();
  vo := TValueObject (m.self);

  TVM (vm).push (TStringObject.Create(vo.helpStr));
end;


// -------------------------------------------------------------------------------

constructor TValueObject.Create;
begin
  inherited;
  // Dont let the garbage collector claim these
  self.blockType := btConstant;
  memoryList.addNode (self);
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


class function TValueObject.getValue(value : TValueObject) : double;
begin
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


initialization
  valueMethods := TValueMethods.Create;
finalization
  valueMethods.Free;
end.
