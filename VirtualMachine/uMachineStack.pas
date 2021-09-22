unit uMachineStack;

// This source is distributed under Apache 2.0

// Copyright (C) 2019-2020 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

Uses Classes, SysUtils, uListObject, uStringObject, uSymbolTable;

type
  TStackType = (stNone, stInteger, stDouble, stBoolean, stString, stSymbol, stLocalSymbol, stList, stModule, stFunction, stObject);
  TMachineStackRecord = record
     stackType : TStackType;  // 1 byte
     module : TModule;
     symbol : TSymbol;
     symbolIndex : integer;
     case TStackType of       // Max 8 bytes
       stInteger  : (iValue : integer);
       stBoolean  : (bValue : boolean);
       stDouble   : (dValue : double);
       stString   : (sValue : TStringObject);
       stList     : (lValue : TListObject);
       stFunction : (fValue : TUserFunction);
     end;

  PMachineStackRecord = ^TMachineStackRecord;
  TMachineStack = array of TMachineStackRecord;

var memCount : integer;
    noneStackType : TMachineStackRecord;

    function stToStr (st : TStackType) : string;

implementation

function stToStr (st : TStackType) : string;
begin
  case st of
     stInteger  : result := 'integer';
     stBoolean  : result := 'boolean';
     stDouble   : result := 'double';
     stString   : result := 'string';
     stList     : result := 'list';
     stModule   : result := 'module';
     stFunction : result := 'function';
     stSymbol   : result := 'symbol';
     stLocalSymbol : result := 'localSymbol';
  end;

end;


initialization
  noneStackType.stackType := stNone;
end.

