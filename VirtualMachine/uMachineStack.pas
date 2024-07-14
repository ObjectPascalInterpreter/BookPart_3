unit uMachineStack;

// This source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

Uses Classes, SysUtils,
     uListObject,
     uStringObject,
     uArrayObject,
     uVectorObject,
     uValueObject,
     uMatrixObject,
     uSymbolTable,
     uMemoryManager,
     uDataObject,
     uRhodusTypes,
     uDataObjectMethods;

type
  TMachineStackRecord = record
     stackType : TElementType;  // 1 byte
     case TElementType of       // Max 8 bytes
       symInteger     : (iValue : integer);
       symBoolean     : (bValue : boolean);
       symDouble      : (dValue : double);
       symObjectMethod: (oValue : TMethodDetails);

      symModule      : (module : TModule);
   symSliceObject    : (sliceValue : TObject); // Currently used to pass slice objects.

     symObject       : (dataObject : TDataObject);   // Used to hold all the TDataObjects, strings, lists etc
     end;

  PMachineStackRecord = ^TMachineStackRecord;
  TMachineStack = array of TMachineStackRecord;

var memCount : integer;
    noneStackType : TMachineStackRecord;

    function stToStr (st : TElementType) : string;

implementation

function stToStr (st : TElementType) : string;
begin
  case st of
     symInteger  : result := 'integer';
     symBoolean  : result := 'boolean';
     symDouble   : result := 'double';
     symString   : result := 'string';
     symList     : result := 'list';
     symArray    : result := 'array';
     symVector   : result := 'vector';
     symMatrix   : result := 'matrix';
  symValueObject : result := 'builtin value (Value Object)';
     symModule   : result := 'module';
     symUserFunc : result := 'function';
     symSymbol   : result := 'symbol';
     symLocalSymbol : result := 'localSymbol';
     symObjectMethod : result := 'objectMethod';
  end;

end;


initialization
  noneStackType.stackType := symNonExistant;
end.

