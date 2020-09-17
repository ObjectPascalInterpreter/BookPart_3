unit uBuiltInStr;

interface

Uses SysUtils, Classes, uModule, uBuiltInModule;

type
  TBuiltInStr = class (TBuiltInModule)

     procedure   getLength (vm : TObject);
     constructor Create;
  end;


implementation

Uses Math, uSymbolTable, uVM, uStringObject, uListObject, uMemoryManager;


constructor TBuiltInStr.Create;
begin
  inherited Create ('str', 'String Module');

  addMethod(getLength,   1, 'len', 'Return the length of a string');
end;


procedure TBuiltInStr.getLength (vm : TObject);
var s : TStringObject;
begin
   s := TVM (vm).popString;
   TVM (vm).push(length (s.value));
end;


end.
