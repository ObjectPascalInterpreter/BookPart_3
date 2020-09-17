unit uBuiltInList;

interface

Uses SysUtils, Classes, uModule, uBuiltInModule;

type
  TBuiltInList = class (TBuiltInModule)

     procedure   getLength (vm : TObject);
     constructor Create;
  end;


implementation

Uses Math, uSymbolTable, uVM, uStringObject, uListObject, uMemoryManager;


constructor TBuiltInList.Create;
begin
  inherited Create ('list', 'List Module');

  addMethod(getLength,   1, 'len', 'Return the length of a list');
end;


procedure TBuiltInList.getLength (vm : TObject);
var s : TListObject;
begin
   s := TVM (vm).popList;
   TVM (vm).push(s.list.Count);
end;


end.

