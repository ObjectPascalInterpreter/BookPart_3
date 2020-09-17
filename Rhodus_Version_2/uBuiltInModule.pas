unit uBuiltInModule;

interface

Uses SysUtils, Classes, uSymbolTable;

type
  TBuiltInModule = class (TObject)

    moduleIndex : integer;

    procedure   addMethod(methodPtr : TxBuiltInFunction; nArgs : integer; name, helpStr : string);
    procedure   addValue (name : string; value : double; helpStr : string);

    procedure   getCat (vmObj : TObject);
    constructor Create (name : string; helpStr : string);
  end;


implementation

Uses uVM, uStringObject, uListObject, uMemoryManager, uModule;


constructor TBuiltInModule.Create (name : string; helpStr : string);
var f : TUserFunction;
begin
  moduleIndex := moduleList.Add (TModule.create (name));
  moduleList[moduleIndex].helpStr := helpStr;

  f := TUserFunction.Create('dir', 0, getcat);
  f.helpStr := 'Get a list of the supprted methods and values';
  moduleList[moduleIndex].symbolTable.addSymbol (f, True); // // locked = True
end;


procedure TBuiltInModule.getCat (vmObj : TObject);
var i : integer;
    cat : string;
    l : TListObject;
    s : TStringObject;
    vm : TVM;
begin
  vm := TVM (vmObj);
  l := TListObject.create (0);
  for i := 0 to moduleList[moduleIndex].symbolTable.Count - 1 do
      begin
      s := TStringObject.create(moduleList[moduleIndex].symbolTable[i].symbolName);
      s.blockType := btOwned;
      l.append(s);
      end;
  vm.push (l);
end;


procedure TBuiltInModule.addValue (name : string; value : double; helpStr : string);
begin
  moduleList[moduleIndex].symbolTable.addSymbol(name, value, True, helpStr);
end;


procedure TBuiltInModule.addMethod (methodPtr : TxBuiltInFunction; nArgs : integer; name, helpStr : string);
var f : TUserFunction; index : integer;
begin
  f := TUserFunction.Create(name, nArgs, methodPtr);
  f.helpStr := helpStr;
  index := moduleList[moduleIndex].symbolTable.addSymbol (f, True);  // locked = True
end;

end.
