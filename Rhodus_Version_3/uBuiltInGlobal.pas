unit uBuiltInGlobal;


// This source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface


Uses SysUtils, Classes, uLibModule, uSymbolTable;

function  getMainModule : TModule;
procedure computeBaseLineMemoryAllocated;
procedure addGlobalMethodsToModule (module : TModuleLib);

var mainModule : TModuleLib;
    baseLineMemoryAllocated : integer;

implementation

Uses Math,
     StrUtils,
     uVM,
     uVMExceptions,
     uStringObject,
     uListObject,
     uArrayObject,
     uMemoryManager,
     uMachineStack,
     uCompile,
     uAssembler,
     uRhodusTypes;

type
  TBuiltInGlobal = class (TModuleLib)
     private
     public
       procedure createArray (vm : TObject);
       procedure myInt (vm : TObject);
       procedure myFloat (vm : TObject);
       procedure readNumber (vm : TObject);
       procedure readString (vm : TObject);
       procedure getChar (vm : TObject);  // Convert int into character
       procedure getAsc (vm : TObject);   // Convert char in to ascii
       procedure listSymbols (vm : TObject);
       procedure myHelp (vm : TObject);
       procedure getType (vm : TObject);
       procedure getAttr (vm : TObject);
       procedure myAssertTrueEx (vm : TObject);
       procedure myAssertFalseEx (vm : TObject);
       procedure listModules (vm : TObject);
       procedure getMemoryUsed (vm : TObject);
       procedure myMain (vm : TObject);
       procedure dis (vm : TObject);
       procedure stackInfo (vm : TObject);

       constructor Create;
       destructor  Destroy; override;
  end;

  TDoubleArray = array of double;
  TArrayConstructor = class (TObject)
       elementCount : integer;
       arrayObject : TArrayObject;
       procedure determineDimensions (argument : string; var dims : TIndexArray);
       function  addLevel (alist : TListObject) : string;
       procedure getDimensions (alist : TListObject; var dims : TIndexArray);
       function  countValues (alist : TListObject; var count : integer) : integer;
  end;



var  builtInGlobal : TBuiltInGlobal;

// -------------------------------------------------------------------------------------

procedure addGlobalMethodsToModule (module : TModuleLib);
var i : integer;
begin
  module.addMethod (builtInGlobal.createArray,   -1, 'array',         'Create an array: x = array (3, 4)');
  module.addMethod (builtInGlobal.myInt,          1, 'int',           'Convert float to integer: int (3.4)');
  module.addMethod (builtInGlobal.myFloat,        1, 'float',         'Convert and integer to a float: float (3)');
  module.addMethod (builtInGlobal.readNumber,     0, 'readNumber',    'Read an integer from the console');
  module.addMethod (builtInGlobal.readString,     0, 'readString',    'Rread a string from the console');
  module.addMethod (builtInGlobal.listSymbols,    1, 'symbols',       'Returns list of symbols in the specified module: symbols(math). Use ' + TSymbol.mainModuleId + ' to get the symbols for the main module');
  module.addMethod (builtInGlobal.getType,        1, 'type',          'Returns the type of a given variable: type (x)');
  module.addMethod (builtInGlobal.getAttr,        2, 'getAttr',       'Returns the value attached to the symbol attribute: getAttr (mylib, "x")');
  module.addMethod (builtInGlobal.listModules,    0, 'modules',       'Get a list of all currently loaded mdules');
  module.addMethod (builtInGlobal.getMemoryUsed,  0, 'mem',           'Get the amount of memory currently in use.');
  module.addMethod (builtInGlobal.myAssertTrueEx, 1, 'assertTrueEx',  'Assert argument is true, return . of F');
  module.addMethod (builtInGlobal.myAssertFalseEx,1, 'assertFalseEx', 'Assert argument is false, return . of F');
  module.addMethod (builtInGlobal.myMain,         0, 'main',          'Returns a reference to the main module');
  module.addMethod (builtInGlobal.dis,            1, 'dis',           'dissassemble module or function');
  module.addMethod (builtInGlobal.stackInfo,      0, 'stackInfo',     'Get the current state of the VM stack');
  module.addMethod (builtInGlobal.getChar,        1, 'chr',           'Get the character equivalent of an integer value');
  module.addMethod (builtInGlobal.getAsc,         1, 'asc',           'Get the ascii equivalent of a single character');
end;


procedure argMustBeNumber;
begin
  raise ERuntimeException.Create('argument must be a number');
end;


function getMemoryAllocated : integer;
var st: TMemoryManagerState; sb: TSmallBlockTypeState;
    value : integer;
begin
  getMemoryManagerState(st);
  value :=  st.TotalAllocatedMediumBlockSize + st.TotalAllocatedLargeBlockSize;
  for sb in st.SmallBlockTypeStates do
      value := value + sb.UseableBlockSize * sb.AllocatedBlockCount;
  result := value;
end;


procedure computeBaseLineMemoryAllocated;
begin
  baseLineMemoryAllocated := getMemoryAllocated;
end;


function getMainModule : TModule;
begin
  result := mainModule;
end;


constructor TBuiltInGlobal.Create;
begin
  inherited Create (TSymbol.globalId, 'Global Module');

  addMethod (createArray,   -1, 'array',         'Create an array of a given size: a = array(4,5,2)');
  addMethod (myInt,          1, 'int',           'Convert float to integer: int (3.4)');
  addMethod (myFloat,        1, 'float',         'Convert an integer to a float: float (3)');
  addMethod (readNumber,     0, 'readNumber',    'Read an integer from the console');
  addMethod (readString,     0, 'readString',    'Rread a string from the console');
  addMethod (listSymbols,    1, 'symbols',       'Returns list of symbols in the specified module: symbols(math). Use ' + TSymbol.mainModuleId + ' to get the symbols for the main module');
  addMethod (getType,        1, 'type',          'Returns the type of a given variable: type (x)');
  addMethod (getAttr,        2, 'getAttr',       'Returns the value attached to the symbol attribute: getAttr (mylib, "x")');
  addMethod (listModules,    0, 'modules',       'Get a list of all currently loaded mdules');
  addMethod (getMemoryUsed,  0, 'mem',           'Get the amount of memory currently in use.');
  addMethod (myAssertTrueEx, 1, 'assertTrueEx',  'Assert argument is true, return . of F');
  addMethod (myAssertFalseEx,1, 'assertFalseEx', 'Assert argument is false, return . of F');
  addMethod (myMain,         0, 'main',          'Returns a reference to the main module');
  addMethod (dis,            1, 'dis',           'dissassemble module or function');
  addMethod (stackInfo,      0, 'stackInfo',     'Get the current state of the VM stack');
  addMethod (getChar,        1, 'chr',           'Get the character equivalent of an integer value');
  addMethod (getAsc,         1, 'asc',           'Get the ascii equivalent of a single character');
end;


destructor TBuiltInGlobal.Destroy;
begin
  inherited;
end;


function getAllocatedSymbols (module : TModule) : string;
var len : integer; f : TUserFunction;
    key, astr: string;
begin
  result := Format('%-14s%-12s%-22s%-10s', ['Name', 'Type', 'Value', 'Size']) + sLineBreak;
  // Write out all the symbols
  for key in module.symbolTable.keys do
      begin
      if module.symbolTable.Items[key] = nil then
         continue;

      if module.symbolTable.items[key].symbolType <> symUndefined then
         begin
         result := result + Format ('%-14s', [module.symbolTable.items[key].symbolName]);
         case module.symbolTable.items[key].symbolType of
              symInteger: result := result + Format ('%-12s%-22d%-6d',  ['int', module.symbolTable.items[key].iValue, sizeof (integer)]) + sLineBreak;
              symBoolean: result := result + Format ('%-12s%-22s%-6d',  ['boolean', boolToStr (module.symbolTable.items[key].bValue), sizeof (Boolean)]) + sLineBreak;
              symDouble : result := result + Format ('%-12s%-22g%-6d%', ['float', module.symbolTable.items[key].dValue, sizeof (double)]) + sLineBreak;
              symString : begin
                          astr :=  '"' + leftStr (module.symbolTable.items[key].sValue.value, 50);
                          if length (module.symbolTable.items[key].sValue.value) > 50 then
                             astr := astr + '...."'
                          else
                             astr := astr + '"';

                          result := result + Format ('%-12s', ['string']) + Format ('%-22s', [astr]);
                          len := length (module.symbolTable.items[key].sValue.value);
                          result := result + Format ('%-12d', [len]) + sLineBreak;
                          end;
              symList   : result := result + Format ('%-12s%-22s%-6d%', ['list', module.symbolTable.items[key].lValue.listToString, module.symbolTable.items[key].lValue.getsize()]) + sLineBreak;
             symUserFunc: begin
                          f := module.symbolTable.items[key].fValue;
                          result := result + Format ('%-12s%-22s%-12d', ['ufunc', 'NA', f.getSize()]) + sLineBreak;
                          end;
              symModule : begin
                          result := result + Format ('%-12s%-22s%-12d', ['module', 'NA', (module.symbolTable.Items[key].mValue as TModule).getSize]) + sLineBreak;
                          end;
              symUndefined : continue;
         else
             raise Exception.Create('This type not yet supported in symbols');
         end;
      end;
      end;
end;


procedure TBuiltInGlobal.myMain (vm : TObject);
begin
 TVM (vm).pushModule (getMainModule());
end;


procedure TBuiltInGlobal.getMemoryUsed (vm : TObject);
begin
  TVM (vm).push (memoryList.getMemoryListSize());
end;


function TArrayConstructor.countValues (alist : TListObject; var count : integer) : integer;
var i : integer;
begin
  for i := 0 to alist.list.Count - 1 do
      if alist.list[i].itemType <> liList then
         inc (count)
      else
         countValues (alist.list[i].lValue, count);
end;


procedure TArrayConstructor.determineDimensions (argument : string; var dims : TIndexArray);
var i, j : integer;
    count : double;

    depth : integer;
    dimensions : integer;
    upOrdown : integer;
    c : Char;
begin
  setLength (dims, 0);
  upOrDown := 0;
  dimensions := 0;
  depth := 0;
  for i := 0 to length (argument) - 1 do
      begin
      upOrDown := 0;
      c := argument[i+1];
      if (c = '[') then
         begin
         upOrDown := 1;  // determine if we're going down in depth
         end
      else
         begin
         if (c = ']')  then
            upOrDown := -1;
         end;

      depth := depth + upOrDown;

      if upOrDown = 1 then
         begin
         if depth > dimensions then
            begin
            dimensions := depth;
            setLength (dims, dimensions);
            end;
         dims[depth - 1] := 0;
         end;

      if not ((c = '[') or (c = ']')) then
         begin
         if c = ',' then
            begin
            if dims[depth-1] = 0 then
               dims[depth-1] := 2
            else
               dims[depth-1] := dims[depth-1] + 1;
            end
         else
            begin
            if dims[depth-1] = 0 then
               dims[depth - 1] := 1;
            end;
         end;
       end;
end;


function TArrayConstructor.addLevel (alist : TListObject) : string;
var i : integer;
begin
  result := '[';
  for i := 0 to alist.list.Count - 1 do
      if alist.list[i].itemType = liList then
         begin
         if result[length(result)] = ']' then
            result := result + ',';
         result := result + addLevel (alist.list[i].lValue);
         end
      else
         begin
         if result[length(result)] = ']' then
            result := result + ',';

         if i < alist.list.Count - 1  then
            result := result + '0' + ','
         else
            result := result + '0';
         arrayObject.data[elementCount] := alist.list[i].getScalar();
         inc (elementCount);
         end;
  result := result + ']';
end;


procedure TArrayConstructor.getDimensions (alist : TListObject; var dims : TIndexArray);
var i : integer;
    astr : string;
begin
  elementCount := 0;
  astr := addLevel (alist);

  determineDimensions (astr, dims);
  for i := 0 to length (dims) - 1 do
      write (dims[i], ' ');
  writeln;
end;


function convertListToArray (alist : TListObject) :TArrayObject;
var i : integer;
    count : integer;
    dims : TIndexArray;
    ac : TArrayConstructor;
begin
   ac := TArrayConstructor.Create;
   try
     count := 0;
     // Get number of elements
     ac.countValues (alist, count);
     ac.arrayObject := TArrayObject.Create();
     setLength (ac.arrayObject.data, count);

     // Get the dimensions of the array
     ac.getDimensions(alist, dims);
     ac.arrayObject.dim := copy (dims);
     result := ac.arrayObject;
   finally
     ac.Free;
   end;
end;


procedure TBuiltInGlobal.createArray (vm : TObject);
var nArgs, i : integer;
    ar : TArrayObject;
    dim : TIndexArray;
    alist : TListObject;
    argument : PMachineStackRecord;
begin
  nArgs := TVM (vm).popInteger;
  argument := TVM (vm).pop;
  if argument.stackType = stList then
     begin
     alist := argument.lValue;
     ar := convertListToArray (alist);
     TVM (vm).push(ar);
     end
  else
     begin
     setLength (dim, nArgs);
     dim[0] := argument.iValue;
     for i := nArgs - 1 downto 1 do
         begin
         argument := TVM (vm).pop;
         dim[i] := argument.iValue;
         end;

     ar := TArrayObject.Create (dim);
     TVM (vm).push(ar);
     end;
end;


procedure TBuiltInGlobal.listSymbols (vm : TObject);
var module : TModule;
begin
  module := TVM (vm).popModule;
  TVM (vm).push (TStringObject.create (getAllocatedSymbols (module)));
end;


function getModuleHelp (m : TModule) : string;
var f : TUserFunction;
    key : string;
begin
  result := 'Module: ' + m.name + ', ' + m.helpStr + sLineBreak;
  result := result + Format('%-12s%-12s%-11s%', ['Type', 'Name', 'Help']) + sLineBreak;
  for key in m.symbolTable.keys do
      begin
      case m.symbolTable.Items[key].symbolType of
         symUserFunc :
             begin
             f := m.symbolTable.Items[key].fValue;
             result := result + Format('%-12s', ['Function:']) +
                        Format('%-12s', [f.name])  +  f.helpStr + sLineBreak;
             end
      else
         result := result + Format('%-12s', ['Variable:']) +
                Format ('%-12s', [m.symbolTable.Items[key].symbolName])  + m.symbolTable.Items[key].helpStr + sLineBreak;
      end;
      end;
end;


procedure TBuiltInGlobal.myHelp (vm : TObject);
var x : PMachineStackRecord;
begin
  x := TVM (vm).pop;
  case x.stackType of
    stInteger  : TVM (vm).push (TStringObject.create ('Integer Value'));
    stBoolean  : TVM (vm).push (TStringObject.create ('Boolean Value'));
    stDouble   : TVM (vm).push (TStringObject.create ('Double value: ' + floattostr (x.dValue)));
    stString   : TVM (vm).push (TStringObject.create ('String Value'));
    stList     : TVM (vm).push (TStringObject.create ('List Value'));
    stModule   : TVM (vm).push (TStringObject.create (getModuleHelp (x.module)));
    stFunction : TVM (vm).push (TStringObject.create ('User Function: ' + x.fvalue.helpStr));
  else
    TVM (vm).push (TStringObject.create ('Undefined Value'));
  end;
end;


procedure TBuiltInGlobal.readString (vm : TObject);
var s : string;
    sObj : TStringObject;
begin
  readln(s);
  sobj := TStringObject.create (s);
  TVM (vm).push (sObj);
end;


procedure TBuiltInGlobal.readNumber (vm : TObject);
var s : string;
    iValue : integer;
    dValue : double;
begin
  readln(s);
  while (not TryStrToInt(s, iValue)) and (not TryStrToFloat(s, dValue)) do
      begin
      writeln ('Number error: ' + s + ' is not a number, try again');
      readln (s);
      end;
  if TryStrToInt(s, iValue) then
     TVM (vm).push (iValue)
  else
    TVM (vm).push (dValue);
end;


procedure TBuiltInGlobal.myInt (vm : TObject);
var x : PMachineStackRecord; tmp : int32;
begin
  x := TVM (vm).pop;
  case x.stackType of
       stInteger : tVM (vm).push (x.iValue);
       stDouble  : begin
                   // Do it this way inorder to get a range check error
                   // if dValue can't be case to a 32-bit integer
                   tmp := trunc (x.dValue);
                   TVM (vm).push (int32 (tmp));
                   end;
  else
     argMustBeNumber;
  end;
end;


procedure TBuiltInGlobal.myFloat (vm : TObject);
var x : PMachineStackRecord; tmp : int32;
begin
  x := TVM (vm).pop;
  case x.stackType of
       stInteger : TVM (vm).push (double (x.iValue));
       stDouble  : TVM (vm).push (x.dValue);

  else
     argMustBeNumber;
  end;
end;


procedure TBuiltInGlobal.dis (vm : TObject);
var x : PMachineStackRecord;
begin
  x := TVM (vm).pop;
  case x.stackType of
    stModule : TVM (vm).push (TStringObject.create (dissassemble (x.module, x.module.code)));
    stFunction :
      begin
      if x.fValue.isbuiltInFunction then
         TVM (vm).push (TStringObject.create (x.fValue.name + ' is a builtin function'))
      else
         TVM (vm).push (TStringObject.create (dissassemble (x.fValue.moduleRef, x.fValue.funccode)));
      end;
  else
     raise ERuntimeException.Create('dis argument can only be a module or a function');
  end;
end;


procedure TBuiltInGlobal.listModules (vm : TObject);
var ls : TListObject;
    i : integer;
    s : TStringObject;
    m : TModule;
    key : string;
begin
  ls := TListObject.Create (0);

  m := getMainModule();
  for Key in m.symbolTable.Keys do
      begin
      if m.symbolTable.Items[key].symbolType = symModule then
         begin
         s := TStringObject.create (key);
         ls.append (s);
         end;
      end;
  TVM (vm).push(ls);
end;


procedure TBuiltInGlobal.myAssertTrueEx (vm : TObject);
var st : PMachineStackRecord;
begin
  st := TVM (vm).pop;
  if st.stackType = stBoolean then
     begin
     if st.bValue = True then
        TVM (vm).push(TStringObject.Create('.'))
    else
        TVM (vm).push(TStringObject.Create('F'));
     end
  else
   raise ERuntimeException.Create('Argument to assertErrorTrueEx must be a boolean');
end;


procedure TBuiltInGlobal.myAssertFalseEx (vm : TObject);
var st : PMachineStackRecord;
begin
  st := TVM (vm).pop;
  if st.stackType = stBoolean then
     begin
     if st.bValue = False then
        TVM (vm).push(TStringObject.Create('.'))
    else
        TVM (vm).push(TStringObject.Create('F'));
     end
  else
   raise ERuntimeException.Create('Argument to assertErrorTrueEx must be a boolean');
end;


procedure TBuiltInGlobal.getType (vm : TObject);
var x : PMachineStackRecord;
begin
  x := TVM (vm).pop;
  case x.stackType of
    stInteger : TVM (vm).push (TStringObject.Create ('int'));
    stDouble : TVM (vm).push (TStringObject.Create ('float'));
    stBoolean : TVM (vm).push (TStringObject.Create ('bool'));
    stString : TVM (vm).push (TStringObject.Create ('string'));
    stList : TVM (vm).push (TStringObject.Create ('list'));
    stFunction : TVM (vm).push (TStringObject.Create ('function'));
    stModule : TVM (vm).push (TStringObject.Create ('module'));
    stNone :  TVM (vm).push (TStringObject.Create ('none'));
  else
    TVM (vm).push (TStringObject.Create ('not sure'));
  end;
end;


procedure TBuiltInGlobal.getAttr (vm : TObject);
var strObj : TStringObject;
    attribute : string;
    module : TModule;
    symbol: TSymbol;
begin
  strObj := TVM (vm).popString;
  attribute := strObj.value;
  module := TVM (vm).popModule;
  if module.symbolTable.find (attribute, symbol) then
     begin
     case symbol.symbolType of
       symInteger : TVM (vm).push (symbol.iValue);
       symDouble : TVM (vm).push (symbol.dValue);
       symBoolean : TVM (vm).push (symbol.bValue);
       symString : TVM (vm).push (symbol.sValue);
       symList : TVM (vm).push (symbol.lValue);
       symUserFunc : TVM (vm).push (symbol.fValue);
       symModule : TVM (vm).pushModule (symbol.mValue);
       symUndefined :  TVM (vm).push (TStringObject.create ('undefined'));
     end;
     end
  else
     raise ERuntimeException.Create('Attribute not found in module');
end;


// Convert int into character
procedure TBuiltInGlobal.getChar (vm : TObject);
var x : integer;
begin
  x := TVM (vm).popInteger();
  TVM (vm).push (TStringObject.create(Chr (x)));
end;


 // Convert char in to ascii
procedure TBuiltInGlobal.getAsc (vm : TObject);
var x: string;
begin
  x := TVM (vm).popString().value;
  if (length (x) = 0) or (length (x) > 1) then
     raise ERuntimeException.Create('Character to asc function should be a single character string');
  TVM (vm).push (Ord (x[1]));
end;


procedure TBuiltInGlobal.stackInfo (vm : TObject);
var vm1 : TVM;
begin
  vm1 := TVM (vm);
  vm1.push(vm1.getStackInfo().stacktop);
end;



initialization
  builtInGlobal := TBuiltInGlobal.Create;
finalization
  builtInGlobal.Free;
end.
