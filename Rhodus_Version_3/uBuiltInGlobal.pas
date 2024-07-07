unit uBuiltInGlobal;

{
  Unit:    uBuiltInGlobal.pas
  Author:  Herbert M sauro
  Date:    10/2021
  Purpose: This file implements the global methods that are avaialable
           at all times in the Rhodus interpreter.

  Ths source is distributed under Apache 2.0
  See https://www.apache.org/licenses/LICENSE-2.0.txt for further information

  Copyright (C)  2019-2024 Herbert M Sauro

  Author Contact Information:
  email: hsauro@gmail.com
}

interface


Uses SysUtils, Classes, uLibModule, uSymbolTable, uListObject, uArrayObject;

function  getMainModule : TModule;
procedure computeBaseLineMemoryAllocated;
procedure addGlobalMethodsToModule (module : TModuleLib);

var mainModule : TModuleLib;
    baseLineMemoryAllocated : UInt64;

    procedure createGlobalBuiltIns;
    function convertListToArray (alist : TListObject) : TArrayObject;

implementation

Uses Math,
     StrUtils,
     uVM,
     uVMExceptions,
     uStringObject,
     uValueObject,
     uMemoryManager,
     uMachineStack,
     uCompile,
     uHelpUnit,
     uAssembler,
     uRhodusTypes;

type
  TBuiltInGlobal = class (TModuleLib)
     private
        //readCallback : TVMReadStringCallBack;
     public
       class procedure createArray (vm : TObject);
       class procedure createDictionary (vm : TObject);
       class procedure myInt (vm : TObject);
       class procedure myIntToHex (vm : TObject);
       class procedure myFloat (vm : TObject);
       class procedure readNumber (vm : TObject);
       class procedure readString (vm : TObject);
       class procedure getChar (vm : TObject);  // Convert int into character
       class procedure getAsc (vm : TObject);   // Convert char in to ascii
       class procedure listSymbols (vm : TObject);
       class procedure getType (vm : TObject);
       class procedure getAttr (vm : TObject);
       class procedure myAssertTrueEx (vm : TObject);
       class procedure myAssertFalseEx (vm : TObject);
       class procedure listModules (vm : TObject);
       class procedure getMemoryUsed (vm : TObject);
       class procedure myMain (vm : TObject);
       class procedure dis (vm : TObject);
       class procedure stackInfo (vm : TObject);
       class procedure getHelp (_vm : TObject);
       class procedure startDebug (vm : TObject);
       class procedure test (vm : TObject);

       //constructor Create;
       //destructor  Destroy; override;
  end;

var  builtInGlobal : TBuiltInGlobal;
     debugCallback : TUserFunction;

procedure debugProc (vm : TObject);
var oldDebug : boolean;
begin
  oldDebug := TVM (vm).getDebuggerFlag;
  TVM (vm).setDebuggerFlag (False);
  TVM (vm).push(debugCallback);
  TVM (vm).callUserFunction(0);
  TVM (vm).setDebuggerFlag (oldDebug);
end;

// -------------------------------------------------------------------------------------

procedure addGlobalMethodsToModule (module : TModuleLib);
begin
  module.addMethod (TBuiltInGlobal.createArray, VARIABLE_ARGS, 'array',      'Create an array from a list or dimensions: eg a = array ([1,2,3]) or a = array(4,5,2)');
  module.addMethod (TBuiltInGlobal.createDictionary, VARIABLE_ARGS, 'dict',  'Create a dictionary: x = dict (["Red", 67], ["Green", 87], ["Blue", 34])');
  module.addMethod (TBuiltInGlobal.myInt,          1, 'int',           'Convert float to integer: int (3.4XXX)');
  module.addMethod (TBuiltInGlobal.myIntToHex,     1, 'hex',           'Convert integer to hex string: hex (56)');
  module.addMethod (TBuiltInGlobal.myFloat,        1, 'float',         'Convert and integer to a float: float (3)');
  module.addMethod (TBuiltInGlobal.readNumber,    VARIABLE_ARGS, 'readNumber',  'Read an integer from the input channel: : str = readNumber ("Enter answer: ")');
  module.addMethod (TBuiltInGlobal.readString,    VARIABLE_ARGS, 'readString',  'Read a string from the input channel: str = readString ("Enter name")');
  module.addMethod (TBuiltInGlobal.listSymbols,    1, 'symbols',       'Returns list of symbols in the specified module: symbols(math). Use main() as the argument to get the symbols for the main module');
  module.addMethod (TBuiltInGlobal.getType,        1, 'type',          'Returns the type of a given variable: type (x)');
  module.addMethod (TBuiltInGlobal.getAttr,        2, 'getAttr',       'Returns the value attached to the symbol attribute: getAttr (mylib, "x")');
  module.addMethod (TBuiltInGlobal.listModules,    0, 'modules',       'Get a list of all currently loaded mdules');
  module.addMethod (TBuiltInGlobal.getMemoryUsed,  0, 'mem',           'Get the amount of memory currently in use.');
  module.addMethod (TBuiltInGlobal.myAssertTrueEx, 1, 'assertTrueEx',  'Assert argument is true, return . of F');
  module.addMethod (TBuiltInGlobal.myAssertFalseEx,1, 'assertFalseEx', 'Assert argument is false, return . of F');
  module.addMethod (TBuiltInGlobal.myMain,         0, 'getMain',       'Returns a reference to the main module');
  module.addMethod (TBuiltInGlobal.dis,            1, 'dis',           'dissassemble module or function');
  module.addMethod (TBuiltInGlobal.stackInfo,      0, 'stackInfo',     'Get the current state of the VM stack');
  module.addMethod (TBuiltInGlobal.getChar,        1, 'chr',           'Get the character equivalent of an integer value');
  module.addMethod (TBuiltInGlobal.getAsc,         1, 'asc',           'Get the ascii equivalent of a single character');
  //module.addMethod (builtInGlobal.getHelp,        1, 'help',          'Get the help associated with the object');
  module.addMethod (TBuiltInGlobal.startDebug,     1, 'debug',         'Attached method to the debugger: debug (fcn)');
  module.addMethod (TBuiltInGlobal.test,           2, 'test',          'Attached method to the debugger: debug (fcn)');

end;


procedure argMustBeNumber;
begin
  raise ERuntimeException.Create('argument must be a number');
end;


function getMemoryAllocated : integer;
var st: TMemoryManagerState; sb: TSmallBlockTypeState;
    value : UInt64;
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


//constructor TBuiltInGlobal.Create;
//begin
  //inherited Create (TSymbol.globalId, THelp.Create ('Global Module'));

//  // -1  means variable arguments, call function pushes the actual number of arguments provided
//  addMethod (createArray,     -1, 'array',       'Create an array from a list or dimensions: eg a = array ([1,2,3]) or a = array(4,5,2)');
//  addMethod (createDictionary,-1, 'dict',        'Create a dictionary: a = dict (["Red", 67], ["Green", 87], ["Blue", 34])');
//  //addMethod (myInt,          1, 'int',         'Convert float to integer: int (3.4)');
//  addMethod (myIntTohex,       1, 'hex',           'Convert integer to hex string: hex(56)');
//  addMethod (myFloat,          1, 'float',         'Convert an integer to a float: float (3)');
//  addMethod (readNumber,     0, 'readNumber',    'Read an integer from the console');
//  addMethod (readString,     0, 'readString',    'Read a string from the console');
//  addMethod (listSymbols,    1, 'symbols',       'Returns list of symbols in the specified module: symbols(math). Use ' + TSymbol.mainModuleId + ' to get the symbols for the main module');
//  addMethod (getType,        1, 'type',          'Returns the type of a given variable: type (x)');
//  addMethod (getAttr,        2, 'getAttr',       'Returns the value attached to the symbol attribute: getAttr (mylib, "x")');
//  addMethod (listModules,    0, 'modules',       'Get a list of all currently loaded mdules');
//  addMethod (getMemoryUsed,  0, 'mem',           'Get the amount of memory currently in use.');
//  addMethod (myAssertTrueEx, 1, 'assertTrueEx',  'Assert argument is true, return . of F');
//  addMethod (myAssertFalseEx,1, 'assertFalseEx', 'Assert argument is false, return . of F');
//  addMethod (myMain,         0, 'main',          'Returns a reference to the main module');
//  addMethod (dis,            1, 'dis',           'dissassemble module or function');
//  addMethod (stackInfo,      0, 'stackInfo',     'Get the current state of the VM stack');
//  addMethod (getChar,        1, 'chr',           'Get the character equivalent of an integer value');
//  addMethod (getAsc,         1, 'asc',           'Get the ascii equivalent of a single character');
//end;
//
//
//destructor TBuiltInGlobal.Destroy;
//begin
//  inherited;
//end;


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


class procedure TBuiltInGlobal.myMain (vm : TObject);
begin
 TVM (vm).push (getMainModule());
end;


class procedure TBuiltInGlobal.getMemoryUsed (vm : TObject);
begin
  TVM (vm).push (memoryList.getMemoryListSize());
end;


function countElements (alist : TListObject) : integer;
begin
  result := 0;

  for var i := 0 to alist.list.Count - 1 do
      if alist.list[i].itemType <> liList then
         begin
          inc (result);
         end
      else
         result := result + countElements (alist.list[i].lValue);
end;


function getDimensions (alist : TListObject; count : integer) : TIndexArray;
var firstdim : integer;
    n, dimIndex : integer;
    aitem : TListItem;
begin
  firstdim := alist.list.Count;
  if firstdim = 0 then
     begin
     setLength (result, 0);
     exit;
     end;

  if alist.list[0].itemType <> lilist  then
     begin
     setLength (result, 1);
     result[0] := firstdim;
     exit;
     end;

  dimIndex := 1;
  setLength (result, dimIndex);
  result[dimIndex-1] := firstdim;

  aitem := alist.list[0];
  while aitem.itemType = lilist do
      begin
      n := aitem.lValue.list.Count;
      dimIndex := dimIndex + 1;
      setLength (result, dimIndex);
      result[dimIndex-1] := n;
      aitem := aitem.lValue.list[0];
      end;
end;


var elementCount : integer;
procedure collectData (alist : TListObject; arrayObj : TArrayObject);
var i : integer;
begin
  for i := 0 to alist.list.Count - 1 do
      if alist.list[i].itemType = liList then
         begin
         collectData (alist.list[i].lValue, arrayObj);
         end
      else
         begin
         arrayObj.dataf[elementCount] := alist.list[i].getScalar();
         inc (elementCount);
         end;
end;


// The strategy is:
//   Count the number of elements to workout how much memory we need
//   Allocate memory to the data component of an array
//   Collect the data from the list and store in teh data component
//   Get the dimensions of the array
//   Work out the dimensions from the list
//   Check if the list if rectangular, if not error
//   Return the array object
function convertListToArray (alist : TListObject) :TArrayObject;
var total, count : integer;
    dims : TIndexArray;
    arrayObject : TArrayObject;
    i : integer;
begin
   try
     // Note: if there is a problem we don't have to free
     // arrayObject, because it's managed by the garbage collector.
     arrayObject := TArrayObject.Create();
     // Count the number of elements in the array
     count := countElements (alist);

     // reserve some space for the data base on count
     setLength (arrayObject.dataf, count);

     // Collect the data and insert into arrayObject
     // elementCount is a global variable since
     // collectData is recusrive and elementCount
     // is used to track the data array as it
     // traverses the list
     elementCount := 0;
     collectData (alist, arrayObject);

     // From the structure of the list, detemrine the dimensions of the array
     dims := getDimensions (alist, count);

     // Check that the array is rectangular by comparing the
     // size predicted from dims with number of actual elements found
     total := 1;
     for i := 0 to length (dims) - 1 do
         total := total*dims[i];
     if total <> count then
        raise ERuntimeException.Create('Array must be rectanglar');

     arrayObject.dim := copy (dims);
     result := arrayObject;
   finally
   end;
end;


// Can be used to create an array from a list or a sequence of dimensions,
// eg a = array ([1,2,3,4]), or a = array(2,3,4)
class procedure TBuiltInGlobal.createArray (vm : TObject);
var nArgs, i : integer;
    ar : TArrayObject;
    dim : TIndexArray;
    alist : TListObject;
begin
  nArgs := TVM (vm).popInteger;
  if TVM (vm).peek.stackType = stList then
     begin
     alist := TVM (vm).popList;
     ar := convertListToArray (alist);
     TVM (vm).push(ar);
     end
  else
     begin
     setLength (dim, nArgs);
     for i := nArgs - 1 downto 0 do
         dim[i] := TVM (vm).popInteger;

     ar := TArrayObject.Create (dim);
     TVM (vm).push(ar);
     end;
end;


class procedure TBuiltInGlobal.createDictionary (vm : TObject);
begin
  raise ERuntimeException.Create('dict not yet implemented');
end;


class procedure TBuiltInGlobal.listSymbols (vm : TObject);
var module : TModule;
begin
  module := TVM (vm).popModule;
  TVM (vm).push (TStringObject.create (getAllocatedSymbols (module)));
end;


function getModuleHelp (m : TModule) : string;
var f : TUserFunction;
    vo : TValueObject;
    key : string;
begin
  result := 'Module: ' + m.moduleName + ', ' + m.help.getHelp() + sLineBreak;
  result := result + Format('%-12s%-12s%-11s%', ['Type', 'Name', 'Help']) + sLineBreak;
  for key in m.symbolTable.keys do
      begin
      case m.symbolTable.Items[key].symbolType of
         symUserFunc :
             begin
             f := m.symbolTable.Items[key].fValue;
             result := result + Format('%-12s', ['Function:']) +
                        Format('%-12s', [f.methodName])  +  f.help.getHelp() + sLineBreak;
             end;
         symValueObject :
             begin
             vo := m.symbolTable.Items[key].voValue;
             result := result + Format('%-12s', ['Function:']) +
                        Format('%-12s', [f.methodName])  +  f.help.getHelp() + sLineBreak;

             end
      else
         result := 'Ther is no help for these kinds of objects'; // HMS
         //result := result + Format('%-12s', ['Variable:']) +
         //       Format ('%-12s', [m.symbolTable.Items[key].symbolName])  + m.symbolTable.Items[key].helpStr + sLineBreak;
      end;
      end;
end;


//procedure TBuiltInGlobal.myHelp (vm : TObject);
//var x : PMachineStackRecord;
//begin
//  x := TVM (vm).pop;
//  case x.stackType of
//    stInteger  : TVM (vm).push (TStringObject.create ('Integer Value'));
//    stBoolean  : TVM (vm).push (TStringObject.create ('Boolean Value'));
//    stDouble   : TVM (vm).push (TStringObject.create ('Double value: ' + floattostr (x.dValue)));
//    stString   : TVM (vm).push (TStringObject.create ('String Value'));
//    stList     : TVM (vm).push (TStringObject.create ('List Value'));
//    stModule   : TVM (vm).push (TStringObject.create (getModuleHelp (x.module)));
//    stFunction : TVM (vm).push (TStringObject.create ('User Function: ' + x.fvalue.helpStr));
//  else
//    TVM (vm).push (TStringObject.create ('Undefined Value'));
//  end;
//end;


class procedure TBuiltInGlobal.readString (vm : TObject);
var s : string;
    sObj : TStringObject;
    prompt : AnsiString;
    nArgs : integer;
begin
  nArgs := TVM (vm).popInteger;
  case nArgs of
    0 : prompt := '';
    1 : prompt := AnsiString (TVM (vm).popString ().value);
  else
    raise ERuntimeException.Create('readString takes a single string argument or none at all');
  end;

  if Assigned (TVM (vm).readStringCallbackPtr) then
     s := string (AnsiString (TVM (vm).readStringCallbackPtr(prompt)));
  sobj := TStringObject.create (s);
  TVM (vm).push (sObj);
end;


class procedure TBuiltInGlobal.readNumber (vm : TObject);
var s : string;
    iValue : integer;
    dValue : double;
    prompt : AnsiString;
    nArgs : integer;
begin
  nArgs := TVM (vm).popInteger;
  case nArgs of
    0 : prompt := '';
    1 : prompt := AnsiString (TVM (vm).popString ().value);
  else
    raise ERuntimeException.Create('readString takes a single string argument or none at all');
  end;

  if Assigned (TVM (vm).readStringCallbackPtr) then
     s := string (AnsiString (TVM (vm).readStringCallbackPtr(prompt)));

  while (not TryStrToInt(s, iValue)) and (not TryStrToFloat(s, dValue)) do
      begin
      if assigned (TVM (vm).printlnCallbackPtr) then
         TVM (vm).printlnCallbackPtr (AnsiString ('Number error: ' + s + ' is not a number, try again'));
      if Assigned (TVM (vm).readStringCallbackPtr) then
         s := string (AnsiString (TVM (vm).readStringCallbackPtr(prompt)));
      end;
  if TryStrToInt(s, iValue) then
     TVM (vm).push (iValue)
  else
    TVM (vm).push (dValue);
end;


class procedure TBuiltInGlobal.myInt (vm : TObject);
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


class procedure TBuiltInGlobal.myIntToHex (vm : TObject);
var value : integer;
begin
  value := TVM (vm).popInteger;
  TVM (vm).push(TStringObject.Create(IntToHex(value)));
end;


class procedure TBuiltInGlobal.myFloat (vm : TObject);
var x : PMachineStackRecord;
begin
  x := TVM (vm).pop;
  case x.stackType of
       stInteger : TVM (vm).push (double (x.iValue));
       stDouble  : TVM (vm).push (x.dValue);

  else
     argMustBeNumber;
  end;
end;


class procedure TBuiltInGlobal.dis (vm : TObject);
var x : PMachineStackRecord;
begin
  x := TVM (vm).pop;
  case x.stackType of
    stModule : TVM (vm).push (TStringObject.create (dissassemble (x.module, x.module.moduleProgram)));
    stFunction :
      begin
      if x.fValue.isbuiltInFunction then
         TVM (vm).push (TStringObject.create (x.fValue.methodName + ' is a builtin function'))
      else
         TVM (vm).push (TStringObject.create (dissassemble (x.fValue.moduleRef, x.fValue.codeBlock)));
      end;
  else
     raise ERuntimeException.Create('dis argument can only be a module or a function');
  end;
end;


class procedure TBuiltInGlobal.listModules (vm : TObject);
var ls : TListObject;
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


class procedure TBuiltInGlobal.myAssertTrueEx (vm : TObject);
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


class procedure TBuiltInGlobal.myAssertFalseEx (vm : TObject);
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


class procedure TBuiltInGlobal.getType (vm : TObject);
var x : PMachineStackRecord;
begin
  x := TVM (vm).pop;
  case x.stackType of
    stInteger : TVM (vm).push (TStringObject.Create ('int'));
    stDouble : TVM (vm).push (TStringObject.Create ('float'));
    stBoolean : TVM (vm).push (TStringObject.Create ('bool'));
    stString : TVM (vm).push (TStringObject.Create ('string'));
    stList : TVM (vm).push (TStringObject.Create ('list'));
    stArray : TVM (vm).push (TStringObject.Create ('array'));
    stVector : TVM (vm).push (TStringObject.Create ('vector'));
    stMatrix : TVM (vm).push (TStringObject.Create ('matrix'));
    stFunction : TVM (vm).push (TStringObject.Create ('function'));
    stModule : TVM (vm).push (TStringObject.Create ('module'));
    stNone :  TVM (vm).push (TStringObject.Create ('none'));
  else
    TVM (vm).push (TStringObject.Create ('I''m not sure'));
  end;
end;


class procedure TBuiltInGlobal.getAttr (vm : TObject);
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
       symModule : TVM (vm).push (symbol.mValue);
       symUndefined :  TVM (vm).push (TStringObject.create ('undefined'));
     end;
     end
  else
     raise ERuntimeException.Create('Attribute not found in module');
end;


// Convert int into character
class procedure TBuiltInGlobal.getChar (vm : TObject);
var x : integer;
begin
  x := TVM (vm).popInteger();
  TVM (vm).push (TStringObject.create(Chr (x)));
end;


 // Convert char in to ascii
class procedure TBuiltInGlobal.getAsc (vm : TObject);
var x: string;
begin
  x := TVM (vm).popString().value;
  if (length (x) = 0) or (length (x) > 1) then
     raise ERuntimeException.Create('Character to asc function should be a single character string');
  TVM (vm).push (Ord (x[1]));
end;


class procedure TBuiltInGlobal.stackInfo (vm : TObject);
var vm1 : TVM;
begin
  vm1 := TVM (vm);
  vm1.push(vm1.getStackInfo().stacktop);
end;


class procedure TBuiltInGlobal.getHelp (_vm : TObject);
var st : PMachineStackRecord;
    vm : TVM;
begin
  vm := TVM (_vm);
  st := vm.pop;
  case st.stackType of
     stInteger : vm.push(TStringObject.Create('integer'));
     stDouble  : vm.push(TStringObject.Create('double'));
     stBoolean : vm.push(TStringObject.Create('boolean'));
     stString  : vm.push(TStringObject.Create('string'));
     stList    : vm.push(TStringObject.Create('list'));
     stArray   : vm.push(TStringObject.Create('array'));
     stModule  : vm.push(TStringObject.Create (st.module.help.getHelp()));

     stFunction :
           vm.push(TStringObject.Create (st.fValue.help.getHelp()));
     stObjectMethod :
           vm.push(TStringObject.Create (st.oValue.helpStr));
  else
     raise ERuntimeException.Create('Unkown object type in help');
  end;
end;


class procedure TBuiltInGlobal.startDebug (vm : TObject);
var f : TUserFunction;
begin
  debugCallback := TVM (vm).popUserFunction();
  TVM (vm).setDebugCallBack(debugProc);
  TVM (vm).enableDebugging;
  //TVM (vm).push(integer (vm));
  //TVM (vm).callUserFunction(1);
  TVM (vm).pushNone;
end;

class procedure TBuiltInGlobal.test (vm : TObject);
var f : TUserFunction;
    x, y: integer;
begin
  x := TVM (vm).popInteger;
  y := TVM (vm).popInteger;
  TVM (vm).push(x + y);
end;


procedure createGlobalBuiltIns;
begin
  //builtInGlobal := TBuiltInGlobal.Create;
end;

initialization

finalization
  //builtInGlobal.Free;
end.
