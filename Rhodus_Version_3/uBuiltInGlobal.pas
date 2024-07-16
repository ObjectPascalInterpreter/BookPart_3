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

{$WARN SYMBOL_PLATFORM OFF}


interface


Uses SysUtils, Classes, uLibModule, uSymbolTable, uListObject, uArrayObject;

function  getMainModule : TModule;
procedure computeBaseLineMemoryAllocated;
procedure addGlobalMethodsToModule (module : TModuleLib);

var mainModule : TModuleLib;
    baseLineMemoryAllocated : UInt64;

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
     Generics.Collections,
     System.Generics.Defaults,
     uDataObjectMethods,
     uDataObject,
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
       class procedure getMain (vm : TObject);
       class procedure dis (vm : TObject);
       class procedure stackInfo (vm : TObject);
       class procedure getHelp (_vm : TObject);
       class procedure startDebug (vm : TObject);
       class procedure test (vm : TObject);

       class procedure document (vm : TObject);

       //constructor Create;
       //destructor  Destroy; override;
  end;

type
   THelpPair = record
      name : string;
      methodCount : integer;
      help : THelpModule;
   end;


var  debugCallback : TUserFunction;

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
  module.addMethod (TBuiltInGlobal.listSymbols,    1, 'symbols',       'Returns a list of symbols in the specified module: e.g symbols(math). Use main() as the argument to get the symbols for the main module');
  module.addMethod (TBuiltInGlobal.getType,        1, 'type',          'Returns the type of a given variable: type (x)');
  module.addMethod (TBuiltInGlobal.getAttr,        2, 'getAttr',       'Returns the value attached to the symbol attribute: getAttr (mylib, "x")');
  module.addMethod (TBuiltInGlobal.listModules,    0, 'modules',       'Get a list of all currently loaded mdules');
  module.addMethod (TBuiltInGlobal.getMemoryUsed,  0, 'mem',           'Get the amount of memory currently in use.');
  module.addMethod (TBuiltInGlobal.myAssertTrueEx, 1, 'assertTrueEx',  'Assert argument is true, return . of F');
  module.addMethod (TBuiltInGlobal.myAssertFalseEx,1, 'assertFalseEx', 'Assert argument is false, return . of F');
  module.addMethod (TBuiltInGlobal.getMain,        0, 'main',          'Returns a reference to the main module');
  module.addMethod (TBuiltInGlobal.dis,            1, 'dis',           'dissassemble module or function');
  module.addMethod (TBuiltInGlobal.stackInfo,      0, 'stackInfo',     'Get the current state of the VM stack');
  module.addMethod (TBuiltInGlobal.getChar,        1, 'chr',           'Get the character equivalent of an integer value');
  module.addMethod (TBuiltInGlobal.getAsc,         1, 'asc',           'Get the ascii equivalent of a single character');
  //module.addMethod (builtInGlobal.getHelp,       1, 'help',          'Get the help associated with the object');
  module.addMethod (TBuiltInGlobal.startDebug,     1, 'debug',         'Attached method to the debugger: debug (fcn)');
  module.addMethod (TBuiltInGlobal.test,           2, 'test',          'Attached method to the debugger: debug (fcn)');
  module.addMethod (TBuiltInGlobal.document, VARIABLE_ARGS, 'document', 'Generate LaTeX documentation for the module specificed in the argument; e.g docs = document (math)' +
               'The second optional argument speciifies the format, ''latex'' or ''md'' (markdown). Default is LaTeX');

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
    dobj : TDataObject;
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
                          // Only print out the first 50 characters in case the string is too long.
                          astr :=  '"' + leftStr ((module.symbolTable.items[key].dataObject as TStringObject).value, 50);
                          if length ((module.symbolTable.items[key].dataObject as TStringObject).value) > 50 then
                             astr := astr + '...."'
                          else
                             astr := astr + '"';

                          result := result + Format ('%-12s', ['string']) + Format ('%-22s', [astr]);
                          len := length ((module.symbolTable.items[key].dataObject as TStringObject).value);
                          result := result + Format ('%-12d', [len]) + sLineBreak;
                          end;
              symList   : result := result + Format ('%-12s%-22s%-6d%', ['list', module.symbolTable.items[key].dataObject.toString, module.symbolTable.items[key].dataObject.getsize()]) + sLineBreak;
             symUserFunc: begin
                          dobj := module.symbolTable.items[key].dataObject;
                          result := result + Format ('%-12s%-22s%-12d', ['ufunc', 'NA', dobj.getSize()]) + sLineBreak;
                          end;
             symValueObject :
                          begin
                          result := result + Format ('%-12s%-22s%-12d', ['ValueObject', 'NA', module.symbolTable.items[key].dataObject.getSize()]) + sLineBreak;
                          end;
              symModule : begin
                          result := result + Format ('%-12s%-22s%-12d', ['module', 'NA', (module.symbolTable.Items[key].mValue as TModule).getSize]) + sLineBreak;
                          end;
              symUndefined : continue;
         else
             raise Exception.Create('Internal error: This type not yet supported in getAllocatedSymbols()');
         end;
      end;
      end;
end;


class procedure TBuiltInGlobal.getMain (vm : TObject);
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
      if alist.list[i].itemType <> symList then
         begin
          inc (result);
         end
      else
         result := result + countElements (TListObject (alist.list[i].dataObject));
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

  if alist.list[0].itemType <> symlist  then
     begin
     setLength (result, 1);
     result[0] := firstdim;
     exit;
     end;

  dimIndex := 1;
  setLength (result, dimIndex);
  result[dimIndex-1] := firstdim;

  aitem := alist.list[0];
  while aitem.itemType = symlist do
      begin
      n := TListObject (aitem.dataObject).list.Count;
      dimIndex := dimIndex + 1;
      setLength (result, dimIndex);
      result[dimIndex-1] := n;
      aitem := TListObject (aitem.dataObject).list[0];
      end;
end;


var elementCount : integer;
procedure collectData (alist : TListObject; arrayObj : TArrayObject);
var i : integer;
begin
  for i := 0 to alist.list.Count - 1 do
      if alist.list[i].itemType = symList then
         begin
         collectData (TListObject (alist.list[i].dataObject), arrayObj);
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
  if TVM (vm).peek.stackType = symList then
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
       symInteger : tVM (vm).push (x.iValue);
       symDouble  : begin
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
       symInteger : TVM (vm).push (double (x.iValue));
       symDouble  : TVM (vm).push (x.dValue);

  else
     argMustBeNumber;
  end;
end;


class procedure TBuiltInGlobal.dis (vm : TObject);
var x : PMachineStackRecord;
begin
  x := TVM (vm).pop;
  case x.stackType of
    symModule : TVM (vm).push (TStringObject.create (dissassemble (x.module, x.module.moduleProgram)));
    symUserFunc :
      begin
      if TUserFunction (x.dataObject).isbuiltInFunction then
         TVM (vm).push (TStringObject.create (TUserFunction (x.dataObject).methodName + ' is a builtin function'))
      else
         TVM (vm).push (TStringObject.create (dissassemble (TUserFunction (x.dataObject).moduleRef, TUserFunction (x.dataObject).codeBlock)));
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
  if st.stackType = symBoolean then
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
  if st.stackType = symBoolean then
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
    symInteger : TVM (vm).push (TStringObject.Create ('int'));
    symDouble : TVM (vm).push (TStringObject.Create ('float'));
    symBoolean : TVM (vm).push (TStringObject.Create ('bool'));
    symString : TVM (vm).push (TStringObject.Create ('string'));
    symList : TVM (vm).push (TStringObject.Create ('list'));
    symArray : TVM (vm).push (TStringObject.Create ('array'));
    symVector : TVM (vm).push (TStringObject.Create ('vector'));
    symMatrix : TVM (vm).push (TStringObject.Create ('matrix'));
    symUserFunc : TVM (vm).push (TStringObject.Create ('function'));
    symModule : TVM (vm).push (TStringObject.Create ('module'));
    symNonExistant :  TVM (vm).push (TStringObject.Create ('none'));
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
       symString : TVM (vm).push (symbol.dataObject);
       symList : TVM (vm).push (symbol.dataObject);
       symUserFunc : TVM (vm).push (symbol.dataObject);
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
     symInteger : vm.push(TStringObject.Create('integer'));
     symDouble  : vm.push(TStringObject.Create('double'));
     symBoolean : vm.push(TStringObject.Create('boolean'));
     symString  : vm.push(TStringObject.Create('string'));
     symList    : vm.push(TStringObject.Create('list'));
     symArray   : vm.push(TStringObject.Create('array'));
     symModule  : vm.push(TStringObject.Create (st.module.help.getHelp()));

     symUserFunc :
           vm.push(TStringObject.Create (st.dataObject.help.getHelp()));
     symObjectMethod :
           vm.push(TStringObject.Create (st.oValue.help.description));
  else
     raise ERuntimeException.Create('Unknown object type in help');
  end;
end;


class procedure TBuiltInGlobal.startDebug (vm : TObject);
//var f : TUserFunction;
begin
  //debugCallback := TVM (vm).popUserFunction();
  TVM (vm).setDebugCallBack(debugProc);
  TVM (vm).enableDebugging;
  //TVM (vm).push(integer (vm));
  //TVM (vm).callUserFunction(1);
  TVM (vm).pushNone;
end;

class procedure TBuiltInGlobal.test (vm : TObject);
var x, y: integer;
begin
  x := TVM (vm).popInteger;
  y := TVM (vm).popInteger;
  TVM (vm).push(x + y);
end;


// ------------------------------------------------------------------
// Documentation support code
// ------------------------------------------------------------------

procedure processModuleToLaTeX (vm : TObject; module : TModule);
var astr : string;
    Item: TPair<string, TSymbol>;
    helpPair : THelpPair;
    Comparison: IComparer<THelpPair>;
    list : TList<THelpPair>;
    doc : TStringObject;
begin
  astr := '\documentclass[11pt]{article}' + sLineBreak;
  astr := astr + '\usepackage{enumitem}' + sLineBreak;
  astr := astr + '\setlength{\parindent}{0pt}' + sLineBreak;
  astr := astr + '\begin{document}' + sLineBreak;

  astr := astr + '{\bfseries\LARGE Rhodus Module Reference}' + sLineBreak;
  astr := astr + '\vspace{1cm}' + sLineBreak + sLineBreak;;

  astr := astr + '\section*{' + module.moduleName + ' Module }';
  astr := astr + '{\bfseries\large Module: } \hspace{18pt} {\large\textsf{'
       + module.moduleName + '}} \hspace{45pt} {\large\textsf{' + module.help.description + '}}';

  astr := astr + sLineBreak + sLineBreak + '\vspace{-7pt}' + sLineBreak + '\rule{\textwidth}{1pt}' + sLineBreak + sLineBreak;

  // This complicted bit of code is to get the help records in to the order
  // that they were added to the module. This allows the methods to be
  // ordered in a more senstible way. The order is set by the methodCount
  // which is set during the addMetdod call.
  // I tried to do the ordering insitue but I couldnlt get the syntax right.
  // Instead I extract the elements and sort them in a separate list.
  list := TList<THelpPair>.Create;
  try
    for Item in module.symbolTable do
        case Item.Value.symbolType of
          TElementType.symUserFunc:
           begin
           if Item.Value.dataObject.help <> nil then
              begin
              if Item.Value.dataObject.help.methodName <> '' then
                 begin
                 helpPair.name := Item.Value.dataObject.help.methodName;
                 helpPair.methodCount := Item.Value.methodCount;
                 helpPair.help := Item.Value.dataObject.help;
                 list.Add(helpPair);
                 end;
              end;
           end;
          TElementType.symValueObject :
           begin
           helpPair.name := Item.Value.symbolName;
           helpPair.methodCount := Item.Value.methodCount;
           helpPair.help := Item.Value.dataObject.help;
           list.Add(helpPair);
            end;
        end;
  comparison := TComparer<THelpPair>.Construct(
    function(const Left, Right: THelpPair): Integer
        begin
        Result := CompareValue(Left.methodCount, Right.methodCount)
       end);

  // Sort the list in ascending order
  list.Sort(comparison);

  for helpPair in list do
       begin
        if helpPair.help <> nil then
           begin
           if Item.Value.dataObject.help.methodName <> '' then
              begin
              astr := astr + helpPair.help.toLatex;
              astr := astr + sLineBreak + sLineBreak + '\vspace{5pt}' + sLineBreak;
              end;
           end;
       end;
  finally
    list.free;
  end;

  astr := astr + sLineBreak + '\end{document}' + sLineBreak;

  doc := TStringObject.Create (astr);
  TVM (vm).push(doc);
end;


procedure processDataObjectToLaTeX (vm : TObject);
var stackType : TElementType;
    Obj : TDataObject;
    methodDetails : TMethodDetails;
    doc : TStringObject;
    astr : string;
    i : integer;
begin
  stackType := TVM (vm).peek.stackType;
  if isDataObject (stackType) then
     begin
     obj := TVM (vm).pop.dataObject;
     for i := 0 to Obj.methods.methodList.Count - 1 do
         begin
         methodDetails := Obj.methods.methodList[i];
         astr := astr + '{\bfseries\textsf{Method: ' + methodDetails.name ;
         astr := astr + '}}' + sLineBreak + sLineBreak;
         astr := astr + Obj.methods.methodList[i].help.description + sLineBreak;
         end;
     end;
  doc := TStringObject.Create (astr);
  TVM (vm).push(doc);
end;


procedure laTeXFormat (vm : TObject);
var module : TModule;
begin
  if TVM (vm).peek.stackType = TElementType.symModule then
     begin
     module := TVM (vm).popModule;
     processModuleToLaTeX (vm, module);
     end
  else
    processDataObjectToLaTeX (vm);
end;


procedure processModuleToMarkDown (vm : TObject; module : TModule);
var astr : string;
    doc : TStringObject;
    Item: TPair<string, TSymbol>;
    helpPair : THelpPair;
    Comparison: IComparer<THelpPair>;
    list : TList<THelpPair>;
begin
  //astr := astr + '\setlength{\parindent}{0pt}' + sLineBreak;

  astr := '# Rhodus Module Reference' + sLineBreak;

  //astr := astr + '\vspace{1cm}' + sLineBreak + sLineBreak;;

  astr := astr + '## ' + module.moduleName + ' Module' + sLineBreak;

  astr := astr + 'Module: $$~$$ ' + module.moduleName + ' $$~~~~~~~~~~~~~~~~~~~$$ ' + module.help.description + sLineBreak;

  astr := astr + sLineBreak + '---' + sLineBreak + sLineBreak;

  // This complicted bit of code is to get the help records in to the order
  // that they were added to the module. This allows the methods to be
  // ordered in a more senstible way. The order is set by the methodCount
  // which is set during the addMetdod call.
  // I tried to do the ordering insitue but I couldnlt get the syntax right.
  // Instead I extract the elements and sort them in a separate list.
  list := TList<THelpPair>.Create;
  try
    for Item in module.symbolTable do
        case Item.Value.symbolType of
          TElementType.symUserFunc:
           begin
           if Item.Value.dataObject.help <> nil then
              begin
              if Item.Value.dataObject.help.methodName <> '' then
                 begin
                 helpPair.name := Item.Value.dataObject.help.methodName;
                 helpPair.methodCount := Item.Value.methodCount;
                 helpPair.help := Item.Value.dataObject.help;
                 list.Add(helpPair);
                 end;
              end;
           end;
          TElementType.symValueObject :
           begin
           helpPair.name := Item.Value.symbolName;
           helpPair.methodCount := Item.Value.methodCount;
           helpPair.help := Item.Value.dataObject.help;
           list.Add(helpPair);
            end;
        end;
  comparison := TComparer<THelpPair>.Construct(
    function(const Left, Right: THelpPair): Integer
        begin
        Result := CompareValue(Left.methodCount, Right.methodCount)
       end);

  // Sort the list in ascending order
  list.Sort(comparison);

  for helpPair in list do
       begin
        if helpPair.help <> nil then
           begin
           if Item.Value.dataObject.help.methodName <> '' then
              begin
              astr := astr + helpPair.help.toMarkDown;
              astr := astr + sLineBreak + sLineBreak;
              end;
           end;
       end;
  finally
    list.free;
  end;

  doc := TStringObject.Create (astr);
  TVM (vm).push(doc);
end;


procedure processDataObjectToMarkDown (vm : TObject);
var stackType : TElementType;
    Obj : TDataObject;
    methodDetails : TMethodDetails;
    doc : TStringObject;
    astr : string;
    i : integer;
begin
  stackType := TVM (vm).peek.stackType;
  if isDataObject (stackType) then
     begin
     obj := TVM (vm).pop.dataObject;
     astr := '# **' + obj.methods.helpStr + '**' + sLineBreak + sLineBreak;
     astr := astr + '---' + sLineBreak;

     astr := astr + '## Associated Methods:' + sLineBreak;

     for i := 0 to Obj.methods.methodList.Count - 1 do
         begin
         methodDetails := Obj.methods.methodList[i];
         astr := astr + '**Method: ' + methodDetails.name ;
         astr := astr + '**' + sLineBreak;
         astr := astr + Obj.methods.methodList[i].help.description + sLineBreak;
         end;
     end;
  doc := TStringObject.Create (astr);
  TVM (vm).push(doc);
end;


procedure markDownFormat (vm : TObject);
var module : TModule;
begin
  if TVM (vm).peek.stackType = TElementType.symModule then
     begin
     module := TVM (vm).popModule;
     processModuleToMarkDown (vm, module);
     end
  else
    processDataObjectToMarkDown (vm);
end;


procedure laTeXOrMDFormat (vm : TObject);
var fmt : TStringObject;
begin
  fmt := TVM (vm).popString;
  if fmt.value = 'latex' then
     laTeXFormat (vm)
  else if fmt.value = 'md' then
     markDownFormat (vm)
  else
     raise ERuntimeException.Create('Format not recognized: ' + fmt.value);
end;


class procedure TBuiltInGlobal.document (vm : TObject);
var nArgs: integer;
begin
  nArgs := TVM (vm).popInteger;
  case nArgs of
     1 : laTeXFormat (vm);
     2 : laTeXOrMDFormat (vm);
  else
     raise ERuntimeException.Create ('Expecting one or two arguments');
  end;
end;


initialization
finalization
end.
