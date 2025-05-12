unit uSymbolTable;

// This source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

Uses Classes, SysUtils, Generics.Collections,
     uListObject,
     uStringObject,
     uArrayObject,
     uVectorObject,
     uMatrixObject,
     uValueObject,
     uOpCodes,
     uConstantTable,
     uVMExceptions,
     uProgramCode,
     uMemoryManager,
     uRhodusTypes,
     uDataObjectMethods,
     uDataObject,
     uHelpUnit;

type
   // There are too many dependencies between these classes to separate them
   // into individual files, so we group them together here.

   TSymbol = class;
   TSymbolTable = class;
   TLocalSymbolTable = class;


   // Base module class. When importing a module we'll
   // use the derived class TLibModule
   TModule = class (TObject)
       moduleName : string;
       moduleProgram : TProgram;      // Module level code
       symbolTable : TSymbolTable;
       help : THelpModule;
       compiled : boolean;   // Not currently used

       function    getSize : integer;
       procedure   getHelp (vm : TObject);
       function    find (name : string) : TSymbol; overload;
       function    find (moduleName, symbolName : string) : TSymbol; overload;
       procedure   clearCode;
       constructor Create (name : string);
       destructor  Destroy; override;
   end;

   // Methods attached to the user function object
   TUserFunctionMethods = class (TMethodsBase)
      procedure getName (vm : TObject);
      procedure getnArgs (vm : TObject);
      procedure getCode (vm : TObject);
      procedure getHelp (vm : TObject);
      constructor Create;
      destructor  Destroy; override;
  end;

   TxBuiltInFunction = procedure (vm : TObject) of object;
   TUserFunction = class (TDataObject)
       methodName : string;
       nArgs : integer;  // Expected number of arguments
       localSymbolTable : TLocalSymbolTable;  // The local symbol table is accessed by index not name
       moduleName : string;   // Not actually used at the moment, probabhyl will be removed
       moduleRef : TModule;
       globalVariableList : TStringList;
       codeBlock : TProgram;
       //helpStr : string;

       isbuiltInFunction : boolean;
       builtInPtr : TxBuiltInFunction;

       userFunctionMethods : TUserFunctionMethods;

       function    clone : TDataObject; override;
       function    getSize : integer; override;
       function    ToString : string; override;
       constructor Create; overload;
       constructor Create (methodName : string); overload;
       constructor Create (methodName : string; nArgs : integer; funcPtr : TxBuiltInFunction); overload;
       destructor  Destroy; override;
   end;


   TSymbol = class (TObject)
       symbolType : TElementType;
       symbolName : string;
       helpStr : string;
       locked  : boolean;  // If true, symbol is read only

       // The following fields could be replaced with a single
       // field that we cast each time to the appropriate type
       iValue  : integer;
       dValue  : double;
       bValue  : boolean;
       //sValue  : TStringObject;
       //lValue  : TListObject;
       //aValue  : TArrayObject;
       //vValue  : TVectorObject;
       //matValue : TMatrixObject;
       //voValue : TValueObject;
       //fValue  : TUserFunction;
       mValue  : TModule;

       dataObject     : TDataObject;

       methodCount : integer;   // Used for ordering list of methods in the documentation

       // A convenient place to store these
       class var   mainModuleId : string;
       class var   localModuleId : string;
       class var   globalId : string;

       function    clone : TSymbol;
       function    getSize : integer;
       function    convertToString : string;
       constructor Create;
       destructor  Destroy; override;
   end;


   // Stores module level symbols, identified in the VM by name
   TSymbolDictionary = TObjectDictionary<string, TSymbol>;
   TSymbolTable = class (TSymbolDictionary)
      private
        procedure checkForExistingData (symbol : TSymbol); inline;
      public
        id : string;
        moduleRef : TModule;
        function  addModule (mValue : TModule) : TSymbol;
        function  addSymbol (name : string) : TSymbol; overload;
        procedure addSymbol (name : string; dValue : double;  locked : boolean; helpStr : string); overload;
        procedure addSymbol (name : string; sValue : TStringObject; locked : boolean; help : THelpModule); overload;
        procedure addSymbol (name : string; lValue : TListObject; locked : boolean; help : THelpModule); overload;
        procedure addSymbol (name : string; aValue : TArrayObject; locked : boolean; helpStr : string); overload;
        procedure addSymbol (name : string; vValue : TVectorObject; locked : boolean; helpStr : string); overload;
        procedure addSymbol (name : string; matValue : TMatrixObject; locked : boolean; helpStr : string); overload;
        procedure addSymbol (name : string; voValue : TValueObject; locked : boolean; help : THelpModule); overload;

        function addSymbol (fValue : TUserFunction; locked : boolean) : TSymbol; overload;

        function  find (name : string; var symbol : TSymbol) : boolean;

        procedure storeInteger (symbol : TSymbol; iValue : integer);
        procedure storeBoolean (symbol : TSymbol; bValue : boolean);
        procedure storeDouble  (symbol : TSymbol; dValue : double);
        procedure storeString  (symbol : TSymbol; sValue : TStringObject);
        procedure storeList    (symbol : TSymbol; lValue : TListObject);
        procedure storeArray   (symbol : TSymbol; aValue : TArrayObject);
        procedure storeVector  (symbol : TSymbol; vValue : TVectorObject);
        procedure storeMatrix  (symbol : TSymbol; matValue : TMatrixObject);
        procedure storeValueObject  (symbol : TSymbol; voValue : TValueObject);

        procedure storeFunction (symbol : TSymbol; fValue : TUserFunction);
        //procedure storeObject (symbol : TSymbol; obj : TDataObject);
        procedure storeModule (symbol : TSymbol; mValue : TObject);

        constructor Create;
        destructor  Destroy; override;
   end;


   // Stores user function level symbols, identified in the VM by index
   TLocalSymbolTable = class (TObjectList<TSymbol>)
      private
      public
        function addSymbol (name : string) : integer; overload;
        function addSymbol (fValue : TUserFunction; locked : boolean) : integer; overload;
        function find (name : string; var index : integer) : boolean;
        function isUserFunction (name : string; var index : integer) : boolean;
        function reverseFind (name : string; var index : integer): boolean;

        procedure storeToSymbolTable (index, ivalue : integer); overload;
        procedure storeToSymbolTable (index : integer; bValue : boolean); overload;
        procedure storeToSymbolTable (index : integer; dValue : double); overload;
        procedure storeToSymbolTable (index : integer; lValue : TListObject); overload;
        procedure storeToSymbolTable (index : integer; sValue : TStringObject); overload;
        procedure storeToSymbolTable (index : integer; aValue : TArrayObject); overload;
        procedure storeToSymbolTable (index : integer; fValue : TUserFunction); overload;

        function  getFromSymbolTable (index : integer) : TSymbol;

        procedure checkForExistingData (index : integer); inline;

        function    Clone : TLocalSymbolTable;
        constructor Create;
        destructor  Destroy; override;
   end;

   // Helper functions to add the builtin libraries.
   function  addModule (module : TModule; lib : TModule) : TSymbol;
   procedure addAllBuiltInLibraries (module : TModule);

var SysLibraryRef : TModule;

implementation

Uses uVM,
     uMachineStack,
     uBuiltInGlobal, 
     uBuiltinMath, 
     uBuiltInList,
     uBuiltInRandom,
     uListOfBuiltIns,
     uBuiltInOS, 
     uBuiltInStr,
     uBuiltInMatrix,
     uBuiltInArray,
     uBuiltInFile,
     uBuiltInConfig,
     uBuiltInSys,
     uBuiltInTurtle;

var _userFunctionMethods : TUserFunctionMethods;
     

function addModule (module : TModule; lib : TModule) : TSymbol;
begin
  result := module.symbolTable.addModule (lib);
end;


procedure addAllBuiltInLibraries (module : TModule);
begin
  // Import the common modules.
  addModule (module, TBuiltInList.Create);
  addModule (module, TBuiltInStr.Create);
  addModule (module, TBuiltInMatrix.Create);
  addModule (module, TBuiltInArray.Create);
  //addModule (module, TBuiltInMath.Create);

  // Needed by the compiler to access the path variable
  SysLibraryRef := TBuiltInSys.Create;
  addModule (module, SysLibraryRef);

  // Uncomment if you want to enable turtle support.
  //addModule (module, TBuiltInTurtle.Create);
end;

// -------------------------------------------------------------------------------

// HMS
constructor TModule.Create (name : string);
begin
  self.moduleName := name;
  moduleProgram := TProgram.Create;
  symbolTable := TSymbolTable.Create;
  symbolTable.id := name;
  if help = nil then
     self.help := THelpModule.CreateModule(name)
  else
     self.help := THelpModule.Create ('No help available on this module');
  compiled := False;
end;


destructor  TModule.Destroy;
begin
  moduleProgram.Free;
  symbolTable.Free;
  help.Free;
  inherited;
end;


procedure TModule.clearCode;
begin
  moduleProgram.clearCode;
  compiled := False;
end;


function TModule.find (name : string) : TSymbol;
begin
  if not symbolTable.find(name, result) then
     result := nil;
end;


function TModule.find (moduleName, symbolName : string) : TSymbol;
var symbol : TSymbol;
begin
  symbolTable.find(moduleName, symbol);
  if symbol = nil then
     raise ERuntimeException.Create ('Module: ' + moduleName + ' cannot be found, import it first');

  result := symbol.mValue.find(symbolName);
  if result = nil then
     raise ERuntimeException.Create ('Internal error, unable to locate: ' + moduleName + ':' + symbolName);
end;


function TModule.getSize : integer;
var sym : TSymbol;
begin
   result := self.InstanceSize;
   if self.moduleProgram <> nil then
      result := result + self.moduleProgram.getSize();

   if self.symbolTable <> nil then
      begin
      for sym in self.symbolTable.Values do
          result := result + sym.getSize;

      result := result + self.symbolTable.InstanceSize;
      end;
end;


procedure TModule.getHelp(vm: TObject);
begin
  if help <> nil then
     begin
      TVM (vm).push(TStringObject.Create(help.getHelp()));
     end
  else
     TVM (vm).push (TStringObject.Create('No help'));
end;


// ------------------------------------------------------------------------------------------
constructor TUserFunctionMethods.Create;
begin
  methodList := TMethodList.Create (self);

  methodList.Add(TMethodDetails.Create ('name',   0, 'Returns the name of the function: func.name ()', getName));
  methodList.Add(TMethodDetails.Create ('nargs',  0, 'Returns the number of arguments the function expects. Returns -1 if the number is variable', getnArgs));
  methodList.Add(TMethodDetails.Create ('code',   0, 'Returns the byte code associated with the function: func.code ()', getCode));
  methodList.Add(TMethodDetails.Create ('help',   0, 'Returns the help string associated with the function. func.help ()', getHelp));

  methodList.Add(TMethodDetails.Create ('dir',    0, 'dir of string object methods', dir));
end;


destructor TUserFunctionMethods.Destroy;
begin
  inherited;
end;



// ------------------------------------------------------------------------------------------
procedure TUserFunctionMethods.getName (vm : TObject);
var f : TUserFunction;
    om : TMethodDetails;
begin
  // No arguments for this method
  om :=TVM (vm).popMethodDetails;
  f := TUserFunction (om.self);

  TVM (vm).push(TStringObject.Create(f.methodName));
end;


procedure TUserFunctionMethods.getnArgs (vm : TObject);
var f : TUserFunction;
    om : TMethodDetails;
begin
  // No arguments for this method
  om :=TVM (vm).popMethodDetails;
  f := TUserFunction (om.self);

  TVM (vm).push(f.nArgs);
end;


procedure TUserFunctionMethods.getCode (vm : TObject);
var f : TUserFunction;
    om : TMethodDetails;
    i : integer;
    ls, tmpls: TListObject;
begin
  // No arguments for this method
  om :=TVM (vm).popMethodDetails;
  f := TUserFunction (om.self);

  if f.isbuiltInFunction then
     begin
     TVM (vm).push(TStringObject.Create('builtin'));
     exit
     end;
  
  ls := TListObject.Create (0);
  for i := 0 to length (f.codeBlock.code) - 1 do
      begin
      tmpls := TListObject.Create(0);
      tmpls.append (f.codeBlock.code[i].opCode);
      tmpls.append (f.codeBlock.code[i].index);
      if f.codeBlock.code[i].moduleName <> '' then
         tmpls.append (TStringObject.Create (f.codeBlock.code[i].moduleName));
      if f.codeBlock.code[i].symbolName <> '' then
         tmpls.append (TStringObject.Create (f.codeBlock.code[i].symbolName));
      
      ls.append(tmpls);
      end;
  TVM (vm).push(ls);      
end;


procedure TUserFunctionMethods.getHelp (vm : TObject);
var f : TUserFunction;
    m : TMethodDetails;
begin
  // No arguments for this method
  m := TVM (vm).popMethodDetails();
  f := TUserFunction (m.self);

  if f.help <> nil then
     begin
      TVM (vm).push(TStringObject.Create(f.help.getHelp()));
     end
  else
     TVM (vm).push (TStringObject.Create(f.help.getHelp()));
end;
      

// ------------------------------------------------------------------------------------------
// User functions like lists and strings are kept in the heap memory pool, see uMemoryManager
constructor TUserFunction.Create;
begin
  inherited Create;
  objectType := TElementType.symUserFunc;
  isbuiltInFunction := False;
  blockType := btBound;  // bound to the name of the user function
  userFunctionMethods := _userFunctionMethods;
end;


constructor TUserFunction.Create (methodName : string);
begin
  Create;
  nArgs := 0;
  self.methodName := methodName;
  codeBlock := TProgram.Create;
  localSymbolTable := TLocalSymbolTable.Create;
  globalVariableList := TStringList.Create;
  globalVariableList.Sorted := True;
  builtInPtr := nil;
  isbuiltInFunction := False;
end;


// Use this when creating a builtin function
constructor TUserFunction.Create (methodName : string; nArgs : integer; funcPtr : TxBuiltInFunction);
begin
  Create;   // Adds object to memory manager
  self.nArgs := nArgs;
  self.methodName := methodName;
  self.builtInPtr := funcPtr;
  isbuiltInFunction := True;
end;


destructor TUserFunction.Destroy;
begin
  codeBlock.Free;
  localSymbolTable.Free;
  globalVariableList.Free;
  inherited;
end;


function TUserFunction.clone : TDataObject;
var f : TUserFunction;
begin
  f := TUserFunction.Create;
  f.nArgs := self.nArgs;
  f.methodName := self.methodName;
  if help <> nil  then
     f.help := help.clone;
  f.blockType := btBound;
  f.moduleRef := self.moduleRef;
  if self.isbuiltInFunction then
     begin
     f.builtInPtr := self.builtInPtr;
     f.isbuiltInFunction := True;
     end
  else
     begin
     f.moduleName := self.moduleName;
     f.isbuiltInFunction := False;
     f.builtInPtr := nil;
     f.localSymbolTable := self.localSymbolTable.clone;
     f.globalVariableList := TStringList.Create;
     f.globalVariableList.Assign (self.globalVariableList);
     f.codeBlock := self.codeBlock.clone;
   end;
  result := f;
   // Don't forget to add the clone to the heap memory pool
   //memoryList.addNode (result);   ?????
end;


// This tries to get some estimate for the size of the user function
function TUserFunction.getSize : integer;
begin
   result := self.InstanceSize;
   if self.codeBlock <> nil then
      begin
      result := result + self.codeBlock.InstanceSize;
      if length (self.codeBlock.code) > 0 then
        result := result + self.codeBlock.getSize();
      end;
   if self.localSymbolTable <> nil then
      result := result + self.localSymbolTable.InstanceSize;
end;


function TUserFunction.toString : string;
begin
  result := methodName;
end;


// ---------------------------------------------------------------------------------

// Symbols are stored in the module level symbol table
constructor TSymbol.Create;
begin
  dataObject := nil;
  symbolType := symUndefined;
  locked := False;   // used for things that shouldn't be changed, eg math.pi
  //helpStr := 'No help on this symbol';
end;


destructor TSymbol.destroy;
begin
  case symbolType of
    symString      : dataObject.blockType := btGarbage;
    symList        : dataObject.blockType := btGarbage;
    symArray       : dataObject.blockType := btGarbage;
    symVector      : dataObject.blockType := btGarbage;
    symMatrix      : dataObject.blockType := btGarbage;
    symValueObject : dataObject.blockType := btGarbage;
    symNonExistant : begin end;
    symBoolean     : begin end;
    symInteger     : begin end;
    symDouble      : begin end;
    symUndefined   : begin end;
    symModule      : mValue.Free;
    //symObject      : obj.blockType := btGarbage;
    symUserFunc    : dataObject.blockType := btGarbage
  else
     raise Exception.Create('Unknown TSymbol symbolType in destroy: ' + inttostr (integer (symbolType)));
  end;
  inherited;
end;


function TSymbol.clone : TSymbol;
begin
  raise Exception.Create('TSymbol Clone not implemented');
end;


function TSymbol.getSize : integer;
begin
  if dataObject = nil then
     result := 0
  else
     result := dataObject.getSize;
end;


function TSymbol.convertToString : string;
begin
  case symbolType of
    symBoolean    : result := 'boolean';
    symInteger    : result := 'integer';
    symDouble     : result := 'float';
    symString     : result := 'string';
    symList       : result := 'list';
    symArray      : result := 'array';
    symVector     : result := 'vector';
    symMatrix     : result := 'matrix';
    symValueObject: result := 'valueObject';
    symUserFunc   : result := 'function';
    symUndefined  : result := 'Undefined variable';
  else
    raise Exception.Create('toString type in TSymbol not implemented: ' + inttostr (integer (symbolType)));
  end;
end;

// ---------------------------------------------------------------------------------

constructor TSymbolTable.Create;
begin
  // Make the symbol table own the cotnents so that on freeing
  // it will free the contents as well
  inherited Create ([doOwnsValues]);
end;


destructor TSymbolTable.Destroy;
begin
  inherited;
end;



function TSymbolTable.addSymbol (fValue : TUserFunction; locked : boolean) : TSymbol;
var symbol : TSymbol;
begin
  symbol := TSymbol.Create;
  symbol.dataObject := TUserFunction (fValue);
  symbol.symbolName := fValue.methodName;
  symbol.symbolType := symUserFunc;
  symbol.locked := locked;
  Add (fValue.methodName, symbol);
  result := symbol;
end;


function TSymbolTable.addModule (mValue : TModule) : TSymbol;
var symbol : TSymbol;
begin
  symbol := TSymbol.Create;
  symbol.dataObject := nil; // Just to keep things clean
  symbol.dataObject := nil;
  symbol.mValue := mValue;
  symbol.symbolName := TModule (mValue).moduleName;
  symbol.symbolType := symModule;
  symbol.locked := False;
  Add (symbol.symbolName, symbol);
  result := symbol;
end;


procedure TSymbolTable.addSymbol (name : string; dValue : double; locked : boolean; helpStr : string);
var symbol : TSymbol;
begin
  symbol := TSymbol.Create;
  symbol.dValue := dValue;
  symbol.symbolName := name;
  symbol.symbolType := symDouble;
  //symbol.helpStr := helpStr;
  symbol.locked := locked;;
  Add (name, symbol);
end;


procedure TSymbolTable.addSymbol (name : string; lValue : TListObject; locked : boolean; help : THelpModule);
var symbol : TSymbol;
begin
  symbol := TSymbol.Create;
  symbol.dataObject := lValue;
  symbol.symbolName := name;
  symbol.symbolType := symList;
  symbol.dataObject.help := help;
  symbol.locked := locked;;
  Add (name, symbol);
end;


procedure TSymbolTable.addSymbol (name : string; sValue : TStringObject; locked : boolean; help : THelpModule);
var symbol : TSymbol;
begin
  symbol := TSymbol.Create;
  symbol.dataObject := sValue;
  symbol.dataObject.blockType := btBound;  // protect the string object from the garbage collector.
  symbol.symbolName := name;
  symbol.symbolType := symString;
  symbol.dataObject.help := help;
  symbol.locked := locked;;
  Add (name, symbol);
end;


procedure TSymbolTable.addSymbol (name : string; aValue : TArrayObject; locked : boolean; helpStr : string);
var symbol : TSymbol;
begin
  symbol := TSymbol.Create;
  symbol.dataObject := aValue;
  symbol.symbolName := name;
  symbol.symbolType := symArray;
  //symbol.helpStr := helpStr;
  symbol.locked := locked;;
  Add (name, symbol);
end;


procedure TSymbolTable.addSymbol (name : string; vValue : TVectorObject; locked : boolean; helpStr : string);
var symbol : TSymbol;
begin
  symbol := TSymbol.Create;
  symbol.dataObject := vValue;
  symbol.symbolName := name;
  symbol.symbolType := symVector;
  //symbol.helpStr := helpStr;
  symbol.locked := locked;;
  Add (name, symbol);
end;


procedure TSymbolTable.addSymbol (name : string; matValue : TMatrixObject; locked : boolean; helpStr : string);
var symbol : TSymbol;
begin
  symbol := TSymbol.Create;
  symbol.dataObject := matValue;
  symbol.symbolName := name;
  symbol.symbolType := symMatrix;
  //symbol.helpStr := helpStr;
  symbol.locked := locked;;
  Add (name, symbol);
end;


procedure TSymbolTable.addSymbol (name : string; voValue : TValueObject; locked : boolean; help : THelpModule);
var symbol : TSymbol;
begin
  symbol := TSymbol.Create;
  voValue.help := help;
  symbol.dataObject := voValue;
  symbol.symbolName := name;
  symbol.symbolType := symValueObject;
  symbol.locked := locked;;
  Add (name, symbol);
end;


function TSymbolTable.find (name : string; var symbol : TSymbol) : boolean;
begin
  symbol := nil;
  result := False;
  if self.TryGetValue(name, symbol) = True then
     begin
     result := True;
     exit
     end;
end;


function TSymbolTable.addSymbol (name : string) : TSymbol;
var symbol : TSymbol;
begin
  symbol := TSymbol.Create;
  symbol.symbolName := name;
  symbol.dataObject := nil;
  symbol.dataObject := nil;
  symbol.symbolType := symUndefined;
  Add (name, symbol);
  result := symbol;
end;


procedure TSymbolTable.checkForExistingData (symbol : TSymbol);
begin
   if symbol <> nil then

     case symbol.symbolType of
       symList   : begin
                   if symbol.dataObject <> nil then
                       symbol.dataObject.blockType := btGarbage
                   else
                       raise ERuntimeException.Create('Internal Error: checkingForExistingData (list)');
                   end;
       symString : begin
                   if symbol.dataObject <> nil then
                      symbol.dataObject.blockType := btGarbage
                   else
                       raise ERuntimeException.Create('Internal Error: checkingForExistingData (string)');
                   end;
       symArray  : begin
                   if symbol.dataObject <> nil then
                      symbol.dataObject.blockType := btGarbage
                   else
                       raise ERuntimeException.Create('Internal Error: checkingForExistingData (array)');
                   end;
       symVector : begin
                   if symbol.dataObject <> nil then
                      symbol.dataObject.blockType := btGarbage
                   else
                       raise ERuntimeException.Create('Internal Error: checkingForExistingData (vector)');
                   end;
      symMatrix : begin
                  if symbol.dataObject <> nil then
                      symbol.dataObject.blockType := btGarbage
                   else
                       raise ERuntimeException.Create('Internal Error: checkingForExistingData (matrix)');
                   end;
 symValueObject : begin
                  if symbol.dataObject <> nil then
                      symbol.dataObject.blockType := btGarbage
                   else
                       raise ERuntimeException.Create('Internal Error: checkingForExistingData (value object)');
                   end;
     symUserFunc : begin
                   if symbol.dataObject <> nil then
                      begin
                      symbol.dataObject.blockType := btGarbage
                      end
                   else
                       raise ERuntimeException.Create('Internal Error: checkingForExistingData (userfunction)');
                   end;
     end;
end;


procedure TSymbolTable.storeInteger (symbol : TSymbol; iValue : integer);
begin
  checkForExistingData (symbol);

  symbol.iValue := iValue;
  symbol.symbolType := symInteger;
  symbol.helpStr := 'This is an integer value';
end;


procedure TSymbolTable.storeBoolean   (symbol : TSymbol; bValue : boolean);
begin
  checkForExistingData (symbol);

  symbol.bValue := bValue;
  symbol.symbolType := symBoolean;;
end;


procedure TSymbolTable.storeDouble   (symbol : TSymbol; dValue : double);
begin
  checkForExistingData (symbol);

  symbol.dValue := dValue;
  symbol.symbolType := symDouble;
end;


procedure TSymbolTable.storeString (symbol : TSymbol; sValue : TStringObject);
begin
  checkForExistingData (symbol);

  if (sValue.blockType = btConstant) or (sValue.blockType = btBound) then
     symbol.dataObject := sValue.clone as TStringObject
  else
     symbol.dataObject := sValue;

  symbol.dataObject.blockType := btBound;
  symbol.symbolType := symString;
end;


procedure TSymbolTable.storeList   (symbol : TSymbol; lValue : TListObject);
begin
  checkForExistingData (symbol);

  if  (lValue.blockType = btConstant) or
      (lValue.blockType = btBound) or
      (lValue.blockType = btOwned) then
         symbol.dataObject := lValue.clone as TListObject
  else
     symbol.dataObject := lValue;
  symbol.dataObject.blockType := btBound;
  symbol.symbolType := symList;
end;


procedure TSymbolTable.storeArray  (symbol : TSymbol; aValue : TArrayObject);
begin
  checkForExistingData (symbol);

  if  (aValue.blockType = btConstant) or
      (aValue.blockType = btBound) or
      (aValue.blockType = btOwned) then
         symbol.dataObject := aValue.clone as TArrayObject
  else
     symbol.dataObject := aValue;
  symbol.dataObject.blockType := btBound;
  symbol.symbolType := symArray;
end;


procedure TSymbolTable.storeVector  (symbol : TSymbol; vValue : TVectorObject);
begin
  checkForExistingData (symbol);

  if  (vValue.blockType = btConstant) or
      (vValue.blockType = btBound) or
      (vValue.blockType = btOwned) then
         symbol.dataObject := vValue.clone as TVectorObject
  else
     symbol.dataObject := vValue;
  symbol.dataObject.blockType := btBound;
  symbol.symbolType := symVector;
end;


procedure TSymbolTable.storeMatrix  (symbol : TSymbol; matValue : TMatrixObject);
begin
  checkForExistingData (symbol);

  if  (matValue.blockType = btConstant) or
      (matValue.blockType = btBound) or
      (matValue.blockType = btOwned) then
         symbol.dataObject := matValue.clone as TMatrixObject
  else
     symbol.dataObject := matValue;
  symbol.dataObject.blockType := btBound;
  symbol.symbolType := symMatrix;
end;


procedure TSymbolTable.storeValueObject  (symbol : TSymbol; voValue : TValueObject);
begin
  checkForExistingData (symbol);

  if  (voValue.blockType = btConstant) or
      (voValue.blockType = btBound) or
      (voValue.blockType = btOwned) then
         symbol.dataObject := voValue.clone as TValueObject
  else
     symbol.dataObject := voValue;
  symbol.dataObject.blockType := btBound;
  symbol.symbolType := symValueObject;
end;


procedure TSymbolTable.storeFunction (symbol : TSymbol; fValue : TUserFunction);
begin
  checkForExistingData (symbol);

  if (fValue.blockType = btConstant) or
     (fValue.blockType = btBound) or
     (fValue.blockType = btOwned) then
        symbol.dataObject := fValue.clone as TUserFunction
  else
     symbol.dataObject := fValue;

  symbol.dataObject.blockType := btBound;
  symbol.symbolType := symUserFunc;
end;


//procedure TSymbolTable.storeObject (symbol : TSymbol; obj : TDataObject);
//begin
//  if symbol.locked then
//    raise ERuntimeException.Create ('Value is locked, you cannot change it');
//
//  if (obj.blockType = btConstant) or (obj.blockType = btBound) then
//     symbol.obj := obj.clone
//  else
//     symbol.obj := obj;
//
//  symbol.obj.blockType := btBound;
//  symbol.symbolType := symObject;
//end;


procedure TSymbolTable.storeModule (symbol : TSymbol; mValue : TObject);
begin
  if symbol.locked then
    raise ERuntimeException.Create ('Value is locked, you cannot change it');

  checkForExistingData (symbol);

  symbol.symbolType := symModule;
  symbol.mValue := TModule (mValue);
end;

//
//procedure TSymbolTable.storeToSymbolTable (index, iValue : integer);
//begin
//  if self[index] = nil then
//     raise Exception.Create('Symbol: ' + inttostr (index) + ' does not exist');
//
//  if self[index].locked then
//     raise ERuntimeException.Create ('Value is locked, you cannot change it');
//
//  checkForExistingData (index);
//
//  self[index].symbolType := symInteger;
//  self[index].iValue := iValue;
//end;

// ---------------------------------------------------------------------------------

constructor TLocalSymbolTable.Create;
begin
  inherited;
  self.OwnsObjects := False;
end;


destructor TLocalSymbolTable.Destroy;
var i : integer;
begin
  for i := Count - 1 downto 0 do
      begin
      if self[i] <> nil then
        case self[i].symbolType of
           symList        : self[i].dataObject.blockType := btGarbage;
           symString      : self[i].dataObject.blockType := btGarbage;
           symArray       : self[i].dataObject.blockType := btGarbage;
           symVector      : self[i].dataObject.blockType := btGarbage;
           symMatrix      : self[i].dataObject.blockType := btGarbage;
           symValueObject : self[i].dataObject.blockType := btGarbage;
           symUserFunc    : self[i].dataObject.blockType := btGarbage;
        end;
      self[i].free;
      end;
  inherited;
end;


function TLocalSymbolTable.addSymbol (fValue : TUserFunction; locked : boolean) : integer;
var symbol : TSymbol;
begin
  symbol := TSymbol.Create;
  symbol.dataObject := TUserFunction (fValue);
  symbol.symbolName := fValue.methodName;
  symbol.symbolType := symUserFunc;
  symbol.locked := locked;
  result := Add (symbol);
end;


function TLocalSymbolTable.find (name : string; var index : integer) : boolean;
var i : integer;
begin
  result := false;
  for i := 0 to Count - 1 do
      if self[i] <> nil then
         if self[i].symbolName = name then
            begin
            index := i;
            exit (True);
            end;
end;


function TLocalSymbolTable.isUserFunction (name : string; var index : integer) : boolean;
begin
  result := false;
  if find (name, index) then
     begin
     if self[index].symbolType = symUserFunc then
        exit (True);
     end
  else
     exit (False);
end;


function TLocalSymbolTable.reverseFind (name : string; var index : integer): boolean;
var i : integer;
begin
  result := false;
  for i := count - 1  downto 0 do
      if self[i] <> nil then
         if self[i].symbolName = name then
            begin
            index := i;
            exit (True);
            end;
end;


function TLocalSymbolTable.addSymbol (name : string) : integer;
var symbol : TSymbol;
begin
  symbol := TSymbol.Create;
  symbol.symbolName := name;
  symbol.dataObject := nil;
  symbol.dataObject := nil;
  symbol.symbolType := symUndefined;
  result := Add (symbol);
end;


function TLocalSymbolTable.Clone : TLocalSymbolTable;
var i : integer;
begin
  result := TLocalSymbolTable.Create;
  for i := 0 to self.Count - 1 do
      begin
       case self[i].symbolType of
         symUndefined :
             begin
             result.addSymbol (self[i].symbolName);
             result[i].symbolType := symUndefined;
             end;
         symInteger :
             begin
             result.addSymbol (self[i].symbolName);
             result[i].symbolType := symInteger;
             result[i].iValue := self[i].iValue;
             end;
         symDouble :
            begin
            result.addSymbol (self[i].symbolName);
            result[i].symbolType := symDouble;
            result[i].dValue := self[i].dValue;
            end;
         symBoolean :
            begin
            result.addSymbol (self[i].symbolName);
            result[i].symbolType := symBoolean;
            result[i].bValue := self[i].bValue;
            end;
         symString :
            begin
            result.addSymbol (self[i].symbolName);
            result[i].symbolType := symString;
            result[i].dataObject := self[i].dataObject.clone as TStringObject;
            end;
         symList :
            begin
            result.addSymbol (self[i].symbolName);
            result[i].symbolType := symList;
            result[i].dataObject := self[i].dataObject.clone as TListObject;
            end;
         symArray :
            begin
            result.addSymbol (self[i].symbolName);
            result[i].symbolType := symArray;
            result[i].dataObject := self[i].dataObject.clone as TArrayObject;
            end;
         symVector :
            begin
            result.addSymbol (self[i].symbolName);
            result[i].symbolType := symVector;
            result[i].dataObject := self[i].dataObject.clone as TVectorObject;
            end;
         symMatrix :
            begin
            result.addSymbol (self[i].symbolName);
            result[i].symbolType := symMatrix;
            result[i].dataObject := self[i].dataObject.clone as TMatrixObject;
            end;
         symValueObject :
            begin
            raise ERuntimeException.Create('valueObject, clone not implemented');
            end
       else
           raise Exception.Create('Unsupported type in function clone');
       end;

      end;
end;


procedure TLocalSymbolTable.checkForExistingData (index : integer);
begin
   if self[index] <> nil then

     case self[index].symbolType of
       symList   : begin
                   if self[index].dataObject <> nil then
                       self[index].dataObject.blockType := btGarbage
                   else
                       raise ERuntimeException.Create('Internal Error: checkingForExistingData (list)');
                   end;
       symString : begin
                   if self[index].dataObject <> nil then
                      self[index].dataObject.blockType := btGarbage
                   else
                       raise ERuntimeException.Create('Internal Error: checkingForExistingData (string)');
                   end;
      symArray   : begin
                   if self[index].dataObject <> nil then
                      self[index].dataObject.blockType := btGarbage
                   else
                       raise ERuntimeException.Create('Internal Error: checkingForExistingData (array)');
                   end;
      symVector  : begin
                   if self[index].dataObject <> nil then
                      self[index].dataObject.blockType := btGarbage
                   else
                       raise ERuntimeException.Create('Internal Error: checkingForExistingData (vector)');
                   end;
      symMatrix : begin
                   if self[index].dataObject <> nil then
                      self[index].dataObject.blockType := btGarbage
                   else
                       raise ERuntimeException.Create('Internal Error: checkingForExistingData (matrix)');
                   end;
 symValueObject : begin
                   if self[index].dataObject <> nil then
                      self[index].dataObject.blockType := btGarbage
                   else
                       raise ERuntimeException.Create('Internal Error: checkingForExistingData (matrix)');
                   end;
       symUserFunc:begin
                   if self[index].dataObject <> nil then
                      begin
                      self[index].dataObject.blockType := btGarbage
                       //self[index].fValue.Free;
                      //self[index].symbolType := symUndefined;
                      end
                   else
                       raise ERuntimeException.Create('Internal Error: checkingForExistingData (userfunction)');
                   end;
     end;
end;


procedure TLocalSymbolTable.storeToSymbolTable (index, iValue : integer);
begin
  if self[index] = nil then
     raise Exception.Create('Symbol: ' + inttostr (index) + ' does not exist');

  if self[index].locked then
     raise ERuntimeException.Create ('Value is locked, you cannot change it');

  checkForExistingData (index);

  self[index].symbolType := symInteger;
  self[index].iValue := iValue;
end;


procedure TLocalSymbolTable.storeToSymbolTable (index : integer; bValue : boolean);
begin
  if self[index] = nil then
     raise Exception.Create('Symbol: ' + inttostr (index) + ' does not exist');

  if self[index].locked then
     raise ERuntimeException.Create ('Value is locked, you cannot change it');

  checkForExistingData (index);

  self[index].symbolType := symBoolean;
  self[index].bValue := bValue;
end;


procedure TLocalSymbolTable.storeToSymbolTable (index : integer; dValue : double);
begin
  if self[index] = nil then
     raise Exception.Create('Symbol: ' + inttostr (index) + ' does not exist');

  if self[index].locked then
     raise ERuntimeException.Create ('Value is locked, you cannot change it');

  checkForExistingData (index);

  self[index].symbolType := symDouble;
  self[index].dValue := dValue;
end;


procedure TLocalSymbolTable.storeToSymbolTable (index : integer; sValue : TStringObject);
begin
  if self[index] = nil then
     raise Exception.Create('Symbol: ' + inttostr (index) + ' does not exist');

 if self[index].locked then
     raise ERuntimeException.Create ('Value is locked, you cannot change it');

  checkForExistingData (index);

  if (sValue.blockType = btConstant) or (sValue.blockType = btBound) then
     self[index].dataObject := sValue.clone as TStringObject
  else
     self[index].dataObject := sValue;

  self[index].dataObject.blockType := btBound;
  self[index].symbolType := symString;
end;


// Assign a list object to a symbol
procedure TLocalSymbolTable.storeToSymbolTable (index : integer; lValue : TListObject);
begin
  if self[index] = nil then
     raise Exception.Create('Symbol: ' + inttostr (index) + ' does not exist');

 if self[index].locked then
     raise ERuntimeException.Create ('Value is locked, you cannot change it');

  checkForExistingData (index);

  if  (lValue.blockType = btConstant) or
      (lValue.blockType = btBound) or
      (lValue.blockType = btOwned) then
         self[index].dataObject := lValue.clone as TListObject
  else
     self[index].dataObject := lValue;
  self[index].dataObject.blockType := btBound;
  self[index].symbolType := symList;
end;


// Assign a list object to a symbol
procedure TLocalSymbolTable.storeToSymbolTable (index : integer; aValue : TArrayObject);
begin
  if self[index] = nil then
     raise Exception.Create('Symbol: ' + inttostr (index) + ' does not exist');

 if self[index].locked then
     raise ERuntimeException.Create ('Value is locked, you cannot change it');

  checkForExistingData (index);

  if  (aValue.blockType = btConstant) or
      (aValue.blockType = btBound) or
      (aValue.blockType = btOwned) then
         self[index].dataObject := aValue.clone as TArrayObject
  else
     self[index].dataObject := aValue;
  self[index].dataObject.blockType := btBound;
  self[index].symbolType := symArray;
end;


// This isn't currently used, double check
procedure TLocalSymbolTable.storeToSymbolTable (index : integer; fValue : TUserFunction);
begin
  if self[index] = nil then
     raise Exception.Create('Symbol: ' + inttostr (index) + ' does not exist');

 if self[index].locked then
     raise ERuntimeException.Create ('Value is locked, you cannot change it');

  checkForExistingData (index);

  self[index].symbolType := symUserFunc;
  self[index].dataObject := fValue;
end;


function TLocalSymbolTable.getFromSymbolTable (index : integer) : TSymbol;
begin
  if self[index] = nil then
     raise Exception.Create('Symbol: ' + inttostr (index) + ' does not exist');

  result := self[index];
end;



initialization
   TSymbol.mainModuleId := '__main__';
   TSymbol.localModuleId := '_localScope_'; // inside functions
   TSymbol.globalId := 'globalSpace';
   _userFunctionMethods := TUserFunctionMethods.Create;
   SysLibraryRef := nil;
finalization
  _userFunctionMethods.Free;
end.
   


