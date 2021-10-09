unit uSymbolTable;

// This source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

Uses Classes, SysUtils, Generics.Collections, uListObject, uStringObject, uOpCodes,
     uConstantTable, uVMExceptions, uProgramCode, uMemoryManager, uRhodusTypes;

type
   // There are too many dependencies between these classes to separate them
   // into individual files, so we group them together here.

   TSymbol = class;
   TSymbolTable = class;
   TLocalSymbolTable = class;


   // Base module class. When importing a module we'll
   // use the derived class TLibModule
   TModule = class (TObject)
       name : string;
       code : TProgram;      // Module level code
       symbolTable : TSymbolTable;
       helpStr : string;
       compiled : boolean;   // Not currently used

       function    getSize : integer;
       function    find (name : string) : TSymbol;
       procedure   clearCode;
       constructor Create (name : string);
       destructor  Destroy; override;
   end;

   TxBuiltInFunction = procedure (vm : TObject) of object;
   TUserFunction = class (TRhodusObject)
       name : string;
       nArgs : integer;
       localSymbolTable : TLocalSymbolTable;  // The local symbol table is accessed by index not name
       moduleName : string;   // Not actually used at the moment
       moduleRef : TModule;
       globalVariableList : TStringList;
       funcCode : TProgram;
       helpStr : string;
       isbuiltInFunction : boolean;
       builtInPtr : TxBuiltInFunction;

       function    clone : TUserFunction;
       function    getSize : integer;
       constructor Create; overload;
       constructor Create (functionName : string); overload;
       constructor Create (functionName : string; nArgs : integer; funcPtr : TxBuiltInFunction); overload;
       destructor  Destroy; override;
   end;


   TSymbol = class (TObject)
       symbolType : TSymbolElementType;
       symbolName : string;
       helpStr : string;
       locked  : boolean;  // If true, symbol is read only

       // The following fields could be replaced with a single
       // field that we cast each time to the appropriate type
       iValue  : integer;
       dValue  : double;
       bValue  : boolean;
       sValue  : TStringObject;
       lValue  : TListObject;
       fValue  : TUserFunction;
       mValue  : TModule;

       // A convenient place to store these
       class var   mainModuleId : string;
       class var   localModuleId : string;
       class var   globalId : string;

       function    Clone : TSymbol;
       function    toString : string;
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
        procedure addSymbol (name : string; sValue : TStringObject; locked : boolean; helpStr : string); overload;
        procedure addSymbol (name : string; lValue : TListObject; locked : boolean; helpStr : string); overload;
        procedure addSymbol (fValue : TUserFunction; locked : boolean); overload;

        function  find (name : string; var symbol : TSymbol) : boolean;

        procedure storeInteger (symbol : TSymbol; iValue : integer);
        procedure storeBoolean   (symbol : TSymbol; bValue : boolean);
        procedure storeDouble   (symbol : TSymbol; dValue : double);
        procedure storeString (symbol : TSymbol; sValue : TStringObject);
        procedure storeList   (symbol : TSymbol; lValue : TListObject);

        procedure storeFunction (symbol : TSymbol; fValue : TUserFunction);
        procedure storeModule (symbol : TSymbol; mValue : TObject);

        constructor Create;
        destructor  Destroy; override;
   end;


   // Stores user function level symbols, identified in the VM by index
   TLocalSymbolTable = class (TObjectList<TSymbol>)
      private
      public
        function addSymbol (name : string) : integer; overload;
        function addSymbol (name : string; dValue : double;  locked : boolean; helpStr : string) : integer; overload;
        function addSymbol (fValue : TUserFunction; locked : boolean) : integer; overload;
        function find (name : string; var index : integer) : boolean;
        function isUserFunction (name : string; var index : integer) : boolean;
        function reverseFind (name : string; var index : integer): boolean;

        procedure storeToSymbolTable (index, ivalue : integer); overload;
        procedure storeToSymbolTable (index : integer; bValue : boolean); overload;
        procedure storeToSymbolTable (index : integer; dValue : double); overload;
        procedure storeToSymbolTable (index : integer; lValue : TListObject); overload;
        procedure storeToSymbolTable (index : integer; sValue : TStringObject); overload;
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

Uses uBuiltInGlobal, uBuiltinMath, uBuiltInList, uBuiltInRandom, uListOfBuiltIns,
     uBuiltInOS, uBuiltInStr, uBuiltInFile, uBuiltInConfig, uBuiltInSys, uBuiltInTurtle;


function addModule (module : TModule; lib : TModule) : TSymbol;
begin
  result := module.symbolTable.addModule (lib);
end;


procedure addAllBuiltInLibraries (module : TModule);
begin
  // Import the common modules.
  addModule (module, TBuiltInList.Create);
  addModule (module, TBuiltInStr.Create);

  // Needed by the compiler to access the path variable
  SysLibraryRef := TBuiltInSys.Create; addModule (module, SysLibraryRef);

  //addLib (module, TBuiltInTurtle.Create);
end;

// -------------------------------------------------------------------------------

constructor TModule.Create (name : string);
begin
  self.name := name;
  code := TProgram.Create;
  symbolTable := TSymbolTable.Create;
  symbolTable.id := name;
  helpStr := 'No help available on this module';
  compiled := False;
end;


destructor  TModule.Destroy;
begin
  code.Free;
  symbolTable.Free;
  inherited;
end;


procedure TModule.clearCode;
begin
  code.clearCode;
  compiled := False;
end;


function TModule.find (name : string) : TSymbol;
begin
  symbolTable.find(name, result);
end;


function TModule.getSize : integer;
begin
   result := self.InstanceSize;
   if self.code <> nil then
      result := result + length (self.code.code);

   if self.symbolTable <> nil then
      result := result + self.symbolTable.InstanceSize;
end;


// ------------------------------------------------------------------------------------------
// User functions like lists and strings are kept in the heap memory pool, see uMemoryManager
constructor TUserFunction.Create;
begin
  inherited Create;
  objectType := TSymbolElementType.symUserFunc;
  isbuiltInFunction := False;
  blockType := btBound;  // bound to the name of the user function
end;


constructor TUserFunction.Create (functionName : string);
begin
  Create;
  nArgs := 0;
  name := functionName;
  funcCode := TProgram.Create;
  localSymbolTable := TLocalSymbolTable.Create;
  globalVariableList := TStringList.Create;
  globalVariableList.Sorted := True;
  builtInPtr := nil;
  isbuiltInFunction := False;
  helpStr := 'No help on this function';
  memoryList.addNode (self);
end;


// Use this when creating a builtin function
constructor TUserFunction.Create (functionName : string; nArgs : integer; funcPtr : TxBuiltInFunction);
begin
  Create;
  self.nArgs := nArgs;
  name := functionName;
  self.builtInPtr := funcPtr;
  isbuiltInFunction := True;
  memoryList.addNode (self);
end;


destructor TUserFunction.Destroy;
begin
  funcCode.Free;
  localSymbolTable.Free;
  globalVariableList.Free;
  inherited;
end;


function TUserFunction.clone : TUserFunction;
var i : integer;
begin
  result := TUserFunction.Create;
  result.nArgs := self.nArgs;
  result.name := self.name;
  result.blockType := btBound;
  result.moduleRef := self.moduleRef;
  if self.isbuiltInFunction then
     begin
     result.builtInPtr := self.builtInPtr;
     result.isbuiltInFunction := True;
     end
  else
     begin
     result.moduleName := self.moduleName;
     result.helpStr := self.helpStr;
     result.isbuiltInFunction := False;
     result.builtInPtr := nil;
     result.localSymbolTable := self.localSymbolTable.clone;
     result.globalVariableList := TStringList.Create;
     result.globalVariableList.Assign (self.globalVariableList);
     result.funcCode := self.funcCode.Clone;
   end;
   // Don't forget to add the clone to the heap memory pool
   memoryList.addNode (result);
end;


// This tries to get some estimate for the size of the user function
function TUserFunction.getSize : integer;
begin
   result := self.InstanceSize;
   if self.funcCode <> nil then
      begin
      result := result + self.funcCode.InstanceSize;
      result := result + length (self.funcCode.code);
      end;
   if self.localSymbolTable <> nil then
      result := result + self.localSymbolTable.InstanceSize;
end;

// ---------------------------------------------------------------------------------

// Symbols are stored in the module level symbol table
constructor TSymbol.Create;
begin
  lValue := nil;
  sValue := nil;
  fValue := nil;
  symbolType := symUndefined;
  locked := False;   // used for things that shouldn't be changed, eg math.pi
  helpStr := 'No help on this symbol';
end;


destructor TSymbol.destroy;
begin
  case symbolType of
    symString : sValue.blockType := btGarbage;
    symList        : lValue.blockType := btGarbage;
    symNonExistant : begin end;
    symBoolean     : begin end;
    symInteger     : begin end;
    symDouble      : begin end;
    symUndefined   : begin end;
    symModule      : mValue.Free;
    symUserFunc    : begin
                     if fValue.isbuiltInFunction then
                        begin
                        fValue.blockType := btGarbage;
                        end
                     else
                        fValue.blockType := btGarbage;
                     end
  else
     raise Exception.Create('Unknown TSymbol symbolType in destroy: ' + inttostr (integer (symbolType)));
  end;
  inherited;
end;


function TSymbol.Clone : TSymbol;
begin
  raise Exception.Create('TSymbol Clone not implemented');
end;



function TSymbol.toString : string;
begin
  case symbolType of
    symBoolean   : result := 'boolean';
    symInteger   : result := 'integer';
    symDouble    : result := 'float';
    symString    : result := 'string';
    symList      : result := 'list';
    symUserFunc  : result := 'function';
    symUndefined : result := 'Undefined variable';
  else
    raise Exception.Create('toString type in TSymbol not implemented: ' + inttostr (integer (symbolType)));
  end;
end;

// ---------------------------------------------------------------------------------

constructor TSymbolTable.Create;
begin
  inherited Create ([doOwnsValues]);
end;


destructor TSymbolTable.Destroy;
begin
  inherited;
end;



procedure TSymbolTable.addSymbol (fValue : TUserFunction; locked : boolean);
var symbol : TSymbol;
begin
  symbol := TSymbol.Create;
  symbol.fValue := TUserFunction (fValue);
  symbol.symbolName := fValue.name;
  symbol.symbolType := symUserFunc;
  symbol.locked := locked;
  Add (fValue.name, symbol);
end;


function TSymbolTable.addModule (mValue : TModule) : TSymbol;
var symbol : TSymbol;
begin
  symbol := TSymbol.Create;
  symbol.lValue := nil; // Just to keep things clean
  symbol.sValue := nil;
  symbol.mValue := mValue;
  symbol.symbolName := TModule (mValue).name;
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
  symbol.helpStr := helpStr;
  symbol.locked := locked;;
  Add (name, symbol);
end;


procedure TSymbolTable.addSymbol (name : string; lValue : TListObject; locked : boolean; helpStr : string);
var symbol : TSymbol;
begin
  symbol := TSymbol.Create;
  symbol.lValue := lValue;
  symbol.symbolName := name;
  symbol.symbolType := symList;
  symbol.helpStr := helpStr;
  symbol.locked := locked;;
  Add (name, symbol);
end;


procedure TSymbolTable.addSymbol (name : string; sValue : TStringObject; locked : boolean; helpStr : string);
var symbol : TSymbol;
begin
  symbol := TSymbol.Create;
  symbol.sValue := sValue;
  symbol.symbolName := name;
  symbol.symbolType := symString;
  symbol.helpStr := helpStr;
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
  symbol.lValue := nil;
  symbol.sValue := nil;
  symbol.symbolType := symUndefined;
  Add (name, symbol);
  result := symbol;
end;


procedure TSymbolTable.checkForExistingData (symbol : TSymbol);
begin
   if symbol <> nil then

     case symbol.symbolType of
       symList   : begin
                   if symbol.lValue <> nil then
                       symbol.lValue.blockType := btGarbage
                   else
                       raise ERuntimeException.Create('Internal Error: checkingForExistingData (list)');
                   end;
       symString : begin
                   if symbol.sValue <> nil then
                      symbol.sValue.blockType := btGarbage
                   else
                       raise ERuntimeException.Create('Internal Error: checkingForExistingData (string)');
                   end;
       symUserFunc:begin
                   if symbol.fValue <> nil then
                      begin
                      symbol.fValue.blockType := btGarbage
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
     symbol.sValue := sValue.clone
  else
     symbol.sValue := sValue;

  symbol.sValue.blockType := btBound;
  symbol.symbolType := symString;
end;


procedure TSymbolTable.storeList   (symbol : TSymbol; lValue : TListObject);
begin
  checkForExistingData (symbol);

  if  (lValue.blockType = btConstant) or
      (lValue.blockType = btBound) or
      (lValue.blockType = btOwned) then
         symbol.lValue := lValue.clone
  else
     symbol.lValue := lValue;
  symbol.lValue.blockType := btBound;
  symbol.symbolType := symList;
end;


procedure TSymbolTable.storeFunction (symbol : TSymbol; fValue : TUserFunction);
begin
  checkForExistingData (symbol);

  if (fValue.blockType = btConstant) or
     (fValue.blockType = btBound) or
     (fValue.blockType = btOwned) then
        symbol.fValue := fValue.clone
  else
     symbol.fValue := fValue;

  symbol.fValue.blockType := btBound;
  symbol.symbolType := symUserFunc;
end;


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
           symList   : self[i].lValue.blockType := btGarbage;
           symString : self[i].sValue.blockType := btGarbage;
           //symUserFunc : self[i].fValue.Free;
           symUserFunc : self[i].fValue.blockType := btGarbage;
        end;
      self[i].free;
      end;
  inherited;
end;


function TLocalSymbolTable.addSymbol (fValue : TUserFunction; locked : boolean) : integer;
var symbol : TSymbol;
begin
  symbol := TSymbol.Create;
  symbol.fValue := TUserFunction (fValue);
  symbol.symbolName := fValue.name;
  symbol.symbolType := symUserFunc;
  symbol.locked := locked;
  result := Add (symbol);
end;


function TLocalSymbolTable.addSymbol (name : string; dValue : double; locked : boolean; helpStr : string) : integer;
var symbol : TSymbol;
begin
  symbol := TSymbol.Create;
  symbol.dValue := dValue;
  symbol.symbolName := name;
  symbol.symbolType := symDouble;
  symbol.helpStr := helpStr;
  symbol.locked := locked;;
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
  symbol.lValue := nil;
  symbol.sValue := nil;
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
             self[i].iValue := self[i].iValue;
             end;
         symDouble :
            begin
            result.addSymbol (self[i].symbolName);
            result[i].symbolType := symDouble;
            self[i].dValue := self[i].dValue;
            end;
         symBoolean :
            begin
            result.addSymbol (self[i].symbolName);
            result[i].symbolType := symBoolean;
            self[i].bValue := self[i].bValue;
            end;
         symString :
            begin
            result.addSymbol (self[i].symbolName);
            result[i].symbolType := symString;
            self[i].sValue := self[i].sValue.clone;
            end;
         symList :
            begin
            result.addSymbol (self[i].symbolName);
            result[i].symbolType := symList;
            self[i].lValue := self[i].lValue.clone;
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
                   if self[index].lValue <> nil then
                       self[index].lValue.blockType := btGarbage
                   else
                       raise ERuntimeException.Create('Internal Error: checkingForExistingData (list)');
                   end;
       symString : begin
                   if self[index].sValue <> nil then
                      self[index].sValue.blockType := btGarbage
                   else
                       raise ERuntimeException.Create('Internal Error: checkingForExistingData (string)');
                   end;
       symUserFunc:begin
                   if self[index].fValue <> nil then
                      begin
                      self[index].fValue.blockType := btGarbage
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
     self[index].sValue := sValue.clone
  else
     self[index].sValue := sValue;

  self[index].sValue.blockType := btBound;
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
         self[index].lValue := lValue.clone
  else
     self[index].lValue := lValue;
  self[index].lValue.blockType := btBound;
  self[index].symbolType := symList;
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
  self[index].fValue := fValue;
end;


function TLocalSymbolTable.getFromSymbolTable (index : integer) : TSymbol;
begin
  if self[index] = nil then
     raise Exception.Create('Symbol: ' + inttostr (index) + ' does not exist');

  result := self[index];
end;



initialization
   TSymbol.mainModuleId := '_main_';
   TSymbol.localModuleId := '_localScope_'; // inside functions
   TSymbol.globalId := 'globalSpace';
finalization
end.
   


