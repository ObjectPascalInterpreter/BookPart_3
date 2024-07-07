unit uCompile;


// This source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

Uses Classes,
     SysUtils,
     uAST,
     uASTNodeType,
     uSymbolTable,
     uOpCodes,
     Generics.Collections,
     uProgramCode;

type
  TBreakStack = TStack<integer>;
  TSlicingStack = TStack<boolean>;

  TCompilerError  = record
        lineNumber, columnNumber : integer;
        errorMsg : string;
  end;

  TCompiler = class(TObject)
  private
    stackOfBreakStacks: TStack<TBreakStack>;

    code: TProgram;
    currentUserFunction: TUserFunction;
    currentModule: TModule;
    compilingFunction: boolean;
    globalStmt: TASTGlobal;
    inAssignment : boolean;
    inAssignment_NextToEquals : boolean;
    slicingSubscripts : TSlicingStack;

    procedure handleError (node : TASTErrorNode);
    procedure importBuiltIn (moduleName :string; index: integer);

    procedure compileIfStatement(node: TASTIf);

    procedure compileForStatement(node: TASTFor);
    procedure compileForNextStatment (node : TASTFor);
    procedure compileForInStatment (node : TASTFor);

    procedure compileRepeatStatement(node: TASTRepeat);
    procedure compileWhileStatement(node: TASTNode);

    function  checkIfGlobalSymbol (node : TASTIdentifier) : boolean;

    procedure compileRightHandSide (node : TASTIdentifier);
    procedure compileLeftHandSide (node : TASTIdentifier);

    procedure compileAssignment(node: TASTAssignment);
    procedure compileUserFunction(node: TASTNode);
    procedure compileList(node: TASTCreatelist);
    procedure compileCreateVector(node: TASTCreateVector);
    procedure compileCreateMatrix(node: TASTCreateMatrix);
    procedure compileGlobalVariable(node: TASTNode);
    procedure compileStatementList(node: TASTNode);
    procedure compilePowerOperator(node: TASTPowerOp);
    procedure compileBinOperator(node: TASTBinOp; opCode: Byte);
    procedure compileNotOperator (node : TASTNotOp);
    procedure compileUniOperator(node: TASTUniOp; opCode: Byte);
    procedure compilePrintStatement(node: TASTNode);
    procedure compileSetColor (node : TASTNode);
    procedure compileAssert(node: TASTNode);
    procedure compileHelp (node: TASTNode);
    procedure compileSwitchStatement(node: TASTSwitch);
    procedure compileImportStmt(node: TASTImport);
    procedure compileIdentifier (node : TASTIdentifier);

    procedure compilePrimary (node : TASTPrimary);
    procedure compilePrimaryPeriod (node : TASTPrimaryPeriod);
    procedure compilePrimaryIndex (node : TASTPrimaryIndex);
    procedure compilePrimaryFunction (node : TASTPrimaryFunction);
    procedure compileSlice (node : TASTNode);
    procedure compileSliceAll (node : TASTNode);
    procedure compileSubscripts(subscripts: TChildNodes);

    procedure compileCode(node: TASTNode);

  public
    interactive: boolean;
    function startCompilation(module: TModule; node: TASTNode; var error : TCompilerError) : boolean;
    constructor Create(module: TModule);
    destructor Destroy; override;
  end;

  ECompilerException = class(Exception)
      lineNumber, columnNumber : integer;
      errorMsg : string;
      constructor Create (errorMsg : string; lineNumber, columnNumber : integer);
  end;


implementation

Uses uGlobal,
     RTTI,
     uConstantTable,
     uScanner,
     uSyntaxParser,
     uConstructAST,
     IOUtils,
     uBuiltInMath,
     uBuiltInGlobal,
     uLibModule,
     uListObject,
     uMemoryManager,
     uRhodusTypes,
     uStringObject,
     uListOfBuiltins,
     uTokenVector;


constructor ECompilerException.Create (errorMsg : string; lineNumber, columnNumber : integer);
begin
  self.errorMsg := errorMsg;
  self.lineNumber := lineNumber;
  self.columnNumber := columnNumber;
end;


constructor TCompiler.Create(module: TModule);
begin
  inherited Create;
  self.currentModule := module;
  code := module.moduleProgram;
  stackOfBreakStacks := TStack<TBreakStack>.Create;
  compilingFunction := False;
  interactive := False;
  inAssignment := False;
  inAssignment_NextToEquals := False;
  slicingSubscripts := TSlicingStack.Create;
end;


destructor TCompiler.Destroy;
begin
  stackOfBreakStacks.Free;
  slicingSubscripts.Free;
  inherited;
end;


// AST:
// (if) -> (condition) and (thenStatementList) and (elseStatementList)
procedure TCompiler.compileIfStatement(node: TASTIf);
var
  jumpLocation_1, jumpLocation_2: integer;
begin
  compileCode(node.condition);
  jumpLocation_1 := code.addByteCode(oJmpIfFalse, node.lineNumber);
  compileCode(node.thenStatementList);

  if node.elseStatementList <> nil then
    begin
      jumpLocation_2 := code.addByteCode(oJmp, node.lineNumber);
      code.setGotoLabel(jumpLocation_1, code.getCurrentInstructionPointer - jumpLocation_1);
      compileCode(node.elseStatementList);
      code.setGotoLabel(jumpLocation_2, code.getCurrentInstructionPointer - jumpLocation_2);
    end
  else
    code.setGotoLabel(jumpLocation_1, code.getCurrentInstructionPointer - jumpLocation_1);
end;


// forStatement = FOR identifier = expression TO/DOWNTO expression DO statementList END
// AST:
// (for) -> (Iteration) and (body)
// (body) -> (statementList)
// (Iteration) -> (assign) and (upper) and (to/downto)
// (assign) -> (symbol) and (expression)
// (upper) -> (expression)
procedure TCompiler.compileForNextStatment (node : TASTFor);
var jumpLocation_1, jumpLocation_2, again, breakJump: integer;
    loopSymbol: TASTIdentifier;
    breakStack: TStack<integer>;
    localSymbolIndex: integer;
    symbol: TSymbol;
begin
  breakStack := TStack<integer>.Create;
  stackOfBreakStacks.Push(breakStack);

  try
    // There won't be an error condition to check here
    // because assignment is made of symbol and righthand
    // side, both of which were checked during parse time.

    // i = 0
    // Note: this will install the loop variable into the
    // symbol table if it's not there already
    if compilingFunction then
       begin
      if not currentUserFunction.localSymbolTable.find(node.iterationBlock.iterationSymbol.symbolName, localSymbolIndex) then
         localSymbolIndex := currentUserFunction.localSymbolTable.addSymbol (node.iterationBlock.iterationSymbol.symbolName);
       end
    else
       begin
       if not currentModule.symbolTable.find(node.iterationBlock.iterationSymbol.symbolName, symbol) then
          begin
          currentModule.symbolTable.addSymbol(node.iterationBlock.iterationSymbol.symbolName);
          currentModule.symbolTable.find(node.iterationBlock.iterationSymbol.symbolName, symbol); // get the symbol we just created
          end;
       end;

    // HMS watch out local symbols
    compileCode (node.iterationBlock.lower);

    inAssignment := True;
    inAssignment_NextToEquals := True;
    compileCode(node.iterationBlock.iterationSymbol);
    inAssignment := False;
    inAssignment_NextToEquals := False;

    // i > n
    again := code.getCurrentInstructionPointer;
    loopSymbol := node.iterationBlock.iterationSymbol;  // No need to check this for ntError, see comment above

    // Get the symbol table index for the loop symbol
    if compilingFunction then
       begin
       if not currentUserFunction.localSymbolTable.find(loopSymbol.symbolName, localSymbolIndex) then
          raise ECompilerException.Create('Variable: ' + loopSymbol.symbolName + ' not defined', 0, 0);
       end
    else
       begin
       // get the symbol table entry index
       currentModule.symbolTable.find(loopSymbol.symbolName, symbol);
       end;

    // load i onto the stack
    compileCode(loopSymbol);

    // load n onto the stack
    compileCode(node.iterationBlock.upper); // upper limit

    // count up or down
    if node.iterationBlock.direction.nodeType = ntTo then
      code.addByteCode(oIsGt, node.lineNumber)
    else
       code.addByteCode(oIsLt, node.lineNumber);
    jumpLocation_1 := code.addByteCode(oJmpIfTrue, node.lineNumber);

    // Compile the body
    compileCode(node.body);

    // Emit the inc or dec instructions for i

    if node.iterationBlock.direction.nodeType = ntTo then
      begin
        if compilingFunction then
           code.addByteCode(oLocalInc, localSymbolIndex, node.iterationBlock.direction.lineNumber)
        else
           code.addByteCode(oInc, symbol.symbolName, node.iterationBlock.stepValue, node.iterationBlock.iterationSymbol.lineNumber)
       end
    else
       begin
        if compilingFunction then
           code.addLocalForByteCode(oLocalDec, localSymbolIndex, node.iterationBlock.stepValue)
        else
           code.addByteCode(oDec, symbol.symbolName, node.iterationBlock.stepValue, node.iterationBlock.lineNumber);
       end;

    jumpLocation_2 := code.addByteCode(oJmp, node.iterationBlock.lineNumber);
    code.setGotoLabel(jumpLocation_2, again - code.getCurrentInstructionPointer + 1);
    code.setGotoLabel(jumpLocation_1, code.getCurrentInstructionPointer -  jumpLocation_1);

    while breakStack.Count > 0 do
      begin
        breakJump := breakStack.Pop;
        code.setGotoLabel(breakJump, code.getCurrentInstructionPointer - breakJump);
      end;
  finally
    breakStack := stackOfBreakStacks.Pop;
    breakStack.Free;
  end;
end;


// forStatement = FOR identifier IN expression DO statementList END
procedure TCompiler.compileForInStatment (node : TASTFor);
var breakStack: TStack<integer>;
    breakJump: integer;
begin
  raise ECompilerException.Create('"for in" statements not yet implemented', node.lineNumber, 0);

  breakStack := TStack<integer>.Create;
  stackOfBreakStacks.Push(breakStack);

  try

  // CODE HERE
  // Introduce a special op code to do iteration?
  // eg
  // push variable
  // push list
  // code block
  // ??

    while breakStack.Count > 0 do
      begin
        breakJump := breakStack.Pop;
        code.setGotoLabel(breakJump, code.getCurrentInstructionPointer -
          breakJump);
      end;
  finally
    breakStack := stackOfBreakStacks.Pop;
    breakStack.Free;
  end;

end;


procedure TCompiler.compileForStatement(node: TASTFor);
begin
  case node.iterationBlock.forLoopType of
     flForIn   : compileForInStatment (node);
     flForNext : compileForNextStatment (node);
  end;
end;


// AST:
// (while) -> (condition) and (statementList)
procedure TCompiler.compileWhileStatement(node: TASTNode);
var
  breakJump: integer;
  breakStack: TStack<integer>;
  again: integer;
  jumpLocation_exit, jumpLocation_back: integer;
begin
  breakStack := TStack<integer>.Create;
  stackOfBreakStacks.Push(breakStack);
  try
    again := code.getCurrentInstructionPointer;

    // condition code
    compileCode((node as TASTWhile).condition);

    // record the jump location because we're going to patch the relative jump value later
    jumpLocation_exit := code.addByteCode(oJmpIfFalse, node.lineNumber);

    // compile the body of while loop
    compileCode((node as TASTWhile).statementList);

    // Record the location of the 'jump back' to jmp instruction
    jumpLocation_back := code.addByteCode(oJmp, node.lineNumber);

    // Lastly, patch the relative jump instructions
    code.setGotoLabel(jumpLocation_back, again - code.getCurrentInstructionPointer + 1);
    code.setGotoLabel(jumpLocation_exit, code.getCurrentInstructionPointer - jumpLocation_exit);

    while breakStack.Count > 0 do
      begin
        breakJump := breakStack.Pop;
        code.setGotoLabel(breakJump, code.getCurrentInstructionPointer -
          breakJump);
      end;
  finally
    breakStack := stackOfBreakStacks.Pop;
    breakStack.Free;
  end;
end;


// AST:
// (repeat) -> (statementList) and (condition)
procedure TCompiler.compileRepeatStatement(node: TASTRepeat);
var
  again: integer;
  jumpLocation: integer;
  breakJump: integer;
  breakStack1: TStack<integer>;
begin
  breakStack1 := TStack<integer>.Create;
  stackOfBreakStacks.Push(breakStack1);

  try
    again := code.getCurrentInstructionPointer;

    // compile the body of the repeat statement
    compileCode(node.statementList);

    // compile the condition statement in until
    compileCode(node.condition);

    jumpLocation := code.addByteCode(oJmpIfFalse, node.lineNumber);
    code.setGotoLabel(jumpLocation,
      again - code.getCurrentInstructionPointer + 1);

    while breakStack1.Count > 0 do
      begin
        breakJump := breakStack1.Pop;
        code.setGotoLabel(breakJump, code.getCurrentInstructionPointer -
          breakJump);
      end;
  finally
    breakStack1 := stackOfBreakStacks.Pop;
    breakStack1.Free;
  end;
end;


// AST:
// (assignment) -> (left-side) and (right-side)
procedure TCompiler.compileAssignment(node: TASTAssignment);
begin
  // right-hand side first
  compileCode(node.rightSide);
  if node.leftSide.nodeType = ntPrimary then
     begin
     inAssignment := True;
     try
       compileCode (node.leftSide);
     finally
       inAssignment := False;
       // Reset this back to false in case it was set to true.
       inAssignment_NextToEquals := False;
     end;
     end
  else
     raise ECompilerException.Create('Internal Error in compileAssignment', 0, 0)
end;


// primary     =>  factor primaryPlus
// primaryPlus =>
//    primaryPeriod   => '.' identifier primaryPlus
//    primaryIndex    => '[' exp list ']' primaryPlus
//    primaryFunction => '(' exp list ')' primaryPlus
//    empty

// This will be responsible for emiting code that either loads
// a value if its on the right-hand side or stores a value if its
// on the left-hand side. An added problem is that something like:
// a[1] needs to emit a load first for the symbol 'a' then a store for [1].
// There could be any number of layers like this. The item nearest the
// '=' sign needs an actual store bytecode.
procedure TCompiler.compilePrimary (node : TASTPrimary);
var pp : TASTPrimaryPeriod;
begin
  if inAssignment then
     begin
     if node.primaryPlus.nodeType = ntPrimaryPeriod then
        begin
        compileCode (node.factor);
        pp := TASTPrimaryPeriod (node.primaryPlus);
        if pp.primaryPlus.nodeType = ntNull then
           begin
           inAssignment_NextToEquals := True;
           compileCode (pp);
           end
        else
           begin
           inAssignment_NextToEquals := False;
           compilePrimaryPeriod(pp);
           end;
        exit;
        end;

     if node.primaryPlus.nodeType = ntNull then
        begin
        inAssignment_NextToEquals := True;
        compileCode (node.factor);
        end
     else
       begin
       inAssignment_NextToEquals := False;
       compileCode (node.factor);
       compileCode (node.primaryPlus);
       end;

     end
  else
     begin
     compileCode (node.factor);
     compileCode (node.primaryPlus);
     end;
end;



// Deal with code like x = a[2]
procedure TCompiler.compileSubscripts(subscripts: TChildNodes);
var
  i: integer;
  old_inAssignment : boolean;
  slicing : boolean;
  slice : TASTSlice;
begin
  slicing := False;
  // We need to check if there are any slicing requests
  // among the subscripts. If there are then we will
  // treat all subscrupts as a slicing operation eg in a[1:2,3]
  // the 3 is not treated as a subscript but as the slice 3:3, ie a[1:2,3:3]

  // First find out if there is any slicing present in the subscript
  for i := 0 to subscripts.Count - 1 do
      if subscripts[i].nodeType = ntSlice then
         begin
         // We use a stack to record this because there could
         // be slicing syntax in the slicing expressions themselves,
         // eg a[b[2:3]:4]
         slicingSubscripts.push (True);
         slicing := True;  // Yes we detected some slicing
         break;
         end;

  for i := 0 to subscripts.Count - 1 do
      begin
      // Compile the subscript expression
      old_inAssignment := inAssignment;
      inAssignment := False;
      if slicing then
         if subscripts[i].nodeType <> ntSlice then
            begin
            slice := TASTSlice.Create (nil, nil, 0);
            slice.lower := subscripts[i];
            slice.upper := TASTSliceEqual.Create;
            subscripts[i] := slice;  // Substitute the subscripting with slice info
            end;

      // Subscript expressions always use load opcodes, hence set inAssingmet = False
      // If there was slice, we will compile that here
      // If there is no slicing that subscript is what came in at the functi0on call above
      compileCode(subscripts[i]);
      inAssignment := old_inAssignment;

      if inAssignment then
         begin
         // Special case, we only save if its the last index, otherwise we load
         if i = subscripts.Count - 1 then
            code.addByteCode(oSvecIdx, subscripts.Count, subscripts[i].lineNumber)
         else
            code.addByteCode(oLvecIdx, subscripts.Count, subscripts.lineNumber);
         end
      else
         begin
         if not slicing then
            code.addByteCode(oLvecIdx, subscripts.Count, subscripts.lineNumber)
         end;
      end;
  if slicing then
     code.addByteCode(oSliceObj, subscripts.Count, subscripts.lineNumber);

  if slicing then
     slicingSubscripts.Pop;
end;


procedure TCompiler.compileUserFunction(node: TASTNode);
var
  oldCode: TProgram;
  functionNode: TASTUserFunction;
  i: integer;
  symbol: TSymbol;
begin
  functionNode := node as TASTUserFunction;

  // A user function has to be compiled into its own TProgram block
  oldCode := code;
  try
    currentUserFunction := TUserFunction.Create(functionNode.functionName);

    currentUserFunction.nArgs := functionNode.argumentList.list.Count;
    for i := 0 to currentUserFunction.nArgs - 1 do
        currentUserFunction.localSymbolTable.addSymbol ((functionNode.argumentList.list[i] as TASTIdentifier).symbolName);

    // This is a special situation to take care of for recursive functions. The name of the function
    // needs to be in the symbol table before we start building the body of the function.

    if currentModule.symbolTable.find(functionNode.functionName, symbol) then
       begin
       symbol.fValue.blockType := btGarbage;
       symbol.fValue := currentUserFunction;
       end
    else
       currentModule.symbolTable.addSymbol(currentUserFunction, false);

    code := currentUserFunction.codeBlock;

    globalStmt := nil;
    compilingFunction := True;
    compileCode(functionNode.body);
    compilingFunction := False;
    code.addByteCode(oPushNone, functionNode.lineNumber);
    code.addByteCode(oRet, node.lineNumber); // This is to make sure we return
    code.compactCode();
  finally
    code := oldCode;
    currentUserFunction := nil;
  end;
end;


procedure TCompiler.compileList(node: TASTCreateList);
var i: integer;
begin
  if node <> nil then
    begin
      for i := 0 to node.list.Count - 1 do
        compileCode(node.list[i]);
      code.addByteCode(oCreateList, node.list.Count, node.lineNumber);
    end
  else
    code.addByteCode(oCreateList, 0, node.lineNumber); // empty list
end;


procedure TCompiler.compileCreateVector(node: TASTCreateVector);
var i: integer;
begin
  if node <> nil then
    begin
      for i := 0 to node.list.Count - 1 do
        compileCode(node.list[i]);
      code.addByteCode(oCreateVector, node.list.Count, node.lineNumber);
    end
  else
    code.addByteCode(oCreateVector, 0, node.lineNumber); // empty list
end;


procedure TCompiler.compileCreateMatrix(node: TASTCreateMatrix);
var i: integer;
begin
  if node <> nil then
    begin
      for i := 0 to node.list.Count - 1 do
        compileCode(node.list[i]);
      code.addByteCode(oCreateMatrix, node.list.Count, node.lineNumber);
    end
  else
    code.addByteCode(oCreateMatrix, 0, node.lineNumber); // empty list
end;


procedure TCompiler.compileGlobalVariable(node: TASTNode);
var
  symbolIndex: integer;
  astSymbol: TASTIdentifier;
  i: integer;
  symbol: TSymbol;
begin
  globalStmt := node as TASTGlobal;

  for i := 0 to globalStmt.variableList.list.Count - 1 do
    begin
      astSymbol := globalStmt.variableList.list[i] as TASTIdentifier;
      // Check if proposed global variable isn't already a local variable
      // If it is a local variable then we can't declare it as global.
      if currentUserFunction.localSymbolTable.find(astSymbol.symbolName, symbolIndex) then
        raise ECompilerException.Create('Global variable name ' + astSymbol.symbolName + ' already declared as a local variable', 0, 0);

      // Look for the global variable to make sure it exists
      if not currentModule.symbolTable.find(astSymbol.symbolName, symbol) then
        raise ECompilerException.Create('No such global variable exists: ' + astSymbol.symbolName, 0, 0);
    end;
end;


procedure TCompiler.compileStatementList(node: TASTNode);
var
  statList: TASTStatementList;
  i: integer;
begin
  statList := node as TASTStatementList;
  for i := 0 to statList.statementList.Count - 1 do
    compileCode(statList.statementList[i]);
end;

procedure TCompiler.compileBinOperator(node: TASTBinOp; opCode: Byte);
begin
  compileCode(node.left);
  compileCode(node.right);
  code.addByteCode(opCode, node.lineNumber);
end;

procedure TCompiler.compilePowerOperator(node: TASTPowerOp);
begin
  compileCode(node.left);
  compileCode(node.right);
  code.addByteCode(oPower, node.lineNumber);
end;


procedure TCompiler.compileNotOperator (node : TASTNotOp);
begin
  compileCode (node.expression);
  code.addByteCode (oNot, node.lineNumber);
end;


procedure TCompiler.compileUniOperator(node: TASTUniOp; opCode: Byte);
begin
  compileCode(node.left);
  code.addByteCode(opCode, node.lineNumber);
end;

procedure TCompiler.compilePrintStatement(node: TASTNode);
var
  i: integer;
begin
  if node is TASTPrint then
    begin
      for i := 0 to (node as TASTPrint).argumentList.list.Count - 1 do
        compileCode((node as TASTPrint).argumentList.list[i]);

      code.addByteCode(oPushi, (node as TASTPrint).argumentList.list.Count, node.lineNumber);
      code.addByteCode(oPrint, node.lineNumber);
    end
  else
    begin
      for i := 0 to (node as TASTPrintLn).argumentList.list.Count - 1 do
        compileCode((node as TASTPrintLn).argumentList.list[i]);

      code.addByteCode(oPushi, (node as TASTPrintLn).argumentList.list.Count, node.lineNumber);
      code.addByteCode(oPrintln, node.lineNumber);
    end;
end;


procedure TCompiler.compileSetColor (node : TASTNode);
begin
  compileCode((node as TASTSetColor).expression);
  code.addByteCode(oSetColor, node.lineNumber);
end;


procedure TCompiler.compileAssert(node: TASTNode);
begin
  if node is TASTAssertTrue then
    begin
      compileCode((node as TASTAssertTrue).expression);
      code.addByteCode(oAssertTrue, node.lineNumber);
    end
  else
    begin
      compileCode((node as TASTAssertFalse).expression);
      code.addByteCode(oAssertFalse, node.lineNumber);
    end;
end;


procedure TCompiler.compileHelp (node: TASTNode);
begin
  compileCode((node as TASTHelp).expression);
  code.addByteCode(oHelp, node.lineNumber);
end;


procedure TCompiler.compileSwitchStatement(node: TASTSwitch);
var
  listOfCaseStatements: TASTListOfCaseStatements;
  i: integer;
  entryLocation, jumpToEndLocation, jumpToLocation: array of integer;
  caseValues: array of integer;
  lastInstruction: integer;
  elseJump: integer;
  elseDestination: integer;
begin
  listOfCaseStatements := node.caseList as TASTListOfCaseStatements;
  setLength(entryLocation, listOfCaseStatements.list.Count);
  setLength(jumpToEndLocation, listOfCaseStatements.list.Count);
  setLength(jumpToLocation, listOfCaseStatements.list.Count);

  // Get case values
  setLength(caseValues, listOfCaseStatements.list.Count);
  for i := 0 to listOfCaseStatements.list.Count - 1 do
    caseValues[i] := (listOfCaseStatements.list[i] as TASTCaseStatement).caseValue.iValue;

  compileCode(node.switchExpression);
  for i := 0 to listOfCaseStatements.list.Count - 1 do
    begin
      code.addByteCode(oDup, node.switchExpression.lineNumber);
      code.addByteCode(oPushi, caseValues[i], node.lineNumber);
      code.addByteCode(oIsEq, node.switchExpression.lineNumber);
      jumpToLocation[i] := code.addByteCode(oJmpIfTrue, node.switchExpression.lineNumber);
    end;
  elseJump := code.addByteCode(oJmp, node.lineNumber);

  for i := 0 to listOfCaseStatements.list.Count - 1 do
    begin
      entryLocation[i] := code.getCurrentInstructionPointer;
      compileCode((listOfCaseStatements.list[i] as TASTCaseStatement).statementList);
      jumpToEndLocation[i] := code.addByteCode(oJmp, node.lineNumber);
    end;

  elseDestination := code.getCurrentInstructionPointer;
  if node.elseStatement <> nil then
    compileCode(node.elseStatement);

  //code.addByteCode(oNop);
  code.setGotoLabel(elseJump, elseDestination - elseJump);

  lastInstruction := code.addByteCode(oPopDup, node.lineNumber); // pop the dup
  for i := 0 to listOfCaseStatements.list.Count - 1 do
    begin
      code.setGotoLabel(jumpToLocation[i], entryLocation[i] - jumpToLocation[i]);
      code.setGotoLabel(jumpToEndLocation[i], lastInstruction - jumpToEndLocation[i]);
    end;
end;


procedure TCompiler.importBuiltIn (moduleName :string; index: integer);
var ctx : TRttiContext;
    lType:TRttiType;
    instance : TRttiInstanceType;
    value : TValue;
    m : TModule;
begin
   ctx := TRttiContext.Create;
   lType:= ctx.FindType(listOfBuiltIns[index].className);
   if lType<>nil then
      begin
      instance := lType.AsInstance;
      value := instance.GetMethod('Create').Invoke(instance.MetaclassType,[]);
      // Returns a module so cast it
      m := value.AsObject as TModule;
      // This statement effectively imports the module
      uSymbolTable.addModule (currentModule, m);
      end
   else
      raise ECompilerException.Create('Internal Error: Couldn''t find builtin module: ' + moduleName, 0, 0);
end;


procedure TCompiler.compileImportStmt(node: TASTImport);
var
  scanner : TScanner;
  syntaxParser : TSyntaxParser;
  sym: TConstructAST;
  src : string;
  compiler: TCompiler;
  root: TASTNode;
  module: TModuleLib;
  symbol: TSymbol;
  paths : TListObject;
  path : string;
  found : integer;
  index : integer;
  syntaxError : TSyntaxError;
  compilerError : TCompilerError;
begin
  // Check if it's a builtin first
  if listofBuiltIns.find (node.importName, index) then
     begin
     // Then check if its already loaded
     if currentModule.find(node.importName) = nil then
        importBuiltIn (node.importName, index);
     exit;
     end;

  found := -1;
  paths := SysLibraryRef.find ('path').lValue;
  for var i := 0 to paths.list.Count - 1 do
      begin
      if fileExists (paths.list[i].sValue.value + '\\' + node.importName + '.rh') then
         begin
         found := i;
         break;
         end;
      end;
  if found = -1 then
     raise ECompilerException.Create('Unable to locate imported module: ' + node.importName, 0, 0)
  else
     path := paths.list[found].sValue.value + '\\' + node.importName + '.rh';

  // If a module of that name has already been loaded we just get out.
  if currentModule.symbolTable.find(node.importName, symbol) then
     if symbol.symbolType = symModule then
        begin
        //writeln ('Module ' + node.importName + ' is already loaded');
        exit;
        end;

  // Check if we're trying to load the same module into itself
  if currentModule.moduleName = node.importName then
     raise ECompilerException.Create('Warning: Attempted import module <' + node.importName + '> into itself.', 0, 0);

  // Otherwise lets read it in
  scanner := TScanner.Create;
  syntaxParser := TSyntaxParser.Create (scanner);
  sym := TConstructAST.Create(syntaxParser.tokenVector);

  try
     src := TFile.ReadAllText(path);
     scanner.scanString(src);
     if syntaxParser.syntaxCheck(syntaxError) then
        begin
        module := sym.buildModuleAST(node.importName, root);

        addGlobalMethodsToModule (module);
        addAllBuiltInLibraries(module);

        // Add the name of the module to the current module's
        // symbol table, because user functions might need it for globals
        currentModule.symbolTable.addModule(module);

        compiler := TCompiler.Create(module);
        try
          if not compiler.startCompilation(module, root, compilerError) then
             raise ECompilerException.Create (compilerError.errorMsg, compilerError.lineNumber, compilerError.columnNumber);

          module.moduleProgram.addByteCode(oHalt, 0);
        finally
          root.Free;
          compiler.Free;
        end;

        // import the module
        currentModule.moduleProgram.addModuleByteCode(oImportModule, node.importName);
        end
     else
        raise ECompilerException.Create('In module: ' + node.importName + ' ' + syntaxError.errorMsg, syntaxError.lineNumber, syntaxError.columnNumber);
   finally
      scanner.Free;
      syntaxParser.Free;
      sym.Free;
   end;
end;


function TCompiler.checkIfGlobalSymbol (node : TASTIdentifier) : boolean;
var index : integer;
begin
  if globalStmt = nil then
     result := false
  else
      begin
      if globalStmt.find (node.symbolName, index) then
         result := true
      else
         result := false;
      end;
end;


// = x
procedure TCompiler.compileRightHandSide (node : TASTIdentifier);
var symbol : TSymbol;
    localSymbolIndex : integer;
begin
   // if we're in a user function we could be dealing
   // with a global or local symbol.
   if compilingFunction then
      begin
      if checkIfGlobalSymbol (node) then
         code.addSymbolByteCode (oLoadSymbol, node.symbolName)
      else
         begin
         // Check if its in the local space, if yes then emit the local load opecode
         // if not it could be in the module space
         if currentUserFunction.localSymbolTable.find(node.symbolName, localSymbolIndex) then
            code.addByteCode(oLoadLocal, localSymbolIndex, node.lineNumber)
         else
            begin
            if currentModule.symbolTable.find(node.symbolName, symbol) then
               code.addSymbolByteCode (oLoadSymbol, node.symbolName)
            else
              // This means we couldn't find the symbol at the local or module level
              raise ECompilerException.Create('Undeclared variable: ' + node.symbolName, 0, 0);
            end;
         end;
      end
   else
      code.addSymbolByteCode (oLoadSymbol, node.symbolName);
 end;


// x =
procedure TCompiler.compileLeftHandSide (node : TASTIdentifier);
var symbol : TSymbol;
    localSymbolIndex : integer;
begin
  if compilingFunction then
      begin
      if checkIfGlobalSymbol (node) then
         begin
         // Depending on whether we are next to a '=' or not....
         if inAssignment_NextToEquals then
            code.addSymbolByteCode (oStoreSymbol, node.symbolName)
         else
            code.addSymbolByteCode (oLoadSymbol, node.symbolName)
         end
      else
         begin
         if not currentUserFunction.localSymbolTable.find(node.symbolName, localSymbolIndex) then
            localSymbolIndex := currentUserFunction.localSymbolTable.addSymbol (node.symbolName);
         if inAssignment_NextToEquals then
            code.addByteCode (oStoreLocal, localSymbolIndex, node.lineNumber)
         else
            code.addByteCode (oLoadLocal, localSymbolIndex, node.lineNumber)
         end
      end
   else
      begin
      // If the symbol is new, add it to the symboltable
      if not currentModule.symbolTable.find(node.symbolName, symbol) then
         currentModule.symbolTable.addSymbol(node.symbolName);

      // Depending on whether we are next to a '=' or not....
      if inAssignment_NextToEquals then
         code.addSymbolByteCode (oStoreSymbol, node.symbolName)
      else
         code.addSymbolByteCode (oLoadSymbol, node.symbolName)
      end
end;


procedure TCompiler.compileIdentifier (node : TASTIdentifier);
begin
   if inAssignment then
      compileLeftHandSide (node)
   else
      compileRightHandSide (node);
end;


procedure TCompiler.compilePrimaryPeriod (node : TASTPrimaryPeriod);
begin
  if inAssignment then
     begin
     // Depending on whether we are next to a '=' or not....
     if inAssignment_NextToEquals then
         code.addSymbolByteCode (oStoreAttr, node.identifier.symbolName)
      else
         code.addSymbolByteCode (oLoadAttr, node.identifier.symbolName);
     compileCode (node.primaryPlus);
     end
  else
     begin
     code.addSymbolByteCode (oLoadAttr, node.identifier.symbolName);
     compileCode (node.primaryPlus);
     end;
end;


procedure TCompiler.compilePrimaryIndex (node : TASTPrimaryIndex);
begin
   compileSubscripts(node.subscriptList.list);
   compileCode (node.primaryPlus);
end;


procedure TCompiler.compilePrimaryFunction (node : TASTPrimaryFunction);
var anode : TASTNode;
begin
  // Check if we are at the last specifir in the expression
  // If true and we parsing the left-hand side of an assignment
  // then this is an illegal operation.
  if (node.primaryPlus.nodeType = ntNull) and inAssignment then
     raise ECompilerException.Create('You can''t assign to a user function', 0 , 0);

  for anode in node.argumentList.list do
      compileCode (anode);

  code.addByteCode(oCall, node.argumentList.list.Count, node.lineNumber);
  compileCode (node.primaryPlus);
end;


procedure TCompiler.compileSlice (node : TASTNode);
var sliceNode : TASTSlice;
begin
  sliceNode := TASTSlice (node);
  compileCode (sliceNode.lower);
  compileCode (sliceNode.upper);
  code.addByteCode(oBuildSlice, node.lineNumber);
end;


procedure TCompiler.compileSliceAll (node : TASTNode);
begin
  code.addByteCode(oSliceAll, node.lineNumber);
end;


procedure TCompiler.handleError (node : TASTErrorNode);
begin
  raise ESyntaxException.Create(node.errorMsg, node.lineNumber, node.columnNumber);
end;


procedure TCompiler.compileCode(node: TASTNode);
var index : integer;
begin
  if node = nil then
    exit;

  case node.nodeType of
    ntError :
      handleError (node as TASTErrorNode);
    ntIf:
      compileIfStatement(node as TASTIf);
    ntFor:
      compileForStatement(node as TASTFor);
    ntRepeat:
      compileRepeatStatement(node as TASTRepeat);
    ntWhile:
      compileWhileStatement(node);
    ntSwitch:
      compileSwitchStatement(node as TASTSwitch);
    ntIdentifier :
      compileIdentifier (node as TASTIdentifier);
    ntPrimary:
       compilePrimary(node as TASTPrimary);
    ntPrimaryPeriod:
      compilePrimaryPeriod (node as TASTPrimaryPeriod);
    ntPrimaryIndex:
      compilePrimaryIndex(node as TASTPrimaryIndex);
    ntPrimaryFunction:
      compilePrimaryFunction (node as TASTPrimaryFunction);
    ntSlice:
      compileSlice (node);
    ntSliceAll:
      compileSliceAll (node);
    ntSliceEqual:
       code.addByteCode(oPushi, SLICE_EQUAL, 0);
    ntCreateList:
      compileList(node as TASTCreateList);
    ntCreateVector:
      compileCreateVector(node as TASTCreateVector);
    ntCreateMatrix:
      compileCreateMatrix(node as TASTCreateMatrix);
    ntAssignment:
      compileAssignment (node as TASTAssignment);
    // An expression on its own, has to be dealt with separately
    // All exprssions including functions return something even
    // if its a none type. This is dealt differently whether
    // we evaluate the expression at the console or inside a script.
    ntExpressionStatement:
      begin
        compileCode((node as TASTExpressionStatement).expression);
        //if not interactive then
          code.addByteCode(oPop, node.lineNumber);
      end;
    ntFunction:
      compileUserFunction(node);
    ntReturn:
      begin
        compileCode((node as TASTReturn).expression);
        code.addByteCode(oRet, node.lineNumber);
      end;
    ntGlobalStmt:
      compileGlobalVariable(node);
    ntImportStmt:
      compileImportStmt(node as TASTImport);
    ntExpression:
      compileCode((node as TASTExpression).expression);
    ntStatementList:
      compileStatementList(node);
    ntPrint, ntPrintln:
      compilePrintStatement(node);
    ntSetColor :
      compileSetColor (node);
    ntAssertTrue, ntAssertFalse:
      compileAssert(node);
    ntHelp :
       compileHelp (node);

    ntAdd:
      compileBinOperator(node as TASTBinOp, oAdd);
    ntSub:
      compileBinOperator(node as TASTBinOp, oSub);
    ntMult:
      compileBinOperator(node as TASTBinOp, oMult);
    ntDiv:
      compileBinOperator(node as TASTBinOp, oDivide);
    ntPower:
      compilePowerOperator(node as TASTPowerOp);
    ntMod:
      compileBinOperator(node as TASTBinOp, oMod);
    ntDivI:
      compileBinOperator(node as TASTBinOp, oDivI);
   ntDotProduct:
      compileBinOperator(node as TASTBinOp, oDotProduct);
    ntAND:
      compileBinOperator(node as TASTBinOp, oAnd);
    ntOR:
      compileBinOperator(node as TASTBinOp, oOr);
    ntXOR:
      compileBinOperator(node as TASTBinOp, oXor);
    ntNOT:
      compileNotOperator(node as TASTNotOp);
    ntUnaryMinus:
      compileUniOperator(node as TASTUniOp, oUmi);

    ntEQ:
      compileBinOperator(node as TASTBinOp, oIsEq);
    ntNE:
      compileBinOperator(node as TASTBinOp, oIsNotEq);
    ntGT:
      compileBinOperator(node as TASTBinOp, oIsGt);
    ntLT:
      compileBinOperator(node as TASTBinOp, oIsLt);
    ntGE:
      compileBinOperator(node as TASTBinOp, oIsGte);
    ntLE:
      compileBinOperator(node as TASTBinOp, oIsLte);

    ntBoolean:
      code.addByteCode(oPushb, (node as TASTBoolean).bValue, node.lineNumber);
    ntInteger:
      code.addByteCode(oPushi, (node as TASTInteger).iValue, node.lineNumber);
    ntFloat:
      begin
      index := currentModule.moduleProgram.constantValueTable.Add (TConstantValueElement.Create((node as TASTFloat).dValue));
      code.addByteCode(oPushd, index, node.lineNumber);
      end;
    ntString :
      begin
      index := currentmodule.moduleProgram.constantValueTable.Add (TConstantValueElement.Create ((node as TASTString).sValue));
      code.addByteCode(oPushs, index, node.lineNumber);
      end;
    ntBreak:
      // place holder for the jmp instruction
      stackOfBreakStacks.Peek.Push(code.addByteCode(oJmp, node.lineNumber));
    ntNull : begin end;
   else
      raise ECompilerException.Create('Internal error: Unrecognized node type in AST (compileCode): ' +   TRttiEnumerationType.GetName(node.nodeType), 0, 0);
  end;
end;


function TCompiler.startCompilation(module: TModule; node: TASTNode; var error : TCompilerError) : boolean;
begin
  result := True;
  try
    currentModule := module;
    compileCode(node);
    module.compiled := True;
  except
    on e: ECompilerException do
       begin
       error.errorMsg := e.errorMsg;
       error.lineNumber := e.lineNumber;
       error.columnNumber := e.columnNumber;
       result := False;
       end;
  end;
end;


initialization

end.




