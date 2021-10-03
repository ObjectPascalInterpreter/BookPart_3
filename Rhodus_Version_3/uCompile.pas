unit uCompile;


// This source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

Uses Classes, SysUtils, uAST, uASTNodeType, uSymbolTable, uOpCodes,
  Generics.Collections, uProgramCode;

type
  TBreakStack = TStack<integer>;

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

    procedure handleError (node : TASTErrorNode);
    function isLocal(primary: TASTPrimary): boolean;
    function  isLocal2(primary: TASTPrimary): boolean;
    procedure importBuiltIn (moduleName :string; index: integer);

    procedure visitNode(node: TASTNode);
    procedure compileIfStatement(node: TASTIf);
    procedure compileForStatement(node: TASTFor);
    procedure compileRepeatStatement(node: TASTRepeat);
    procedure compileWhileStatement(node: TASTNode);

    procedure compilePrimaryLoad (node : TASTPrimaryOld);
    procedure compilePrimaryLoad2 (node : TASTPrimary);

    procedure compilePrimaryStore2(primary: TASTPrimary);

    procedure compilePeriod (node : TASTPeriod);

    procedure compileSubscriptsStore(subscripts: TChildNodes; isLocal: boolean);
    procedure compileSubscripts(subscripts: TChildNodes);

    procedure writeOutPrimaryLoadCode (node : TASTPrimaryOld);  // Helper method
    procedure compilePrimaryLoadInFunction (node : TAStPrimaryOld);
    procedure compilePrimaryLoadOutSideFunction (node : TAStPrimaryOld);

    function  checkIfGlobalSymbol (node : TASTIdentifier) : boolean;

    procedure compileRightHandSide (node : TASTIdentifier);
    procedure compileLeftHandSide (node : TASTIdentifier);

    procedure compileAssignment(node: TASTAssignment2);
    procedure compileUserFunction(node: TASTNode);
    procedure compileList(node: TASTCreatelist);
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
    procedure compilePrimaryPeriod (node : TASTPrimaryPeriod);
    procedure compilePrimaryIndex (node : TASTPrimaryIndex);
    procedure compilePrimaryFunction (node : TASTPrimaryFunction);

    procedure compileCode(node: TASTNode);

  public
    interactive: boolean;
    procedure startCompilation(module: TModule; node: TASTNode);
    constructor Create(module: TModule);
    destructor Destroy; override;
  end;

  ECompilerException = class(Exception);

implementation

Uses uGlobal,
     RTTI,
     uConstantTable,
     uScanner,
     uConstructAST,
     IOUtils,
     uBuiltInMath,
     uBuiltInGlobal,
     uLibModule,
     uListObject,
     uMemoryManager,
     uRhodusTypes,
     uStringObject,
     uListOfBuiltins;


constructor TCompiler.Create(module: TModule);
begin
  inherited Create;
  self.currentModule := module;
  code := module.code;
  stackOfBreakStacks := TStack<TBreakStack>.Create;
  compilingFunction := False;
  interactive := False;
  inAssignment := False;
  inAssignment_NextToEquals := False;
end;


destructor TCompiler.Destroy;
begin
  stackOfBreakStacks.Free;
  inherited;
end;


// If we're in a user fucntion then symbol could be local
// If the symbol is in the global variable list then its not local
function TCompiler.isLocal(primary: TASTPrimary): boolean;
var
  i: integer;
  globalSymbol: TASTPrimaryOld;
begin
  result := false;

  result := compilingFunction;
  result := false;
  // Takes care of recursion
  //if currentUserFunction <> nil then
  //  if currentUserFunction.name = primary.primaryName then
  //    exit(False);

//  if globalStmt <> nil then
//    begin
//      for i := 0 to globalStmt.variableList.list.Count - 1 do
//        begin
//          globalSymbol := globalStmt.variableList.list[i] as TASTPrimaryOld;
//          if globalSymbol.primaryName = primary.primaryName then
//            exit(False);
//        end;
//    end;
end;


// If we're in a user fucntion then symbol could be local
// If the symbol is in the global variable list then its not local
function TCompiler.isLocal2(primary: TASTPrimary): boolean;
var
  i: integer;
  globalSymbol: TASTPrimaryOld;
begin
  exit (False);

  result := compilingFunction;

//  // Takes care of recursion
//  if currentUserFunction <> nil then
//    if currentUserFunction.name = primary.symbolName then
//      exit(False);
//
//  if globalStmt <> nil then
//    begin
//      for i := 0 to globalStmt.variableList.list.Count - 1 do
//        begin
//          globalSymbol := globalStmt.variableList.list[i] as TASTPrimaryOld;
//          if globalSymbol.primaryName = primary.symbolName then
//            exit(False);
//        end;
//    end;
end;

// AST:
// (if) -> (condition) and (thenStatementList) and (elseStatementList)
procedure TCompiler.compileIfStatement(node: TASTIf);
var
  jumpLocation_1, jumpLocation_2: integer;
begin
  compileCode(node.condition);
  jumpLocation_1 := code.addByteCode(oJmpIfFalse);
  compileCode(node.thenStatementList);

  if node.elseStatementList <> nil then
    begin
      jumpLocation_2 := code.addByteCode(oJmp);
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
procedure TCompiler.compileForStatement(node: TASTFor);
var
  jumpLocation_1, jumpLocation_2, again, breakJump: integer;
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
          raise ECompilerException.Create('Variable: ' + loopSymbol.symbolName + ' not defined');
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
      code.addByteCode(oIsGt)
    else
       code.addByteCode(oIsLt);
    jumpLocation_1 := code.addByteCode(oJmpIfTrue);

    // Compile the body
    compileCode(node.body);

    // Emit the inc or dec instructions for i

    if node.iterationBlock.direction.nodeType = ntTo then
      begin
        if compilingFunction then
           code.addByteCode(oLocalInc, localSymbolIndex)
        else
           code.addByteCode(oInc, symbol.symbolName, node.iterationBlock.stepValue)
       end
    else
       begin
        if compilingFunction then
           code.addLocalForByteCode(oLocalDec, localSymbolIndex, node.iterationBlock.stepValue)
        else
           code.addByteCode(oDec, symbol.symbolName, node.iterationBlock.stepValue);
       end;

    jumpLocation_2 := code.addByteCode(oJmp);
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
    jumpLocation_exit := code.addByteCode(oJmpIfFalse);

    // compile the body of while loop
    compileCode((node as TASTWhile).statementList);

    // Record the location of the 'jump back' to jmp instruction
    jumpLocation_back := code.addByteCode(oJmp);

    // Lastly, patch the relative jump instructions
    code.setGotoLabel(jumpLocation_back,
      again - code.getCurrentInstructionPointer + 1);
    code.setGotoLabel(jumpLocation_exit, code.getCurrentInstructionPointer -
      jumpLocation_exit);

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

    jumpLocation := code.addByteCode(oJmpIfFalse);
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
procedure TCompiler.compileAssignment(node: TASTAssignment2);
begin
  // right-hand side first
  compileCode(node.rightSide);
  if node.leftSide.nodeType = ntPrimary then
     begin
     inAssignment := True;
     try
       compileCode (node.leftSide);
     finally
       inAssignment := False
     end;

     end
  else
     raise ECompilerException.Create('Internal Error in compileAssignment')
end;


procedure TCompiler.visitNode(node: TASTNode);
var index : integer;
begin
  case node.nodeType of
    ntBreak:
      stackOfBreakStacks.Peek.Push(code.addByteCode(oJmp));
      // place holder for the jmp instruction
  else
    raise ECompilerException.Create('Unrecognized node type in AST: ' +   TRttiEnumerationType.GetName(node.nodeType));
  end;
end;


procedure TCompiler.compilePrimaryLoadInFunction (node : TASTPrimaryOld);
var i, j : integer;
    symbol: TSymbol;
    userFunc: TASTFunctionCall;
    localSymbolIndex : integer;
begin
   // If it's not in the local space check the module space
   if not currentUserFunction.localSymbolTable.find(node.primaryName, localSymbolIndex) then
      begin
      if currentModule.symbolTable.find(node.primaryName, symbol) then
         compilePrimaryLoadOutSideFunction (node)
       else
         // This means we couldn't find the symbol at the local or module level
         raise ECompilerException.Create('Undeclared variable: ' + node.primaryName);
       exit;
     end
   else
     // If we got here it means it did find a local symbol
     code.addByteCode(oLoadLocal, localSymbolIndex, -1);

   for i := 0 to node.nodes.Count - 1 do
       case node.nodes[i].nodeType of
            ntPeriod :
               begin
               code.addStoreByteCode(oLoadSecondary, (node.nodes.list[i] as TASTPeriod).name);
               end;
        ntFunctionCall:
              begin
              userFunc := node.nodes.list[i] as TASTFunctionCall;
              // Compile any arguments to the function call
              for j := 0 to userFunc.argumentList.list.Count - 1 do
                 compileCode(userFunc.argumentList.list[j]);

              code.addByteCode(oCall, userFunc.argumentList.list.Count);
              // <- userFunc.argumentList.Count used to test for arity at run time
             end;
        ntSubscript:
            begin
            compileSubscripts((node.nodes.list[i] as TASTSubscript).subscripts.list);
            // True means local
            end;
        ntError:
            handleError (node.nodes.list[i] as TASTErrorNode);
       end;
end;


procedure TCompiler.writeOutPrimaryLoadCode (node : TASTPrimaryOld);
var i, j : integer;
    userFunc: TASTFunctionCall;
begin
   code.addSymbolByteCode (oLoadSymbol, node.primaryName);
   for i := 0 to node.nodes.Count - 1 do
       case node.nodes.list[i].nodeType of
          ntPeriod :
             begin
             code.addStoreByteCode(oLoadSecondary, (node.nodes.list[i] as TASTPeriod).name);
             end;
      ntFunctionCall:
            begin
            userFunc := node.nodes.list[i] as TASTFunctionCall;
            // Compile any arguments to the function call
            for j := 0 to userFunc.argumentList.list.Count - 1 do
               compileCode(userFunc.argumentList.list[j]);

            code.addByteCode(oCall, userFunc.argumentList.list.Count);
           // <- arg count used to test for arity at run time
           end;
      ntSubscript:
          begin
           compileSubscripts((node.nodes.list[i] as TASTSubscript).subscripts.list);
           // false = means not local
          end;
      ntError :
          handleError (node.nodes.list[i] as TASTErrorNode);
       end;
end;


procedure TCompiler.compilePrimaryLoadOutSideFunction (node : TASTPrimaryOld);
var symbol: TSymbol;
begin
  if currentModule.symbolTable.find(node.primaryName, symbol) then
     writeOutPrimaryLoadCode (node)
  else
     raise ECompilerException.Create('Undeclared variable: ' + node.primaryName);
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
// a[1] needs to emit a load first for a then a store for [1]. There
// could be any number of layers like this, on the item nearest the
// '=' sign needs an actual store bytecode.
procedure TCompiler.compilePrimaryLoad2 (node : TASTPrimary);
var pm : TASTPrimaryIndex;
    pf : TASTPrimaryFunction;
    index, j: integer;
begin
  if inAssignment then
     begin
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


// This is a term that we've found on the right-hand side.
// It doesn't involve terms on the left-side that store data
procedure TCompiler.compilePrimaryLoad (node : TASTPrimaryOld);
begin
  if compilingFunction then
     compilePrimaryLoadInFunction (node)
  else
     compilePrimaryLoadOutSideFunction (node);
end;


// Compile something like X.b
procedure TCompiler.compilePeriod (node : TASTPeriod);
var symbol: TSymbol;
begin
  if currentModule.symbolTable.find(node.name, symbol) then
     code.addSymbolByteCode (oLoadSecondary, node.name)
  else
    raise ECompilerException.Create('Undeclared variable: ' + node.name);
end;


// Deal with code like a[2] = 3
procedure TCompiler.compileSubscriptsStore(subscripts: TChildNodes; isLocal: boolean);
var
  i: integer;
begin
  if subscripts.Count = 1 then
     begin
     compileCode(subscripts[0]);
     if isLocal then
       code.addByteCode(oLocalSvecIdx)   // Note this is a store opcode
     else
       code.addByteCode(oSvecIdx);  // Note this is a store opcode
     end
  else
    begin
      for i := 0 to subscripts.Count - 2 do
          begin
          // Note that in  situation like a[1][2][3][4] = 4, we use the load opcode
          // on the first three subscripts and issue a store right at the end.
          compileCode(subscripts[i]);
          if isLocal then
            code.addByteCode(oLocalLvecIdx)
          else
            code.addByteCode(oLvecIdx);
          end;
      compileCode(subscripts[subscripts.Count - 1]);
      if isLocal then
        code.addByteCode(oLocalSvecIdx)  // Note this is a store opcode
      else
        code.addByteCode(oSvecIdx);   // Note this is a store opcode
    end;
end;


// Deal with code like x = a[2]
procedure TCompiler.compileSubscripts(subscripts: TChildNodes);
var
  i: integer;
  old_inAssignment : boolean;
begin
  for i := 0 to subscripts.Count - 1 do
      begin
      // Compile the subscript expression
      old_inAssignment := inAssignment;
      inAssignment := False;
      // Subscript expressions always use load opcodes, hence set inAssignmet = False
      compileCode(subscripts[i]);
      inAssignment := old_inAssignment;

      if inAssignment then
         begin
         // Special case, we only save is its the last index, otherwise we load
         if i = subscripts.Count - 1 then
            code.addByteCode(oSvecIdx)
         else
            code.addByteCode(oLvecIdx);
         end
      else
         code.addByteCode(oLvecIdx)
      end;
end;


procedure TCompiler.compilePrimaryStore2(primary: TASTPrimary);
var
  symbol: TSymbol;
  localSymbolIndex: integer;
  i, j : integer;
  userFunc : TASTFunctionCall;
  lastOne : integer;
begin
  if primary.primaryPlus.nodeType = ntNull then
     case primary.factor.nodeType of
         ntIdentifier :
            begin
            if not currentModule.symbolTable.find((primary.factor as TASTIdentifier).symbolName, symbol) then
               begin
               currentModule.symbolTable.addSymbol((primary.factor as TASTIdentifier).symbolName);
               currentModule.symbolTable.find((primary.factor as TASTIdentifier).symbolName, symbol); // get the symbol we just created
               end;
            code.addSymbolByteCode(oStoreSymbol, (primary.factor as TASTIdentifier).symbolName);
            end
     else
         raise ECompilerException.Create('Cannot assign to factor');
     end
  else
     begin


     end;


//  if compilingFunction then
//    begin
//    if isLocal2(primary) then
//       // Don't store global variables in the local symbol table
//       begin
//       if not currentUserFunction.localSymbolTable.find(primary.symbolName, localSymbolIndex) then
//          localSymbolIndex := currentUserFunction.localSymbolTable.addSymbol (primary.symbolName);
//       end;
//    end
//  else
//    begin
//      if not currentModule.symbolTable.find(primary.symbolName, symbol) then
//         begin
//         currentModule.symbolTable.addSymbol(primary.symbolName);
//         currentModule.symbolTable.find(primary.symbolName, symbol); // get the symbol we just created
//         end;
//    end;
//  code.addSymbolByteCode(oStoreSymbol, primary.symbolName);

//  if isLocal2(primary) then
//    begin
//    if primary.nodes.Count > 0 then
//       begin
//       code.addByteCode(oLoadLocal, localSymbolIndex);
//       for i := 0 to primary.nodes.Count - 1 do
//           begin
//           case primary.nodes.list[i].nodeType of
//              ntSubscript :
//                 begin
//                 compileSubscriptsStore((primary.nodes[i] as TASTSubscript).subscripts.list, isLocal(primary as TASTPrimary));
//                 end;
//              ntPeriod :
//                 begin
//                 // Doesn't work
//                 code.addStoreByteCode(oLoadSecondary, ((primary.nodes.list[i] as TASTPeriod).name));
//                 end
//           else
//              raise ECompilerException.Create ('Unsupported left-hand side assignment');
//           end;
//           end;
//       end
//    else
//       code.addByteCode(oStoreLocal, localSymbolIndex);
//    end
//  else
//    begin
//       if primary.nodes.Count > 0 then
//          begin
//          code.addSymbolByteCode(oLoadSymbol, primary.primaryName);
//          // Output code that does loads except for the last one which we store.
//          for i := 0 to primary.nodes.Count - 2 do
//              begin
//              case primary.nodes[i].nodeType of
//                  ntSubscript :
//                     begin
//                     compileSubscriptsLoad((primary.nodes[i] as TASTSubscript).subscripts.list, False);
//                     end;
//            ntFunctionCall:
//                     begin
//                     userFunc := primary.nodes[i] as TASTFunctionCall;
//                     // Compile any arguments to the function call
//                     for j := 0 to userFunc.argumentList.list.Count - 1 do
//                         compileCode(userFunc.argumentList.list[j]);
//
//                     code.addByteCode(oCall, userFunc.argumentList.list.Count);
//                     // <- arg count used to test for arity at run time
//                     end;
//            ntPeriod :
//                     begin
//                     code.addStoreByteCode(oLoadSymbol, (primary.nodes.list[i] as TASTPeriod).name);
//                     end;
//                else
//                     raise ECompilerException.Create('Unrecongnized variable in assignment.');
//              end;
//              end;
//              // Now output the store instruction on the last operator
//              lastOne := primary.nodes.Count - 1;
//              case primary.nodes[lastOne].nodeType of
//                  ntSubscript :
//                     compileSubscriptsStore((primary.nodes[lastOne] as TASTSubscript).subscripts.list, False); // false = not local
//                  ntPeriod :
//                     code.addStoreByteCode(oStoreSecondary, (primary.nodes[lastOne] as TASTPeriod).name);
//                  ntFunctionCall:
//                   raise ECompilerException.Create('You cannot assign to a function call.');
//              else
//                   raise ECompilerException.Create('Unrecongnized variable in assignment.');
//              end;
//          end
//       else
//          begin
//          code.addSymbolByteCode(oStoreSymbol, primary.primaryName);
//          end;
//    end;
end;


procedure TCompiler.compileUserFunction(node: TASTNode);
var
  oldCode: TProgram;
  functionNode: TASTUserFunction;
  index, i: integer;
  symbol: TSymbol;
  x : integer;
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

    code := currentUserFunction.funcCode;

    globalStmt := nil;
    compilingFunction := True;
    compileCode(functionNode.body);
    compilingFunction := False;
    code.addByteCode(oPushNone);
    code.addByteCode(oRet); // This is to make sure we return
    code.compactCode();
  finally
    code := oldCode;
    currentUserFunction := nil;
  end;
end;


procedure TCompiler.compileList(node: TASTCreatelist);
var
  i: integer;
begin
  if node <> nil then
    begin
      for i := 0 to node.list.Count - 1 do
        compileCode(node.list[i]);
      code.addByteCode(oCreateList, node.list.Count);
    end
  else
    code.addByteCode(oCreateList, 0); // empty list
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
        raise ECompilerException.Create('Global variable name ' + astSymbol.symbolName + ' already declared as a local variable');

      // Look for the global variable to make sure it exists
      if not currentModule.symbolTable.find(astSymbol.symbolName, symbol) then
        raise ECompilerException.Create('No such global variable exists: ' + astSymbol.symbolName);
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
  code.addByteCode(opCode);
end;

procedure TCompiler.compilePowerOperator(node: TASTPowerOp);
begin
  compileCode(node.left);
  compileCode(node.right);
  code.addByteCode(oPower);
end;


procedure TCompiler.compileNotOperator (node : TASTNotOp);
begin
  compileCode (node.expression);
  code.addByteCode (oNot);
end;


procedure TCompiler.compileUniOperator(node: TASTUniOp; opCode: Byte);
begin
  compileCode(node.left);
  code.addByteCode(opCode);
end;

procedure TCompiler.compilePrintStatement(node: TASTNode);
var
  i: integer;
begin
  if node is TASTPrint then
    begin
      for i := 0 to (node as TASTPrint).argumentList.list.Count - 1 do
        compileCode((node as TASTPrint).argumentList.list[i]);

      code.addByteCode(oPushi, (node as TASTPrint).argumentList.list.Count);
      code.addByteCode(oPrint);
    end
  else
    begin
      for i := 0 to (node as TASTPrintLn).argumentList.list.Count - 1 do
        compileCode((node as TASTPrintLn).argumentList.list[i]);

      code.addByteCode(oPushi, (node as TASTPrintLn).argumentList.list.Count);
      code.addByteCode(oPrintln);
    end;
end;


procedure TCompiler.compileSetColor (node : TASTNode);
begin
  compileCode((node as TASTSetColor).expression);
  code.addByteCode(oSetColor);
end;


procedure TCompiler.compileAssert(node: TASTNode);
begin
  if node is TASTAssertTrue then
    begin
      compileCode((node as TASTAssertTrue).expression);
      code.addByteCode(oAssertTrue);
    end
  else
    begin
      compileCode((node as TASTAssertFalse).expression);
      code.addByteCode(oAssertFalse);
    end;
end;


procedure TCompiler.compileHelp (node: TASTNode);
begin
  compileCode((node as TASTHelp).expression);
  code.addByteCode(oHelp);
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
      code.addByteCode(oDup);
      code.addByteCode(oPushi, caseValues[i]);
      code.addByteCode(oIsEq);
      jumpToLocation[i] := code.addByteCode(oJmpIfTrue);
    end;
  elseJump := code.addByteCode(oJmp);

  for i := 0 to listOfCaseStatements.list.Count - 1 do
    begin
      entryLocation[i] := code.getCurrentInstructionPointer;
      compileCode((listOfCaseStatements.list[i] as TASTCaseStatement).statementList);
      jumpToEndLocation[i] := code.addByteCode(oJmp);
    end;

  elseDestination := code.getCurrentInstructionPointer;
  if node.elseStatement <> nil then
    compileCode(node.elseStatement);

  code.addByteCode(oNop);
  code.setGotoLabel(elseJump, elseDestination - elseJump);

  lastInstruction := code.addByteCode(oPopDup); // pop the dup
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
      raise ECompilerException.Create('Internal Error: Couldn''t find builtin module: ' + moduleName);
end;


procedure TCompiler.compileImportStmt(node: TASTImport);
var
  scm: TScanner;
  sym: TConstructAST;
  src: string;
  compiler: TCompiler;
  root: TASTNode;
  module: TModuleLib;
  symbol: TSymbol;
  paths : TListObject;
  path : string;
  found : integer;
  index : integer;
begin
  // Check if it's a builtin first
  if listofBuiltIns.find (node.importName, index) then
     begin
     importBuiltIn (node.importName, index);
     exit;
     end;

  found := -1;
  paths := OSLibraryRef.find ('path').lValue;
  for var i := 0 to paths.list.Count - 1 do
      begin
      if fileExists (paths.list[i].sValue.value + '\\' + node.importName + '.rh') then
         begin
         found := i;
         break;
         end;
      end;
  if found = -1 then
     raise ECompilerException.Create('Unable to locate imported module: ' + node.importName)
  else
     path := paths.list[found].sValue.value + '\\' + node.importName + '.rh';


   // If a module of that name has already been loaded we just get out.
   if currentModule.symbolTable.find(node.importName, symbol) then
      if symbol.symbolType = symModule then
         exit;

   // Otherwise lets read it in
   scm := TScanner.Create;
   sym := TConstructAST.Create(scm);
   try
      src := TFile.ReadAllText(path);
      scm.scanString(src);
      scm.nextToken;
      // root is the resulting AST
      module := sym.parseModule(node.importName, root);

      addGlobalMethodsToModule (module);
      addAllBuiltInLibraries(module);

      compiler := TCompiler.Create(module);
      try
        compiler.startCompilation(module, root);
        module.code.addByteCode(oHalt);
      finally
        root.Free;
        compiler.Free;
      end;

      // Add the name of the module to the current module's
      // symbol table, because user functions might need it for globals
      currentModule.symbolTable.addModule(module);
      // import the module
      currentModule.code.addModuleByteCode(oImportModule, node.importName);
   finally
      scm.Free;
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
    index : integer;
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
         // if not it could bein the module space - TO BE IMPLEMENTED
         if currentUserFunction.localSymbolTable.find(node.symbolName, localSymbolIndex) then
            code.addByteCode(oLoadLocal, localSymbolIndex, -1)
         else
            begin
            // HMS Watch out here for long assignment statements, this may not work
            if currentModule.symbolTable.find(node.symbolName, symbol) then
               code.addSymbolByteCode (oLoadSymbol, node.symbolName)
            else
              // This means we couldn't find the symbol at the local or module level
              raise ECompilerException.Create('Undeclared variable: ' + node.symbolName);
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
    index : integer;
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
            code.addByteCode (oStoreLocal, localSymbolIndex, -1)
         else
            code.addByteCode (oLoadLocal, localSymbolIndex, -1)
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
  code.addSymbolByteCode (oLoadSecondary, node.identifier.symbolName);
  compileCode (node.primaryPlus);
end;


procedure TCompiler.compilePrimaryIndex (node : TASTPrimaryIndex);
begin
   compileSubscripts(node.subscriptList.list);
   compileCode (node.primaryPlus);
end;


procedure TCompiler.compilePrimaryFunction (node : TASTPrimaryFunction);
var anode : TASTNode;
begin
  for anode in node.argumentList.list do
      compileCode (anode);

  code.addByteCode(oCall, node.argumentList.list.Count);
  compileCode (node.primaryPlus);
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
       compilePrimaryLoad2(node as TASTPrimary);
    ntPrimaryOld:
      compilePrimaryLoad(node as TASTPrimaryOld);
    ntPrimaryPeriod:
      compilePrimaryPeriod (node as TASTPrimaryPeriod);
    ntPrimaryIndex:
      compilePrimaryIndex(node as TASTPrimaryIndex);
    ntPrimaryFunction:
      compilePrimaryFunction (node as TASTPrimaryFunction);
    ntPeriod :
      compilePeriod (node as TASTPeriod);
    ntCreateList:
      compileList(node as TASTCreatelist);
    ntAssignment:
      compileAssignment(node as TASTAssignment2);
    // An expression on its own, has to be dealt with separately
    // All exprssions including functions return something even
    // if its a none type. This is dealt differently whether
    // we evaluate the expression at the console or inside a script.
    ntExpressionStatement:
      begin
        compileCode((node as TASTExpressionStatement).expression);
        //if not interactive then
          code.addByteCode(oPop);
      end;
    ntFunction:
      compileUserFunction(node);
    ntReturn:
      begin
        compileCode((node as TASTReturn).expression);
        code.addByteCode(oRet);
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
      code.addByteCode(oPushb, (node as TASTBoolean).bValue);
    ntInteger:
      code.addByteCode(oPushi, (node as TASTInteger).iValue);
    ntFloat:
      begin
      index := currentModule.code.constantValueTable.Add (TConstantValueElement.Create((node as TASTFloat).dValue));
      code.addByteCode(oPushd, index);
      end;
    ntString :
      begin
      index := currentmodule.code.constantValueTable.Add (TConstantValueElement.Create ((node as TASTString).sValue));
      code.addByteCode(oPushs, index);
      end;
    ntNull : begin end;
  else
    visitNode(node);
  end;
end;


procedure TCompiler.startCompilation(module: TModule; node: TASTNode);
begin
  currentModule := module;
  compileCode(node);
  module.compiled := True;
end;


initialization

end.




