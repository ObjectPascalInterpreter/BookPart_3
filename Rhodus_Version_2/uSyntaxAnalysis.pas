unit uSyntaxAnalysis;

// Syntax Parser Rhodus Language Part II

// Developed under Delphi for Windows and Mac platforms.

// *** Ths source is distributed under Apache 2.0 ***

// Copyright (C)  2019-2020 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

// Usage:
//
// sc := TScanner.Create;
// sc.scanString ('a = 2; b = a + 5.6')
// syntaxAnalyser := TSyntaxAnalysis.Create (sc);
// syntaxAnalyzer.mainProgram;

interface

uses Classes, SysUtils, uScanner, uSymbolTable, uModule, Generics.Collections, uAST, uASTNodeType;

type
  TBreakStack = TStack<integer>;

  TSyntaxAnalysis = class(TObject)
  private
    stackOfBreakStacks : TStack<TBreakStack>;

    sc: TScanner;
    //module : TModule;

    // Very private. don't use them directly, use the helper routines
    inUserFunctionParsing: boolean;
    currentUserFunction : TUserFunction;
    currentModuleIndex : integer;

    // Helper routines for the above
    procedure   enterUserFunctionScope;
    procedure   exitUserFunctionScope;

    procedure   expect(thisToken: TTokenCode);
    procedure   variable(aProgram: TProgram);
    function    parseList(aProgram: TProgram): integer;
    procedure   parseIndexedVariable(aProgram: TProgram);
    procedure   parseFunctionCall (aProgram : TProgram; var nArguments : integer);
    procedure   parseSymbolInFunction (symbolName : string; aProgram : TProgram);
    procedure   parseSymbol (symbolName : string; aProgram : TProgram);
    procedure   parseIdentifier(aProgram: TProgram);
    function    factor(aProgram: TProgram) : TASTNode;
    function    power(aProgram: TProgram) : TASTNode;
    function    term(aProgram: TProgram) : TASTNode;
    function    simpleExpression(aProgram: TProgram) : TASTNode;
    function    relationalOperators(aProgram: TProgram) : TASTNode;
    function    expression(aProgram: TProgram) : TASTNode;
    function    exprStatement(aProgram: TProgram) : TASTNode;
    function    statement(aProgram: TProgram) : TASTNode;
    function    statementList(aProgram: TProgram) : TASTNode;
    function    expressionList(aProgram: TProgram): integer;
    procedure   ifStatement (aProgram: TProgram);
    procedure   breakStatement (aProgram : TProgram);
    procedure   parseGlobalVariable (aProgram : TProgram);
    procedure   globalStatement (aProgram : TProgram);
    procedure   loadModule (name : string);

    procedure   importStatement (aProgram : TProgram);
    procedure   whileStatement(aProgram: TProgram);
    procedure   repeatStatement(aProgram: TProgram);
    procedure   forStatement(aProgram: TProgram);
    procedure   functionDef(aProgram: TProgram);
    function    argumentList(aProgram: TProgram): integer;
    procedure   argument(aProgram: TProgram);
    procedure   returnStmt(aProgram: TProgram);

    procedure   printlnStatement(aProgram: TProgram);
    procedure   printStatement(aProgram: TProgram);
    procedure   AssertTrueStatement(aProgram: TProgram);
    procedure   AssertFalseStatement(aProgram: TProgram);
  public
    function    parseModule : integer;
    function    mainProgram (main : TModule) : TASTNode;
    function    inUserFunctionScope : boolean;
    constructor Create(sc: TScanner);
    destructor  Destroy; override;
  end;

  ESyntaxException = class(Exception);

implementation

uses Math, IOUtils,  uUtils, uOpCodes, uMachineStack, uConstantTable, uBuiltinFunctions;

constructor TSyntaxAnalysis.Create(sc: TScanner);
begin
  inherited Create;
  self.sc := sc;
  inUserFunctionParsing := False;
  stackOfBreakStacks := TStack<TBreakStack>.Create;
end;


destructor TSyntaxAnalysis.destroy;
begin
  stackOfBreakStacks.Free;
  inherited;
end;


procedure TSyntaxAnalysis.expect(thisToken: TTokenCode);
begin
  if sc.token <> thisToken then
    raise ESyntaxException.Create('expecting: ' + TScanner.tokenToString(thisToken))
  else
    sc.nextToken;
end;



procedure TSyntaxAnalysis.enterUserFunctionScope;
begin
  inUserFunctionParsing  := True;
end;


procedure TSyntaxAnalysis.exitUserFunctionScope;
begin
  inUserFunctionParsing  := False;
end;


function TSyntaxAnalysis.inUserFunctionScope : boolean;
begin
   result := inUserFunctionParsing;
end;


// Parse a function argument  in a function definition
procedure TSyntaxAnalysis.variable;
var
  index: integer;
begin
  if sc.Token <> tIdentifier then
     raise ESyntaxException.Create('expecting identifier in function argument definition');

  // Add the argument symbol to the user function local symbol table
  if not currentUserFunction.symbolTable.find(sc.tokenString, index) then
     currentUserFunction.symbolTable.addSymbol(sc.tokenString);
  sc.nextToken;
end;


// Parse a list of the form: expression ',' expression ','' etc.
// Returns the number of items found in the list
function TSyntaxAnalysis.parseList(aProgram: TProgram): integer;
var
  count: integer;
begin
  count := 1;
  expression(aProgram);
  while sc.token = tComma do
        begin
        sc.nextToken;
        expression(aProgram);
        inc(count);
        end;
  result := count;
end;


// Parse something of the form variable '[' expressionList ']'
// Such indexing applies to lists and strings
procedure TSyntaxAnalysis.parseIndexedVariable (aProgram: TProgram);
begin
  // Much check local scope first if we're in a user function because
  // variables in a user function have higher priority
  if inUserFunctionScope then
     begin
     sc.nextToken;
     expression(aProgram);
     aProgram.addByteCode(oLocalLvecIdx);
     while sc.token = tComma do
           begin
           sc.nextToken;
           expression(aProgram);
           aProgram.addByteCode(oLocalLvecIdx);
           end;

     expect(tRightBracket);
     exit;
     end
  else
     begin
     sc.nextToken;
     expression(aProgram);
     aProgram.addByteCode(oLvecIdx);
     while sc.token = tComma do
           begin
           sc.nextToken;
           expression(aProgram);
           aProgram.addByteCode(oLvecIdx);
           end;
     expect(tRightBracket);
     exit;
     end
end;


procedure TSyntaxAnalysis.parseFunctionCall (aProgram : TProgram; var nArguments : integer);
var identifier : string;
begin
  identifier := sc.tokenString;
  sc.nextToken;
  nArguments := 0;
  if sc.token <> tRightParenthesis then
     nArguments := expressionList(aProgram);

  expect(tRightParenthesis);
end;


procedure TSyntaxAnalysis.parseSymbolInFunction (symbolName : string; aProgram : TProgram);
var globalVariable : boolean;
    index: integer;
    nArguments : integer;
begin
   globalVariable := False;
   if not currentUserFunction.symbolTable.find(symbolName, index) then
      begin
      // It could be a globally declared variable
      if currentUserFunction.globalVariableList.find (symbolName, index) then
        globalVariable := True
      else
        // The symbol could be the name of the function, so we must search
        // the module in case it is.
        if not moduleList[currentModuleIndex].symbolTable.isUserFunction (symbolName, index) then
           // Then its a new local variable, add it to local symbol table
           index := currentUserFunction.symbolTable.addSymbol(symbolName);
      end;

   if (sc.token = tLeftBracket) or (sc.token = tLeftParenthesis) then
       begin
          while (sc.token = tLeftBracket) or (sc.token = tLeftParenthesis) do
             begin
               case sc.token of
                tLeftBracket:
                   begin
                   if globalVariable then
                      begin
                      aProgram.addByteCode(oPushModule, currentModuleIndex);
                      aProgram.addByteCode(oLoad, index);  // Push the symbol table index
                      parseIndexedVariable(aProgram);
                      end
                   else
                      begin
                      aProgram.addByteCode(oLoadLocal, index);  // Push the symbol table index
                      parseIndexedVariable(aProgram);
                      end;
                   end;
                tLeftParenthesis:
                   begin
                   parseFunctionCall (aProgram, nArguments);
                   aProgram.addByteCode(oPushModule, currentModuleIndex);
                   aProgram.addByteCode(oPushFunction, index);
                   aProgram.addByteCode(oCall);
                   end;
                else
                   begin
                   aProgram.addByteCode(oLoadLocal, index)
                   end;
               end;
             end
       end
   else
       begin
       if globalVariable then
          begin
          aProgram.addByteCode(oPushModule, currentModuleIndex);
          aProgram.addByteCode(oLoad, integer (currentUserFunction.globalVariableList.Objects[index]))
          end
       else
          aProgram.addByteCode(oLoadLocal, index);
      end;
 end;


procedure TSyntaxAnalysis.parseSymbol (symbolName : string; aProgram : TProgram);
var symbolIndex : integer;
    nArguments : integer;
begin
  sc.nextToken;

  if inUserFunctionScope then
     parseSymbolInFunction (symbolName, aProgram)
  else
     begin   // We're not in a user function
     symbolIndex := -1;
     if not moduleList[currentModuleIndex].symbolTable.reverseFind(symbolName, symbolIndex) then
        if sc.token <> tLeftParenthesis then
           symbolIndex := moduleList[currentModuleIndex].symbolTable.addSymbol(symbolName);
     if (sc.token = tLeftBracket) or (sc.token = tLeftParenthesis) then
         begin
            while (sc.token = tLeftBracket) or (sc.token = tLeftParenthesis) do
                 case sc.token of
                   tLeftBracket:
                       begin
                       aProgram.addByteCode(oPushModule, currentModuleIndex);
                       aProgram.addByteCode(oLoad, symbolIndex);  // Push the symbol table index
                       parseIndexedVariable(aProgram);
                       end;
                    tLeftParenthesis:
                       begin
                       if symbolIndex = -1 then
                          raise ESyntaxException.Create('Unable to find function: ' + sc.tokenString);
                       parseFunctionCall (aProgram, nArguments);
                       if moduleList[currentModuleIndex].symbolTable[symbolIndex].symbolType = symUserFunc then
                          begin
                          if moduleList[currentModuleIndex].symbolTable[symbolIndex].fValue.nArgs <> nArguments then
                             raise ESyntaxException.Create ('incorrect number of arguments in function call: ' + moduleList[currentModuleIndex].symbolTable[symbolIndex].fValue.name);
                          end;

                       aProgram.addByteCode(oPushModule, currentModuleIndex);
                       aProgram.addByteCode(oPushFunction, symbolIndex);
                       aProgram.addByteCode(oCall);
                       end;
                 end;
         end
     else
         begin
         aProgram.addByteCode(oPushModule, currentModuleIndex);
         aProgram.addByteCode(oLoad, symbolIndex)
         end;
     end;

end;




// There are a number of places where we will find identifiers:
// 1. As a module name, eg m1.a
// 2. As a function call, eg func (a,b)
// 3. As an indexed variable, eg x[i]
// 4. An ordinary variable, eg x
procedure TSyntaxAnalysis.parseIdentifier (aProgram: TProgram);
var
  identifier: string;
  index: integer;
  globalVariable : boolean;
  oldCurrentModuleIndex : integer;
  nArguments : integer;
begin
  globalVariable := False;
  oldCurrentModuleIndex := currentModuleIndex;
  try
  identifier := sc.tokenString;
  if sc.peek.FToken = tLeftParenthesis then
     begin
     if builtinList.find (identifier, index)  then
        begin
        sc.nextToken;
        parseFunctionCall (aProgram, nArguments);
        if builtinList[index].nArguments <> nArguments then
           raise ESyntaxException.Create ('incorrect number of arguments in builtin function');
        aProgram.addByteCode(oPushi, index);
        aProgram.addByteCode(oBuiltin);
        exit;
        end
      end;

  // Check first if the name is the name of a loaded module
  if moduleList.find (identifier, index) then
     begin
     // In that case we expect a period '.'
     currentModuleIndex := index;
     sc.nextToken;
     if sc.token <> tPeriod then
        begin
        aProgram.addByteCode(oPushModule, currentModuleIndex);
        exit;
        end;
     expect (tPeriod);
     if sc.token <> tIdentifier then
        raise ESyntaxException.Create ('identifier expected after module name');
     identifier := sc.tokenString;
     end
  else
    currentModuleIndex := 0;

  parseSymbol (identifier, aProgram);

  finally
    currentModuleIndex := oldCurrentModuleIndex;
  end;
end;


// factor = integer | float | '(' expression ')' | etc
function TSyntaxAnalysis.factor(aProgram: TProgram) : TASTNode;
var
  nArguments : integer;
begin
  case sc.token of
    tInteger:
       begin
       result := TASTNode.createLeaf (sc.tokenInteger);
       aProgram.addByteCode(oPushi, sc.TokenInteger);
       sc.nextToken;
       end;

    tFloat:
       begin
       result := TASTNode.createLeaf (sc.tokenFloat);
        aProgram.addByteCode(oPushd, sc.tokenFloat);
       sc.nextToken;
       end;

    tIdentifier: parseIdentifier(aProgram);

    tLeftParenthesis:
       begin
       sc.nextToken;
       result := expression(aProgram);
       expect(tRightParenthesis);
       end;

    tLeftCurleyBracket:
       begin
       sc.nextToken;
       nArguments := 0;
       if sc.token <> tRightCurleyBracket then
          nArguments := parseList(aProgram);
       expect(tRightCurleyBracket);
       aProgram.addByteCode(oCreateList, nArguments);
       end;

    tString:
       begin
       // addByteCode will add the string to the constant table
       aProgram.addByteCode(oPushs, sc.tokenString);
       sc.nextToken;
       end;

    tNOT:
       begin
       sc.nextToken;
       expression(aProgram);
       aProgram.addByteCode(oNot);
      end;

    tFalse:
      begin
        aProgram.addByteCode(oPushb, False);
        sc.nextToken;
      end;

    tTrue:
      begin
        aProgram.addByteCode(oPushb, True);
        sc.nextToken;
      end
  else
    raise
      ESyntaxException.Create('expecting scalar, identifier or left parentheses');
  end;
end;


// power = {'+' | '-'} factor [ '^' power ]
function TSyntaxAnalysis.power(aProgram: TProgram) : TASTNode;
var unaryMinus_count, i: integer;
    leftNode, rightNode: TASTNode;
begin
  unaryMinus_count := 0;
  // Handle unary operators, count only '-'
  while (sc.token = tMinus) or (sc.token = tPlus) do
    begin
    case sc.token of
      tMinus: inc(unaryMinus_count);
    end;
    sc.nextToken;
    end;

  leftNode := factor(aProgram);
  if sc.token = tPower then
     begin
     sc.nextToken;
     rightNode := power(aProgram);
     leftNode := TAstNode.createNode (leftNode, rightNode, TASTNodeType.ntPower);
     aProgram.addByteCode(oPower);
     end;
  for i := 0 to unaryMinus_count - 1 do
      aProgram.addByteCode(oUmi);
  result := leftNode;
end;


// term = power { ('*', '/', MOD, DIV) power }
function TSyntaxAnalysis.term(aProgram: TProgram) : TASTNode;
var op: TTokenCode;
    leftNode, rightNode: TASTNode;
begin
  leftNode := power(aProgram);
  while sc.token in [tMult, tDivide, tIDiv, tMod] do
  begin
    op := sc.token; // remember the token
    sc.nextToken;
    rightNode := power(aProgram);
    case op of
      tMult:
        begin
        aProgram.addByteCode(oMult);
        leftNode := TASTNode.createNode (leftNode, rightNode, ntMult);
        end;
      tDivide:
        aProgram.addByteCode(oDivide);
      tMod:
        aProgram.addByteCode(oMod);
      tIDiv:
        aProgram.addByteCode(oDivi);
    end;
  end;
  result := leftNode;
end;


// expression = term { ('+' | '-' | MOD | DIV) power }
function TSyntaxAnalysis.simpleExpression(aProgram: TProgram) : TASTNode;
var op: TTokenCode;
    leftNode, rightNode: TASTNode;
begin
  leftNode := term(aProgram);
  while sc.token in [tPlus, tMinus] do
  begin
    op := sc.token; // remember the token
    sc.nextToken;
    rightNode := term(aProgram);
    case op of
      tPlus:
        begin
        aProgram.addByteCode(oAdd);
        leftNode := TASTNode.createNode (leftNode, rightNode, TASTNodeType.ntAdd);
        end;
      tMinus:
        aProgram.addByteCode(oSub);
    end;
  end;
  result := leftNode;
end;


// expression = simpleExpression | simpleExpression relationalOp simpleExpression
function TSyntaxAnalysis.relationalOperators(aProgram: TProgram) : TASTNode;
var
  op: TTokenCode;
  leftNode, rightNode: TASTNode;
begin
  leftNode := simpleExpression(aProgram);
  while sc.token in [tLessThan, tLessThanOrEqual, tMoreThan, tMoreThanOrEqual,
    tNotEqual, tEquivalence] do
  begin
    op := sc.token;
    sc.nextToken;
    simpleExpression(aProgram);
    case op of
      tEquivalence: aProgram.addByteCode(oIsEq);
      tLessThan: aProgram.addByteCode(oIsLt);
      tMoreThan: aProgram.addByteCode(oIsGt);
      tMoreThanOrEqual: aProgram.addByteCode(oIsGte);
      tLessThanOrEqual: aProgram.addByteCode(oIsLte);
      tNotEqual: aProgram.addByteCode(oIsNotEq);
    end;
  end;
  result := leftNode;
end;


function TSyntaxAnalysis.expression(aProgram: TProgram) : TASTNode;
var
  op: TTokenCode;
  leftNode, rightNode: TASTNode;
begin
  leftNode := relationalOperators(aProgram);
  while sc.token in [tOr, tXor, tAnd] do
  begin
    op := sc.token; // remember the token
    sc.nextToken;
    relationalOperators(aProgram);
    case op of
      tOr: aProgram.addByteCode(oOr);
      tAnd: aProgram.addByteCode(oAnd);
      tXor: aProgram.addByteCode(oXOR);
    end;
  end;
  result := leftNode;
end;


// statement = exprStatement | forStatement | ifStatement
//                           | whileStatement | repeatStatement
//                           | returnStatment | breakStatement
//                           | function
// exprStatement =   assignment
//                 | rightHandSide
// assignment = leftHandSide '=' expression
// rightHandside = expression
// leftHandSide = identifier ( '[' expression ']' )
function TSyntaxAnalysis.statement(aProgram: TProgram) : TASTNode;
begin
  result := nil;
  case sc.token of
    tIf: ifStatement(aProgram);
    tFor: forStatement(aProgram);
    tWhile: whileStatement(aProgram);
    tRepeat: repeatStatement(aProgram);
    tReturn: returnStmt(aProgram);
    tFunction: functionDef(aProgram);
    tBreak   : breakStatement (aProgram);
    tGlobal  : globalStatement (aProgram);
    tImport  : importStatement (aProgram);
    tPrint: printStatement(aProgram);
    tPrintln: printlnStatement(aProgram);
    tAssertTrue : AssertTrueStatement (aProgram);
    tAssertFalse : AssertFalseStatement (aProgram);
    tEnd : exit;
    tUntil : exit;  // To allow repeat until False
    tEndOfStream: exit;
  else
    result := exprStatement(aProgram);
  end;
end;


// statementList = statement { statement }
function TSyntaxAnalysis.statementList(aProgram: TProgram) : TASTNode;
var leftNode, rightNode: TASTNode;
begin
  leftNode := statement(aProgram);
  while True do
     begin
     if sc.token = tSemicolon then // semicolons optional
        expect(tSemicolon);
     if sc.token in [tUntil, tEnd, tElse, tEndOfStream] then
        exit (leftNode);
     statement(aProgram);
     end;
  result := leftNode;
end;


procedure TSyntaxAnalysis.printlnStatement(aProgram: TProgram);
var
  count: integer;
begin
  sc.nextToken;
  if sc.token = tLeftParenthesis then
    begin
    sc.nextToken; count := 0;
    // It could be an empty function call
    if sc.token <> tRightParenthesis then
       count := expressionList(aProgram);
    expect(tRightParenthesis);
    end
  else
    raise ESyntaxException.Create('Expecting opening bracket to println call');
  // Push number of expressions found
  aProgram.addByteCode(oPushi, count);
  aProgram.addByteCode(oPrintln);
end;


procedure TSyntaxAnalysis.printStatement(aProgram: TProgram);
var
  count: integer;
begin
  sc.nextToken;
  if sc.token = tLeftParenthesis then
    begin
    sc.nextToken; count := 0;
    // It could be an empty function all
    if sc.token <> tRightParenthesis then
       count := expressionList(aProgram);
    expect(tRightParenthesis);
    end
  else
    raise ESyntaxException.Create('Expecting opening bracket to print call');
  // Push number of expressions found
  aProgram.addByteCode(oPushi, count);
  aProgram.addByteCode(oPrint);
end;


procedure TSyntaxAnalysis.AssertTrueStatement(aProgram: TProgram);
begin
  sc.nextToken;
  if sc.token = tLeftParenthesis then
  begin
    sc.nextToken;
    expression(aProgram);
    expect(tRightParenthesis);
    aProgram.addByteCode(oAssertTrue);
  end;
end;


procedure TSyntaxAnalysis.AssertFalseStatement(aProgram: TProgram);
begin
  sc.nextToken;
  if sc.token = tLeftParenthesis then
  begin
    sc.nextToken;
    expression(aProgram);
    expect(tRightParenthesis);
    aProgram.addByteCode(oAssertFalse);
  end;
end;


// exprStatement = expression '=' expression
// There are however some restrictions on the left-hand expression

// Left-hand sides can only be:
// ident '='
// ident '[' expression ']' '='

// They cannot be expressions such as 1 + 5, a + b, etc
// A valid left-hand side will always, when parsed, appear to have load or lvecIdx as the last bytecode

// We first read the left-side into a local program code space, if there
// is no '=' then we exit the method, else we also parse the right-hand side.
// We now append the code for the left-hand side on to the right-hand code
// and patch the last byte code to be either store or svecIdx

// This approach will allow us to identify illegal left-hand sides
// aw well as permit indices for lists to be of any complexity
function TSyntaxAnalysis.exprStatement(aProgram: TProgram) : TAStNode;
var
  programFragment: TProgram;
  leftNode, rightNode: TASTNode;
begin
  programFragment := TProgram.Create;
  try
    leftNode := expression(programFragment);
    if sc.token = tEquals then
    begin
      sc.nextToken;
      expression(aProgram);
      // Check if it's a valid assignable symbol
      if (programFragment.code[programFragment.count - 1].opCode <> oLoad)
         and
         (programFragment.code[programFragment.count - 1].opCode <> oLoadLocal)
         and
         (programFragment.code[programFragment.count - 1].opCode <> oLvecIdx)
         and
         (programFragment.code[programFragment.count - 1].opCode <> oLocalLvecIdx)
         then
           begin
           if programFragment.code[programFragment.count - 1].opCode = oPushModule then
              raise ESyntaxException.Create ('You cannot assign a value to a module')
           else
              raise ESyntaxException.Create('Left-hand side cannot be assigned to');
           end;
      // Patch the bytecode to use store instead
      if (programFragment.code[programFragment.count - 1].opCode = oLoad) or
         (programFragment.code[programFragment.count - 1].opCode = oLoadLocal)
         then
         begin
          if (programFragment.code[programFragment.count - 1].opCode = oLoad) then
            programFragment.code[programFragment.count - 1].opCode := oStore
          else
            programFragment.code[programFragment.count - 1].opCode := oStoreLocal
         end
      else
         begin
         if inUserFunctionParsing then
           programFragment.code[programFragment.count - 1].opCode := oLocalSvecIdx
         else
           begin
           //if programFragment.code[programFragment.count - 1].opCode <> oLvecIdx then
               programFragment.code[programFragment.count - 1].opCode := oSvecIdx;
           end;
         end;
      // <= Updated to deal with expressions in user functions
      aProgram.appendProgram(programFragment);
    end
    else
    begin
      // if there is a non-assigned expression, then pop the value off the stack to dump the value
      if inUserFunctionParsing then
         programFragment.addByteCode (oPop);
      aProgram.appendProgram(programFragment);
      //aProgram.addByteCode(oRet);
      //aProgram.addByteCode(oPushi, 1); // don't forget nArgs
      //aProgram.addByteCode(oPrint);
    end;
    result := leftNode;
  finally
    programFragment.Free;
  end;
end;


// argumentList = expression { ',' expression }
// Returns the number of expressions that were parsed
function TSyntaxAnalysis.expressionList(aProgram: TProgram): integer;
var
  count: integer;
begin
  count := 0;
  expression(aProgram);
  inc(count);
  while sc.token = tComma do
        begin
        sc.nextToken;
        expression(aProgram);
        inc(count);
        end;
  result := count;
end;


// ifStatement = IF expression THEN statement ifEnd
// ifEnd = END | ELSE statementList END
procedure TSyntaxAnalysis.ifStatement(aProgram: TProgram);
var
  jumpLocation_1, jumpLocation_2: integer; // Used by the if parser
begin
  expect(tIf);
  expression(aProgram);
  jumpLocation_1 := aProgram.addByteCode(oJmpIfFalse);
  expect(tThen);

  statementList(aProgram);

  if sc.token = tElse then
     begin
     jumpLocation_2 := aProgram.addByteCode(oJmp);
     aProgram.setGotoLabel(JumpLocation_1, aProgram.getCurrentInstructionPointer - JumpLocation_1);
     sc.nextToken;
     statementList(aProgram);
     aProgram.setGotoLabel(JumpLocation_2, aProgram.getCurrentInstructionPointer - JumpLocation_2);
     expect(tEnd);
     end
  else
     begin
     expect(tEnd);
     aProgram.setGotoLabel(JumpLocation_1, aProgram.getCurrentInstructionPointer -  JumpLocation_1);
     end;
end;


procedure TSyntaxAnalysis.breakStatement (aProgram : TProgram);
begin
  if stackOfBreakStacks.Count = 0 then
     raise ESyntaxException.Create ('Break statement illegal in this context');

  stackOfBreakStacks.Peek.push (aProgram.addByteCode (oJmp));
  sc.nextToken;
end;


// whileStatement = WHILE expression DO statementList END
procedure TSyntaxAnalysis.whileStatement(aProgram: TProgram);
var  breakJump : integer;
     breakStack : TStack<integer>;
     again : integer;
     jumpLocation_exit, jumpLocation_back: integer;
begin
  breakStack := TStack<integer>.Create;
  stackOfBreakStacks.Push (breakStack);
  try
    expect (tWhile);
    again := aProgram.getCurrentInstructionPointer;

    expression(aProgram);

    // record the jump location because we're going to patch the relative jump value later
    jumpLocation_exit := aProgram.addByteCode(oJmpIfFalse);

    expect(tDo);

    statementList(aProgram);

    // Record the location of the 'jump back' to jmp instruction
    JumpLocation_back := aProgram.addByteCode(oJmp);

    // Lastly, patch the relative jump instructions
    aProgram.SetGotoLabel(JumpLocation_back, again - aProgram.getCurrentInstructionPointer + 1);
    aProgram.SetGotoLabel(JumpLocation_exit, aProgram.getCurrentInstructionPointer - JumpLocation_exit);

    expect(tEnd);
    while breakStack.Count > 0 do
          begin
          breakJump := breakStack.Pop;
          aProgram.setGotoLabel (breakJump, aProgram.getCurrentInstructionPointer - breakJump);
          end;
  finally
    breakStack := stackOfBreakStacks.Pop;
    breakStack.Free;
  end;
end;


// repeatStatement = REPEAT statementList UNTIL expression
procedure TSyntaxAnalysis.repeatStatement(aProgram: TProgram);
var
  again, jumpLocation: integer;
  breakJump : integer;
  breakStack1 : TStack<integer>;
begin
  breakStack1 := TStack<integer>.Create;
  stackOfBreakStacks.Push (breakStack1);
  try
    expect(tRepeat);

    again := aProgram.getCurrentInstructionPointer;

    statementList(aProgram);

    expect(tUntil);

    expression(aProgram);

    jumpLocation := aProgram.addByteCode(oJmpIfFalse);
    aProgram.setGotoLabel(jumpLocation,
           again - aProgram.getCurrentInstructionPointer + 1);

    while breakStack1.Count > 0 do
          begin
          breakJump := breakStack1.Pop;
          aProgram.setGotoLabel (breakJump, aProgram.getCurrentInstructionPointer - breakJump);
          end;
  finally
    breakStack1 := stackOfBreakStacks.Pop;
    breakStack1.Free;
  end;
end;


// forStatement = FOR identifier = expression TO/DOWNTO expression DO statementList END
procedure TSyntaxAnalysis.forStatement(aProgram: TProgram);
var
  jumpLocation_1, jumpLocation_2, again, loopIndex: integer;
  toToken: TTokenCode;
  breakJump : integer;
  breakStack : TStack<integer>;
begin
  breakStack := TStack<integer>.Create;
  stackOfBreakStacks.Push (breakStack);
  try
    expect(tFor);

    expect(tIdentifier);
    if inUserFunctionParsing then
       begin
       if not currentUserFunction.symbolTable.find (sc.tokenString, loopindex) then
          loopIndex := currentUserFunction.symbolTable.addSymbol (sc.TokenString);
       end
    else
       begin
       if not moduleList[currentModuleIndex].symbolTable.find(sc.TokenString, loopIndex) then
          loopIndex := moduleList[currentModuleIndex].symbolTable.addSymbol(sc.TokenString);
       end;

    expect(tEquals);
    expression(aProgram);
    if inUserFunctionParsing then
       aProgram.addByteCode(oStoreLocal, loopIndex)
    else
       begin
       aProgram.addByteCode(oPushModule, currentModuleIndex);
       aProgram.addByteCode(oStore, loopIndex);
       end;

    if sc.token in [tTo, tDownTo] then
      begin
      toToken := sc.token;
      sc.nextToken;
      end
    else
      raise ESyntaxException.Create('expecting "to" or "downto" in for loop');

    // i > n
    again := aProgram.getCurrentInstructionPointer;
    if inUserFunctionParsing then
       aProgram.addByteCode(oLoadLocal, loopIndex)
    else
       begin
       aProgram.addByteCode(oPushModule, currentModuleIndex);
       aProgram.addByteCode(oLoad, loopIndex);
       end;

    // Upper limit expression
    expression(aProgram);
    expect(tDo);

    if toToken = tTo then
      aProgram.addByteCode(oIsGt)
    else
      aProgram.addByteCode(oIsLt);

    JumpLocation_1 := aProgram.addByteCode(oJmpIfTrue);

  // The following step is not yet implemented, will bein Version II
  //if sc.token = tStep then


    // .... do <body>
    statementList(aProgram);

    if ToToken = tTO then
      begin
      if inUserFunctionScope then
         aProgram.addByteCode(oLocalInc, loopIndex)
      else
         aProgram.addByteCode(oInc, loopIndex);
      end
    else
      begin
      if inUserFunctionScope then
         aProgram.addByteCode(oLocalDec, loopIndex)
      else
         aProgram.addByteCode(oDec, loopIndex);
      end;

    JumpLocation_2 := aProgram.addByteCode(oJmp);

    aProgram.setGotoLabel(JumpLocation_2, again - aProgram.getCurrentInstructionPointer + 1);
    aProgram.setGotoLabel(JumpLocation_1, aProgram.getCurrentInstructionPointer - JumpLocation_1);

    while breakStack.Count > 0 do
          begin
          breakJump := breakStack.Pop;
          aProgram.setGotoLabel (breakJump, aProgram.getCurrentInstructionPointer - breakJump);
          end;

    expect(tEnd);
  finally
    breakStack := stackOfBreakStacks.Pop;
    breakStack.Free;
  end;
end;


// function = function identifier '(' argumentList ')' statementList
procedure TSyntaxAnalysis.functionDef(aProgram: TProgram);
var
  functionName: string;
  index : integer;
  newUserFunction : boolean;
begin
  newUserFunction := False;
  sc.nextToken;
  if sc.token = tIdentifier then
     begin
     functionName := sc.tokenString;
     newUserFunction := True;
     end
  else
    raise ESyntaxException.Create('expecting function name');

  currentUserFunction := TUserFunction.Create (functionName);
  index := moduleList[currentModuleIndex].symbolTable.addSymbol(currentUserFunction, False); // locked = False

  try
    enterUserFunctionScope();
    try
       sc.nextToken;
      if sc.token = tLeftParenthesis then
         begin
         sc.nextToken;
         currentUserFunction.nArgs := argumentList (currentUserFunction.funcCode);
         expect(tRightParenthesis);
         end;

      if sc.token = tEnd then
         raise ESyntaxException.Create('empty user defined function.');

      statementList(currentUserFunction.funcCode);
    finally
      exitUserfunctionScope();
      inUserFunctionParsing := False;
    end;
    expect(tEnd);
    currentUserFunction.funcCode.addByteCode(oPushNone);
    currentUserFunction.funcCode.addByteCode(oRet); // This is to make sure we return
    currentUserFunction.funcCode.compactCode();

  except
    on Exception do
       begin
       if newUserFunction then
          moduleList[currentModuleIndex].symbolTable.Delete(index);
       raise;
       end;
  end;
end;


// argumentList = argument { ',' argument }
function TSyntaxAnalysis.argumentList(aProgram: TProgram): integer;
begin
  result := 0;
  if sc.token = tIdentifier then
  begin
    result := 1;
    argument(aProgram);
  end;

  while sc.token = tComma do
  begin
    sc.nextToken;
    argument(aProgram);
    inc(result);
  end;
end;


// argument = identifier | REF identifier
procedure TSyntaxAnalysis.argument(aProgram: TProgram);
begin
  if sc.token = tRef then
    sc.nextToken;
  variable(aProgram);
end;


procedure TSyntaxAnalysis.parseGlobalVariable (aProgram : TProgram);
var index : integer;
begin
  // Check if proposed global variable isn't already a local variable
  // If it is a local variable then we can't declare it as global.
  if currentUserFunction.symbolTable.find (sc.tokenString, index) then
     raise ESyntaxException.Create('Global variable name ' + sc.tokenString + ' already declared as a local variable');

  // Look for the global variable and add it to the user function global list
  if moduleList[currentModuleIndex].symbolTable.find (sc.tokenString, index) then
     currentUserFunction.globalVariableList.AddObject (sc.tokenString, TObject (index));

  sc.nextToken;
end;


// global x
procedure TSyntaxAnalysis.globalStatement (aProgram : TProgram);
begin
  expect (tGlobal);
  if inUserFunctionParsing then
     begin
     if sc.token = tIdentifier then
        begin
        parseGlobalVariable (aProgram);

        while sc.token = tComma do
              begin
              sc.nextToken;
              parseGlobalVariable (aProgram);
              end;
        //sc.nextToken;
        // ###
        end
     else
        raise ESyntaxException.Create('Expecting variable in global declaration');
     end
  else
     raise ESyntaxException.Create('The global keyword canponly be used inside user functions');
end;


// Load module from disk
procedure TSyntaxAnalysis.loadModule (name : string);
var scm : TScanner;
    sym : TSyntaxAnalysis;
    src : string;
    index : integer;
begin
  if FileExists (name + '.rh.') then
     begin
     scm := TScanner.Create;
     sym := TSyntaxAnalysis.Create (scm);
     try
       src := TFile.ReadAllText(name + '.rh');
       scm.scanString(src);
       scm.nextToken;
       sym.parseModule;
     finally
       scm.Free;
       sym.Free;
     end;
     end
  else
     raise ESyntaxException.Create('Unable to locate imported module: ' + name);
end;


procedure TSyntaxAnalysis.importStatement (aProgram : TProgram);
var index : integer;
begin
   sc.nextToken();
   if (sc.token = tIdentifier) then
      begin
      // Creaet list of internal modules search if there, then recreate module
      if not (moduleList.find(sc.tokenString, index)) then
         loadModule (sc.tokenString);
      sc.nextToken();
     end;
end;


// returnStatement = RETURN expression
procedure TSyntaxAnalysis.returnStmt(aProgram: TProgram);
begin
  if not inUserFunctionParsing then
     raise ESyntaxException.Create ('You cannot use a return statement outside a user fucntion');

  expect(tReturn);
  expression(aProgram);
  aProgram.addByteCode(oRet);
end;


function TSyntaxAnalysis.parseModule : integer;
var index : integer;
    module : TModule;
begin
  sc.nextToken;
  if sc.token = tIdentifier then
     begin
     // Check if thi ssmodule is already loaded
     if moduleList.find (sc.tokenString, index) then
        moduleList.Delete(index);
     module := TModule.Create (sc.tokenString);
     end
  else
     raise ESyntaxException.Create ('Expecting module name after module keyword');

  currentModuleIndex := moduleList.Add (module);
  //self. module := module;
  sc.nextToken;
  statementList(module.code);
  module.code.addByteCode(oHalt);
  result := currentModuleIndex;
end;


// program = statementList
function TSyntaxAnalysis.mainProgram (main : TModule) : TASTNode;
begin
  currentModuleIndex := 0; // we're in the main module
  result := statementList(main.code);
  main.code.addByteCode(oHalt);
end;

end.




{
procedure TSyntaxAnalysis.parseSymbolInNameSpace (symbolName : string; aProgram : TProgram);
var index : integer;
    globalVariable : boolean;
    nArguments : integer;
begin
  globalVariable := False;
  sc.nextToken;
     if sc.token = tLeftParenthesis then
        begin
        // It's the start of a function call, eg func (1,2)
        // Check that the function already exists in the main symbol table
        // We do a reverse search, look for most recent declared functions
        if moduleList[currentModuleIndex].symbolTable.reverseFind(symbolName, index) then
           begin
           if moduleList[currentModuleIndex].symbolTable[index].symbolType = symUserFunc then
              begin
              parseFunctionCall (aProgram, nArguments);
              aProgram.addByteCode(oPushModule, currentModuleIndex);
              aProgram.addByteCode(oPushFunction, index);
              aProgram.addByteCode(oCall);
              exit;
              end
           else
             raise ESyntaxException.Create('A name was found in front of "(", but the name is not a user functon name: [' + symbolName + ']');
           end
        else
          begin
          if builtinList.find (symbolName, index)  then
             begin
             parseFunctionCall (aProgram, nArguments);
             aProgram.addByteCode(oPushi, index);
             aProgram.addByteCode(oBuiltin);
             exit;
             end
          else
             raise ESyntaxException.Create('Attempted to call non-existant function: [' + symbolName + ']');
          end;
        end;

     // If it's not a function then it must be an ordinary variable.
     // If we're in a user function then it's a local variable or possibly a
     // global variable. If we're not in a user function it's a
     // module variable level. If we're in a user function then is the local variable
     // already declared? If its not, add the symbol to the user function symbol table
     // We also issue either a oLoad or oLoadLocal depending on the scope

     if inUserFunctionScope then
         begin
         if not currentUserFunction.symbolTable.find(symbolName, index) then
            begin
            // It could be a globally declared variable
            if currentUserFunction.globalVariableList.find (symbolName, index) then
               globalVariable := True
            else
               begin
               // The symbol could be the name of the function, so we must search
               // the module in case it is.
               if not moduleList[currentModuleIndex].symbolTable.isUserFunction (symbolName, index) then
                 // Then its a new local variable, add it to local symbol table
                 index := currentUserFunction.symbolTable.addSymbol(symbolName);
               end;
            end;

         if sc.token = tLeftBracket then
            begin
            if globalVariable then
               begin
               aProgram.addByteCode(oPushModule, currentModuleIndex);
               aProgram.addByteCode(oLoad, index);  // Push the symbol table index
               parseIndexedVariable(aProgram);
               end
            else
               begin
               aProgram.addByteCode(oLoadLocal, index);  // Push the symbol table index
               parseIndexedVariable(aProgram);
               end;
            end
         else
            begin
            if globalVariable then
               begin
               aProgram.addByteCode(oPushModule, currentModuleIndex);
               aProgram.addByteCode(oLoad, integer (currentUserFunction.globalVariableList.Objects[index]))
               end
            else
               aProgram.addByteCode(oLoadLocal, index);
            end;
         end
      else
         // Not in a function
         begin
         if not moduleList[currentModuleIndex].symbolTable.find(symbolName, index) then
            index := moduleList[currentModuleIndex].symbolTable.addSymbol(symbolName);
         if sc.token = tLeftBracket then
            begin
            aProgram.addByteCode(oPushModule, currentModuleIndex);
            aProgram.addByteCode(oLoad, index);  // Push the symbol table index
            parseIndexedVariable(aProgram);
            end
         else
            begin
            aProgram.addByteCode(oPushModule, currentModuleIndex);
            aProgram.addByteCode(oLoad, index)
            end;
         end;
end;
}
