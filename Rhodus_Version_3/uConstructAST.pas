unit uConstructAST;

// AST Parser and constructor Rhodus Language Part III

// Developed under Delphi for Windows and Mac platforms.

// *** This source is distributed under Apache 2.0 ***

// Copyright (C)  2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com


// IMPORTANT
// ****************************************************************
// This parser assumes that the Rhodus program script is correct
// and contains no syntax errors. Syntax checking is done by
// the uSyntaxParser.pas
// ****************************************************************

interface

uses Classes,
     SysUtils,
     uScanner,
     uScannerTypes,
     uSymbolTable,
     uLibModule,
     Generics.Collections,
     uTokenVector,
     uAST,
     uASTNodeType,
     uVM;

type
  TBreakStack = TStack<integer>;
  TModuleNameStack = TStack<string>;

  TConstructAST = class(TObject)
  private
    stackOfBreakStacks: TStack<TBreakStack>;
    moduleNameStack: TModuleNameStack;
    primaryModuleName: string;

    sc : TTokenVector;

    // Very private. don't use them directly, use the helper routines
    inUserFunctionParsing: boolean;
    // Used to gather global variable names, this is only
    // used during AST construction, its discarded afterwards.
    globalVariableList: TStringList;

    procedure nextToken;
    // Helper routines for the above
    procedure enterUserFunctionScope;
    procedure exitUserFunctionScope;

    function  expect(thisToken: TTokenCode) : TASTNode;
    function  variable: TASTNode;
    function  parseIndexOrSlice : TASTNode;
    function  parseIndexedVariable : TASTNode;
    function  parseFunctionCall: TASTNode;

    function parseVector : TASTNode;
    function parseMatrix : TASTNode;
    function primary : TASTNode;
    function factor : TASTNode;
    function primaryPlus : TASTNode;

    function power: TASTNode;
    function term: TASTNode;
    function simpleExpression: TASTNode;
    function relationalOperators: TASTNode;
    function expression: TASTNode;
    function exprStatement: TASTNode;
    function statement: TASTNode;
    function statementList: TASTNode;
    function expressionList: TASTNode;
    function ifStatement: TASTNode;
    function breakStatement: TASTNode;
    function globalStatement: TASTNode;
    function switchStatement: TASTNode;

    function importStatement: TASTNode;
    function parseWhileStatement: TASTNode;
    function parseRepeatStatement: TASTNode;
    function forStatement: TASTNode;
    function parseUserDefinedFunction: TASTNode;
    function functionArgumentList: TASTNode;
    function functionArgument: TASTNode;
    function returnStmt: TASTNode;

    function printlnStatement: TASTNode;
    function printStatement: TASTNode;
    function setColorStatement : TASTNode;
    function AssertTrueStatement: TASTNode;
    function AssertTrueExStatement: TASTNode;
    function AssertFalseStatement: TASTNode;
    function helpStatement: TASTNode;
  public
    function buildModuleAST(moduleName: string; var astRoot: TASTNode): TModuleLib;
    function constructAST: TASTNode;
    function inUserFunctionScope: boolean;

    constructor Create(sc: TTokenVector);
    destructor  Destroy; override;
  end;

  ESemanticException = class(Exception)
      lineNumber, columnNumber : integer;
      errorMsg : string;
      constructor Create (errMsg : string; lineNumber, columnNumber : integer);
  end;

implementation

uses Math,
     IOUtils,
     uGlobal,
     uUtils,
     uOpCodes,
     uMachineStack,
     uCompile,
     uHelpUnit,
     uBuiltInGlobal;

// ----------------------------------------------------------------------
constructor ESemanticException.Create (errMsg : string; lineNumber, columnNumber : integer);
begin
  self.errorMsg := errMsg;
  self.lineNumber := lineNumber;
  self.columnNumber := columnNumber;
end;

// Start of AST Class
// ----------------------------------------------------------------------
constructor TConstructAST.Create(sc: TTokenVector);
var
  module: TModule;
begin
  inherited Create;
  self.sc := sc;
  inUserFunctionParsing := False;
  stackOfBreakStacks := TStack<TBreakStack>.Create;
  moduleNameStack := TModuleNameStack.Create;
  module := getMainModule();

  if module = nil then
     raise ESemanticException.Create ('Internal error: AST can''t find main module', sc.tokenRecord.lineNumber, sc.tokenRecord.columnNumber);

  // HMS
  //if not module.symbolTable.find(TSymbol.globalId, globalSymbol) then
  //   raise ESyntaxException.Create ('Internal error: AST can''t find the global space', sc.tokenElement.lineNumber, sc.tokenElement.columnNumber)
  //else
   //globalSpace := globalSymbol.mValue;
end;


destructor TConstructAST.Destroy;
begin
  stackOfBreakStacks.Free;
  moduleNameStack.Free;
  inherited;
end;


procedure TConstructAST.nextToken;
begin
  sc.nextToken;
end;

// Expect works in the following way. If the function finds the expected
// token then it return nil indicating success.
// If it fails to find the token it builds a special Error node and returns that.
// The error node contains some useful information. If a caller receives a non-nil
// node it can safely assume that the node is the error node. When that happens it
// should free up any partially build parts of the ast tree, then return the error node
// to the next caller. This means that the error node will get inserted into the ast tree
// At compilation, if an error node is encountered the compiler can report the error at
// that point in time.
function TConstructAST.expect(thisToken: TTokenCode) : TASTNode;
begin
  result := nil;
  if sc.token <> thisToken then
     result := TASTErrorNode.Create ('expecting ' + TScanner.tokenToString (thisToken),  sc.tokenRecord.lineNumber, sc.tokenRecord.columnNumber)
  else
    sc.nextToken;
end;


procedure TConstructAST.enterUserFunctionScope;
begin
  inUserFunctionParsing := True;
end;


procedure TConstructAST.exitUserFunctionScope;
begin
  inUserFunctionParsing := False;
end;


function TConstructAST.inUserFunctionScope: boolean;
begin
  result := inUserFunctionParsing;
end;


// Parse a function argument in a function definition
function TConstructAST.variable: TASTNode;
begin
  if sc.token <> tIdentifier then
     result := TASTErrorNode.Create ('expecting identifier in function argument definition',  sc.tokenRecord.lineNumber, sc.tokenRecord.columnNumber)
  else
     begin
     // Add the argument symbol to the user function local symbol table
     // if not currentUserFunction.symbolTable.find(sc.tokenString, symbolIndex) then
     // symbolIndex := currentUserFunction.symbolTable.addSymbol(sc.tokenString, TScope.scUserFunction);
     result := TASTIdentifier.Create(sc.tokenString, sc.tokenRecord.lineNumber);
     sc.nextToken;
     end;
end;


// Parse: five possible patters  x:y  :y  x:  :  x
function TConstructAST.parseIndexOrSlice : TASTNode;
var exp1 : TASTNode;
begin
 // check for :x and :  A colon is translated to SliceAll, and x to expression
 if sc.token = tColon then
     begin
     sc.nextToken;
     if sc.token in [tComma, tRightBracket] then  // Must be just a single ':'
        result := TASTSlice.Create (TASTSliceAll.Create, TASTSliceAll.Create, sc.tokenRecord.lineNumber)
     else
        result := TASTSlice.Create (TASTSliceAll.Create, expression(), sc.tokenRecord.lineNumber)
     end
  else
     begin
     // check for x:  x:y   x
     exp1 := expression();
     if sc.token = tColon then
        begin
        nextToken;
        if sc.token in [tComma, tRightBracket] then
           result := TASTSlice.Create (exp1, TASTSliceAll.Create, sc.tokenRecord.lineNumber)
        else
           result := TASTSlice.Create (exp1, expression(), sc.tokenRecord.lineNumber)
        end
     else
        result := exp1;
     end;
end;


// Parse something of the form variable '[' expressionList ']'
// Such indexing applies to lists and strings
function TConstructAST.parseIndexedVariable : TASTNode;
var nodeList : TASTNodeList;
    exp : TASTNode;
begin
  nodelist := TASTNodeList.Create (ntNodeList);
  sc.nextToken;
  if sc.token = tRightBracket then
     begin
     result := TASTErrorNode.Create('indexed operator must have at least one index', sc.tokenRecord.lineNumber, sc.tokenRecord.columnNumber);
     sc.nextToken;
     exit;
     end;

  exp := parseIndexOrSlice();

  nodeList.list.Add(exp);
  while sc.token = tComma do
    begin
    sc.nextToken;
    nodeList.list.Add(parseIndexOrSlice());
    end;
  expect(tRightBracket);
  exit (nodeList);
end;


function TConstructAST.parseFunctionCall: TASTNode;
var node : TASTNode;
begin
  result := nil;
  if sc.token <> tRightParenthesis then
     result := expressionList;

  node := expect(tRightParenthesis);
  // if nil then empty argument list in function call: ()
  if result = nil then
     result := TASTNodeList.Create (ntNodeList);
end;


// primary => factor primaryPlus
function TConstructAST.primary : TASTNode;
var node : TASTNode;
begin
  result := factor;
  node := primaryPlus;
  result := TASTPrimary.Create(result, node, sc.tokenRecord.lineNumber);
end;


// Scan a vector of the form '{' 1, 2, 3 '}'
// Note the first bracket has been scanned in already
function TConstructAST.parseVector : TASTNode;
begin
  // Scan in Vector
  result := TASTCreateVector.Create(sc.tokenRecord.lineNumber);
  if sc.token = tRightCurleyBracket then
     begin
     nextToken;
     Exit;
     end;

  (result as TASTCreateVector).list.Add(expression);
  while sc.token = tComma do
        begin
        nextToken;
       (result as TASTCreateVector).list.Add(expression);
        end;
end;


// Scan in a matrix of the form: {{1,2,3},{4,5,6}} etc
function TConstructAST.parseMatrix : TASTNode;
begin
  nextToken;
  if sc.token = tLeftCurleyBracket then
     begin
     result := TASTCreateMatrix.Create(sc.tokenRecord.lineNumber);
     nextToken;
     (result as TASTCreateMatrix).list.Add(parseVector);
     expect(tRightCurleyBracket);
     while sc.token = tComma do
           begin
           nextToken;
           nextToken;
           (result as TASTCreateMatrix).list.Add(parseVector);
           expect(tRightCurleyBracket);
           end;
     end
  else
     // Else it's just a vector
     result := parseVector;
end;


function TConstructAST.factor : TASTNode;
var node : TASTNode;
begin
  result := nil;
  case sc.token of
   tInteger:
      begin
        result := TASTInteger.Create(sc.tokenInteger, sc.tokenRecord.lineNumber);
        sc.nextToken;
      end;
   tFloat:
      begin
        result := TASTFloat.Create(sc.tokenFloat, sc.tokenRecord.lineNumber);
        sc.nextToken;
      end;
   tIdentifier :
        begin
        result := TASTIdentifier.Create (sc.tokenString, sc.tokenRecord.lineNumber);
        sc.nextToken;
        end;
   tString:
        begin
        result := TASTString.Create(sc.tokenString, sc.tokenRecord.lineNumber);
        sc.nextToken;
        end;
    tNOT:
      begin
        sc.nextToken;
        node := expression();
        if node.nodeType = ntError then
           exit (node);
        result := TASTNotOp.Create(node, sc.tokenRecord.lineNumber);
      end;
    tFalse:
      begin
        result := TASTBoolean.Create(False, sc.tokenRecord.lineNumber);
        sc.nextToken;
      end;
    tTrue:
      begin
        result := TASTBoolean.Create(True, sc.tokenRecord.lineNumber);
        sc.nextToken;
      end;
    tLeftParenthesis:
      begin
        sc.nextToken;
        result := expression();
        expect(tRightParenthesis);
      end;

    tLeftBracket:
      begin
        sc.nextToken;
        result := TASTCreateList.Create;
        if sc.token <> tRightBracket then
           begin
           (result as TASTCreateList).list.Add(expression);
           while sc.token = tComma do
              begin
              sc.nextToken;
              (result as TASTCreateList).list.Add(expression);
              end;
           end;
      expect(tRightBracket);
      end;

     // Reserved for maps
     tLeftCurleyBracket :
       begin
       result := parseMatrix;
       expect(tRightCurleyBracket);
       end;

    tError:
       begin
       result := TASTErrorNode.Create ('Expecting a factor [literal, identifier, or ''{''] but found ' + sc.tokenRecord.FTokenCharacter, sc.tokenRecord.lineNumber, sc.tokenRecord.columnNumber);
       end
   else
      result := TASTErrorNode.Create ('Expecting a factor [literal, identifier, or ''{''] but found ' + TTokenVector.tokenToString ( sc.token), sc.tokenRecord.lineNumber, sc.tokenRecord.columnNumber);
  end;
end;


function TConstructAST.primaryPlus : TASTNode;
var periodStr : string;
    nodeList : TASTNode;
begin
  case sc.token of
     tPeriod :
         begin
         sc.nextToken;
         periodStr := sc.tokenString;
         sc.nextToken;
         result := TASTPrimaryPeriod.Create (TASTIdentifier.Create(periodStr, sc.tokenRecord.lineNumber), primaryPlus, sc.tokenRecord.lineNumber);
         end;
     tLeftParenthesis:  // '(' expression list ')'
         begin
         sc.nextToken;
         nodeList := parseFunctionCall;
         result := TASTPrimaryFunction.Create(nodeList as TASTNodeList, primaryPlus, sc.tokenRecord.lineNumber);
         end;
    tLeftBracket: // '[' expression list ']'
         begin
         nodeList := parseIndexedVariable;
         result := TASTPrimaryIndex.Create(nodeList as TASTNodeList, primaryPlus, sc.tokenRecord.lineNumber);
         end;
  else
      begin result := TASTNull.Create; end;
  end;
end;


// power = {'+' | '-'} factor [ '^' power ]
function TConstructAST.power: TASTNode;
var
  unaryMinus_count, i: integer;
  leftNode, rightNode: TASTNode;
begin
  unaryMinus_count := 0;
  // Handle unary operators, but only count '-'. ++2 is the same as +2 but --2 is not the same as -2
  while (sc.token = tMinus) or (sc.token = tPlus) do
  begin
    case sc.token of
      tMinus:
        inc(unaryMinus_count);
    end;
    sc.nextToken;
  end;

  leftNode := primary;

  if sc.token = tPower then
     begin
     sc.nextToken;
     rightNode := power;
     leftNode := TASTPowerOp.Create(leftNode, rightNode, sc.tokenRecord.lineNumber);
     end;
  for i := 0 to unaryMinus_count - 1 do
      leftNode := TASTUniOp.Create(leftNode, TASTNodeType.ntUnaryMinus, sc.tokenRecord.lineNumber);
  result := leftNode;
end;


// term = power { ('*', '/', MOD, DIV) power }
function TConstructAST.term: TASTNode;
var
  op: TTokenCode;
  leftNode, rightNode: TASTNode;
begin
  leftNode := power;

  while sc.token in [tMult, tDivide, tDivI, tMod, tDotproduct] do
    begin
    op := sc.token; // remember the token
    sc.nextToken;
    rightNode := power;

    case op of
      tMult:
        leftNode := TAstBinOp.Create(leftNode, rightNode, TASTNodeType.ntMult, sc.tokenRecord.lineNumber);
      tDivide:
        leftNode := TAstBinOp.Create(leftNode, rightNode, TASTNodeType.ntDiv, sc.tokenRecord.lineNumber);
      tMod:
        leftNode := TAstBinOp.Create(leftNode, rightNode, TASTNodeType.ntMod, sc.tokenRecord.lineNumber);
      tDivI:
        leftNode := TAstBinOp.Create(leftNode, rightNode, TASTNodeType.ntDivI, sc.tokenRecord.lineNumber);
      tDotproduct:
        leftNode := TAstBinOp.Create(leftNode, rightNode, TASTNodeType.ntDotProduct, sc.tokenRecord.lineNumber);
    end;
    end;
  result := leftNode;
end;


// expression = term { ('+' | '-' | MOD | DIV) power }
function TConstructAST.simpleExpression: TASTNode;
var
  op: TTokenCode;
  leftNode, rightNode: TASTNode;
begin
  leftNode := term;

  while sc.token in [tPlus, tMinus] do
     begin
       op := sc.token; // remember the token
       sc.nextToken;
       rightNode := term;
       case op of
         tPlus:
           leftNode := TASTBinOp.Create(leftNode, rightNode, TASTNodeType.ntAdd, sc.tokenRecord.lineNumber);
         tMinus:
           leftNode := TASTBinOp.Create(leftNode, rightNode, TASTNodeType.ntSub, sc.tokenRecord.lineNumber);
       end;
     end;
  result := leftNode;
end;


// expression = simpleExpression | simpleExpression relationalOp simpleExpression
function TConstructAST.relationalOperators: TASTNode;
var
  op: TTokenCode;
  leftNode, rightNode: TASTNode;
begin
  leftNode := simpleExpression;

  while sc.token in [tLessThan, tLessThanOrEqual, tMoreThan, tMoreThanOrEqual,
    tNotEqual, tEquivalence] do
  begin
    op := sc.token;
    sc.nextToken;
    rightNode := simpleExpression;

    case op of
      tEquivalence:
        leftNode := TAstBinOp.Create(leftNode, rightNode, TASTNodeType.ntEQ, sc.tokenRecord.lineNumber);
      tLessThan:
        leftNode := TAstBinOp.Create(leftNode, rightNode, TASTNodeType.ntLT, sc.tokenRecord.lineNumber);
      tMoreThan:
        leftNode := TAstBinOp.Create(leftNode, rightNode, TASTNodeType.ntGT, sc.tokenRecord.lineNumber);
      tMoreThanOrEqual:
        leftNode := TAstBinOp.Create(leftNode, rightNode, TASTNodeType.ntGE, sc.tokenRecord.lineNumber);
      tLessThanOrEqual:
        leftNode := TAstBinOp.Create(leftNode, rightNode, TASTNodeType.ntLE, sc.tokenRecord.lineNumber);
      tNotEqual:
        leftNode := TAstBinOp.Create(leftNode, rightNode, TASTNodeType.ntNE, sc.tokenRecord.lineNumber);
    end;
  end;
  result := leftNode;
end;


function TConstructAST.expression: TASTNode;
var
  op: TTokenCode;
  leftNode, rightNode: TASTNode;
begin
  leftNode := relationalOperators;

  while sc.token in [tOr, tXor, tAnd] do
  begin
    op := sc.token; // remember the token
    sc.nextToken;
    rightNode := relationalOperators;
    case op of
      tOr:
        leftNode := TAstBinOp.Create(leftNode, rightNode, TASTNodeType.ntOR, sc.tokenRecord.lineNumber);
      tAnd:
        leftNode := TAstBinOp.Create(leftNode, rightNode, TASTNodeType.ntAnd, sc.tokenRecord.lineNumber);
      tXor:
        leftNode := TAstBinOp.Create(leftNode, rightNode, TASTNodeType.ntXor, sc.tokenRecord.lineNumber);
    end;
  end;
  result := leftNode;
end;


// statement = exprStatement | forStatement | ifStatement
// | whileStatement | repeatStatement
// | returnStatment | breakStatement
// | function
// exprStatement =   assignment
// | rightHandSide
// assignment = leftHandSide '=' expression
// rightHandside = expression
// leftHandSide = identifier ( '[' expression ']' )
function TConstructAST.statement: TASTNode;
begin
  result := nil;
  case sc.token of
    tIf:
      result := ifStatement;
    tFor:
      result := forStatement;
    tWhile:
      result := parseWhileStatement;
    tRepeat:
      result := parseRepeatStatement;
    tReturn:
      result := returnStmt;
    tFunction:
      result := parseUserDefinedFunction;
    tBreak:
      result := breakStatement;
    tGlobal:
      result := globalStatement;
    tSwitch:
      result := switchStatement;
    tImport:
      result := importStatement;
    tPrint:
      result := printStatement;
    tPrintln:
      result := printlnStatement;
    tSetColor:
      result := setColorStatement;
    tAssertTrue:
      result := AssertTrueStatement;
    tAssertFalse:
      result := AssertFalseStatement;
    //tHelp:
    //  result := helpStatement;
    tEnd:
      result := nil;
    tUntil:
      exit; // To allow repeat until False
    tEndOfStream:
      exit;
  else
    result := exprStatement;
  end;
end;


// statementList = statement { [ ';' ] statement }
function TConstructAST.statementList: TASTNode;
var node : TASTNode;
    stat : TASTStatementList;
begin
  node := statement();

  stat := TASTStatementList.Create (sc.tokenRecord.lineNumber);
  stat.statementList.Add(node);
  while True do
     begin
     if sc.token = tSemicolon then // semicolons optional
        expect(tSemicolon);
     // Note these are all thing that can end a statement list.
     if sc.token in [tUntil, tEnd, tElse, tCase, tEndOfStream] then
         exit(stat);
     node := statement();
     stat.statementList.Add(node);
     end;
  result := stat;
end;


function TConstructAST.printlnStatement: TASTNode;
var funcArgs: TASTNodeList;
begin
  sc.nextToken;
  if sc.token = tLeftParenthesis then
  begin
    sc.nextToken;
    // It could be an empty function call
    if sc.token <> tRightParenthesis then
      funcArgs := expressionList as TASTNodeList
    else
      funcArgs := TASTNodeList.Create (ntNodeList);
      expect(tRightParenthesis);
  end
  else
    begin
    result := TASTErrorNode.Create ('Expecting opening bracket to println call', sc.tokenRecord.lineNumber, sc.tokenRecord.columnNumber);
    exit;
    end;
  result := TASTPrintln.Create(funcArgs, sc.tokenRecord.lineNumber);
end;


function TConstructAST.printStatement: TASTNode;
var funcArgs: TASTNodeList;
begin
  sc.nextToken;
  if sc.token = tLeftParenthesis then
     begin
     sc.nextToken;
     // It could be an empty function call
     if sc.token <> tRightParenthesis then
        funcArgs := expressionList as TASTNodeList
     else
        funcArgs := TASTNodeList.Create (ntNodeList);
     expect(tRightParenthesis);
     end
  else
    begin
    result := TASTErrorNode.Create ('Expecting opening bracket to print call', sc.tokenRecord.lineNumber, sc.tokenRecord.columnNumber);
    exit;
    end;

  result := TASTPrint.Create(funcArgs, sc.tokenRecord.lineNumber);
end;


function TConstructAST.setColorStatement : TASTNode;
begin
  result := nil;
  sc.nextToken;
  if sc.token = tLeftParenthesis then
     begin
     sc.nextToken;
     result := expression;
     expect (tRightParenthesis);
     result := TASTSetColor.Create(TASTExpression.Create(result, sc.tokenRecord.lineNumber), sc.tokenRecord.lineNumber);
     end
  else
    begin
    result := TASTErrorNode.Create ('Expecting opening bracket to setColor call', sc.tokenRecord.lineNumber, sc.tokenRecord.columnNumber);
    end;
end;


function TConstructAST.AssertTrueStatement: TASTNode;
begin
  result := nil;
  sc.nextToken;
  if sc.token = tLeftParenthesis then
     begin
     sc.nextToken;
     result := expression;
     expect (tRightParenthesis);
     result := TASTAssertTrue.Create(TASTExpression.Create(result, sc.tokenRecord.lineNumber), sc.tokenRecord.lineNumber);
     end;
end;


function TConstructAST.AssertTrueExStatement: TASTNode;
begin
  result := nil;
  sc.nextToken;
  if sc.token = tLeftParenthesis then
     begin
     sc.nextToken;
     result := expression;
     expect (tRightParenthesis);
     result := TASTAssertTrueEx.Create(TASTExpression.Create(result, sc.tokenRecord.lineNumber), sc.tokenRecord.lineNumber);
     end;
end;


function TConstructAST.AssertFalseStatement: TASTNode;
begin
  result := nil;
  sc.nextToken;
  if sc.token = tLeftParenthesis then
     begin
     sc.nextToken;
     result := expression;
     expect (tRightParenthesis);
     result := TASTAssertFalse.Create(TASTExpression.Create(result, sc.tokenRecord.lineNumber), sc.tokenRecord.lineNumber);
     end;
end;


function TConstructAST.helpStatement: TASTNode;
begin
  result := nil;
  sc.nextToken;
  if sc.token = tLeftParenthesis then
     begin
     sc.nextToken;
     result := expression;
     expect (tRightParenthesis);
     result := TASTHelp.Create(TASTExpression.Create(result, sc.tokenRecord.lineNumber), sc.tokenRecord.lineNumber);
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
// as well as permit indices for lists to be of any complexity

// AST:
// (exprStatement) -> (identifier) and (expression)
// OR (exprStatement) -> (expression)
function TConstructAST.exprStatement: TASTNode;
var
  expressionNode: TASTExpression;
  node: TASTNode;
  exprNode : TASTNode;
begin
  node := expression;

   if sc.token = tEquals then
      begin
      // Then its of the form a = ?
      sc.nextToken;
      exprNode := expression;
      expressionNode := TASTExpression.Create(exprNode, sc.tokenRecord.lineNumber);

      //if node.nodeType <> ntPrimary then
      if node.nodeType <> ntPrimary then
         begin
         expressionNode.freeAST;
         node.freeAST;
         result := TASTErrorNode.Create ('Expecting an identifier on the left-hand side', sc.tokenRecord.lineNumber, sc.tokenRecord.columnNumber);
         exit;
         end;

      result := TASTAssignment.Create(node as TASTPrimary, expressionNode, sc.tokenRecord.lineNumber);
      end
   else
      result := TASTExpressionStatement.Create (node, sc.tokenRecord.lineNumber);
end;


// argumentList = expression { ',' expression }
// Returns the number of expressions that were parsed
function TConstructAST.expressionList: TASTNode;
var exp : TASTNode;
    nodeList : TASTNodeList;
begin
  exp := expression;

  nodeList := TASTNodeList.Create (ntNodeList);
  nodeList.list.Add(exp);
  while sc.token = tComma do
    begin
      sc.nextToken;
      nodeList.list.Add(expression());
    end;
  result := nodeList;
end;


{
  switch index do
  case 1 : println ("1")
  case 2 : println ("2"); println ("1")
  else
  println ("Else")
  end }
function TConstructAST.switchStatement: TASTNode;
var
  switchExpression: TASTNode;
  listOfCaseStatements: TASTListOfCaseStatements;
  caseValue: TASTInteger;
  caseStatementList: TASTStatementList;
  elseStatement: TASTStatementList;
  node : TASTNode;
begin
  expect(tSwitch);

  switchExpression := simpleExpression;
  if switchExpression.nodeType = ntError then
     exit (switchExpression);

  listOfCaseStatements := TASTListOfCaseStatements.Create (sc.tokenRecord.lineNumber);

  caseStatementList := nil;
  elseStatement := nil;

  while sc.token = tCase do
    begin
      expect(tCase);

      if sc.token = tInteger then
         caseValue := TASTInteger.Create(sc.tokenInteger, sc.tokenRecord.lineNumber)
      else
         begin
         switchExpression.freeAST;
         listOfCaseStatements.freeAST;
         result := TASTErrorNode.Create ('Expecting integer in case value', sc.tokenRecord.lineNumber, sc.tokenRecord.columnNumber);
         exit;
         end;

      sc.nextToken;
      node := expect(tColon);
      if node <> nil then
         begin
         switchExpression.freeAST;
         listOfCaseStatements.freeAST;
         caseValue.freeAST;
         exit (node);
         end;
      caseStatementList := statementList as TASTStatementList;
      if caseStatementList.nodeType = ntError then
         begin
         switchExpression.freeAST;
         caseValue.freeAST;
         exit (caseStatementList);
         end;
      listOfCaseStatements.list.Add(TASTCaseStatement.Create(caseValue, caseStatementList, sc.tokenRecord.lineNumber));
    end;

  if sc.token = tElse then
     begin
     sc.nextToken;
     elseStatement := statementList as TASTStatementList;
     end
  else
     elseStatement := nil;

  node := expect(tEnd);
  if node <> nil then
     begin
     switchExpression.freeAST;
     listOfCaseStatements.freeAST;
     if elseStatement <>  nil then
        elseStatement.freeAST;
     exit (node);
     end;
  if listOfCaseStatements <> nil then
     exit (TASTSwitch.Create(switchExpression, listOfCaseStatements, elseStatement, sc.tokenRecord.lineNumber));
  result := TASTErrorNode.Create('Empty switch construct', sc.tokenRecord.lineNumber, sc.tokenRecord.columnNumber);
end;


// ifStatement = IF expression THEN statement ifEnd
// ifEnd = END | ELSE statementList END
// AST:
// (if) -> (condition) and (thenStatementList) and (elseStatementList)
function TConstructAST.ifStatement: TASTNode;
var
  condition, listOfStatements, listOfElseStatements, node: TASTNode;
begin
  expect(tIf);

  condition := expression;

  node := expect(tThen);

  listOfStatements := statementList;

  if sc.token = tElse then
     begin
     sc.nextToken;
     listOfElseStatements := statementList;
     node := expect(tEnd);
     result := TASTIf.Create(condition, listOfStatements, listOfElseStatements, sc.tokenRecord.lineNumber);
     end
  else
     begin
     node := expect(tEnd);
     result := TASTIf.Create(condition, listOfStatements, nil, sc.tokenRecord.lineNumber);
     end;
end;


function TConstructAST.breakStatement: TASTNode;
begin
  if stackOfBreakStacks.Count = 0 then
     begin
     result := TASTErrorNode.Create ('Break statement illegal in this context', sc.tokenRecord.lineNumber, sc.tokenRecord.columnNumber);
     sc.nextToken;
     exit;
     end;

  result := TASTNode.Create(TASTNodeType.ntBreak, sc.tokenRecord.lineNumber);
  sc.nextToken;
end;


// whileStatement = WHILE expression DO statementList END
// AST:
// (while) -> (condition) and (statementList)
function TConstructAST.parseWhileStatement: TASTNode;
var
  breakJump: integer;
  breakStack: TStack<integer>;
  listOfStatements: TASTStatementList;
  condition: TASTExpression;
  node : TASTNode;
begin
  breakStack := TStack<integer>.Create;
  stackOfBreakStacks.Push(breakStack);
  try
    expect(tWhile);   // Guaranteed to be true

    condition := TASTExpression.Create(expression, sc.tokenRecord.lineNumber);

    node := expect(tDo);

    listOfStatements := statementList() as TASTStatementList;

    node := expect(tEnd);

    result := TASTWhile.Create(condition, listOfStatements, sc.tokenRecord.lineNumber);

    while breakStack.Count > 0 do
        breakJump := breakStack.Pop;
  finally
    breakStack := stackOfBreakStacks.Pop;
    breakStack.Free;
  end;
end;


// repeatStatement = REPEAT statementList UNTIL expression
// AST:
// (repeat) -> (statementList) and (condition)
function TConstructAST.parseRepeatStatement: TASTNode;
var
  breakJump: integer;
  breakStack1: TStack<integer>;
  condition, listOfStatements, node: TASTNode;
begin
  breakStack1 := TStack<integer>.Create;
  stackOfBreakStacks.Push(breakStack1);
  try
    expect(tRepeat);   // Guaranteed to be true

    listOfStatements := statementList as TASTStatementList;

    node := expect(tUntil);

    condition := expression;

    result := TASTRepeat.Create(listOfStatements, condition, sc.tokenRecord.lineNumber);
    while breakStack1.Count > 0 do
        breakJump := breakStack1.Pop;
  finally
    breakStack1 := stackOfBreakStacks.Pop;
    breakStack1.Free;
  end;
end;


// forStatement = FOR identifier = expression TO/DOWNTO expression DO statementList END
// AST:
// (for) -> (Iteration) and (body)
// (body) -> (statementList)
// (Iteration) -> (assign) and (upper) and (to/downto)
// (assign) -> (symbol) and (expression)
function TConstructAST.forStatement: TASTNode;
var
  toToken: TTokenCode;
  breakJump: integer;
  breakStack: TStack<integer>;
  assignment: TASTAssignment;
  iterationBlock: TASTIterationBlock;
  body, node: TASTNode;
  lower, upper, expr : TASTExpression;
  id : TASTIdentifier;
begin
  breakStack := TStack<integer>.Create;
  stackOfBreakStacks.Push(breakStack);
  try
    expect(tFor);    // Guaranteed to be true

    node := expect(tIdentifier);

    id := TASTIdentifier.Create (sc.tokenString, sc.tokenRecord.lineNumber);

   if sc.token = tIn then
       begin
       nextToken;
       node := expression;
       expect (tDo);

       iterationBlock := TASTIterationBlock.Create(id, node, sc.tokenRecord.lineNumber);

       // .... do <body>
       body := statementList;

       result := TASTFor.Create(iterationBlock, body, sc.tokenRecord.lineNumber);

       while breakStack.Count > 0 do
         breakJump := breakStack.Pop;

       expect(tEnd);
       exit;
       end;

    node := expect(tEquals);

    node := expression;
    lower := TASTExpression.Create(node, sc.tokenRecord.lineNumber);

    if sc.token in [tTo, tDownTo] then
       begin
       toToken := sc.token;
       sc.nextToken;
       end
    else
      begin
      id.freeAST;
      lower.freeAST;
      result := TASTErrorNode.Create ('expecting "to" or "downto" in for loop', sc.tokenRecord.lineNumber, sc.tokenRecord.columnNumber);
      exit;
      end;

    // Parse the upper limit expression
    node := expression;
    upper := TASTExpression.Create(node, sc.tokenRecord.lineNumber);

    iterationBlock := TASTIterationBlock.Create(id, lower, upper, sc.tokenRecord.lineNumber);
    if toToken = tTo then
      iterationBlock.direction := TASTNode.Create(TASTNodeType.ntTo, sc.tokenRecord.lineNumber)
    else
      iterationBlock.direction := TASTNode.Create(TASTNodeType.ntDownTo, sc.tokenRecord.lineNumber);

    // Deal with any step keyword
    iterationBlock.stepValue := 1;
    if sc.token = tStep then
    begin
      sc.nextToken;
      if sc.token = tInteger then
         begin
         iterationBlock.stepValue := sc.tokenRecord.FTokenInteger;
         sc.nextToken;
         end
    end;

    node := expect(tDo);

    // .... do <body>
    body := statementList;

    result := TASTFor.Create(iterationBlock, body, sc.tokenRecord.lineNumber);

    while breakStack.Count > 0 do
      breakJump := breakStack.Pop;

    node := expect(tEnd);
  finally
    breakStack := stackOfBreakStacks.Pop;
    breakStack.Free;
  end;
end;


// function = function identifier '(' argumentList ')' statementList
function TConstructAST.parseUserDefinedFunction: TASTNode;
var
  functionName: string;
  statementlistNode: TASTStatementList;
  argList: TASTNodeList;
  node : TASTNode;
begin
  sc.nextToken;
  if sc.token = tIdentifier then
     functionName := sc.tokenString
  else
     begin
     result := TASTErrorNode.Create ('expecting function name', sc.tokenRecord.lineNumber, sc.tokenRecord.columnNumber);
     exit;
     end;

  globalVariableList := TStringList.Create;
  try
    enterUserFunctionScope();
    try
      sc.nextToken;
      if sc.token = tLeftParenthesis then
         begin
         sc.nextToken;
         node := functionArgumentList();
         argList := node as TASTNodeList;
         node := expect(tRightParenthesis);
         end
     else
         argList := TASTNodeList.Create (ntNodeList);

      statementlistNode := statementList as TASTStatementList;
    finally
      exitUserFunctionScope();
      globalVariableList.Free;
    end;
    node := expect(tEnd);
    // currentModuleName is required so that we can add the name of the function to the symbol table
    // early and before we parse the body of the fucntion so that recursive function calls can be handled.
    result := TASTUserFunction.Create(primaryModuleName, functionName, argList, statementlistNode, sc.tokenRecord.lineNumber);

   except
    on Exception do
    begin
      raise;
    end;
  end;
end;


// argumentList = argument { ',' argument }
// AST:
// nodeList -> (arg) and (arg) and (arg) and ....
function TConstructAST.functionArgumentList: TASTNode;
var alist : TASTNodeList;
begin
  alist := TASTNodeList.Create (ntNodeList);
  if sc.token = tIdentifier then
     alist.list.Add(functionArgument);

  while sc.token = tComma do
    begin
    sc.nextToken;
    alist.list.Add(functionArgument);
    end;
  exit (alist);
end;


// argument = identifier | REF identifier
function TConstructAST.functionArgument: TASTNode;
begin
  if sc.token = tRef then
     sc.nextToken;
  result := variable;
end;


// global x, y, ....
function TConstructAST.globalStatement: TASTNode;
var
  variableList: TASTNodeList;
begin
  expect(tGlobal);
  if inUserFunctionParsing then
      begin
      variableList := TASTNodeList.Create (ntNodeList);
      if sc.token = tIdentifier then
        begin
          // We're keeping a list of declared global variables for a given
          // user function. This list is simply used to check for duplicates such
          // as global a, b, a
          // or global a
          // global a
          // After the function has been parsed the global list is deleted.
          globalVariableList.Add(sc.tokenString);
          variableList.list.Add(TASTIdentifier.Create(sc.tokenString, sc.tokenRecord.lineNumber));

          sc.nextToken;
          while sc.token = tComma do
              begin
              sc.nextToken;

              if globalVariableList.IndexOf(sc.tokenString) = -1 then
                globalVariableList.Add(sc.tokenString)
              else
                begin
                result := TASTErrorNode.Create ('Duplicate global variable', sc.tokenRecord.lineNumber, sc.tokenRecord.columnNumber);
                exit;
                end;

              variableList.list.Add(TASTIdentifier.Create(sc.tokenString, sc.tokenRecord.lineNumber));
              sc.nextToken;
              end;

          result := TASTGlobal.Create(primaryModuleName, variableList, sc.tokenRecord.lineNumber);
        end
      else
        begin
        result := TASTErrorNode.Create ('Expecting variable name in global declaration', sc.tokenRecord.lineNumber, sc.tokenRecord.columnNumber);
        exit;
        end;
      end
  else
      begin
      result := TASTErrorNode.Create ('The global keyword can only be used inside user functions', sc.tokenRecord.lineNumber, sc.tokenRecord.columnNumber);
      exit;
      end;
end;


function TConstructAST.importStatement: TASTNode;
begin
  sc.nextToken();
  if (sc.token = tIdentifier) or (sc.token = tString) then
      begin
      result := TASTImport.Create(sc.tokenString, sc.tokenRecord.lineNumber);
      sc.nextToken();
      end
  else
      begin
      result := TASTErrorNode.Create ('Expecting name of import file after import keyword', sc.tokenRecord.lineNumber, sc.tokenRecord.columnNumber);
      sc.nextToken;
      exit;
      end;
end;


// returnStatement = RETURN expression
function TConstructAST.returnStmt: TASTNode;
begin
  if not inUserFunctionParsing then
     begin
     result := TASTErrorNode.Create ('You cannot use a return statement outside a user function', sc.tokenRecord.lineNumber, sc.tokenRecord.columnNumber);
     exit;
     end;

  expect(tReturn);
  result := TASTReturn.Create(TASTExpression.Create(expression, sc.tokenRecord.lineNumber), sc.tokenRecord.lineNumber);
end;


function TConstructAST.buildModuleAST(moduleName: string; var astRoot: TASTNode): TModuleLib;
begin
  result := TModulelib.Create(moduleName, THelp.Create (''));

  sc.reset;
  sc.mode := vtReading;
  sc.nextToken;

  primaryModuleName := moduleName;
  astRoot := statementList;
end;


// program = statementList
function TConstructAST.constructAST: TASTNode;
begin
  // reset the token vector and switch to reading mode
  sc.reset;
  sc.mode := vtReading;
  sc.nextToken;
  primaryModuleName := TSymbol.mainModuleId; // we're in the main module
  if sc.token = tEndOfStream then
     result := nil
  else
     result := statementList;
end;

end.
