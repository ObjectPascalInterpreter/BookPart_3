unit uSyntaxParser;


// Parser and constructor Rhodus Language Part III

// Developed under Delphi for Windows and Mac platforms.

// *** This source is distributed under Apache 2.0 ***

// Copyright (C)  2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

// Usage:
//
// sc := TScanner.Create;
// sc.scanString ('a = 2; b = a + 5.6')
// syntaxParser := TSyntaxParser.Create (sc);


interface

uses Classes,
     SysUtils,
     uScanner, uSymbolTable, uLibModule,
  Generics.Collections, uVM;

type
  TBreakStack = TStack<integer>;
  TModuleNameStack = TStack<string>;

  TSyntaxParser = class(TObject)
  private
    stackOfBreakStacks: TStack<TBreakStack>;
    moduleNameStack: TModuleNameStack;
    primaryModuleName: string;

    sc: TScanner;

    // Very private. don't use them directly, use the helper routines
    inUserFunctionParsing: boolean;
    // Used to gather global variable names, this is only
    // used during AST construction, its discarded afterwards.
    globalVariableList: TStringList;

    // Helper routines for the above
    procedure enterUserFunctionScope;
    procedure exitUserFunctionScope;

    procedure expect(thisToken: TTokenCode);
    procedure variable;
    procedure parseList;
    procedure parseIndexedVariable;
    procedure parseFunctionCall;

    procedure primary;
    procedure factor2;
    procedure primaryPlus;

    procedure power;
    procedure term;
    procedure simpleExpression;
    procedure relationalOperators;
    procedure expression;
    procedure exprStatement;
    procedure statement;
    procedure statementList;
    procedure expressionList;
    procedure ifStatement;
    procedure breakStatement;
    procedure globalStatement;
    procedure switchStatement;

    procedure importStatement;
    procedure parseWhileStatement;
    procedure parseRepeatStatement;
    procedure forStatement;
    procedure parseUserDefinedFunction;
    procedure functionArgumentList;
    procedure functionArgument;
    procedure returnStmt;

    procedure printlnStatement;
    procedure printStatement;
    procedure setColorStatement;
    procedure AssertTrueStatement;
    procedure AssertTrueExStatement;
    procedure AssertFalseStatement;
    procedure helpStatement;
  public
    procedure   parseModule(moduleName: string);
    procedure   parseProgram;
    function    inUserFunctionScope: boolean;
    constructor Create(sc: TScanner);
    destructor  Destroy; override;
  end;

  ESyntaxException = class(Exception)
      lineNumber, columnNumber : integer;
      errorMsg : string;
      constructor Create (errMsg : string; lineNumber, columnNumber : integer);
  end;

implementation

uses Math, IOUtils, uGlobal, uUtils, uOpCodes, uMachineStack, uCompile, uBuiltInGlobal;

// ----------------------------------------------------------------------
constructor ESyntaxException.Create (errMsg : string; lineNumber, columnNumber : integer);
begin
  self.errorMsg := errMsg;
  self.lineNumber := lineNumber;
  self.columnNumber := columnNumber;
end;

// Start of parser Class
// ----------------------------------------------------------------------
constructor TSyntaxParser.Create(sc: TScanner);
var
  globalSymbol: TSymbol;
  module: TModule;
  symbol : TSymbol;
begin
  inherited Create;
  self.sc := sc;
  inUserFunctionParsing := False;
  stackOfBreakStacks := TStack<TBreakStack>.Create;
  moduleNameStack := TModuleNameStack.Create;
  module := getMainModule();

  if module = nil then
     raise ESyntaxException.Create ('Internal error: AST can''t find main module', sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);
end;



destructor TSyntaxParser.Destroy;
begin
  stackOfBreakStacks.Free;
  moduleNameStack.Free;
  inherited;
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
procedure TSyntaxParser.expect(thisToken: TTokenCode);
var err : string;
begin
  if sc.token <> thisToken then
     raise ESyntaxException.Create ('expecting ' + TScanner.tokenToString (thisToken),  sc.tokenElement.lineNumber, sc.tokenElement.columnNumber)
  else
    sc.nextToken;
end;


procedure TSyntaxParser.enterUserFunctionScope;
begin
  inUserFunctionParsing := True;
end;


procedure TSyntaxParser.exitUserFunctionScope;
begin
  inUserFunctionParsing := False;
end;


function TSyntaxParser.inUserFunctionScope: boolean;
begin
  result := inUserFunctionParsing;
end;


// Parse a function argument in a function definition
procedure TSyntaxParser.variable;
begin
  if sc.token <> tIdentifier then
     raise ESyntaxException.Create ('expecting identifier in function argument definition',  sc.tokenElement.lineNumber, sc.tokenElement.columnNumber)
  else
     sc.nextToken;
end;

// Parse a list of the form: expression ',' expression ','' etc.
// Returns the number of items found in the list
procedure TSyntaxParser.parseList;
begin
  expression;
  while sc.token = tComma do
    begin
      sc.nextToken;
      expression;
    end;
end;


// Parse something of the form variable '[' expressionList ']'
// Such indexing applies to lists and strings
procedure TSyntaxParser.parseIndexedVariable;
begin
  sc.nextToken;
  if sc.token = tRightBracket then
     begin
     raise ESyntaxException.Create('indexed operator must have at least one index', sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);
     sc.nextToken;
     exit;
     end;

  expression();

  while sc.token = tComma do
    begin
    sc.nextToken;
    expression;
    end;
  expect(tRightBracket);
end;


procedure TSyntaxParser.parseFunctionCall;
begin
  if sc.token <> tRightParenthesis then
     begin
     expressionList;
     end;
  expect(tRightParenthesis);
end;


// primary => factor primaryPlus
procedure TSyntaxParser.primary;
begin
  factor2;
  primaryPlus;
end;


procedure TSyntaxParser.factor2;
begin
  case sc.token of
   tInteger:
      begin
        sc.nextToken;
      end;
   tFloat:
      begin
        sc.nextToken;
      end;
   tIdentifier :
        begin
        sc.nextToken;
        end;
   tString:
        begin
        sc.nextToken;
        end;
    tNOT:
      begin
        sc.nextToken;
        expression();
      end;
    tFalse:
      begin
        sc.nextToken;
      end;
    tTrue:
      begin
        sc.nextToken;
      end;
    tLeftParenthesis:
      begin
        sc.nextToken;
        expression();
        expect(tRightParenthesis);
      end;
    tLeftCurleyBracket:
      begin
        sc.nextToken;
        if sc.token <> tRightCurleyBracket then
           begin
           expression;
           while sc.token = tComma do
              begin
              sc.nextToken;
              expression;
              end;
           end;

        expect(tRightCurleyBracket);
       end;
    tError:
       begin
       raise ESyntaxException.Create ('Expecting a factor [literal, identifier, or ''{''] but found ' + sc.tokenElement.FTokenCharacter, sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);
       end
   else
      raise ESyntaxException.Create ('Expecting a factor [literal, identifier, or ''{''] but found ' + sc.tokenToString, sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);
  end;
end;


procedure TSyntaxParser.primaryPlus;
begin
  case sc.token of
     tPeriod :
         begin
         sc.nextToken;
         expect (tIdentifier);
         primaryPlus;
         end;
     tLeftParenthesis:  // '(' expression list ')'
         begin
         sc.nextToken;
         parseFunctionCall;
         primaryPlus
         end;
    tLeftBracket: // '[' expression list ']'
         begin
         parseIndexedVariable;
         primaryPlus;
         end;
  end;
end;


// power = {'+' | '-'} factor [ '^' power ]
procedure TSyntaxParser.power;
var
  unaryMinus_count, i: integer;
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

  primary;

  if sc.token = tPower then
     begin
     sc.nextToken;
     power;
     end;
end;


// term = power { ('*', '/', MOD, DIV) power }
procedure TSyntaxParser.term;
var
  op: TTokenCode;
begin
  power;

  while sc.token in [tMult, tDivide, tDivI, tMod] do
    begin
    op := sc.token; // remember the token
    sc.nextToken;
    power;
    end;
end;


// expression = term { ('+' | '-' | MOD | DIV) power }
procedure TSyntaxParser.simpleExpression;
var
  op: TTokenCode;
begin
  term;

  while sc.token in [tPlus, tMinus] do
     begin
       op := sc.token; // remember the token
       sc.nextToken;
       term;
     end;
end;


// expression = simpleExpression | simpleExpression relationalOp simpleExpression
procedure TSyntaxParser.relationalOperators;
var
  op: TTokenCode;
begin
  simpleExpression;

  while sc.token in [tLessThan, tLessThanOrEqual, tMoreThan, tMoreThanOrEqual,
    tNotEqual, tEquivalence] do
  begin
    op := sc.token;
    sc.nextToken;
    simpleExpression;
  end;
end;


procedure TSyntaxParser.expression;
var
  op: TTokenCode;
begin
  relationalOperators;

   while sc.token in [tOr, tXor, tAnd] do
  begin
    op := sc.token; // remember the token
    sc.nextToken;
    relationalOperators;
  end;
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
procedure TSyntaxParser.statement;
begin
  case sc.token of
    tIf:
      ifStatement;
    tFor:
      forStatement;
    tWhile:
      parseWhileStatement;
    tRepeat:
      parseRepeatStatement;
    tReturn:
      returnStmt;
    tFunction:
      parseUserDefinedFunction;
    tBreak:
      breakStatement;
    tGlobal:
      globalStatement;
    tSwitch:
     switchStatement;
    tImport:
      importStatement;
    tPrint:
      printStatement;
    tPrintln:
      printlnStatement;
    tSetColor:
      setColorStatement;
    tAssertTrue:
      AssertTrueStatement;
    tAssertFalse:
     AssertFalseStatement;
    tHelp:
     helpStatement;
    tEnd:
      begin end;
    tUntil:
      exit; // To allow repeat until False
    tEndOfStream:
      exit;
  else
    exprStatement;
  end;
end;


// statementList = statement { [ ';' ] statement }
procedure TSyntaxParser.statementList;
begin
  statement();

  while True do
     begin
     if sc.token = tSemicolon then // semicolons optional
        expect(tSemicolon);
     // Note these are all thing that can end a statement list.
     if sc.token in [tUntil, tEnd, tElse, tCase, tEndOfStream] then
         exit();
     statement();
     end;
end;


procedure TSyntaxParser.printlnStatement;
begin
  sc.nextToken;
  if sc.token = tLeftParenthesis then
  begin
    sc.nextToken;
    // It could be an empty function call
    if sc.token <> tRightParenthesis then
      expressionList;
    expect(tRightParenthesis);
  end
  else
    begin
    //result := TASTErrorNode.Create ('Expecting opening bracket to println call', sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);
    exit;
    end;
end;


procedure TSyntaxParser.printStatement;
begin
  sc.nextToken;
  if sc.token = tLeftParenthesis then
     begin
     sc.nextToken;
     // It could be an empty function call
     if sc.token <> tRightParenthesis then
        expressionList;

     expect(tRightParenthesis);
     end
  else
    begin
    raise ESyntaxException.Create ('Expecting opening bracket to print call', sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);
    exit;
    end;

end;


procedure TSyntaxParser.setColorStatement;
begin
  sc.nextToken;
  if sc.token = tLeftParenthesis then
     begin
     sc.nextToken;
     expression;
     expect(tRightParenthesis);
     end
  else
    begin
    raise ESyntaxException.Create ('Expecting opening bracket to setColor call', sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);
    exit;
    end;
end;


procedure TSyntaxParser.AssertTrueStatement;
begin
  sc.nextToken;
  if sc.token = tLeftParenthesis then
     begin
     sc.nextToken;
     expression;

     expect(tRightParenthesis);
     end;
end;


procedure TSyntaxParser.AssertTrueExStatement;
begin
  sc.nextToken;
  if sc.token = tLeftParenthesis then
     begin
     sc.nextToken;
     expression;
     expect(tRightParenthesis);
     end;
end;


procedure TSyntaxParser.AssertFalseStatement;
begin
  sc.nextToken;
  if sc.token = tLeftParenthesis then
     begin
     sc.nextToken;
     expression;
     expect(tRightParenthesis);
     end;
end;


procedure TSyntaxParser.helpStatement;
begin
  sc.nextToken;
  if sc.token = tLeftParenthesis then
     begin
     sc.nextToken;
     expression;

     expect(tRightParenthesis);
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
procedure TSyntaxParser.exprStatement;
begin
  expression;

   if sc.token = tEquals then
      begin
      // Then its of the form a = ?
      sc.nextToken;
      expression;
      end
end;


// argumentList = expression { ',' expression }
// Returns the number of expressions that were parsed
procedure TSyntaxParser.expressionList;
begin
  expression;
  while sc.token = tComma do
    begin
      sc.nextToken;
      expression();
    end;
end;


{
  switch index do
  case 1 : println ("1")
  case 2 : println ("2"); println ("1")
  else
  println ("Else")
  end }
procedure TSyntaxParser.switchStatement;
begin
  expect(tSwitch);

  while sc.token = tCase do
    begin
      expect(tCase);

      if sc.token <> tInteger then
         raise ESyntaxException.Create ('Expecting integer in case value', sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);

      sc.nextToken;
      expect(tColon);

      statementList;
    end;

  if sc.token = tElse then
     begin
     sc.nextToken;
     statementList;;
     end
  else
     expect(tEnd);
  raise ESyntaxException.Create('Empty switch construct', sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);
end;


// ifStatement = IF expression THEN statement ifEnd
// ifEnd = END | ELSE statementList END
// AST:
// (if) -> (condition) and (thenStatementList) and (elseStatementList)
procedure TSyntaxParser.ifStatement;
begin
  expect(tIf);

  expression;

  expect(tThen);

  statementList;

  if sc.token = tElse then
     begin
     sc.nextToken;
     statementList;
     expect(tEnd);
     end
  else
     begin
     expect(tEnd);
     end;
end;


procedure TSyntaxParser.breakStatement;
begin
  if stackOfBreakStacks.Count = 0 then
     begin
     //result := TASTErrorNode.Create ('Break statement illegal in this context', sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);
     sc.nextToken;
     exit;
     end;
  sc.nextToken;
end;


// whileStatement = WHILE expression DO statementList END
// AST:
// (while) -> (condition) and (statementList)
procedure TSyntaxParser.parseWhileStatement;
var
  breakJump: integer;
  breakStack: TStack<integer>;
begin
  breakStack := TStack<integer>.Create;
  stackOfBreakStacks.Push(breakStack);
  try
    expect(tWhile);   // Guaranteed to be true

    expression;

    expect(tDo);

    statementList();

    expect(tEnd);

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
procedure TSyntaxParser.parseRepeatStatement;
var
  breakJump: integer;
  breakStack1: TStack<integer>;
begin
  breakStack1 := TStack<integer>.Create;
  stackOfBreakStacks.Push(breakStack1);
  try
    expect(tRepeat);   // Guaranteed to be true

    statementList;

    expect(tUntil);

    expression;

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
procedure TSyntaxParser.forStatement;
var
  toToken: TTokenCode;
  breakJump: integer;
  breakStack: TStack<integer>;
begin
  breakStack := TStack<integer>.Create;
  stackOfBreakStacks.Push(breakStack);
  try
    expect(tFor);    // Guaranteed to be true

    expect(tIdentifier);

    expect(tEquals);

    expression;

    if sc.token in [tTo, tDownTo] then
       begin
       toToken := sc.token;
       sc.nextToken;
       end
    else
      raise ESyntaxException.Create ('expecting "to" or "downto" in for loop', sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);

    // Parse the upper limit expression
    expression;

    // Deal with any step keyword
    if sc.token = tStep then
    begin
      sc.nextToken;
      if (sc.token = tInteger) or (sc.token = tFloat) then
         begin
          sc.nextToken;
         end
      else
         raise ESyntaxException.Create ('step value must be an integer ro float value', sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);
    end;

    expect(tDo);

    // .... do <body>
    statementList;

    while breakStack.Count > 0 do
      breakJump := breakStack.Pop;

    expect(tEnd);
  finally
    breakStack := stackOfBreakStacks.Pop;
    breakStack.Free;
  end;
end;


// function = function identifier '(' argumentList ')' statementList
procedure TSyntaxParser.parseUserDefinedFunction;
var
  functionName: string;
  newUserFunction: boolean;
begin
  newUserFunction := False;
  sc.nextToken;
  if sc.token = tIdentifier then
     begin
     functionName := sc.tokenString;
     newUserFunction := True;
     end
  else
     begin
     //result := TASTErrorNode.Create ('expecting function name', sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);
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
         functionArgumentList();

         expect(tRightParenthesis);
         end;

      statementList;
    finally
      exitUserFunctionScope();
      globalVariableList.Free;
    end;
    expect(tEnd);

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
procedure TSyntaxParser.functionArgumentList;
begin
  if sc.token = tIdentifier then
     functionArgument;

  while sc.token = tComma do
    begin
    sc.nextToken;
    functionArgument;
    end;
end;


// argument = identifier | REF identifier
procedure TSyntaxParser.functionArgument;
begin
  if sc.token = tRef then
     sc.nextToken;
  variable;
end;


// global x, y, ....
procedure TSyntaxParser.globalStatement;
begin
  expect(tGlobal);
  if inUserFunctionParsing then
      begin
      if sc.token = tIdentifier then
        begin
          // We're keeping a list of declared global variables for a given
          // user function. This list is simply used to check for duplicates such
          // as global a, b, a
          // or global a
          // global a
          // After the function has been parsed the global list is deleted.
          globalVariableList.Add(sc.tokenString);

          sc.nextToken;
          while sc.token = tComma do
              begin
              sc.nextToken;

              if globalVariableList.IndexOf(sc.tokenString) = -1 then
                globalVariableList.Add(sc.tokenString)
              else
                begin
                //result := TASTErrorNode.Create ('Duplicate global variable', sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);
                exit;
                end;
              sc.nextToken;
              end;
        end
      else
        raise ESyntaxException.Create ('Expecting variable name in global declaration', sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);
      end
  else
      raise ESyntaxException.Create ('The global keyword can only be used inside user functions', sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);
end;


procedure TSyntaxParser.importStatement;
begin
  sc.nextToken();
  if (sc.token = tIdentifier) then
      sc.nextToken()
  else
      raise ESyntaxException.Create ('Expecting name of import file after import keyword', sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);
end;


// returnStatement = RETURN expression
procedure TSyntaxParser.returnStmt;
begin
  if not inUserFunctionParsing then
     raise ESyntaxException.Create ('You cannot use a return statement outside a user function', sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);

  expect(tReturn);
  expression;
end;


procedure TSyntaxParser.parseModule(moduleName: string);
begin
  statementList;
end;


// program = statementList
procedure TSyntaxParser.parseProgram;
begin
  primaryModuleName := TSymbol.mainModuleId; // we're in the main module
  if sc.token <> tEndOfStream then
     statementList;
end;

end.