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
     uScanner,
     uScannerTypes,
     uSymbolTable,
     uLibModule,
     Generics.Collections,
     uVM,
     uTokenVector;

type
  TBreakStack = TStack<integer>;
  TModuleNameStack = TStack<string>;

  ESyntaxException = class(Exception)
      lineNumber, columnNumber : integer;
      errorMsg : string;
      constructor Create (errMsg : string; lineNumber, columnNumber : integer);
  end;

  TSyntaxError = record
      lineNumber, columnNumber : integer;
      errorMsg : string;
  end;

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

    procedure nextToken;
    // Helper routines for the above
    procedure enterUserFunctionScope;
    procedure exitUserFunctionScope;

    procedure expect(thisToken: TTokenCode);
    procedure variable;
    procedure parseIndexOrSlice;
    procedure parseIndexedVariable;
    procedure parseFunctionCall;

    procedure parseList;
    procedure primary;
    procedure factor;
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
    tokenVector : TTokenVector;
    procedure   parseModule(moduleName: string);
    function    syntaxCheck (var error : TSyntaxError) : boolean;
    function    inUserFunctionScope: boolean;
    constructor Create(sc: TScanner);
    destructor  Destroy; override;
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
  module: TModule;
begin
  inherited Create;
  self.sc := sc;
  tokenVector := TTokenVector.Create;
  inUserFunctionParsing := False;
  stackOfBreakStacks := TStack<TBreakStack>.Create;
  moduleNameStack := TModuleNameStack.Create;
  module := getMainModule();

  if module = nil then
     raise ESyntaxException.Create ('Internal error: AST can''t find main module', tokenVector.tokenRecord.lineNumber, tokenVector.tokenRecord.columnNumber);
end;



destructor TSyntaxParser.Destroy;
begin
  stackOfBreakStacks.Free;
  moduleNameStack.Free;
  tokenVector.Free;
  inherited;
end;


procedure TSyntaxParser.nextToken;
begin
  sc.nextToken;
  tokenVector.append (sc.tokenElement);
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
begin
  if tokenVector.token <> thisToken then
     raise ESyntaxException.Create ('expecting ' + TTokenVector.tokenToString (tokenvector.token),  tokenVector.tokenRecord.lineNumber, tokenVector.tokenRecord.columnNumber)
  else
    nextToken;
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
  if tokenVector.token <> tIdentifier then
     raise ESyntaxException.Create ('expecting identifier in function argument definition',  tokenVector.tokenRecord.lineNumber, tokenVector.tokenRecord.columnNumber)
  else
     nextToken;
end;


// Parse: x:y  :y  x:  :  x
procedure TSyntaxParser.parseIndexOrSlice;
begin
 // check for :x and :
 if tokenVector.token = tColon then
     begin
     nextToken;
     if not (tokenVector.token in [tComma, tRightBracket]) then
        expression();
     end
  else
     begin
     // check for x: x:y  x
     expression();
     if tokenVector.token = tColon then
        begin
        nextToken;
        if not (tokenVector.token in [tComma, tRightBracket]) then
           expression();
        end
     end;
end;

// Parse something of the form variable '[' expressionList ']'
// Such indexing applies to lists and strings
// Slicing:
// [1:4], [1:5,5:6,2:10]
// [:,4], [1,:]
procedure TSyntaxParser.parseIndexedVariable;
begin
  nextToken;
  if tokenVector.token = tRightBracket then
     begin
     raise ESyntaxException.Create('indexed operator must have at least one index', tokenVector.tokenRecord.lineNumber, tokenVector.tokenRecord.columnNumber);
     nextToken;
     exit;
     end;

  parseIndexOrSlice();

  while tokenVector.token = tComma do
    begin
    nextToken;
    parseIndexOrSlice();
    end;
  expect(tRightBracket);
end;


procedure TSyntaxParser.parseFunctionCall;
begin
  if tokenVector.token <> tRightParenthesis then
     expressionList;
  expect(tRightParenthesis);
end;


// primary => factor primaryPlus
procedure TSyntaxParser.primary;
begin
  factor;
  primaryPlus;
end;


procedure TSyntaxParser.parseList;
begin
  nextToken;
  if tokenVector.token <> tRightBracket then
     begin
     expression;
     while tokenVector.token = tComma do
           begin
           nextToken;
           expression;
           end;
    end;
  expect(tRightBracket);
end;


procedure TSyntaxParser.factor;
begin
  case tokenVector.token of
   tInteger:
      begin
        nextToken;
      end;
   tFloat:
      begin
        nextToken;
      end;
   tIdentifier :
        begin
        nextToken;
        end;
   tString:
        begin
        nextToken;
        end;
    tNOT:
      begin
        nextToken;
        expression();
      end;
    tFalse:
      begin
        nextToken;
      end;
    tTrue:
      begin
        nextToken;
      end;
    tLeftParenthesis:
      begin
        nextToken;
        expression();
        expect(tRightParenthesis);
      end;
    tLeftBracket:
      begin
        parseList;
      end;

     // Reserved for maps
     //tLeftCurleyBracket :
     //  begin
     //  nextToken;
     //  expect(tRightCurleyBracket);
     //  end;
    tError:
       begin
       raise ESyntaxException.Create ('Expecting a factor [literal, identifier, or ''[''] but found ' + tokenVector.tokenRecord.FTokenCharacter, tokenVector.tokenRecord.lineNumber, tokenVector.tokenRecord.columnNumber);
       end
   else
      raise ESyntaxException.Create ('Expecting a factor [literal, identifier, or ''[''] but found ' + tokenVector.tokenToString (tokenVector.token), tokenVector.tokenRecord.lineNumber, tokenVector.tokenRecord.columnNumber);
  end;
end;


procedure TSyntaxParser.primaryPlus;
begin
  case tokenVector.token of
     tPeriod :
         begin
         nextToken;
         expect (tIdentifier);
         primaryPlus;
         end;
     tLeftParenthesis:  // '(' expression list ')'
         begin
         nextToken;
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
  unaryMinus_count : integer;
begin
  unaryMinus_count := 0;
  // Handle unary operators, but only count '-'. ++2 is the same as +2 but --2 is not the same as -2
  while (tokenVector.token = tMinus) or (tokenVector.token = tPlus) do
  begin
    case tokenVector.token of
      tMinus:
        inc(unaryMinus_count);
    else
        raise ESyntaxException.Create ('Too many plus symbols',  tokenVector.tokenRecord.lineNumber, tokenVector.tokenRecord.columnNumber)
    end;
    nextToken;
  end;

  primary;

  if tokenVector.token = tPower then
     begin
     nextToken;
     power;
     end;
end;


// term = power { ('*', '/', MOD, DIV) power }
procedure TSyntaxParser.term;
begin
  power;

  while tokenVector.token in [tMult, tDivide, tDivI, tMod, tDotproduct] do
    begin
    nextToken;
    power;
    end;
end;


// expression = term { ('+' | '-' | MOD | DIV) power }
procedure TSyntaxParser.simpleExpression;
begin
  term;

  while tokenVector.token in [tPlus, tMinus] do
     begin
       nextToken;
       term;
     end;
end;


// expression = simpleExpression | simpleExpression relationalOp simpleExpression
procedure TSyntaxParser.relationalOperators;
begin
  simpleExpression;

  while tokenVector.token in [tLessThan, tLessThanOrEqual, tMoreThan, tMoreThanOrEqual,
    tNotEqual, tEquivalence] do
    begin
    nextToken;
    simpleExpression;
  end;
end;


procedure TSyntaxParser.expression;
begin
  relationalOperators;

  while tokenVector.token in [tOr, tXor, tAnd] do
        begin
        nextToken;
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
  case tokenVector.token of
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
    //tHelp:
    // helpStatement;
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
     if tokenVector.token = tSemicolon then // semicolons optional
        expect(tSemicolon);
     // Note these are all thing that can end a statement list.
     if tokenVector.token in [tUntil, tEnd, tElse, tCase, tEndOfStream] then
         exit();
     statement();
     end;
end;


procedure TSyntaxParser.printlnStatement;
begin
  nextToken;
  if tokenVector.token = tLeftParenthesis then
     begin
     nextToken;
     // It could be an empty function call
     if tokenVector.token <> tRightParenthesis then
        expressionList;
     expect(tRightParenthesis);
     end
 else
     raise ESyntaxException.Create ('Expecting opening bracket to println call',  tokenVector.tokenRecord.lineNumber, tokenVector.tokenRecord.columnNumber)
end;


procedure TSyntaxParser.printStatement;
begin
  nextToken;
  if tokenVector.token = tLeftParenthesis then
     begin
     nextToken;
     // It could be an empty function call
     if tokenVector.token <> tRightParenthesis then
        expressionList;

     expect(tRightParenthesis);
     end
  else
    raise ESyntaxException.Create ('Expecting opening bracket to print call', tokenVector.tokenRecord.lineNumber, tokenVector.tokenRecord.columnNumber);
end;


procedure TSyntaxParser.setColorStatement;
begin
  nextToken;
  if tokenVector.token = tLeftParenthesis then
     begin
     nextToken;
     expression;
     expect(tRightParenthesis);
     end
  else
    raise ESyntaxException.Create ('Expecting opening bracket to setColor call', tokenVector.tokenRecord.lineNumber, tokenVector.tokenRecord.columnNumber);
end;


procedure TSyntaxParser.AssertTrueStatement;
begin
  nextToken;
  if tokenVector.token = tLeftParenthesis then
     begin
     nextToken;
     expression;
     expect(tRightParenthesis);
     end;
end;


procedure TSyntaxParser.AssertTrueExStatement;
begin
  nextToken;
  if tokenVector.token = tLeftParenthesis then
     begin
     nextToken;
     expression;
     expect(tRightParenthesis);
     end;
end;


procedure TSyntaxParser.AssertFalseStatement;
begin
  nextToken;
  if tokenVector.token = tLeftParenthesis then
     begin
     nextToken;
     expression;
     expect(tRightParenthesis);
     end;
end;


procedure TSyntaxParser.helpStatement;
begin
  nextToken;
  if tokenVector.token = tLeftParenthesis then
     begin
     nextToken;
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

   if tokenVector.token = tEquals then
      begin
      // Then its of the form a = ?
      nextToken;
      expression;
      end
end;


// argumentList = expression { ',' expression }
// Returns the number of expressions that were parsed
procedure TSyntaxParser.expressionList;
begin
  expression;
  while tokenVector.token = tComma do
    begin
      nextToken;
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

  simpleExpression;

  while tokenVector.token = tCase do
    begin
      expect(tCase);

      if tokenVector.token <> tInteger then
         raise ESyntaxException.Create ('Expecting integer in case value', tokenVector.tokenRecord.lineNumber, tokenVector.tokenRecord.columnNumber);

      nextToken;
      expect(tColon);

      statementList;
    end;

  if tokenVector.token = tElse then
     begin
     nextToken;
     statementList;;
     end;
  expect(tEnd);
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

  if tokenVector.token = tElse then
     begin
     nextToken;
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
     nextToken;
     exit;
     end;
  nextToken;
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
  breakStack: TStack<integer>;
begin
  breakStack := TStack<integer>.Create;
  stackOfBreakStacks.Push(breakStack);
  try
    expect(tRepeat);   // Guaranteed to be true

    statementList;

    expect(tUntil);

    expression;

    while breakStack.Count > 0 do
        breakJump := breakStack.Pop;
  finally
    breakStack := stackOfBreakStacks.Pop;
    breakStack.Free;
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

    if tokenVector.token = tIn then
       begin
       nextToken;
       expression;
       expect (tDo);

       // .... do <body>
       statementList;

       while breakStack.Count > 0 do
         breakJump := breakStack.Pop;

       expect(tEnd);
       exit;
       end;

    expect(tEquals);

    expression;

    if tokenVector.token in [tTo, tDownTo] then
       begin
       toToken := tokenVector.token;
       nextToken;
       end
    else
      raise ESyntaxException.Create ('expecting "to" or "downto" in for loop', tokenVector.tokenRecord.lineNumber, tokenVector.tokenRecord.columnNumber);

    // Parse the upper limit expression
    expression;

    // Deal with any step keyword
    if tokenVector.token = tStep then
       begin
       nextToken;
       if tokenVector.token = tInteger then
           nextToken
       else
          raise ESyntaxException.Create ('step value must be an integer', tokenVector.tokenRecord.lineNumber, tokenVector.tokenRecord.columnNumber);
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
begin
  nextToken;
  if tokenVector.token = tIdentifier then
     begin
     functionName := tokenVector.tokenString;
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
      nextToken;
      if tokenVector.token = tLeftParenthesis then
         begin
         nextToken;
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
  if tokenVector.token = tIdentifier then
     functionArgument;

  while tokenVector.token = tComma do
    begin
    nextToken;
    functionArgument;
    end;
end;


// argument = identifier | REF identifier
procedure TSyntaxParser.functionArgument;
begin
  if tokenVector.token = tRef then
     nextToken;
  variable;
end;


// global x, y, ....
procedure TSyntaxParser.globalStatement;
begin
  expect(tGlobal);
  if inUserFunctionParsing then
      begin
      if tokenVector.token = tIdentifier then
        begin
          // We're keeping a list of declared global variables for a given
          // user function. This list is simply used to check for duplicates such
          // as global a, b, a
          // or global a
          // global a
          // After the function has been parsed the global list is deleted.
          globalVariableList.Add(tokenVector.tokenString);

          nextToken;
          while tokenVector.token = tComma do
              begin
              nextToken;

              if globalVariableList.IndexOf(tokenVector.tokenString) = -1 then
                globalVariableList.Add(tokenVector.tokenString)
              else
                begin
                //result := TASTErrorNode.Create ('Duplicate global variable', tokenVector.tokenElement.lineNumber, tokenVector.tokenElement.columnNumber);
                exit;
                end;
              nextToken;
              end;
        end
      else
        raise ESyntaxException.Create ('Expecting variable name in global declaration', tokenVector.tokenRecord.lineNumber, tokenVector.tokenRecord.columnNumber);
      end
  else
      raise ESyntaxException.Create ('The global keyword can only be used inside user functions', tokenVector.tokenRecord.lineNumber, tokenVector.tokenRecord.columnNumber);
end;


procedure TSyntaxParser.importStatement;
begin
  nextToken();
  if (tokenVector.token = tIdentifier) then
      nextToken()
  else
      raise ESyntaxException.Create ('Expecting name of import file after import keyword', tokenVector.tokenRecord.lineNumber, tokenVector.tokenRecord.columnNumber);
end;


// returnStatement = RETURN expression
procedure TSyntaxParser.returnStmt;
begin
  if not inUserFunctionParsing then
     raise ESyntaxException.Create ('You cannot use a return statement outside a user function', tokenVector.tokenRecord.lineNumber, tokenVector.tokenRecord.columnNumber);

  expect(tReturn);
  expression;
end;


procedure TSyntaxParser.parseModule(moduleName: string);
begin
  tokenVector.clearCode;
  nextToken;
  statementList;
end;


// program = statementList
function TSyntaxParser.syntaxCheck (var error : TSyntaxError) : boolean;
begin
  result := True;
  try
    tokenVector.clearCode;
    nextToken;
    primaryModuleName := TSymbol.mainModuleId; // we're in the main module
    if tokenVector.token <> tEndOfStream then
       statementList;
  except
    on e: ESyntaxException do
       begin
       error.lineNumber := e.lineNumber;
       error.columnNumber := e.columnNumber;
       error.errorMsg := e.errorMsg;
       result := False;
       end;
    on e: EScannerError do
       begin
       error.lineNumber := e.lineNumber;
       error.columnNumber := e.columnNumber;
       error.errorMsg := e.errorMsg;
       result := False;
       end;
  end;
  //writeln ('Number of tokens = ', tokenVector.count);
end;

end.