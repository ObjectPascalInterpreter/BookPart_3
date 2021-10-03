unit uConstructAST;

// AST Parser and constructor Rhodus Language Part III

// Developed under Delphi for Windows and Mac platforms.

// *** This source is distributed under Apache 2.0 ***

// Copyright (C)  2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

// Usage:
//
// sc := TScanner.Create;
// sc.scanString ('a = 2; b = a + 5.6')
// syntaxAnalyser := TConstructAST.Create (sc);
// syntaxAnalyzer.mainProgram;

interface

uses Classes, SysUtils, uScanner, uSymbolTable, uLibModule,
  Generics.Collections, uAST, uASTNodeType, uVM;

type
  TBreakStack = TStack<integer>;
  TModuleNameStack = TStack<string>;

  TConstructAST = class(TObject)
  private
    stackOfBreakStacks: TStack<TBreakStack>;
    moduleNameStack: TModuleNameStack;
    primaryModuleName: string;
    //globalSpace: TModule; // Contains global symbols such as int(), readNumber() etc

    sc: TScanner;

    // Very private. don't use them directly, use the helper routines
    inUserFunctionParsing: boolean;
    // Used to gather global variable names, this is only
    // used during AST construction, its discarded afterwards.
    globalVariableList: TStringList;

    // Helper routines for the above
    procedure enterUserFunctionScope;
    procedure exitUserFunctionScope;

    function expect(thisToken: TTokenCode) : TASTNode;
    function variable: TASTNode;
    function parseList: TASTNode;
    function parseIndexedVariable: TASTNode;
    function parseIndexedVariable2 : TASTNode;
    function parseFunctionCall: TASTNode;

    function parseAtom (moduleName, identifier: string): TASTNode;
    function parseSecondary: TASTNode;

    function primary : TASTNode;
    function factor2 : TASTNode;
    function primaryPlus : TASTNode;

    function factor: TASTNode;
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
    function parseModule(moduleName: string; var astRoot: TASTNode): TModuleLib;
    function parseProgram: TASTNode;
    function inUserFunctionScope: boolean;
    constructor Create(sc: TScanner);
    destructor Destroy; override;
  end;

  ESyntaxException = class(Exception)
      lineNumber, columnNumber : integer;
      errorMsg : string;
      constructor Create (errMsg : string; lineNumber, columnNumber : integer);
  end;

implementation

uses Math, IOUtils, uGlobal, uUtils, uOpCodes, uMachineStack, uCompile, uBuiltInGlobal;

type
  TPrintClass = class(TObject)
    procedure print(st: PMachineStackRecord);
    procedure println(st: PMachineStackRecord);
  end;

// Print methods to support output from the Vm.
// ----------------------------------------------------------------------
procedure TPrintClass.print(st: PMachineStackRecord);
begin
  if st <> nil then
    case st.stackType of
      stNone:
        begin
        end; // write ('undefined value'); end;
      stInteger:
        write(st.iValue);
      stDouble:
        write(Format('%g', [st.dValue]));
      stString:
        write(st.sValue.value);
      stBoolean:
        if st.bValue = True then
          write('True')
        else
          write('False');
      stList:
        begin
          write(st.lValue.listToString);
        end;
    else
      writeln('Unrecognized value from print');
    end;
end;

procedure TPrintClass.println(st: PMachineStackRecord);
begin
  print(st);
  writeln;
end;


// ----------------------------------------------------------------------
constructor ESyntaxException.Create (errMsg : string; lineNumber, columnNumber : integer);
begin
  self.errorMsg := errMsg;
  self.lineNumber := lineNumber;
  self.columnNumber := columnNumber;
end;

// Start of AST Class
// ----------------------------------------------------------------------
constructor TConstructAST.Create(sc: TScanner);
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
     result := TASTErrorNode.Create ('expecting ' + TScanner.tokenToString (thisToken),  sc.tokenElement.lineNumber, sc.tokenElement.columnNumber)
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
     result := TASTErrorNode.Create ('expecting identifier in function argument definition',  sc.tokenElement.lineNumber, sc.tokenElement.columnNumber)
  else
     begin
     // Add the argument symbol to the user function local symbol table
     // if not currentUserFunction.symbolTable.find(sc.tokenString, symbolIndex) then
     // symbolIndex := currentUserFunction.symbolTable.addSymbol(sc.tokenString, TScope.scUserFunction);
     result := TASTIdentifier.Create(sc.tokenString);
     sc.nextToken;
     end;
end;

// Parse a list of the form: expression ',' expression ','' etc.
// Returns the number of items found in the list
function TConstructAST.parseList: TASTNode;
var node : TASTNodeList;
begin
  node := TASTNodeList.Create (ntNodeList);
  node.list.Add(expression);
  while sc.token = tComma do
    begin
      sc.nextToken;
      node.list.Add(expression);
    end;
  exit (node);
end;


// Parse something of the form  '[' expressionList ']'
// Such indexing applies to lists and strings
// The list gets stored in aan ast node list
function TConstructAST.parseIndexedVariable: TASTNode;
var node : TASTNode;
    nodeList : TASTNodeList;
begin
  nodelist := TASTNodeList.Create (ntNodeList);
  sc.nextToken;
  nodeList.list.Add(expression);
  while sc.token = tComma do
  begin
    sc.nextToken;
    nodeList.list.Add(expression);
  end;
  node := expect(tRightBracket);
  if node <> nil then
     nodeList.list.Add (node);   // easist thing to do is add the error node to the expression list
  exit (nodeList);
end;


// Parse something of the form variable '[' expressionList ']'
// Such indexing applies to lists and strings
function TConstructAST.parseIndexedVariable2 : TASTNode;
var node : TASTNode;
    nodeList : TASTNodeList;
    exp : TASTNode;
begin
  nodelist := TASTNodeList.Create (ntNodeList);
  sc.nextToken;
  if sc.token = tRightBracket then
     begin
     result := TASTErrorNode.Create('indexed operator must have at least one index', sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);
     sc.nextToken;
     exit;
     end;

  exp := expression();

  nodeList.list.Add(exp);
  while sc.token = tComma do
    begin
    sc.nextToken;
    nodeList.list.Add(expression);
    end;
  node := expect(tRightBracket);
  if node <> nil then
     nodeList.list.Add (node);   // easist thing to do is add the error node to the expression list
  exit (nodeList);
end;


function TConstructAST.parseFunctionCall: TASTNode;
var node : TASTNode;
begin
  result := nil;
  if sc.token <> tRightParenthesis then
     begin
     result := expressionList;
     if result.nodeType = ntError then
        exit (result);
     end;
  node := expect(tRightParenthesis);
  if node = nil then // no error
     begin
     if result = nil then
        result := TASTNodeList.Create (ntNodeList);
     exit (result);
     end;

  // Error
  result.freeAST;
  exit (node);
end;


function TConstructAST.parseAtom(moduleName, identifier: string): TASTNode;
begin
  if inUserFunctionParsing then
     begin
     if globalVariableList.IndexOf(identifier) <> -1 then
       result := TASTPrimaryOld.Create(identifier)
     else
       result := TASTPrimaryOld.Create(identifier);
     end
  else
    result := TASTPrimaryOld.Create(identifier)
end;


function TConstructAST.parseSecondary: TASTNode;
var
  identifier: string;
  argumentList: TASTNodeList;
  m: TASTSubscript;
  node: TASTPrimaryOld;
  nodeList : TASTNode;
begin
  case sc.token of
    tIdentifier:
        begin
        node := TASTPrimaryOld.Create(sc.tokenString);
        sc.nextToken;
        result := node;
        end;
    tPeriod:
        begin
        sc.nextToken;
        if sc.token <> tIdentifier then
          begin
          result := TASTErrorNode.Create ('Expecting identifier after period', sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);
          exit;
          end;
        result := TASTPeriod.Create (sc.tokenString);
        sc.nextToken;
        end;
    tLeftBracket: // '[' expression list ']'
        begin
        nodeList := parseIndexedVariable();
        if nodeList.nodeType = ntError then
           exit (nodeList);

        m := TASTSubscript.Create(nodeList as TASTNodeList);
        result := m;
        end;
    tLeftParenthesis: // '(' argument list ')'
        begin
        argumentList := parseFunctionCall as TASTNodeList;
        if argumentList.nodeType = ntError then
           exit (argumentList);

        result := TASTFunctionCall.Create(argumentList);
        end
  else
    result := TASTErrorNode.Create ('Expecting identifer, period, left bracket or left parentheses', sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);
  end;
end;

// primary => factor primaryPlus
function TConstructAST.primary : TASTNode;
var node : TASTNode;
begin
  result := factor2;
  if result.nodeType = ntError then
     exit (result);

  node := primaryPlus;
  if node.nodeType = ntError then
     begin
     result.freeAST;
     result := node;
     end
  else
     result := TASTPrimary.Create(result, node);
end;


function TConstructAST.factor2 : TASTNode;
var node, primary : TASTNode;
    alist: TASTNodeList;
begin
  result := nil;
  case sc.token of
   tInteger:
      begin
        result := TASTInteger.Create(sc.tokenInteger);
        sc.nextToken;
      end;
   tFloat:
      begin
        result := TASTFloat.Create(sc.tokenFloat);
        sc.nextToken;
      end;
   tIdentifier :
        begin
        result := TASTIdentifier.Create (sc.tokenString);
        sc.nextToken;
        end;
   tString:
        begin
        result := TASTString.Create(sc.tokenString);
        sc.nextToken;
        end;
    tNOT:
      begin
        sc.nextToken;
        node := expression();
        if node.nodeType = ntError then
           exit (node);
        result := TASTNotOp.Create(node);
      end;
    tFalse:
      begin
        result := TASTBoolean.Create(False);
        sc.nextToken;
      end;
    tTrue:
      begin
        result := TASTBoolean.Create(True);
        sc.nextToken;
      end;
    tLeftParenthesis:
      begin
        sc.nextToken;
        result := expression();
        if result.nodeType = ntError then
           exit (result);

        node := expect(tRightParenthesis);
        if node <> nil then
           begin
           result.freeAST;
           result := node;
           end;
      end;
    tLeftCurleyBracket:
      begin
        sc.nextToken;
        alist := nil;
        result := TASTCreateList.Create;
        if sc.token <> tRightCurleyBracket then
           begin
           (result as TASTCreateList).list.Add(expression);
           while sc.token = tComma do
              begin
              sc.nextToken;
              (result as TASTCreateList).list.Add(expression);
              end;
           end;

        node := expect(tRightCurleyBracket);
        if node <> nil then
           begin
           alist.freeAST;
           exit (node);
           end;
       end;
   else
      result := TASTErrorNode.Create ('Expecting a factor [literal, identifier, or ''{'']' , sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);
  end;
end;


function TConstructAST.primaryPlus : TASTNode;
var periodStr : string;
    nodeList : TASTNode;
    nIndices : integer;
begin
  case sc.token of
     tPeriod :
         begin
         sc.nextToken;
         periodStr := sc.tokenString;
         sc.nextToken;
         result := TASTPrimaryPeriod.Create (TASTIdentifier.Create(periodStr), primaryPlus);
         end;
     tLeftParenthesis:  // '(' expression list ')'
         begin
         // to be done
         sc.nextToken;
         nodeList := parseFunctionCall;
         if nodeList.nodeType = ntError then
            exit (nodeList);

         result := TASTPrimaryFunction.Create(nodeList as TASTNodeList, primaryPlus);
         end;
    tLeftBracket: // '[' expression list ']'
         begin
         nodeList := parseIndexedVariable2;
         if nodeList.nodeType = ntError then
            exit (nodeList);

         result := TASTPrimaryIndex.Create(nodeList as TASTNodeList, primaryPlus);
         end;
  else
      begin result := TASTNull.Create; end;
  end;
end;


// factor = integer | float | '(' expression ')' | etc
function TConstructAST.factor: TASTNode;
var
  alist: TASTNodeList;
  identifier: string;
  root, node : TASTNode;
  primary: TASTNode;
  token: TTokenRecord;
  symbol: TSymbol;
  action : boolean;
  i : integer;
  errMsg, astr : string;
begin
  case sc.token of
    tInteger:
      begin
        result := TASTInteger.Create(sc.tokenInteger);
        sc.nextToken;
      end;

    tFloat:
      begin
        result := TASTFloat.Create(sc.tokenFloat);
        sc.nextToken;
      end;

    // variable = simpleVariable | indexVariable
    // simpleVariable = identifer | module '.' identifier
    // :=   identifier
    // | '[' expressionlist ']'
    // | '(' argumentList ')'
    // | '.' identifier

    // This code can deal with things: abc.x()[1], or abc(func())[1]()[4,3], etc
    // This is the start of the primary symbol
    tIdentifier:
      begin
        identifier := sc.tokenString;
        // Accepts:
        //  a
        // main().a
        // int()
        // globalSpace.int()
        // anything that could start with an identifier

        // This is a hack to allow users to type int (4.5) rather than the full qualifier globalSpace.int (4.5)
        // We do it by faking globalSpace.int in the scanner stream.
        // Note globalSpace.int is also acceptable but so is the short version int

        //if globalSpace <> nil then
        //   action := globalSpace.symbolTable.find(identifier, symbol)
        //else
        //   action := False;

        if action then
           begin
           // This takes care of things like globalSpace.int()
           // Push the period back into the scanner stream.
           token.FToken := tPeriod;
           sc.pushBackToken(token);
           sc.nextToken; // Then make it official by pulling it back out

           // Push the attribute to globalSpace, eg int back into the scanner stream
           token.FToken := tIdentifier;
           token.FTokenString := identifier;
           sc.pushBackToken(token);

           // Now we're ready to parse the fragment .int
           // Finally make sure the primary name is globalSpace
           identifier := TSymbol.globalId;
           end
        else
           begin
           // This is to ensure an identifier expression terminates at a line feed
           // It avoids the situation in the folllowing two lines:
           // x = y
           // w = v

           // This could cause a compile error as the compiler doesn't realize that the first
           // expression stops at y and a new one starts at w. An alternative solution is
           // to enforce ';' at the end of statements but I don't want to do that.

           if sc.tokenWasLF then
              begin
              sc.nextToken;
              root := nil;
              result := TASTPrimaryOld.Create(identifier);
              exit;
              end;

           sc.nextToken;
           end;
        primary := TASTPrimaryOld.Create (identifier);
        while sc.token in [tLeftBracket, tLeftParenthesis, tPeriod] do
              (primary as TASTPrimaryOld).nodes.add (parseSecondary());

        result := primary;
      end;

    tLeftParenthesis:
      begin
        sc.nextToken;
        result := expression();
        if result.nodeType = ntError then
           exit (result);

        node := expect(tRightParenthesis);
        if node <> nil then
           begin
           result.freeAST;
           result := node;
           end;
      end;

    tLeftCurleyBracket:
      begin
        sc.nextToken;
        alist := nil;
        result := TASTCreateList.Create;
        if sc.token <> tRightCurleyBracket then
           begin
           (result as TASTCreateList).list.Add(expression);
           while sc.token = tComma do
              begin
              sc.nextToken;
              (result as TASTCreateList).list.Add(expression);
              end;
           end;
        node := expect(tRightCurleyBracket);
        if node <> nil then
           begin
           alist.freeAST;
           exit (node);
           end;

      end;

    tString:
      begin
        astr := sc.tokenString;
        sc.nextToken;
        result := TASTString.Create(astr);
      end;

    tNOT:
      begin
        sc.nextToken;
        node := expression();
        if node.nodeType = ntError then
           exit (node);
        result := TASTNotOp.Create(node);
      end;

    tFalse:
      begin
        result := TASTBoolean.Create(False);
        sc.nextToken;
      end;

    tTrue:
      begin
        result := TASTBoolean.Create(True);
        sc.nextToken;
      end
  else
    begin
    errMsg := 'expecting a literal value, an identifier or an opening ''(''. Instead I found "';
    if sc.token = tString then
       errMsg := errMsg + sc.tokenElement.FTokenCharacter + '"'
    else
       errMsg := errMsg + sc.tokenToString (sc.token) + '"';
    result := TASTErrorNode.Create (errMsg , sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);
    sc.nextToken;
    end;
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
  if leftNode.nodeType = ntError then
     exit (leftNode);

  if sc.token = tPower then
     begin
     sc.nextToken;
     rightNode := power;
     if rightNode.nodeType = ntError then
        begin
        leftNode.freeAST;
        exit (rightNode);
        end;

     leftNode := TAstPowerOp.Create(leftNode, rightNode);
     end;
  for i := 0 to unaryMinus_count - 1 do
      leftNode := TASTUniOp.Create(leftNode, TASTNodeType.ntUnaryMinus);
  result := leftNode;
end;


// term = power { ('*', '/', MOD, DIV) power }
function TConstructAST.term: TASTNode;
var
  op: TTokenCode;
  leftNode, rightNode: TASTNode;
begin
  leftNode := power;
  if leftNode.nodeType = ntError then
     exit (leftNode);

  while sc.token in [tMult, tDivide, tDivI, tMod] do
    begin
    op := sc.token; // remember the token
    sc.nextToken;
    rightNode := power;
    if rightNode.nodeType = ntError then
       begin
       leftNode.freeAST;
       exit (rightNode);
       end;

    case op of
      tMult:
        leftNode := TAstBinOp.Create(leftNode, rightNode, TASTNodeType.ntMult);
      tDivide:
        leftNode := TAstBinOp.Create(leftNode, rightNode, TASTNodeType.ntDiv);
      tMod:
        leftNode := TAstBinOp.Create(leftNode, rightNode, TASTNodeType.ntMod);
      tDivI:
        leftNode := TAstBinOp.Create(leftNode, rightNode, TASTNodeType.ntDivI);
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
  if leftNode.nodeType = ntError then
     exit (leftNode);

  while sc.token in [tPlus, tMinus] do
     begin
       op := sc.token; // remember the token
       sc.nextToken;
       rightNode := term;
       if rightNode.nodeType = ntError then
          begin
          leftNode.freeAST;
          exit (rightNode);
          end;
       case op of
         tPlus:
           leftNode := TASTBinOp.Create(leftNode, rightNode, TASTNodeType.ntAdd);
         tMinus:
           leftNode := TASTBinOp.Create(leftNode, rightNode, TASTNodeType.ntSub);
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
  if leftNode.nodeType = ntError then
     exit (leftNode);

  while sc.token in [tLessThan, tLessThanOrEqual, tMoreThan, tMoreThanOrEqual,
    tNotEqual, tEquivalence] do
  begin
    op := sc.token;
    sc.nextToken;
    rightNode := simpleExpression;
     if rightNode.nodeType = ntError then
        begin
        leftNode.freeAST;
        exit (rightNode);
        end;

    case op of
      tEquivalence:
        leftNode := TAstBinOp.Create(leftNode, rightNode, TASTNodeType.ntEQ);
      tLessThan:
        leftNode := TAstBinOp.Create(leftNode, rightNode, TASTNodeType.ntLT);
      tMoreThan:
        leftNode := TAstBinOp.Create(leftNode, rightNode, TASTNodeType.ntGT);
      tMoreThanOrEqual:
        leftNode := TAstBinOp.Create(leftNode, rightNode, TASTNodeType.ntGE);
      tLessThanOrEqual:
        leftNode := TAstBinOp.Create(leftNode, rightNode, TASTNodeType.ntLE);
      tNotEqual:
        leftNode := TAstBinOp.Create(leftNode, rightNode, TASTNodeType.ntNE);
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
  if leftNode.nodeType = ntError then
     exit (leftNode);

  while sc.token in [tOr, tXor, tAnd] do
  begin
    op := sc.token; // remember the token
    sc.nextToken;
    rightNode := relationalOperators;
    if rightNode.nodeType = ntError then
       begin
       leftNode.freeAST;
       exit (rightNode);
       end;

    case op of
      tOr:
        leftNode := TAstBinOp.Create(leftNode, rightNode, TASTNodeType.ntOR);
      tAnd:
        leftNode := TAstBinOp.Create(leftNode, rightNode, TASTNodeType.ntAnd);
      tXor:
        leftNode := TAstBinOp.Create(leftNode, rightNode, TASTNodeType.ntXor);
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
    tHelp:
      result := helpStatement;
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
  if node = nil then
     begin
     // Return empty statment list
     node := TASTStatementList.Create;
     exit (node)
     end;

  if node.nodeType = ntError then
     exit (node);

  stat := TASTStatementList.Create;
  stat.statementList.Add(node);
  while True do
     begin
     if sc.token = tSemicolon then // semicolons optional
        expect(tSemicolon);
     // Note these are all thing that can end a statement list.
     if sc.token in [tUntil, tEnd, tElse, tCase, tEndOfStream] then
         exit(stat);
     node := statement();
     if node.nodeType = ntError then
        begin
        stat.freeAST;
        exit (node);
        end;
     stat.statementList.Add(node);
     end;
  result := stat;
end;


function TConstructAST.printlnStatement: TASTNode;
var
  funcArgs: TASTNodeList;
  node : TASTNode;
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
    node := expect(tRightParenthesis);
    if node <> nil then
           begin
           funcArgs.freeAST;
           exit (node);
           end;
  end
  else
    begin
    result := TASTErrorNode.Create ('Expecting opening bracket to println call', sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);
    exit;
    end;
  result := TASTPrintln.Create(funcArgs);
end;


function TConstructAST.printStatement: TASTNode;
var
  funcArgs: TASTNodeList;
  node : TASTNode;
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
     node := expect(tRightParenthesis);
     if node <> nil then
        begin
        funcArgs.freeAST;
        exit (node);
        end;
     end
  else
    begin
    result := TASTErrorNode.Create ('Expecting opening bracket to print call', sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);
    exit;
    end;

  result := TASTPrint.Create(funcArgs);
end;


function TConstructAST.setColorStatement : TASTNode;
var  node : TASTNode;
begin
  result := nil;
  sc.nextToken;
  if sc.token = tLeftParenthesis then
     begin
     sc.nextToken;
     result := expression;
     if result.nodeType = ntError then
        exit (result);

     node := expect(tRightParenthesis);
     if node <> nil then
        begin
        result.freeAST;
        exit (node);
        end;

     result := TASTSetColor.Create(TASTExpression.Create(result));
     end
  else
    begin
    result := TASTErrorNode.Create ('Expecting opening bracket to setColor call', sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);
    exit;
    end;
end;


function TConstructAST.AssertTrueStatement: TASTNode;
var node : TASTNode;
begin
  result := nil;
  sc.nextToken;
  if sc.token = tLeftParenthesis then
     begin
     sc.nextToken;
     result := expression;
     if result.nodeType = ntError then
        exit (result);

     node := expect(tRightParenthesis);
     if node <> nil then
        begin
        result.freeAST;
        exit (node);
        end;
     result := TASTAssertTrue.Create(TASTExpression.Create(result));
     end;
end;


function TConstructAST.AssertTrueExStatement: TASTNode;
var node : TASTNode;
begin
  result := nil;
  sc.nextToken;
  if sc.token = tLeftParenthesis then
     begin
     sc.nextToken;
     result := expression;
     if result.nodeType = ntError then
        exit (result);

     node := expect(tRightParenthesis);
     if node <> nil then
        begin
        result.freeAST;
        exit (node);
        end;
     result := TASTAssertTrueEx.Create(TASTExpression.Create(result));
     end;
end;


function TConstructAST.AssertFalseStatement: TASTNode;
var node : TASTNode;
begin
  result := nil;
  sc.nextToken;
  if sc.token = tLeftParenthesis then
     begin
     sc.nextToken;
     result := expression;
     if result.nodeType = ntError then
        exit (result);

     node := expect(tRightParenthesis);
     if node <> nil then
        begin
        result.freeAST;
        exit (node);
        end;
     result := TASTAssertFalse.Create(TASTExpression.Create(result));
     end;
end;


function TConstructAST.helpStatement: TASTNode;
var node : TASTNode;
begin
  result := nil;
  sc.nextToken;
  if sc.token = tLeftParenthesis then
     begin
     sc.nextToken;
     result := expression;
     if result.nodeType = ntError then
        exit (result);

     node := expect(tRightParenthesis);
     if node <> nil then
        begin
        result.freeAST;
        exit (node);
        end;
     result := TASTHelp.Create(TASTExpression.Create(result));
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
  if node.nodeType = ntError then
     exit (node);

   if sc.token = tEquals then
      begin
      // Then its of the form a = ?
      sc.nextToken;
      exprNode := expression;
      if exprNode.nodeType = ntError then
         begin
         node.freeAST;
         exit (exprNode);
         end;
      expressionNode := TASTExpression.Create(exprNode);

      //if node.nodeType <> ntPrimary then
      if node.nodeType <> ntPrimary then
         begin
         expressionNode.freeAST;
         node.freeAST;
         result := TASTErrorNode.Create ('Expecting an identifier on the left-hand side', sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);
         exit;
         end;

      //result := TASTAssignment.Create(node as TASTPrimary, expressionNode);
      result := TASTAssignment2.Create(node as TASTPrimary, expressionNode);
      end
   else
      begin
      // Protect agianst things like 2 3, except it doesn't work
      //if (sc.token <> tEndofStream) and (sc.token <> tSemicolon) then
      //   begin
      //   node.freeAST;
      //   result := TASTErrorNode.Create('I reached the end of an expression and was expecting either a semicolon or the end of the text but found "' + sc.tokenLiteral + '"', sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);
      //   end
      //else
         result := TASTExpressionStatement.Create (node);
      end;
end;


// argumentList = expression { ',' expression }
// Returns the number of expressions that were parsed
function TConstructAST.expressionList: TASTNode;
var exp : TASTNode;
    nodeList : TASTNodeList;
begin
  exp := expression;
  if exp.nodeType = ntError then
     exit (exp);

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

  listOfCaseStatements := TASTListOfCaseStatements.Create;

  caseStatementList := nil;
  elseStatement := nil;

  while sc.token = tCase do
    begin
      expect(tCase);

      if sc.token = tInteger then
         caseValue := TASTInteger.Create(sc.tokenInteger)
      else
         begin
         switchExpression.freeAST;
         listOfCaseStatements.freeAST;
         result := TASTErrorNode.Create ('Expecting integer in case value', sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);
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
      listOfCaseStatements.list.Add(TASTCaseStatement.Create(caseValue, caseStatementList));
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
     exit (TASTSwitch.Create(switchExpression, listOfCaseStatements, elseStatement));
  result := TASTErrorNode.Create('Empty switch construct', sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);
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
  if condition.nodeType = ntError then
     exit (condition);

  node := expect(tThen);
  if node <> nil then
     begin
     condition.freeAST;
     exit (node);
     end;

  listOfStatements := statementList;
  if listOfStatements.nodeType = ntError then
     begin
     condition.freeAST;
     exit (listOfStatements);
     end;

  if sc.token = tElse then
     begin
     sc.nextToken;
     listOfElseStatements := statementList;
     if listOfElseStatements.nodeType = ntError then
        begin
        listOfStatements.freeAST;
        condition.freeAST;
        exit (listOfStatements);
        end;

     node := expect(tEnd);
     if node <> nil then
        begin
        listOfStatements.freeAST;
        exit (node);
        end;

     result := TASTIf.Create(condition, listOfStatements, listOfElseStatements);
     end
  else
     begin
     node := expect(tEnd);
     if node <> nil then
        exit (node);
     result := TASTIf.Create(condition, listOfStatements, nil);
     end;
end;


function TConstructAST.breakStatement: TASTNode;
begin
  if stackOfBreakStacks.Count = 0 then
     begin
     result := TASTErrorNode.Create ('Break statement illegal in this context', sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);
     sc.nextToken;
     exit;
     end;

  result := TASTNode.Create(TASTNodeType.ntBreak);
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

    condition := TASTExpression.Create(expression);
    if condition.nodeType = ntError then
       begin
       exit (condition);
       end;

    node := expect(tDo);
    if node <> nil then
       begin
       condition.freeAST;
       exit (node);
       end;

    listOfStatements := statementList() as TASTStatementList;

    node := expect(tEnd);
    if node <> nil then
       begin
       condition.freeAST;
       listOfStatements.freeAST;
       exit (node);
       end;

    result := TASTWhile.Create(condition, listOfStatements);

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
    if listOfStatements.nodeType = ntError then
       exit (listOfStatements);

    node := expect(tUntil);
    if node <> nil then
       begin
       listOfStatements.freeAST;
       exit (node);
       end;

    condition := expression;

    if condition.nodeType = ntError then
       begin
       listOfStatements.freeAST;
       exit (condition);
       end;

    result := TASTRepeat.Create(listOfStatements, condition);
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
  assignment: TASTAssignment2;
  iterationBlock: TASTIterationBlock;
  rightSide, body, node: TASTNode;
  lower, upper : TASTExpression;
  id : TASTIdentifier;
begin
  breakStack := TStack<integer>.Create;
  stackOfBreakStacks.Push(breakStack);
  try
    expect(tFor);    // Guaranteed to be true

    node := expect(tIdentifier);
    if node <> nil then
       exit (node);

    id := TASTIdentifier.Create (sc.tokenString);
    if id.nodeType = ntError then
       exit (id);

    node := expect(tEquals);
    if node <> nil  then
       begin
       id.freeAST;
       exit (node);
       end;

    node := expression;
    if node.nodeType = ntError then
       begin
       id.freeAST;
       exit (node);
       end;
    lower := TASTExpression.Create(node);

    if sc.token in [tTo, tDownTo] then
       begin
       toToken := sc.token;
       sc.nextToken;
       end
    else
      begin
      id.freeAST;
      lower.freeAST;
      result := TASTErrorNode.Create ('expecting "to" or "downto" in for loop', sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);
      exit;
      end;

    // Parse the upper limit expression
    node := expression;
    if node.nodeType = ntError then
       begin
       id.freeAST;
       lower.freeAST;
       exit (node);
       end;
    upper := TASTExpression.Create(node);

    iterationBlock := TASTIterationBlock.Create(id, lower, upper);
    if toToken = tTo then
      iterationBlock.direction := TASTNode.Create(TASTNodeType.ntTo)
    else
      iterationBlock.direction := TASTNode.Create(TASTNodeType.ntDownTo);

    // Deal with any step keyword
    iterationBlock.stepValue := 1.0;
    if sc.token = tStep then
    begin
      sc.nextToken;
      if (sc.token = tInteger) or (sc.token = tFloat) then
         begin
         iterationBlock.stepValue := sc.getScalar;
         sc.nextToken;
         end
      else
         begin
         iterationBlock.freeAST;
         result := TASTErrorNode.Create ('step value must be an integer ro float value', sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);
         exit;
         end;
    end;

    node := expect(tDo);
    if node <> nil then
       begin
       iterationBlock.freeAST;
       exit (node);
       end;

    // .... do <body>
    body := statementList;

    result := TASTFor.Create(iterationBlock, body);

    while breakStack.Count > 0 do
      breakJump := breakStack.Pop;

    node := expect(tEnd);
    if node <> nil then
       begin
       result.freeAST;
       exit (node);
       end;
  finally
    breakStack := stackOfBreakStacks.Pop;
    breakStack.Free;
  end;
end;


// function = function identifier '(' argumentList ')' statementList
function TConstructAST.parseUserDefinedFunction: TASTNode;
var
  functionName: string;
  newUserFunction: boolean;
  statementlistNode: TASTStatementList;
  argList: TASTNodeList;
  node : TASTNode;
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
     result := TASTErrorNode.Create ('expecting function name', sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);
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
         if node.nodeType = ntError then
            exit (node);

         argList := node as TASTNodeList;
         node := expect(tRightParenthesis);
         if node <> nil then
            begin
            argList.freeAST;
            exit (node);
            end;
         end
     else
         argList := TASTNodeList.Create (ntNodeList);

      statementlistNode := statementList as TASTStatementList;
    finally
      exitUserFunctionScope();
      globalVariableList.Free;
    end;
    node := expect(tEnd);
    if node <> nil then
       begin
       argList.freeAST;
       statementlistNode.freeAST;
       exit (node);
       end;
    // currentModuleName is required so that we can add the name of the function to the symbol table
    // early and before we parse the body of the fucntion so that recursive function calls can be handled.
    result := TASTUserFunction.Create(primaryModuleName, functionName, argList, statementlistNode);

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
          variableList.list.Add(TASTIdentifier.Create(sc.tokenString));

          sc.nextToken;
          while sc.token = tComma do
              begin
              sc.nextToken;

              if globalVariableList.IndexOf(sc.tokenString) = -1 then
                globalVariableList.Add(sc.tokenString)
              else
                begin
                result := TASTErrorNode.Create ('Duplicate global variable', sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);
                exit;
                end;

              variableList.list.Add(TASTIdentifier.Create(sc.tokenString));
              sc.nextToken;
              end;

          result := TASTGlobal.Create(primaryModuleName, variableList);
        end
      else
        begin
        result := TASTErrorNode.Create ('Expecting variable name in global declaration', sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);
        exit;
        end;
      end
  else
      begin
      result := TASTErrorNode.Create ('The global keyword can only be used inside user functions', sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);
      exit;
      end;
end;


function TConstructAST.importStatement: TASTNode;
begin
  sc.nextToken();
  if (sc.token = tIdentifier) then
      begin
      result := TASTImport.Create(sc.tokenString);
      sc.nextToken();
      end
  else
      begin
      result := TASTErrorNode.Create ('Expecting name of import file after import keyword', sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);
      sc.nextToken;
      exit;
      end;
end;


// returnStatement = RETURN expression
function TConstructAST.returnStmt: TASTNode;
begin
  if not inUserFunctionParsing then
     begin
     result := TASTErrorNode.Create ('You cannot use a return statement outside a user fucntion', sc.tokenElement.lineNumber, sc.tokenElement.columnNumber);
     exit;
     end;

  expect(tReturn);
  result := TASTReturn.Create(TASTExpression.Create(expression));
end;


function TConstructAST.parseModule(moduleName: string; var astRoot: TASTNode): TModuleLib;
begin
  result := TModulelib.Create(moduleName, '');

  primaryModuleName := moduleName;
  astRoot := statementList;
end;


// program = statementList
function TConstructAST.parseProgram: TASTNode;
begin
  primaryModuleName := TSymbol.mainModuleId; // we're in the main module
  if sc.token = tEndOfStream then
     result := nil
  else
     result := statementList;
end;

end.
