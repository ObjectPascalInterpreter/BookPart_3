unit uAST;


// This source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com


interface

Uses SysUtils, Classes, Generics.Collections, uASTNodeType, uListObject;

type
   TASTNode = class;
   TChildNodes = class (TList<TASTNode>)  // TList allows me to handle freeing of objects myself
       procedure freeChildNodes;
   end;

   // Experimental way to free nodes if there are errors during parsing
   // Not currently active
   TPool = class (TList<TASTNode>)
      procedure addToPool (obj : TASTNode);
   end;


   // Base AST Node
   TASTNode = class (TObject)
        nodeType : TASTNodeType;
        freeChildren : boolean; // Used for the experimental TPool

        procedure   freeAST;
        constructor Create(nodeType: TASTNodeType);
        destructor  Destroy; override;
   end;

   TASTErrorNode = class (TASTNode)
       lineNumber, columnNumber : integer;
       errorMsg : string;
       constructor Create (errMsg : string; lineNumber, columnNumber : integer);
   end;

   TASTInteger = class (TASTNode)
      iValue : integer;
      constructor Create (ivalue : integer);
   end;

   // Any lists such as a, b, c are stored in this node
   TASTNodeList = class (TASTNode)
       list : TChildNodes;
       constructor Create (nodeType : TASTNodeType);
       destructor  Destroy; override;
   end;

   TASTFloat = class (TASTNOde)
       dValue : double;
       constructor Create (dvalue : double);
   end;

   TASTBoolean = class (TASTNOde)
       bValue : boolean;
       constructor Create (bvalue : boolean);
   end;

   TASTString = class (TASTNode)
      sValue : string;
      constructor Create (sValue : string);
   end;

   TASTCreateList = class (TASTNodeList)
      constructor Create;
      destructor  Destroy; override;
   end;

   TASTArray = class (TASTNodeList)
      constructor Create;
      destructor  Destroy; override;
   end;

   TASTIdentifier = class (TASTNode)
      symbolName : string;
      constructor Create (symbolName : string);
      destructor  Destroy; override;
   end;

   TASTPrimary = class (TASTNode)
      factor : TASTNode;
      primaryPlus : TASTNode;
      constructor Create (factor, primaryPlus : TASTNode);
      destructor  Destroy; override;
   end;

   // '.' identifier primary+
   TASTPrimaryPeriod = class (TASTNode)
      identifier : TASTIdentifier;
      primaryPlus : TASTNode;
      constructor Create (identifier : TASTIdentifier; primaryPlus : TASTNode);
      destructor  Destroy; override;
   end;

   // '[' expression list ']' primary+
   TASTPrimaryIndex = class (TASTNode)
      subscriptList : TASTNodeList;
      primaryPlus : TASTNode;
      constructor Create (subscriptList : TASTNodeList; primaryPlus : TASTNode);
      destructor  Destroy; override;
   end;

   // '(' expression list ')' primary+
   TASTPrimaryFunction = class (TASTNode)
      argumentList : TASTNodeList;
      primaryPlus : TASTNode;
      constructor Create (argumentList : TASTNodeList; primaryPlus : TASTNode);
      destructor  Destroy; override;
   end;

   TASTNull = class (TASTNode)
       constructor Create;
       destructor  Destroy; override;
   end;

   TASTFunctionCall = class (TASTNode)
       argumentList : TASTNodeList;
       constructor Create (argumentList : TASTNodeList);
       destructor  Destroy; override;
   end;

   TASTSubscript = class (TASTNode)
        subscripts : TASTNodeList;
        constructor Create (subscripts : TASTNodeList);
        destructor  Destroy; override;
   end;

   TASTBinOp = class (TASTNode)
      left : TASTNode;
      right : TASTNode;
      constructor Create (left, right : TASTNode; nodeType : TASTNodeType);
      destructor  Destroy; override;
   end;

   TASTPowerOp = class (TASTNode)
      left : TASTNode;
      right : TASTNode;
      constructor Create (left, right : TASTNode);
      destructor  Destroy; override;
   end;

   TASTNotOp = class (TASTNode)
      expression : TASTNode;
      constructor Create (node : TASTNode);
      destructor  Destroy; override;
   end;

   TASTUniOp = class (TASTNode)
      left : TASTNode;
      constructor Create (left : TASTNode; nodeType : TASTNodeType);
      destructor  Destroy; override;
   end;

   TASTExpression = class (TASTNode)
      expression : TASTNode;
      constructor Create (expression : TASTNode);
      destructor  Destroy; override;
   end;

   TASTAssignment = class (TASTNode)
      leftSide : TASTPrimary;
      rightSide : TASTNode;
      constructor Create (leftSide : TASTPrimary; rightSide : TASTNode);
      destructor  Destroy; override;
   end;

   TASTExpressionStatement = class (TASTNode)
     expression : TASTNode;
     constructor Create (expression : TASTNode);
     destructor  Destroy; override;
   end;

   TASTStatementList = class (TASTNode)
      statementList : TChildNodes;
      constructor Create;
      destructor  Destroy; override;
   end;

   TASTReturn = class (TASTNode)
     expression : TASTExpression;
     constructor Create (expression : TASTExpression);
     destructor  Destroy; override;
   end;

   TASTIf = class (TASTNode)
     condition : TASTNode;
     thenStatementList : TASTNode;
     elseStatementList : TASTNode;
     constructor Create (condition, thenStatementList, elseStatementList : TASTNode);
     destructor  Destroy; override;
   end;

   TASTIterationBlock = class (TASTNode)
     iterationSymbol : TASTIdentifier;
     lower : TASTExpression;
     upper : TASTExpression;
     direction : TASTNode;
     stepValue : double;
     constructor Create (iterationSymbol : TASTIdentifier; lower, upper : TASTExpression);
     destructor  Destroy; override;
   end;

   TASTFor = class (TASTNode)
     iterationBlock : TASTIterationBlock;
     body : TASTNode;
     constructor Create (iterationBlock : TASTIterationBlock; body : TASTNode);
     destructor  Destroy; override;
   end;

   TASTRepeat = class (TASTNode)
     public
        statementList : TASTNode;
        condition : TASTNode;
        constructor Create (statementList : TASTNode; condition : TASTNode);
        destructor  Destroy; override;
   end;

   TASTWhile = class (TASTNode)
      condition : TASTExpression;
      statementList : TASTStatementList;
      constructor Create (condition : TASTExpression; listOfStatements : TASTStatementList);
      destructor  Destroy; override;
   end;

   TASTCaseStatement = class (TASTNode)
      caseValue : TASTInteger;
      statementList : TASTStatementList;
      constructor Create (caseValue : TASTInteger; statementList : TASTStatementList);
      destructor  Destroy; override;
   end;

   // repeat of [case value : statementList]
   TASTListOfCaseStatements = class (TASTNode)
      list : TChildNodes;  // list of case statements, TASTCaseStatement
      constructor Create;
      destructor  Destroy; override;
   end;

   // switch switchExpression statemementListNode elseStatement
   TASTSwitch = class (TASTNode)
      switchExpression : TASTNode;
      caseList : TASTListOfCaseStatements;
      elseStatement : TASTStatementList;
      constructor Create (switchExpression : TASTNode; caseList : TASTListOfCaseStatements; elseStatement : TASTStatementList);
      destructor  Destroy; override;
   end;

   TASTSetColor = class (TASTNode)
      expression : TASTExpression;
      constructor Create (expression : TASTExpression);
      destructor  Destroy; override;
   end;

   TASTAssertTrue = class (TASTNode)
      expression : TASTExpression;
      constructor Create (expression : TASTExpression);
      destructor  Destroy; override;
   end;

   TASTAssertTrueEx = class (TASTNode)
      expression : TASTExpression;
      constructor Create (expression : TASTExpression);
      destructor  Destroy; override;
   end;

   TASTAssertFalse = class (TASTNode)
      expression : TASTExpression;
      constructor Create (expression : TASTExpression);
      destructor  Destroy; override;
   end;

   TASTAssertFalseEx = class (TASTNode)
      expression : TASTExpression;
      constructor Create (expression : TASTExpression);
      destructor  Destroy; override;
   end;

   TASTHelp = class (TASTNode)
      expression : TASTExpression;
      constructor Create (expression : TASTExpression);
      destructor  Destroy; override;
   end;

   TASTUserFunction = class (TASTNode)
        moduleName : string; // required to handle recursive callsf
        functionName : string;
        argumentList : TASTNodeList;
        body : TASTStatementList;
        constructor Create (moduleName, functionName : string; argumentList : TASTNodeList; body : TASTStatementList);
        destructor  Destroy; override;
   end;

   TASTPrint = class (TASTNode)
      argumentList : TASTNodeList;
      constructor Create (argumentList : TASTNodeList);
      destructor  Destroy; override;
   end;

   TASTPrintLn = class (TASTNode)
      argumentList : TASTNodeList;
      constructor Create (argumentList : TASTNodeList);
      destructor  Destroy; override;
   end;

   TASTGlobal = class (TASTNode)
      moduleName : string;
      variableList : TASTNodeList;
      function    find (name : string; var index  : integer) : boolean;
      constructor Create (moduleName : string; variableList : TASTNodeList);
      destructor  Destroy; override;
   end;

   TASTImport  = class (TASTNode)
     importName : string;
     constructor Create (importName : string);
     destructor  Destroy; override;
   end;

   function displayAST (root : TASTNode) : string;

   procedure freePool;

implementation

Uses StrUtils, RTTi;

var pool : TPool;

procedure TPool.addToPool (obj : TASTNode);
begin
  add (obj);
end;


procedure freePool;
begin
  for var i := pool.Count - 1 downto 0 do
      begin
      pool[i].Free;
      pool.Delete (i);
      end;
end;


procedure TChildNodes.freeChildNodes;
begin
   for var i := self.count - 1 downto 0 do
       self[i].freeAST;
end;

// ------------------------------------------------------------------

constructor TASTNode.Create(nodeType: TASTNodeType);
begin
  inherited Create;
  freeChildren := True;//False;    // set true so that we can use freeAST
  self.nodeType := nodeType;
  pool.addToPool (self);
end;


destructor TASTNode.Destroy;
begin
  inherited;
end;



// ------------------------------------------------------------------------------------------


constructor TASTErrorNode.Create (errMsg : string; lineNumber, columnNumber : integer);
begin
  inherited Create (ntError);
  self.errorMsg := errMsg;
  self.lineNumber := lineNumber;
  self.columnNumber := columnNumber;
end;


//constructor TASTPeriod.Create (name :string);
//begin
//  inherited Create (ntPeriod);
//  self.name := name;
//end;


//destructor TASTPeriod.Destroy;
//begin
//  inherited;
//end;


constructor TASTPrimaryPeriod.Create (identifier : TASTIdentifier; primaryPlus : TASTNode);
begin
  inherited Create (ntPrimaryPeriod);
  self.identifier := identifier;
  self.primaryPlus := primaryPlus;
end;

destructor TASTPrimaryPeriod.Destroy;
begin
  if freeChildren then
     begin
     identifier.freeAST;
     primaryPlus.freeAST;
     end;
  inherited;
end;

constructor TASTPrimaryIndex.Create(subscriptList: TASTNodeList; primaryPlus: TASTNode);
begin
  inherited Create (ntPrimaryIndex);
  self.subscriptList := subscriptList;
  self.primaryPlus := primaryPlus;
end;


// primaryIndex => subscriptList; primaryPlus
destructor TASTPrimaryIndex.Destroy;
var node : TASTNode;
begin
  if freeChildren then
     begin
     for node in subscriptList.list do
         node.freeAST;
     subscriptList.list.clear;
     primaryPlus.freeAST;
     end;
  subscriptList.free;
  inherited;
end;


constructor TASTPrimaryFunction.Create (argumentList: TASTNodeList; primaryPlus: TASTNode);
begin
  inherited Create (ntPrimaryFunction);
  self.argumentList :=  argumentList;
  self.primaryPlus := primaryPlus;
end;


// primaryIndex => subscriptList; primaryPlus
destructor TASTPrimaryFunction.Destroy;
var node : TASTNode;
begin
  if freeChildren then
     begin
     for node in argumentList.list do
         node.freeAST;
     argumentList.list.clear;
     primaryPlus.freeAST;
     end;
  argumentList.free;
  inherited;
end;


constructor TASTNull.Create;
begin
  inherited Create (ntNull);
end;

destructor  TASTNull.Destroy;
begin
  inherited;
end;


constructor TASTSubscript.Create (subscripts : TASTNodeList);
begin
  inherited Create (ntSubscript);
  self.subscripts := subscripts;
end;


destructor TASTSubscript.Destroy;
var node : TASTNode;
begin
  if freeChildren then
     begin
     for node in subscripts.list do
         node.freeAST;
     subscripts.list.Clear;
     end;
  subscripts.free;
end;


constructor TASTInteger.Create (iValue : integer);
begin
  inherited Create (ntInteger);
  self.iValue := iValue;
end;


constructor TASTPrimary.Create (factor, primaryPlus : TASTNode);
begin
  inherited Create (ntPrimary);
  self.factor := factor;
  self.primaryPlus := primaryPlus;
end;


destructor TASTPrimary.Destroy;
begin
  if freeChildren then
     begin
     factor.freeAST;
     primaryPlus.freeAST;
     end;
  inherited;
end;



constructor TASTFloat.Create (dValue : double);
begin
  inherited Create (ntFloat);
  self.dValue := dValue;
end;


constructor TASTBoolean.Create (bValue : boolean);
begin
  inherited Create (ntBoolean);
  self.bValue := bValue;
end;


constructor TASTString.Create (sValue : string);
begin
  inherited Create (ntString);
  self.sValue := sValue;
end;


constructor TASTCreateList.Create;
begin
  inherited Create (ntCreateList);
end;

destructor TASTCreateList.Destroy;
begin
  inherited;
end;


constructor TASTArray.Create;
begin
  inherited Create (ntArray);
end;


destructor TASTArray.Destroy;
begin
  inherited;
end;



constructor TASTIdentifier.Create (symbolName : string);
begin
  inherited Create (ntIdentifier);
  self.symbolName := symbolName;
end;

destructor TASTIdentifier.Destroy;
begin
  inherited;
end;


constructor TASTNodeList.Create (nodeType : TASTNodeType);
begin
  inherited Create (nodeType);
  list := TChildNodes.Create;
end;


destructor TASTNodeList.Destroy;
begin
  if freeChildren then
     begin
     for var i := list.count - 1 downto 0 do
         list[i].freeAST;
     list.Clear;
     end;
   list.Free;
   inherited;
end;

// -----------------------------------------------------------------------------------------


constructor TASTBinOp.Create (left, right : TASTNode; nodeType : TASTNodeType);
begin
  inherited Create (nodeType);
  self.left := left;
  self.right := right;
end;


destructor TASTBinOp.Destroy;
begin
  if freeChildren then
     begin
     left.freeAST;
     right.freeAST;
     end;
  inherited
end;

constructor TASTPowerOp.Create (left, right : TASTNode);
begin
  inherited Create (ntPower);
  self.left := left;
  self.right := right;
end;


destructor TASTPowerOp.Destroy;
begin
  if freeChildren then
     begin
     left.freeAST;
     right.freeAST;
     end;
  inherited;
end;

constructor TASTNotOp.create (node : TASTNode);
begin
  inherited Create (ntNOT);
  self.expression := node;
end;

destructor TASTNotOp.destroy;
begin
  if freeChildren then
     expression.freeAST;
  inherited
end;

constructor TASTUniOp.create (left : TASTNode; nodeType : TASTNodeType);
begin
  inherited Create (nodeType);
  self.left := left;
end;

destructor TASTUniOp.destroy;
begin
  if freeChildren then
     left.freeAST;
  inherited
end;


constructor TASTExpression.create (expression : TASTNode);
begin
  inherited Create (ntExpression);
  self.expression := expression;
end;


destructor TASTExpression.Destroy;
begin
  if freeChildren then
     expression.freeAST;
  inherited;
end;



constructor TASTAssignment.Create (leftSide : TASTPrimary; rightSide : TASTNode);
begin
  inherited Create (ntAssignment);
  self.leftSide := leftSide;
  self.rightSide := rightSide;
end;


destructor TASTAssignment.destroy;
begin
  if freeChildren then
     begin
     leftSide.freeAST;
     rightSide.freeAST;
     end;
  inherited;
end;



constructor TASTExpressionStatement.Create (expression : TASTNode);
begin
  inherited Create (ntExpressionStatement);
  self.expression := expression;
end;


destructor TASTExpressionStatement.Destroy;
begin
  if freeChildren then
     expression.freeAst;
  inherited;
end;


constructor TASTStatementList.Create;
begin
  inherited Create (ntStatementList);
  statementList := TChildNodes.Create;
end;


destructor TASTStatementList.Destroy;
var i : integer;
begin
  if freeChildren then
     begin
     for i := 0 to statementList.Count - 1 do
         statementList[i].freeAst;
     end;
   statementList.free;
end;


constructor TASTIf.Create (condition, thenStatementList, elseStatementList : TASTNode);
begin
  inherited Create (ntIf);
  self.condition := condition;
  self.thenStatementList := thenStatementList;
  self.elseStatementList := elseStatementList;
end;


destructor TASTIf.Destroy;
begin
  if freeChildren then
     begin
     condition.freeAST;
     thenStatementList.freeAST;
     elseStatementList.freeAST;
     end;
  inherited;
end;

constructor TASTFor.Create (iterationBlock : TASTIterationBlock; body : TASTNode);
begin
  inherited Create (ntFor);
  self.iterationBlock := iterationBlock;
  self.body := body;
end;

destructor TASTFor.destroy;
begin
  if freeChildren then
     begin
     iterationBlock.freeAST;
     body.freeAST;
     end;
  inherited;
end;


constructor TASTIterationBlock.Create (iterationSymbol : TASTIdentifier; lower, upper : TASTExpression);
begin
  inherited Create (ntIter);
  self.iterationSymbol := iterationSymbol;
  self.lower := lower;
  self.upper := upper;
end;


destructor TASTIterationBlock.destroy;
begin
  if freeChildren then
     begin
     iterationSymbol.freeAST;
     lower.freeAST;
     upper.freeAST;
     direction.freeAST;
     end;
  inherited;
end;


constructor TASTReturn.create (expression : TASTExpression);
begin
  inherited Create (ntReturn);
  self.expression := expression;
end;

destructor TASTReturn.destroy;
begin
  if freeChildren then
     expression.freeAST;
  inherited;
end;


constructor TASTRepeat.create (statementList : TASTNode; condition : TASTNode);
begin
  inherited Create (ntRepeat);
  self.statementList := statementList;
  self.condition := condition;
end;


destructor TASTRepeat.destroy;
begin
  if freeChildren then
     begin
     statementList.freeAST;
     condition.freeAST;
     end;
  inherited;
end;

constructor TASTWhile.Create (condition : TASTExpression; listOfStatements : TASTStatementList);
begin
  inherited Create (ntWhile);
  self.condition := condition;
  self.statementList := listOfStatements;
end;


destructor TASTWhile.Destroy;
begin
  if freeChildren then
     begin
     condition.freeAST;
     statementList.freeAST;
     end;
  inherited
end;


constructor TASTCaseStatement.create (caseValue : TASTInteger; statementList : TASTStatementList);
begin
  inherited Create (ntCaseStatement);
  self.caseValue := caseValue;
  self.statementList := statementList;
end;


destructor TASTCaseStatement.Destroy;
begin
  if freeChildren then
     begin
     caseValue.freeAST;
     statementList.freeAST;
     end;
  inherited
end;


constructor TASTListOfCaseStatements.Create;
begin
  inherited Create (ntListOfCaseStatements);
  list := TChildNodes.Create;
end;


destructor TASTListOfCaseStatements.Destroy;
var i : integer;
begin
  if freeChildren then
     begin
     for i := 0 to list.Count - 1 do
         list[i].freeAST;
     end;
  list.free;
  inherited;
end;



constructor TASTSwitch.Create (switchExpression : TASTNode; caseList : TASTListOfCaseStatements; elseStatement : TASTStatementList);
begin
  inherited Create (ntSwitch);
  self.switchExpression := switchExpression;
  self.caseList := caseList;
  self.elseStatement := elseStatement;
end;

destructor TASTSwitch.Destroy;
begin
  if freeChildren then
     begin
     switchExpression.freeAST;
     caseList.freeAST;
     elseStatement.freeAST;
     end;
  inherited;
end;

constructor TASTSetColor.Create (expression : TASTExpression);
begin
  inherited Create (ntSetColor);
  self.expression := expression;
end;


destructor TASTSetColor.destroy;
begin
  if freeChildren then
     expression.freeAST;
  inherited;
end;


constructor TASTAssertTrue.create (expression : TASTExpression);
begin
  inherited Create (ntAssertTrue);
  self.expression := expression;
end;


destructor TASTAssertTrue.destroy;
begin
  if freeChildren then
     expression.freeAST;
  inherited;
end;


constructor TASTAssertTrueEx.Create (expression : TASTExpression);
begin
  inherited Create (ntAssertTrueEx);
  self.expression := expression;
end;


destructor TASTAssertTrueEx.destroy;
begin
  if freeChildren then
     expression.freeAST;
  inherited;
end;

constructor TASTAssertFalse.Create (expression : TASTExpression);
begin
  inherited Create (ntAssertFalse);
  self.expression := expression;
end;


destructor TASTAssertFalse.Destroy;
begin
  if freeChildren then
     expression.freeAST;
  inherited
end;

constructor TASTAssertFalseEx.Create (expression : TASTExpression);
begin
  inherited Create (ntAssertFalseEx);
  self.expression := expression;
end;


destructor TASTAssertFalseEx.destroy;
begin
  if freeChildren then
     expression.freeAST;
  inherited;
end;


constructor TASTHelp.Create (expression : TASTExpression);
begin
  inherited Create (ntHelp);
  self.expression := expression;
end;


destructor TASTHelp.Destroy;
begin
  if freeChildren then
     expression.freeAST;
  inherited;
end;


constructor TASTUserFunction.Create (moduleName, functionName : string; argumentList : TASTNodeList; body : TASTStatementList);
begin
  inherited Create (ntFunction);
  self.moduleName := moduleName;
  self.functionName := functionName;
  self.argumentList := argumentList;
  self.body := body;
end;


destructor TASTUserFunction.destroy;
var node : TASTNode;
begin
  if freeChildren then
     begin
     for node in argumentList.list do
         node.freeAST;
     argumentList.list.Clear;
     body.freeAST;
     end;
  argumentList.Free;
  inherited
end;


constructor TASTFunctionCall.Create (argumentList : TASTNodeList);
begin
  inherited Create (ntFunctionCall);
  self.argumentList := argumentList;
end;


destructor TASTFunctionCall.destroy;
begin
  for var i := 0 to argumentList.list.Count - 1 do
      argumentList.list[i].freeAST;
  argumentList.list.Clear;
  argumentList.free;
  inherited;
end;


constructor TASTPrint.Create (argumentList : TASTNodeList);
begin
  inherited Create (ntPrint);
  self.argumentList := argumentList;
end;

destructor TASTPrint.destroy;
var i : integer;
begin
  if freeChildren then
     begin
     for i := 0 to argumentList.list.Count - 1 do
         argumentList.list[i].freeAST;
     argumentList.list.Clear;
     end;
  argumentList.Free;
  inherited
end;

constructor TASTPrintLn.Create (argumentList : TASTNodeList);
begin
  inherited Create (ntPrintLn);
  self.argumentList := argumentList;
end;

destructor TASTPrintLn.Destroy;
var node : TASTNode;
begin
  if freeChildren then
     begin
     for node in argumentList.list do
         node.freeAST;
     argumentList.list.Clear;
     end;
  argumentList.Free;
  inherited
end;


constructor TASTGlobal.Create (moduleName : string; variableList : TASTNodeList);
begin
  inherited Create (ntGlobalStmt);
  self.moduleName := modulename;
  self.variableList := variableList;
end;


destructor TASTGlobal.Destroy;
var node : TASTNode;
begin
  if freeChildren then
     begin
     for node in variableList.list do
         node.freeAST;
     variableList.list.clear;
     end;
  variableList.free;
  inherited;
end;


function TASTGlobal.find (name : string; var index  : integer) : boolean;
var i: integer;
begin
  for i := 0 to variableList.list .Count - 1 do
      if (variableList.list[i] as TASTIdentifier).symbolName = name then
         begin
         index := i;
         exit (True);
         end;

  exit (False);
end;


constructor TASTImport.Create (importName : string);
begin
  inherited Create (ntImportStmt);
  self.importName := importName;
end;

destructor TASTImport.Destroy;
begin
  inherited;
end;


// To use this method you must set freeChildren to True
procedure TASTNode.freeAST;
var node : TASTNode;
begin
  node := self;
  if node = nil then
     exit;

  node.freeChildren := True;  // By default its off
  case node.nodeType of
    ntError:
       (node as TASTErrorNode).free;
    ntNull:
       (node as TASTNull).free;
    ntFloat:
       (node as TASTFloat).free;
    ntInteger:
       (node as TASTInteger).free;
    ntString:
       (node as TASTString).free;
    ntBoolean:
       (node as TASTBoolean).free;
    ntNodeList :
       (node as TASTNodeList).free;
    ntAdd, ntSub, ntMult, ntDiv, ntDivI, ntMod, ntLT,
            ntEQ, ntLE, ntGT, ntGE, ntNE, ntAnd, ntXor, ntOR:
      (node as TASTBinOp).free;
    ntPower :
       (node as TASTPowerOp).free;
    ntNOT :
       (node as TASTNotOp).free;
    ntUnaryMinus:
       (node as TASTUniOp).free;
    ntExpression:
        (node as TASTExpression).free;
    ntExpressionStatement:
        (node as TASTExpressionStatement).free;
    ntPrimary:
        (node as TASTPrimary).free;
    ntPrimaryPeriod:
        (node as TASTPrimaryPeriod).free;
    ntPrimaryIndex:
        (node as TASTPrimaryIndex).free;
    ntPrimaryFunction:
        (node as TASTPrimaryFunction).free;
    ntIdentifier:
        (node as TASTIdentifier).free;
    ntSubscript :
        (node as TASTSubscript).free;
    ntImportStmt:
        (node as TASTImport).free;
    ntGlobalStmt:
        (node as TASTGlobal).free;
    ntCreateList:
         (node as TASTCreateList).free;
    ntArray :
         (node as TASTArray).free;
    //ntArrayRow :
    //     (node as TASTArrayRow).free;
    ntPrint:
         (node as TASTPrint).free;
    ntPrintln:
        (node as TASTPrintLn).free;
    ntSetColor:
        (node as TASTSetColor).free;
    ntAssertTrue:
        (node as TASTAssertTrue).free;
    ntAssertTrueEx:
        (node as TASTAssertTrueEx).free;
    ntAssertFalse:
        (node as TASTAssertFalse).free;
    ntAssertFalseEx:
        (node as TASTAssertFalseEx).free;
    ntHelp:
        (node as TASTHelp).free;
    ntSwitch:
        (node as TASTSwitch).free;
    ntListOfCaseStatements:
        (node as TASTListOfCaseStatements).free;
    ntCaseStatement:
        (node as TASTCaseStatement).free;
    ntIf:
       (node as TASTIf).free;
    ntWhile:
       (node as TASTWhile).free;
    ntRepeat:
       (node as TASTRepeat).free;
    ntFor:
      (node as TASTFor).free;
    ntIter:
      (node as TASTIterationBlock).free;
    ntTo, ntDownTo:
       node.free;
    ntReturn:
      (node as TASTReturn).free;
    ntAssignment:
      (node as TASTAssignment).free;
    ntBreak:
      node.free;
    ntFunctionCall:
      (node as TASTFunctionCall).free;
    ntFunction :
      (node as TASTUserFunction).free;
    ntStatementList:
       (node as TASTStatementList).free;
  else
     raise Exception.Create('Unrecognized node in freeAST: ' + TRttiEnumerationType.GetName(node.nodeType));
  end;
end;

// ---------------------------------------------------------------------------------------

function visitNode (node : TASTNode; level : integer; indent : string): string;
var pfx : string;
begin
  result := '';
  pfx := DupeString(indent, level);
  result := result + pfx;
  result := result + nodeTypeToString (node.nodeType);
  result := result + '(';
  result := result + ')'
 end;


function nodeToString (node : TASTNode) : string;
begin
  case node.nodeType  of
     ntBoolean : result := booltostr ((node as TASTBoolean).bValue, True);
     ntInteger : result := inttostr ((node as TASTInteger).iValue);
     ntFloat   : result := floattostr ((node as TASTFloat).dValue);
     ntString  : result := '"' + (node as TASTString).sValue + '"';
     ntFunctionCall : result := 'function Call';// + (node as TASTFunctionCall).symbolName;
     ntIdentifier : result := 'Identifier: ' + (node as TASTIdentifier).symbolName;
     ntImportStmt : result := 'import: ' + (node as TASTImport).importName;
     ntNull : result := 'null';
     ntPrimary : result := 'primary';
     ntPrimaryPeriod : result := 'primaryPeriod';
  else
     result := nodeTypeToString  (node.nodeType);
  end;
end;



function print(node: TASTNode; prefix : string) : string;
var i : integer;
    listOfCaseStatements : TASTListOfCaseStatements;
begin
  if node = nil then
     exit;

  result := prefix + '+- ' + nodeToString (node) + sLineBreak;
  case node.nodeType of
        ntError :
          result := result + 'ERROR ' + '[line ' + inttostr ((node as TASTErrorNode).lineNumber) + ', column: ' + inttostr ((node as TASTErrorNode).columnNumber) + '] ' + ((node as TASTErrorNode).errorMsg);
        ntStatementList :
           begin
           for i := 0 to (node as TASTStatementList).statementList.Count - 1 do
               result := result + print ((node as TASTStatementList).statementList[i], prefix + '|  ');
           end;
       ntAssignment :
           begin
           result := result + print ((node as TASTAssignment).leftSide, prefix + '|  ');
           result := result + print ((node as TASTAssignment).rightSide, prefix + '|  ');
           end;
       ntPrimary :
           begin
           result := result + print ((node as TASTPrimary).factor, prefix + '|  ');
           result := result + print ((node as TASTPrimary).primaryPlus, prefix + '|  ');
           end;
       ntPrimaryPeriod :
           begin
           result := result + print ((node as TASTPrimaryPeriod).identifier, prefix + '|  ');
           result := result + print ((node as TASTPrimaryPeriod).primaryPlus, prefix + '|  ');
           end;
       ntPrimaryIndex :
           begin
          for i := 0 to (node as TASTPrimaryIndex).subscriptList.list.Count - 1 do
               result := result + print ((node as TASTPrimaryIndex).subscriptList.list[i], prefix + '|  ');

           result := result + print ((node as TASTPrimaryIndex).primaryPlus, prefix + '|  ');
           end;
       ntPrimaryFunction :
           begin
          for i := 0 to (node as TASTPrimaryFunction).argumentList.list.Count - 1 do
               result := result + print ((node as TASTPrimaryFunction).argumentList.list[i], prefix + '|  ');

           result := result + print ((node as TASTPrimaryFunction).primaryPlus, prefix + '|  ');
           end;
       ntSubscript :
           begin
           for i := 0 to (node as TASTSubscript).subscripts.list.Count - 1 do
               result := result + print ((node as TASTSubscript).subscripts.list[i], prefix + '|  ');
           end;
       ntExpression :
           begin
           result := result + print ((node as TASTExpression).expression, prefix + '|  ');
           end;
       ntExpressionStatement :
           begin
           result := result + print ((node as TASTExpressionStatement).expression, prefix + '|  ');
           end;
       ntGlobalStmt :
          begin
          for i := 0 to (node as TASTGlobal).variableList.list.Count - 1 do
              result := result + prefix + '+- ' + 'global: ' + sLineBreak + print (((node as TASTGlobal).variableList.list[i] as TASTIdentifier), prefix + '|  ');
          end;
       ntCreateList :
           begin
           for i := 0 to (node as TASTCreateList).list.Count - 1 do
              result := result + print ((node as TASTCreateList).list[i], prefix + '|  ');
           end;
       ntArray :
          begin
          result := result + 'ntarray'
          end;
       ntAdd, ntSub, ntMult, ntDiv, ntDivI, ntLT, ntLE, ntGT, ntGE, ntNE, ntEQ,
       ntAnd, ntOr, ntXor :
           begin
           result := result + print ((node as TASTBinOp).left, prefix + '|  ');
           result := result + print ((node as TASTBinOp).right, prefix + '|  ');
           end;
       ntPower :
         begin
           result := result + print ((node as TASTPowerOp).left, prefix + '|  ');
           result := result + print ((node as TASTPowerOp).right, prefix + '|  ');
           end;
       ntNOT :
           result := result + print ((node as TASTNotOp).expression, prefix + '|  ');
       ntUnaryMinus :
           result := result + print ((node as TASTUniOp).left, prefix + '|  ');
       ntReturn :
           result := result + print ((node as TASTReturn).expression, prefix + '| ');
       ntWhile :
           begin
           result := result + print ((node as TASTWhile).condition, prefix + '|  ');
           result := result + print ((node as TASTWhile).statementList, prefix + '|  ');
           end;
       ntRepeat :
          begin
          result := result + print ((node as TASTRepeat).statementList, prefix + '|  ');
          result := result + print ((node as TASTRepeat).condition, prefix + '|  ');
          end;
       ntFor :
          begin
          result := result + print ((node as TASTFor).iterationBlock, prefix + '|  ');
          result := result + print ((node as TASTFor).body, prefix + '|  ');
          end;
       ntIter :
          begin
          result := result + prefix + '|  loop variable: ' + (node as TASTIterationBlock).iterationSymbol.symbolName + sLineBreak;
          result := result + print ((node as TASTIterationBlock).lower, prefix + '|  ');
          result := result + print ((node as TASTIterationBlock).upper, prefix + '|  ');
          end;
       ntIf :
          begin
          result := result + print ((node as TASTIf).condition, prefix + '|  ');
          result := result + print ((node as TASTIf).thenStatementList, prefix + '|  ');
          if (node as TASTIf).elseStatementList <> nil then
             result := result + print ((node as TASTIf).elseStatementList, prefix + '|  ');
          end;
       ntSwitch :
          begin
          result := result + print ((node as TASTSwitch).switchExpression, prefix + '|  ');
          result := result + print ((node as TASTSwitch).caseList, prefix + '|  ');
          end;
       ntListOfCaseStatements :
          begin
          listOfCaseStatements := node as TASTListOfCaseStatements;
          for i := 0 to listOfCaseStatements.list.Count - 1 do
              begin
              result := result + print ((listOfCaseStatements.list[i] as TASTCaseStatement).caseValue, prefix + '|  ');
              result := result + print ((listOfCaseStatements.list[i] as TASTCaseStatement).statementList, prefix + '|  ');
              end;
          end;
       ntPrint :
          begin
          for i := 0 to (node as TASTPrint).argumentList.list.Count - 1 do
              result := result + print ((node as TASTPrint).argumentList.list[i], prefix + '|  ');
           end;
       ntPrintln :
          begin
          for i := 0 to (node as TASTPrintLn).argumentList.list.Count - 1 do
              result := result + print ((node as TASTPrintLn).argumentList.list[i], prefix + '|  ');
          end;
       ntFunction :
          begin
          if (node as TASTUserFunction).argumentList <> nil then
             for i := 0 to (node as TASTUserFunction).argumentList.list.Count - 1 do
                 result := result + print ((node as TASTUserFunction).argumentList.list[i], prefix + '|  ');
          result := result + print ((node as TASTUserFunction).body, prefix + '|  ');
          end;
       ntFunctionCall :
         begin
         for i := 0 to (node as TASTFunctionCall).argumentList.list.Count - 1 do
             result := result + print ((node as TASTFunctionCall).argumentList.list[i], prefix + '|  ');
         end;
       ntAssertTrue :
         begin
         result := result + print ((node as TASTAssertTrue).expression, prefix + '|  ');
         end;
      ntAssertFalse :
         begin
         result := result + print ((node as TASTAssertFalse).expression, prefix + '|  ');
         end
       else
          begin  end;
  end;
end;


// Pretty print AST
function displayAST (root : TASTNode) : string;
begin
  if root = nil then
     exit ('');

  result := print (root, '');
end;

initialization
  pool := TPool.Create;
finalization
  pool.Free;
end.


