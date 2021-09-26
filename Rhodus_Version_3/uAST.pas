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

   // Base AST Node
   TASTNode = class (TObject)
        nodeType : TASTNodeType;
        moduleName : string;

        procedure   freeAST;
        constructor create(nodeType: TASTNodeType);
        constructor createEmptyNode;
        destructor  destroy; override;
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

   TASTCreateList = class (TASTNode)
      nodeList : TChildNodes;
      constructor Create (nodeList : TChildNodes);
      destructor  Destroy; override;
   end;


   // A primary is the first identifier in a symbol, eg a, a.b, a[1], a(), a[1]() etc.,
   TASTPrimary = class (TASTNode)
      primaryName :string;
      nodes : TChildNodes;
      constructor Create (primaryName : string);
      destructor  Destroy; override;
   end;

   TASTPeriod = class (TASTNode)
      name : string;
      constructor Create (name :string);
      destructor  Destroy; override;
   end;

   TASTFunctionCall = class (TASTNode)
       moduleName : string;
       symbolIndex: integer;
       symbolName : string;
       argumentList : TChildNodes;
       constructor Create (moduleName, symbolName : string; argumentList : TChildNodes);
       destructor  Destroy; override;
   end;

   TASTSubscript = class (TASTNode)
        subscripts : TChildNodes;
        constructor Create (subscripts : TChildNodes);
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
     assignment : TASTAssignment;
     upper : TASTExpression;
     direction : TASTNode;
     stepValue : double;
     constructor Create (assignment : TASTAssignment; upper : TASTExpression);
     destructor  Destroy; override;
   end;

   TASTFor = class (TASTNode)
     iterationBlock : TASTIterationBlock;
     body : TASTNode;
     constructor create (iterationBlock : TASTIterationBlock; body : TASTNode);
     destructor destroy; override;
   end;

   TASTRepeat = class (TASTNode)
     public
        statementList : TASTNode;
        condition : TASTNode;
        constructor create (statementList : TASTNode; condition : TASTNode);
        destructor destroy; override;
   end;

   TASTWhile = class (TASTNode)
      condition : TASTExpression;
      statementList : TASTStatementList;
      constructor create (condition : TASTExpression; statementListNode : TASTStatementList);
      destructor destroy; override;
   end;

   TASTCaseStatement = class (TASTNode)
      caseValue : TASTInteger;
      statementList : TASTStatementList;
      constructor create (caseValue : TASTInteger; statementList : TASTStatementList);
      destructor destroy; override;
   end;

   // repeat of [case value : statementList]
   TASTListOfCaseStatements = class (TASTNode)
      list : TChildNodes;  // list of case statements, TASTCaseStatement
      constructor create;
      destructor destroy; override;
   end;

   // switch switchExpression statemementListNode elseStatement
   TASTSwitch = class (TASTNode)
      switchExpression : TASTNode;
      caseList : TASTListOfCaseStatements;
      elseStatement : TASTStatementList;
      constructor create (switchExpression : TASTNode; caseList : TASTListOfCaseStatements; elseStatement : TASTStatementList);
      destructor destroy; override;
   end;

   TASTSetColor = class (TASTNode)
      expression : TASTExpression;
      constructor create (expression : TASTExpression);
      destructor destroy; override;
   end;

   TASTAssertTrue = class (TASTNode)
      expression : TASTExpression;
      constructor create (expression : TASTExpression);
      destructor destroy; override;
   end;

   TASTAssertTrueEx = class (TASTNode)
      expression : TASTExpression;
      constructor create (expression : TASTExpression);
      destructor destroy; override;
   end;

   TASTAssertFalse = class (TASTNode)
      expression : TASTExpression;
      constructor create (expression : TASTExpression);
      destructor destroy; override;
   end;

   TASTAssertFalseEx = class (TASTNode)
      expression : TASTExpression;
      constructor create (expression : TASTExpression);
      destructor destroy; override;
   end;

   TASTUserFunction = class (TASTNode)
        moduleName : string; // required to handle recursive callsf
        functionName : string;
        argumentList : TChildNodes;
        body : TASTStatementList;
        constructor create (moduleName, functionName : string; argumentList : TChildNodes; body : TASTStatementList);
        destructor destroy; override;
   end;

   TASTPrint = class (TASTNode)
      argumentList : TChildNodes;
      constructor create (argumentList : TChildNodes);
      destructor destroy; override;
   end;

   TASTPrintLn = class (TASTNode)
      argumentList : TChildNodes;
      constructor create (argumentList : TChildNodes);
      destructor destroy; override;
   end;

   TASTGlobal = class (TASTNode)
      moduleName : string;
      variableList : TChildNodes;
      constructor create (moduleName : string; variableList : TChildNodes);
      destructor destroy; override;
   end;

   TASTImport  = class (TASTNode)
     importName : string;
     constructor create (importName : string);
     destructor destroy; override;
   end;

   function displayAST (root : TASTNode) : string;

implementation

Uses StrUtils;


procedure TChildNodes.freeChildNodes;
begin
   for var i := self.count - 1 downto 0 do
       self[i].freeAST;
end;

// ------------------------------------------------------------------

constructor TASTNode.Create(nodeType: TASTNodeType);
begin
  inherited Create;
  self.nodeType := nodeType;
end;

constructor TASTNode.CreateEmptyNode;
begin
  inherited Create;
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


constructor TASTPrimary.Create (primaryName :string);
begin
  inherited Create (ntPrimary);
  self.primaryName := primaryName;
  self.nodes := TChildNodes.Create;
end;


destructor TASTPrimary.Destroy;
var i : integer;
begin
  for i := 0 to nodes.Count - 1 do
      nodes[i].freeAST;
  nodes.Free;
  inherited;
end;


constructor TASTPeriod.Create (name :string);
begin
  inherited Create (ntPeriod);
  self.name := name;
end;


destructor TASTPeriod.Destroy;
begin
  inherited;
end;


constructor TASTSubscript.Create (subscripts : TChildNodes);
begin
  inherited Create (ntSubscript);
  self.subscripts := subscripts;
end;


destructor TASTSubscript.Destroy;
var node : TASTNode;
begin
  for node in subscripts do
      node.freeAST;
  subscripts.free;
end;


constructor TASTInteger.Create (iValue : integer);
begin
  inherited Create (ntInteger);
  self.iValue := iValue;
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


constructor TASTCreateList.Create (nodeList : TChildNodes);
begin
  inherited Create (ntCreateList);
  self.nodeList := nodeList;
end;

destructor TASTCreateList.Destroy;
var i : integer;
begin
  if nodelist = nil then
     exit;

  for i := 0 to nodeList.Count - 1 do
      nodeList[i].freeAST;
  nodeList.free;
  inherited;
end;

// -----------------------------------------------------------------------------------------


constructor TASTBinOp.create (left, right : TASTNode; nodeType : TASTNodeType);
begin
  inherited Create (nodeType);
  self.left := left;
  self.right := right;
end;


destructor TASTBinOp.destroy;
begin
  left.freeAST;
  right.freeAST;
  inherited
end;

constructor TASTPowerOp.create (left, right : TASTNode);
begin
  inherited Create (ntPower);
  self.left := left;
  self.right := right;
end;


destructor TASTPowerOp.destroy;
begin
  left.freeAST;
  right.freeAST;
  inherited;
end;

constructor TASTNotOp.create (node : TASTNode);
begin
  inherited Create (ntNOT);
  self.expression := node;
end;

destructor TASTNotOp.destroy;
begin
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
  left.freeAST;
  inherited
end;


constructor TASTExpression.create (expression : TASTNode);
begin
  inherited Create (ntExpression);
  self.expression := expression;
end;


destructor TASTExpression.destroy;
begin
  expression.freeAST;
  inherited;
end;


constructor TASTAssignment.create (leftSide : TASTPrimary; rightSide : TASTNode);
begin
  inherited Create (ntAssignment);
  self.leftSide := leftSide;
  self.rightSide := rightSide;
end;


destructor TASTAssignment.destroy;
begin
  leftSide.freeAST;
  rightSide.freeAST;
  inherited;
end;


constructor TASTExpressionStatement.create (expression : TASTNode);
begin
  inherited Create (ntExpressionStatement);
  self.expression := expression;
end;


destructor TASTExpressionStatement.destroy;
begin
  expression.freeAst;
  inherited;
end;


constructor TASTStatementList.create;
begin
  inherited Create (ntStatementList);
  statementList := TChildNodes.Create;
end;


destructor TASTStatementList.Destroy;
var i : integer;
begin
  for i := 0 to statementList.Count - 1 do
      statementList[i].freeAst;
  statementList.free;
end;


constructor TASTIf.create (condition, thenStatementList, elseStatementList : TASTNode);
begin
  inherited Create (ntIf);
  self.condition := condition;
  self.thenStatementList := thenStatementList;
  self.elseStatementList := elseStatementList;
end;


destructor TASTIf.destroy;
begin
  condition.freeAST;
  thenStatementList.freeAST;
  elseStatementList.freeAST;
  inherited;
end;

constructor TASTFor.create (iterationBlock : TASTIterationBlock; body : TASTNode);
begin
  inherited Create (ntFor);
  self.iterationBlock := iterationBlock;
  self.body := body;
end;

destructor TASTFor.destroy;
begin
  iterationBlock.freeAST;
  body.freeAST;
  inherited;
end;


constructor TASTIterationBlock.create (assignment : TASTAssignment; upper : TASTExpression);
begin
  inherited Create (ntIter);
  self.assignment := assignment;
  self.upper := upper;
end;


destructor TASTIterationBlock.destroy;
begin
  assignment.freeAST;
  upper.freeAST;
  direction.freeAST;
  inherited;
end;


constructor TASTReturn.create (expression : TASTExpression);
begin
  inherited Create (ntReturn);
  self.expression := expression;
end;

destructor TASTReturn.destroy;
begin
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
  statementList.freeAST;
  condition.freeAST;
  inherited;
end;

constructor TASTWhile.create (condition : TASTExpression; statementListNode : TASTStatementList);
begin
  inherited Create (ntWhile);
  self.condition := condition;
  self.statementList := statementListNode;
end;


destructor TASTWhile.destroy;
begin
  condition.freeAST;
  statementList.freeAST;
  inherited
end;


constructor TASTCaseStatement.create (caseValue : TASTInteger; statementList : TASTStatementList);
begin
  inherited Create (ntCaseStatement);
  self.caseValue := caseValue;
  self.statementList := statementList;
end;


destructor TASTCaseStatement.destroy;
begin
  caseValue.freeAST;
  statementList.freeAST;
  inherited
end;


constructor TASTListOfCaseStatements.create;
begin
  inherited Create (ntListOfCaseStatements);
  list := TChildNodes.Create;
end;


destructor TASTListOfCaseStatements.destroy;
var i : integer;
begin
  for i := 0 to list.Count - 1 do
      list[i].freeAST;
  list.free;
  inherited;
end;



constructor TASTSwitch.create (switchExpression : TASTNode; caseList : TASTListOfCaseStatements; elseStatement : TASTStatementList);
begin
  inherited Create (ntSwitch);
  self.switchExpression := switchExpression;
  self.caseList := caseList;
  self.elseStatement := elseStatement;
end;

destructor TASTSwitch.destroy;
begin
  switchExpression.freeAST;
  caseList.freeAST;
  elseStatement.freeAST;
  inherited;
end;

constructor TASTSetColor.create (expression : TASTExpression);
begin
  inherited Create (ntSetColor);
  self.expression := expression;
end;


destructor TASTSetColor.destroy;
begin
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
  expression.freeAST;
  inherited;
end;


constructor TASTAssertTrueEx.create (expression : TASTExpression);
begin
  inherited Create (ntAssertTrueEx);
  self.expression := expression;
end;


destructor TASTAssertTrueEx.destroy;
begin
  expression.freeAST;
  inherited;
end;

constructor TASTAssertFalse.create (expression : TASTExpression);
begin
  inherited Create (ntAssertFalse);
  self.expression := expression;
end;


destructor TASTAssertFalse.destroy;
begin
  expression.freeAST;
  inherited
end;

constructor TASTAssertFalseEx.create (expression : TASTExpression);
begin
  inherited Create (ntAssertFalseEx);
  self.expression := expression;
end;


destructor TASTAssertFalseEx.destroy;
begin
  expression.freeAST;
  inherited;
end;


constructor TASTUserFunction.create (moduleName, functionName : string; argumentList : TChildNodes; body : TASTStatementList);
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
  for node in argumentList do
      node.freeAST;
  argumentList.Free;
  body.freeAST;
  inherited
end;

constructor TASTFunctionCall.create (moduleName, symbolName : string; argumentList : TChildNodes);
begin
  inherited Create (ntFunctionCall);
  self.symbolName := symbolName;
  self.moduleName := moduleName;
  self.argumentList := argumentList;
end;


destructor TASTFunctionCall.destroy;
var node : TASTNode;
begin
  for node in argumentList do
      node.freeAST;
  argumentList.free;
  inherited;
end;


constructor TASTPrint.create (argumentList : TChildNodes);
begin
  inherited Create (ntPrint);
  self.argumentList := argumentList;
end;

destructor TASTPrint.destroy;
var i : integer;
begin
  for i := 0 to argumentList.Count - 1 do
      argumentList[i].freeAST;
  argumentList.Free;
  inherited
end;

constructor TASTPrintLn.create (argumentList : TChildNodes);
begin
  inherited Create (ntPrintLn);
  self.argumentList := argumentList;
end;

destructor TASTPrintLn.destroy;
var node : TASTNode;
begin
  for node in argumentList do
      node.freeAST;
  argumentList.Free;
  inherited
end;


constructor TASTGlobal.create (moduleName : string; variableList : TChildNodes);
begin
  inherited Create (ntGlobalStmt);
  self.moduleName := modulename;
  self.variableList := variableList;
end;


destructor TASTGlobal.destroy;
var node : TASTNode;
begin
  for node in variableList do
      node.freeAST;
  variableList.free;
  inherited;
end;

constructor TASTImport.create (importName : string);
begin
  inherited Create (ntImportStmt);
  self.importName := importName;
end;

destructor TASTImport.destroy;
begin
  inherited;
end;

procedure TASTNode.freeAST;
var node : TASTNode;
begin
  node := self;
  if node = nil then
     exit;

  case node.nodeType of
    ntError:
       (node as TASTErrorNode).free;
    ntFloat:
       (node as TASTFloat).free;
    ntInteger:
       (node as TASTInteger).free;
    ntString:
       (node as TASTString).free;
    ntBoolean:
       (node as TASTBoolean).free;
    ntAdd, ntSub, ntMult, ntDiv, ntDivI, ntMod, ntLT, ntEQ, ntLE, ntGT, ntGE, ntNE, ntAnd, ntXor, ntOR:
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
    ntSubscript :
        (node as TASTSubscript).free;
    ntPeriod:
        (node as TASTPeriod).free;
    ntImportStmt:
        (node as TASTImport).free;
    ntGlobalStmt:
        (node as TASTGlobal).free;
    ntCreateList:
         (node as TASTCreateList).free;
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
     raise Exception.Create('Unrecognized node in freeAST: ' + inttostr (integer (node.nodeType)));
  end;
end;

// ---------------------------------------------------------------------------------------

function visitNode (node : TASTNode; level : integer; indent : string): string;
var pfx : string;
begin
  result := '';
  pfx := DupeString(indent, level);
  result := result + pfx;
  result := result + nodeTypeToName (node.nodeType);
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
     ntFunctionCall : result := 'function Call: ' + (node as TASTFunctionCall).symbolName;
     ntPrimary : result := 'symbol: ' + (node as TASTPrimary).primaryName;
     ntImportStmt : result := 'import: ' + (node as TASTImport).importName;
  else
     result := nodeTypeToName  (node.nodeType);
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
           for i := 0 to (node as TASTPrimary).nodes.Count - 1 do
               result := result + print ((node as TASTPrimary).nodes[i], prefix + '  ');
           end;
       ntPeriod :
          begin
          result := result + prefix + '  +-  ' + (node as TASTPeriod).name + sLineBreak;
          end;
       ntSubscript :
           begin
           for i := 0 to (node as TASTSubscript).subscripts.Count - 1 do
               result := result + print ((node as TASTSubscript).subscripts[i], prefix + '|  ');
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
          for i := 0 to (node as TASTGlobal).variableList.Count - 1 do
              result := result + prefix + '+- ' + 'global: ' + sLineBreak + print (((node as TASTGlobal).variableList[i] as TASTPrimary), prefix + '|  ');
          end;
       ntCreateList :
           begin
           for i := 0 to (node as TASTCreateList).nodeList.Count - 1 do
              result := result + print ((node as TASTCreateList).nodeList[i], prefix + '|  ');
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
          result := result + print ((node as TASTIterationBlock).assignment, prefix + '|  ');
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
          for i := 0 to (node as TASTPrint).argumentList.Count - 1 do
              result := result + print ((node as TASTPrint).argumentList[i], prefix + '|  ');
           end;
       ntPrintln :
          begin
          for i := 0 to (node as TASTPrintLn).argumentList.Count - 1 do
              result := result + print ((node as TASTPrintLn).argumentList[i], prefix + '|  ');
          end;
       ntFunction :
          begin
          if (node as TASTUserFunction).argumentList <> nil then
             for i := 0 to (node as TASTUserFunction).argumentList.Count - 1 do
                 result := result + print ((node as TASTUserFunction).argumentList[i], prefix + '|  ');
          result := result + print ((node as TASTUserFunction).body, prefix + '|  ');
          end;
       ntFunctionCall :
         begin
         for i := 0 to (node as TASTFunctionCall).argumentList.Count - 1 do
             result := result + print ((node as TASTFunctionCall).argumentList[i], prefix + '|  ');
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

end.


