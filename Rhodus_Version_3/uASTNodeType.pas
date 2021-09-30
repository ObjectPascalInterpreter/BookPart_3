unit uASTNodeType;


// This source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com


interface

type
   TASTNodeType = (
      ntError,
      ntFloat,
      ntInteger,
      ntString,
      ntBoolean,
      ntNodeList,
      ntAND,     // 6
      ntOR,
      ntNOT,
      ntXOR,
      ntUnaryMinus, // 10
      ntAdd,
      ntSub,
      ntMult,
      ntDiv,
      ntDivI,
      ntMod,
      ntPower,
      ntLT,
      ntLE,
      ntGT,
      ntGE,  // 21
      ntNE,
      ntEQ,
      ntLeftSide,
      ntExpression,
      ntSubscript,
      ntNode,
      ntPrimary,
      ntPeriod,
      ntImportStmt,
      ntGlobalStmt,   // 31
      ntModule,
      ntCreateList,
      ntPrint,
      ntPrintln,
      ntSetColor,
      ntAssertTrue,
      ntAssertTrueEx,
      ntAssertFalse,
      ntAssertFalseEx,
      ntHelp,
      ntSwitch,           // 42
      ntListOfCaseStatements,
      ntCaseStatement,
      ntIf,
      ntWhile,
      ntRepeat,
      ntFor,
      ntIter,
      ntTo,
      ntDownTo,         // 51
      ntFunction,       // 52
      ntFunctionArguments,
      ntReturn,
      ntAssignment,
      ntAssignmentModule,
      ntBreak,
      ntFunctionCall,
      ntStatementList,
      ntExpressionStatement
   );

function nodeTypeToName (op : TASTNodeType) : string;

implementation

Uses RTTI;

function nodeTypeToName (op : TASTNodeType) : string;
begin
  case op of
     ntAdd          : result := '+';
     ntSub          : result := '-';
     ntMult         : result := '*';
     ntDiv          : result := '/';
     ntPower        : result := '^';
     ntMod          : result := 'mod';
     ntDivI         : result := 'iDiv';
     ntUnaryMinus   : result := 'unaryMinus';
     ntNot          : result := 'not';
     ntAND          : result := 'and';
     ntOR           : result := 'or';
     ntXOR          : result := 'xor';
     ntLT           : result := '<';
     ntLE           : result := '<=';
     ntGT           : result := '>';
     ntGE           : result := '>=';
     ntNE           : result := '!=';
     ntEQ           : result := '==';
     ntExpression   : result := 'expression';
     ntExpressionStatement : result := 'expressionStatement';
     ntLeftSide     : result := 'leftSide';
     ntImportStmt   : result := 'import';
     ntPrimary      : result := 'primary';
     ntPeriod       : result := 'period';
     ntGlobalStmt   : result := 'globalStatement';
     ntModule       : result := 'module';
     ntSubscript    : result := 'subscript';
     ntNode         : result := 'node';
     ntCreateList   : result := 'list';
     ntPrint        : result := 'print';
     ntPrintln      : result := 'println';
     ntSetColor     : result := 'setColor';
     ntAssertTrue   : result := 'assertTrue';
     ntAssertTrueEx : result := 'assertTrueEx';
     ntAssertFalse  : result := 'assertFalse';
     ntAssertFalseEx: result := 'assertFalseEx';
     ntSwitch       : result := 'switch';
     ntListOfCaseStatements : result := 'listOfCaseStatements';
     ntCaseStatement : result := 'caseStatement';
     ntIf           : result := 'if';
     ntWhile        : result := 'while';
     ntRepeat       : result := 'repeat';
     ntFor          : result := 'for';
     ntIter         : result := 'iteration';
     ntTo           : result := 'to';
     ntDownTo       : result := 'downto';
     ntStatementList: result := 'statementList';
     ntBreak        : result := 'break';
     ntAssignment   : result := 'assignment';
     ntAssignmentModule : result := 'assignmentModule';
     ntReturn       : result := 'return';
     ntFunction     : result := 'function';
     ntFunctionCall : result := 'functionCall';
     ntFunctionArguments : result := 'functionArguments';
  else
      result := 'unknown';
  end;
end;


end.

