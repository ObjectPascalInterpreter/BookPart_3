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
      ntPrimaryOld,
      ntPeriod,
      ntImportStmt,
      ntGlobalStmt,   // 32
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
      ntSwitch,           // 43
      ntListOfCaseStatements,
      ntCaseStatement,
      ntIf,
      ntWhile,
      ntRepeat,
      ntFor,
      ntIter,
      ntTo,
      ntDownTo,         // 52
      ntFunction,       // 53
      ntFunctionArguments,
      ntReturn,
      ntAssignment,
      ntAssignmentModule,
      ntBreak,
      ntFunctionCall,
      ntStatementList,
      ntExpressionStatement,
      ntIdentifier,
      ntPrimary,
      ntPrimaryPeriod,
      ntPrimaryIndex,
      ntPrimaryFunction,
      ntNull
   );

function nodeTypeToString (op : TASTNodeType) : string;

implementation

Uses RTTI;

function nodeTypeToString (op : TASTNodeType) : string;
begin
  result := TRttiEnumerationType.GetName(op);
end;


end.

