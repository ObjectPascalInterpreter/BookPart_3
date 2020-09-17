unit uASTNodeType
;

interface

type
   TASTNodeType = (
      ntFloat,
      ntInteger,
      ntString,
      ntAND,
      ntOR,
      ntNOT,
      ntUnaryMinus,
      ntAdd,
      ntSub,
      ntMult,
      ntDiv,
      ntPower,
      ntIdentifier,
      ntAssign,
      ntFunction,
      ntStatements,   // List of statements
      ntArgumentNode
   );

function nodeTypeToName (op : TASTNodeType) : string;

implementation

Uses RTTI;

function nodeTypeToName (op : TASTNodeType) : string;
begin
  case op of
     ntAdd    : result := '+';
     ntSub    : result := '-';
     ntMult   : result := '*';
     ntDiv    : result := '/';
     ntPower  : result := '^';
     ntAssign : result := '=';
  end;
end;


end.

