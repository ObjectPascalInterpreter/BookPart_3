unit uSyntaxNodeType
;

interface

type
   TSyntaxNodeType = (
      ntScalar,
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

function nodeTypeToName (op : TSyntaxNodeType) : string;

implementation

Uses RTTI;

function nodeTypeToName (op : TSyntaxNodeType) : string;
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

