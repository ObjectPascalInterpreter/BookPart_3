unit uAST;

interface

Uses SysUtils, Classes, Generics.Collections, uASTNodeType;

type
   TASTNode = class;
   TChildNodes = TList<TASTNode>;
   TASTNode = class (TObject)
     private
        FChildNodes : TChildNodes;
        FType : TASTNodeType;
     public
        iValue : integer;
        dValue : double;
        sValue : string;
        identifier : string;
        constructor Create(Typ: TASTNodeType);
        destructor Destroy; override;

        //function addChild(Node: TSyntaxNode): TSyntaxNode; overload;
        //function addChild(Typ: TSyntaxNodeType): TSyntaxNode; overload;
        //class function createNode (op : TSyntaxNodeType; leftNode, rightNode : TSyntaxNode) : TSyntaxNode; overload;
        //class function createNode (op : TSyntaxNodeType; value : double) : TSyntaxNode; overload;
        //class function createNode (op : TSyntaxNodeType; sValue : string) : TSyntaxNode; overload;

        property nodeType: TASTNodeType read FType;
        property childNodes : TChildNodes read FChildNodes;

        constructor createLeaf (iValue : integer); overload;
        constructor createLeaf (dValue : double);  overload;
        constructor createLeaf (sValue : string); overload;
        class function  createNode (leftNode, rightNode : TASTNode; nodeType : TASTNodeType) : TASTNode;
        //class function  toString (node : TASTNode) : string;
        //class function  nodeToString (node : TASTNode) : string;
   end;

   function displayAST (root : TASTNode) : string;

implementation

Uses StrUtils;

// ------------------------------------------------------------------

constructor TASTNode.Create(Typ: TASTNodeType);
begin
  inherited Create;
  FChildNodes := TList<TASTNode>.Create;
  FType := Typ;
end;


destructor TASTNode.Destroy;
var
  i: integer;
begin
  for i  := 0 to FChildNodes.Count - 1 do
      FChildNodes[i].free;
  FChildNodes.free;
  inherited;
end;


constructor TASTNode.createLeaf (iValue : integer);
begin
  inherited Create;
  FType := ntInteger;
  self.iValue  := iValue;
end;


constructor TASTNode.createLeaf (dValue : double);
begin
  inherited Create;
  FType := ntFloat;
  self.dValue := dValue;
end;


constructor TASTNode.createLeaf (sValue : string);
begin
  inherited Create;
  FType := ntString;
  self.sValue := sValue;
end;


class function TASTNode.createNode (leftNode, rightNode : TASTNode; nodeType : TASTNodeType) : TASTNode;
begin
  result := TASTNode.Create (nodeType);
  result.FChildNodes.Add(leftNode);
  result.FChildNodes.Add(rightNode);
end;

// Recurse through a node, pretty-printing it.
function visitNode (node : TASTNode; level : integer; indent : string): string;
var pfx : string;
    child : TASTNode;
    count : integer;
begin
  result := '';
  pfx := DupeString(indent, level);
  result := result + pfx;
  result := result + nodeTypeToName (node.FType);
  result := result + '(';

  count := 0;
  if node.FChildNodes <> nil then
     begin
     for child in node.FChildNodes do
         begin
         //if count <> 0 then
         //   result := result + ',';
         result := result + sLineBreak;
         count := count + 1;
         result := result + visitNode(child, level+1, indent);
         //if count <> 0 then
            result := result + sLineBreak
         //write(pfx)
         end
     end
  else
     // No children
     result := result + inttostr (node.iValue);
  result := result + ')'

  //write(pfx)
  //write(repr(node))
 end;


function nodeToString (node : TASTNode) : string;
begin
  case node.FType  of
     ntInteger : result := inttostr (node.iValue);
     ntFloat   : result := floattostr (node.dValue)
  else
     result := nodeTypeToName  (node.FType);
  end;
end;


//function PrintPretty(indent : string, bool last) : string:
//begin
//       Console.Write(indent);
//       if (last)
//       {
//           Console.Write("\\-");
//           indent += "  ";
//       }
//       else
//       {
//           Console.Write("|-");
//           indent += "| ";
//       }
//       Console.WriteLine(Name);
//
//       for (int i = 0; i < Children.Count; i++)
//           Children[i].PrintPretty(indent, i == Children.Count - 1);
//end;


function printTree(node : TASTNode; indent : string; last : boolean) : string;
var i : integer;
begin
  result := result + indent + '+- ' + nodeTypeToName (node.FType) + sLineBreak;
  if last then
     indent := indent + '   '
  else
     indent := indent + '|  ';

  if node.FChildNodes <> nil then
     for i := 0 to node.FChildNodes.Count - 1 do
         begin
         if node.FChildNodes.Count - 1 = i then
            result := result + PrintTree(node.FChildNodes[i], indent, True)
         else
            result := result + PrintTree(node.FChildNodes[i], indent, False)
         end;
end;


function print(node: TASTNode; prefix : string) : string;
var i : integer;
begin
  result := prefix + '+- ' + nodeToString (node) + sLineBreak;
  if node.FChildNodes <> nil then
     for i := 0 to node.FChildNodes.Count - 1 do
         result := result + print(node.FChildNodes[i], prefix + '|  ');
end;


function displayAST (root : TASTNode) : string;
begin
  if root = nil then
     exit ('');
  //result := visitNode (root, 0, '  ')
  result := print (root, '');
end;



//function TSyntaxNode.addChild(Node: TSyntaxNode): TSyntaxNode;
//begin
//  setLength(FChildNodes, Length(FChildNodes) + 1);
//  FChildNodes[Length(FChildNodes) - 1] := Node;
//end;
//
//
//function TSyntaxNode.addChild(Typ: TSyntaxNodeType): TSyntaxNode;
//begin
//  result := AddChild(TSyntaxNode.Create(Typ));
//end;
//
//
//class function TSyntaxNode.createNode (op : TSyntaxNodeType; leftNode, rightNode : TSyntaxNode) : TSyntaxNode;
//begin
//  result := TSyntaxNode.Create(op);
//  result.AddChild(leftNode);
//  result.AddChild(rightNode);
//end;
//
//
//class function TSyntaxNode.createNode (op : TSyntaxNodeType; value : double) : TSyntaxNode;
//begin
//  result := TSyntaxNode.Create(op);
//  result.value := value;
//end;
//
//
//class function TSyntaxNode.createNode (op : TSyntaxNodeType; sValue : string) : TSyntaxNode;
//begin
//  result := TSyntaxNode.Create(op);
//  result.sValue := sValue;
//end;

end.


