unit uParseLiteralArray;

interface

Uses SysUtils, Classes, uArrayObject;

implementation

procedure parseArray;
var arrayObject : TArrayObject;
begin
  arrayObject := TArrayObject.Create();

end;


// Scan in a matrix of the form: {{1,2,3},{4,5,6}} etc
procedure TControlBlock.ScanArray ();
begin
  s.NextToken;
  if s.Token = tLcBracket then
     begin
     Prog.AddOperator (wNewMatrix);
     s.NextToken;
     ScanVector;
     Prog.AddOperator (wAddMatrixItem);
     while s.Token = tComma do
           begin
           s.NextToken;
           if s.Token <> tLcBracket then
              raise EParseException.Create (CurrentModule, 'Expecting inner left curley bracket');
           s.NextToken;
           ScanVector;
           Prog.AddOperator (wAddMatrixItem);
           end;
     if s.Token <> tRcBracket then
        raise EParseException.Create (CurrentModule, 'Expecting outer right curley right bracket');
     s.NextToken;
     end
  else
     // Else it's just a vector
     ScanVector;
end;


// Scan a vector of the form '{' 1, 2, 3 '}'
// Note the first bracket has been scanned in already
procedure TControlBlock.ScanVector;
begin
  // Scan in Vector
  Prog.AddOperator (wNewVector);
  if s.Token = tRcBracket then
     begin
     s.NextToken;
     Exit;
     end;

  ParseSimpleExpression;
  Prog.AddOperator (wAddVectorItem);
  while s.Token = tComma do
        begin
        s.NextToken;
        ParseSimpleExpression;
        Prog.AddOperator (wAddVectorItem);
        end;

  if s.Token <> tRcBracket then
     raise EParseException.Create (CurrentModule, 'Curley right bracket ''}'' expected');
  s.NextToken;
end;


end.
