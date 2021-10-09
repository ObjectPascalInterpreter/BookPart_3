unit uTokenVector;

interface

Uses SysUtils, uScannerTypes;

type
  TVectorTokenMode = (vtReading, vtSaving);
  TTokenVector = class (TObject)
      const
          ALLOC_BY = 512;
      private
          actualLength : integer;
          ptr : integer;
          procedure checkSpace;
          function getTokenCode : TTokenCode;
          function getTokenString : string;
          function getTokenInteger : integer;
          function getTokenFloat : double;
      public
          mode : TVectorTokenMode;

          vector :  TArray<TTokenRecord>;

          tokenRecord : TTokenRecord;

          procedure   compactCode;
          class function  tokenToString (token : TTokenCode) : string;
          function    getScalar : double;
          procedure   append (token : TTokenRecord);
          function    count : integer;
          procedure   reset;
          function    nextToken : TTokenRecord;
          procedure   clearCode;

          property  token : TTokenCode read getTokenCode;
          property  tokenString : string read getTokenString;
          property  tokenFloat : double read getTokenFloat;
          property  tokenInteger : integer read getTokenInteger;

          constructor Create;
          destructor  Destroy; override;
  end;


implementation


constructor TTokenVector.Create;
begin
  inherited;
  actualLength := 0;
  setLength (vector, ALLOC_BY);
  mode := vtSaving;
end;


destructor TTokenVector.Destroy;
begin
  inherited;
end;

procedure TTokenVector.reset;
begin
  ptr := 0;
end;


function TTokenVector.getTokenCode : TTokenCode;
begin
  result := tokenRecord.FToken;
end;


function TTokenVector.getTokenInteger : integer;
begin
  result := tokenRecord.FTokenInteger;
end;


function TTokenVector.getTokenFloat : double;
begin
  result := tokenRecord.FTokenFloat;
end;


function TTokenVector.getTokenString : string;
begin
  result := tokenRecord.FTokenString;
end;


function TTokenVector.getScalar : double;
begin
  result := 0.0;
  if (tokenRecord.FToken = tInteger) then
     result := tokenRecord.FTokenInteger;
  if (tokenRecord.FToken = tFloat) then
     result := tokenRecord.FTokenFloat;
end;


function TTokenVector.nextToken : TTokenRecord;
begin
  if mode = vtSaving then
     begin
     result := vector[actualLength];
     tokenRecord := result;
     end
  else
     begin
     result := vector[ptr];
     tokenRecord := result;
     inc (ptr);
     end;
end;


procedure TTokenVector.clearCode;
begin
  actualLength := 0;
  setLength (vector, 0);
end;


procedure TTokenVector.checkSpace;
begin
  if actualLength = length(vector) then
     setLength(vector, length(vector) + ALLOC_BY);
end;


procedure TTokenVector.append (token : TTokenRecord);
begin
  checkSpace;
  vector[actualLength] := token;
  inc(actualLength);
  tokenRecord := token;
end;


procedure TTokenVector.compactCode;
begin
  setLength (vector, actualLength);
end;


function TTokenVector.count : integer;
begin
  result := actualLength;
end;


class function TTokenVector.tokenToString (token : TTokenCode) : string;
begin
  case token of
        tIdentifier   : result := 'identifier';// <' + tokenElement.FTokenString + '>';
        tInteger      : result := 'integer';// <' + inttostr (tokenElement.FTokenInteger) + '>';
        tFloat        : result := 'float';// + floattostr (tokenElement.FTokenFloat) + '>';
        tString       : result := 'string "';// + tokenElement.FTokenString + '"';
        tMinus        : result := 'character: ''-''';
        tPlus         : result := 'character: ''+''';
        tMult         : result := 'character: ''*''';
        tDivide       : result := 'character: ''/''';
        tPower        : result := 'character: ''^''';
  tRightParenthesis   : result := 'character: '')''';
  tLeftParenthesis    : result := 'character: ''(''';
  tRightBracket       : result := 'character: '']''';
  tLeftBracket        : result := 'character: ''[''';
  tLeftCurleyBracket  : result := 'character: ''{''';
  tRightCurleyBracket : result := 'character: ''}''';
        tEquals       : result := 'character: ''=''';
        tEquivalence  : result := 'spcharacterecial: ''==''';
        tMoreThan     : result := 'character: ''>''';
        tLessThan     : result := 'character: ''<''';
  tMoreThanOrEqual    : result := 'character: ''>=''';
  tLessThanOrEqual    : result := 'character: ''<=''';
        tApostrophy   : result := 'character: Apostrphy';
        tSemicolon    : result := 'character: '';''';
        tColon        : result := 'character: '':''';
        tComma        : result := 'character: '',''';
        tPeriod       : result := 'character: ''.''';
        tDollar       : result := 'character: ''$''';
        tArrow        : result := 'character: ''->''';
        tEnd          : result := 'key word: <end>';
        tIf           : result := 'key word: <if>';
        tThen         : result := 'key word: <then> ';
        tFor          : Result := 'Key word: <for>';
        tTo           : result := 'key word: <to>';
        tDownTo       : result := 'key word: <downto>';
        tWhile        : result := 'key word: <while>';
        tDo           : result := 'key word: <do>';
        tElse         : result := 'key word: <else>';
        tRepeat       : result := 'key word: <repeat>';
        tUntil        : result := 'key word: <until>';
        tCase         : result := 'key word: <case>';
        tSwitch       : result := 'key word: <switch>';
        tReturn       : result := 'key word: <return>';
        tImport       : result := 'key word: <import>';
        tGlobal       : result := 'key word: <global>';
        tFunction     : result := 'key word: <function>';
        tBreak        : result := 'key word: <break>';
        tStep         : result := 'key word: <step>';
        tFalse        : result := 'False';
        tTrue         : result := 'True';
        tPrint        : result := 'print';
        tPrintln      : result := 'println';

        tEndofStream : result := 'End of Stream';
  else
       result := 'unrecognised token in tokenToString: ' + inttostr (integer(token));
  end;
end;

end.
