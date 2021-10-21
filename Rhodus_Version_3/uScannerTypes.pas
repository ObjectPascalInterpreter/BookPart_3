unit uScannerTypes;

interface

type
 TTokenCode = (tIdentifier,
                tFloat,
                tInteger,
                tString,
                tPlus,
                tMinus,
                tMult,
                tDotproduct,
                tDivide,
                tPower,
                tDivI,
                tMod,
                tInc,
                tDec,
                tUnaryMinus,
                tLessThan,
                tLessThanOrEqual,
                tMoreThan,
                tMoreThanOrEqual,
                tNotEqual,
                tRightParenthesis,
                tLeftParenthesis,
                tLeftBracket,
                tRightBracket,
                tLeftCurleyBracket,
                tRightCurleyBracket,
                tEquals,
                tEquivalence,
                tApostrophy,
                tDollar,
                tSemicolon,
                tColon,
                tComma,
                tPeriod,
                tArrow,
                tAnd,
                tOr,
                tNot,
                tXor,
                tError,   // used when an error is detected
                tEnd,
                tEndofStream,

                tPrint,
                tPrintln,
                tSetColor,
                tReadString,
                tReadInteger,
                tAssertTrue,
                tAssertFalse,
                //tHelp,

                tIf,
                tThen,
                tElse,
                tFalse,
                tTrue,
                tFor,
                tDo,
                tTo,
                tDownTo,
                tStep,
                tWhile,
                tRepeat,
                tUntil,
                tBreak,
                tFunction,
                tRef,
                tGlobal,
                tSwitch,
                tCase,
                tImport,
                tReturn);

  TTokenSet = set of TTokenCode;

  TTokenRecord = record
                    lineNumber, columnNumber : integer;
                    FToken          : TTokenCode;
                    FTokenCharacter : Char;
                    FTokenString    : string;
                    FTokenFloat     : double;
                    FTokenInteger   : Integer;
                  end;


implementation

end.
