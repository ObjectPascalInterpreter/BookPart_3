unit uMatrixFunctions;

// This source is distributed under Apache 2.0

// Copyright (C)  2019-2024 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

Uses SysUtils, uMatrixObject, uVMExceptions;

procedure determinant (A : TMatrixObject; var det : double);
function LU (A : TMatrixObject; var L, U, P : TMatrixObject; var swaps: Integer) : boolean;
procedure GaussJordan(A : TMatrixObject; Lb, Ub1, Ub2 : Integer);
procedure solve (A, b, x : TMatrixObject);
procedure reducedRowEchelon (A : TMatrixObject; var echelon : TMatrixObject);
procedure QRFactorization(A: TMatrixObject; var Q, R: TMatrixObject);

implementation

Uses uRhodusTypes, Math;

const
   MachEp = 2.220446049250313E-16;

procedure determinant (A: TMatrixObject; var det : double);
var
  L, U, P: TMatrixObject;
  n, i: Integer;
  swaps: Integer;
begin
  n := A.numRows;

  // Perform LU decomposition
  if not LU (A, L, U, P, swaps) then
     raise ERuntimeException.Create ('LU decomposition failed. Matrix may be singular.');

  // Compute the product of diagonal elements of U
  det := 1;
  for i := 0 to n - 1 do
    det := det * U[i, i];

  // If odd number of swaps, negate the determinant
  if Odd(swaps) then
    det := -det;
end;


// Compute the LU factorizatin for matrix A. This only supports square matrices
function LU (A: TMatrixObject; var L, U, P: TMatrixObject; var swaps: Integer): Boolean;
var
  n, i, j, k, maxIndex: Integer;
  maxValue, temp: Double;
begin
  if A.numRows <> A.numCols then
     raise ERuntimeException.Create ('This implementation of LU decomposition only supports square matrices');
  n := A.numRows;
  L := TMatrixObject.Create(n, n);
  U := TMatrixObject.Create(n, n);
  P := TMatrixObject.Create(n, n);
  swaps := 0;

  // Initialize P as identity matrix
  for i := 0 to n - 1 do
  begin
    for j := 0 to n - 1 do
      P[i, j] := 0;
    P[i, i] := 1;
  end;

  // Copy A to U
  for i := 0 to n - 1 do
    for j := 0 to n - 1 do
      U[i, j] := A[i, j];

  // LU decomposition with partial pivoting
  for k := 0 to n - 1 do
  begin
    // Find pivot
    maxValue := Abs(U[k, k]);
    maxIndex := k;
    for i := k + 1 to n - 1 do
    begin
      if Abs(U[i, k]) > maxValue then
      begin
        maxValue := Abs(U[i, k]);
        maxIndex := i;
      end;
    end;

    // Swap rows if necessary
    if maxIndex <> k then
    begin
      Inc(swaps);
      for j := 0 to n - 1 do
      begin
        temp := U[k, j];
        U[k, j] := U[maxIndex, j];
        U[maxIndex, j] := temp;

        temp := P[k, j];
        P[k, j] := P[maxIndex, j];
        P[maxIndex, j] := temp;

        if j < k then
        begin
          temp := L[k, j];
          L[k, j] := L[maxIndex, j];
          L[maxIndex, j] := temp;
        end;
      end;
    end;

    // Check for singular matrix
    //if U[k, k] = 0 then
    //begin
    //  Result := False;
    //  Exit;
    //end;

    // Compute elements of L and update U
    for i := k + 1 to n - 1 do
    begin
      L[i, k] := U[i, k] / U[k, k];
      for j := k to n - 1 do
        U[i, j] := U[i, j] - L[i, k] * U[k, j];
    end;
  end;

  // Set diagonal of L to 1
  for i := 0 to n - 1 do
    L[i, i] := 1;

  Result := True;
end;



// Taken from TPMath Library
// https://www.unilim.fr/pages_perso/jean.debord/tpmath/tpmath.htm
// But with the determinant argument removed.

{ ------------------------------------------------------------------
  Transforms a matrix according to the Gauss-Jordan method
  ------------------------------------------------------------------
  Input parameters : A        = system matrix
                     Lb       = lower matrix bound in both dim.
                     Ub1, Ub2 = upper matrix bounds
  ------------------------------------------------------------------
  Output parameters: A   = transformed matrix
  ------------------------------------------------------------------
  Not Used:
  Possible results : MatOk     : No error
                     MatErrDim : Non-compatible dimensions
                     MatSing   : Singular matrix
  ------------------------------------------------------------------ }

procedure GaussJordan(A            : TMatrixObject;
                      Lb, Ub1, Ub2 : Integer);
var
  Pvt        : Double;       { Pivot }
  ik, jk     : Integer;      { Pivot's row and column }
  i, j, k    : Integer;      { Loop variables }
  T          : Double;       { Temporary variable }
  PRow, PCol : TIndexArray;  { Stores pivot's row and column }
  MCol       : TDoubleArray; { Stores a column of matrix A }
  tmp : double;

begin
  if Ub1 > Ub2 then
     raise ERuntimeException.Create('Dimensions are not compatible in inversion method');

  setLength (PRow, Ub1+1);
  setLength (PCol, Ub1+1);
  setLength (MCol, Ub1+1);

  k := Lb;
  while k <= Ub1 do
    begin
      // Search for largest pivot in submatrix A[K..Ub1, K..Ub1]
      Pvt := A[k,k];
      ik := k;
      jk := k;
      for i := k to Ub1 do
        for j := k to Ub1 do
          if Abs(A[i,j]) > Abs(Pvt) then
            begin
              Pvt := A[i,j];
              ik := i;
              jk := j;
            end;

      // Store pivot's position
      PRow[k] := ik;
      PCol[k] := jk;

      // Too weak pivot ==> quasi-singular matrix
      if Abs(Pvt) < MachEp then
         raise ERuntimeException.Create('Matrix is close to be singular, unable to find inverse');

      // Exchange current row (K) with pivot row (Ik)
      if ik <> k then
        for j := Lb to Ub2 do
          begin
          tmp := A[ik,j];
          A[ik,j] := A[k,j];
          A[k,j] := tmp;
          end;

      // Exchange current column (K) with pivot column (Jk)
      if jk <> k then
        for i := Lb to Ub1 do
           begin
           tmp := A[i,jk];
           A[i,jk] := A[i,k];
           A[i,k] := tmp;
           end;

      // Store column K of matrix A into MCol and set this column to zero
      for i := Lb to Ub1 do
        if i <> k then
          begin
            MCol[i] := A[i,k];
            A[i,k] := 0.0;
          end
        else
          begin
            MCol[i] := 0.0;
            A[i,k] := 1.0;
          end;

      // Transform pivot row
      T := 1.0 / Pvt;
      for j := Lb to Ub2 do
          A[k,j] := T * A[k,j];

      // Transform other rows
      for i := Lb to Ub1 do
        if i <> k then
          begin
            T := MCol[i];
            for j := Lb to Ub2 do
              A[i,j] := A[i,j] - T * A[k,j];
          end;

      Inc(k);
    end;

  // Exchange lines of inverse matrix
  for i := Ub1 downto Lb do
    begin
      ik := PCol[i];
      if Ik <> i then
        for j := Lb to Ub2 do
          begin
          tmp := A[i,j];
          A[i,j] := A[ik,j];
          A[ik,j] := tmp;
          end;
    end;

  // Exchange columns of inverse matrix
  for j := Ub1 downto Lb do
    begin
      jk := PRow[j];
      if jk <> j then
        for i := Lb to Ub1 do
          begin
          tmp := A[i,j];
          A[i,j] := A[i,jk];
          A[i,jk] := tmp;
          end;
    end;
end;


procedure swapRows(var Matrix: TMatrixObject; Row1, Row2: Integer);
var
  Temp: array of Double;
  i: Integer;
begin
  SetLength(Temp, length (Matrix.row[Row1]));
  for i := 0 to High(Matrix.row[Row1]) do
    Temp[i] := Matrix[Row1, i];

  for i := 0 to High(Matrix.row[Row1]) do
    Matrix[Row1, i] := Matrix[Row2, i];

  for i := 0 to High(Matrix.row[Row1]) do
    Matrix[Row2, i] := Temp[i];
end;


// Determine the reduced row echelon of a matrix A. Answer returned in echelon
procedure reducedRowEchelon (A : TMatrixObject; var echelon : TMatrixObject);
var
  Lead, nRows, nCols, i, j, k: Integer;
  Temp: Double;
begin
  nRows := A.numRows;
  nCols := A.numCols;
  Lead := 0;

  echelon := A.clone as TMatrixObject;;

  for i := 0 to nRows - 1 do
  begin
    if Lead >= nCols then
      Exit;
    j := i;
    while echelon[j, Lead] = 0 do
    begin
      Inc(j);
      if j = nRows then
      begin
        j := i;
        Inc(Lead);
        if nCols = Lead then
          Exit;
      end;
    end;

    SwapRows(echelon, i, j);

    Temp := echelon[i, Lead];
    for j := 0 to nCols - 1 do
      echelon[i, j] := echelon[i, j] / Temp;

    for j := 0 to nRows - 1 do
    begin
      if j <> i then
      begin
        Temp := echelon[j, Lead];
        for k := 0 to nCols - 1 do
          echelon[j, k] := echelon[j, k] - Temp * echelon[i, k];
      end;
    end;
    Inc(Lead);
  end;
end;


// Solve the system Ab = x.
// A is a matrix, b is a column vector (using {{},{}...}), x is a column vector
procedure solve (A, b, x : TMatrixObject);
var L, U, P : TMatrixObject;
    i, j : integer;
    Pb, y : TArray<double>;
    numSwaps, n : integer;
    sum : double;
begin
  if A.numRows > 1 then
     begin
     // Make sure b is in row form
      if b.numRows > 1 then
         b := TMatrixObject.transpose (b);
     end;

  LU(A, L, U, P, numSwaps);

  // Apply permutation matrix P to vector b
  n := b.numCols;
  SetLength(Pb, n);
  for i := 0 to n - 1 do
      begin
      Pb[i] := 0.0;
      for j := 0 to n - 1 do
        Pb[i] := Pb[i] + P[i, j] * b[0,j];
      end;

  // Solve Ly = Pb using forward substitution
  setlength (y, n);
  for i := 0 to n - 1 do
    begin
    sum := 0.0;
    for j := 0 to i - 1 do
       sum := sum + L[i, j] * y[j];
    y[i] := (Pb[i] - sum) / L[i, i];
    end;

  // Solve Ux = y using backward substitution
  for i := n - 1 downto 0 do
      begin
      sum := 0.0;
      for j := i + 1 to n - 1 do
         sum := sum + U[i, j] * x[0,j];
      x[0,i] := (y[i] - sum) / U[i, i];
      end;
end;



function DotProduct(const A, B: TRow): Extended;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(A) do
    Result := Result + A[i] * B[i];
end;


function VectorNorm(const V: TRow): Extended;
begin
  Result := Sqrt(DotProduct(V, V));
end;


function VectorSubtract(const v1, v2: TRow): TRow;
var
  i: Integer;
begin
  SetLength(Result, Length(v1));
  for i := 0 to High(v1) do
    Result[i] := v1[i] - v2[i];
end;


function ScalarMultiply(const v: TRow; scalar: Double): TRow;
var
  i: Integer;
begin
  SetLength(Result, Length(v));
  for i := 0 to High(v) do
    Result[i] := v[i] * scalar;
end;


function HouseholderReflection(const x: TRow): TRow;
var
  alpha, r: Double;
  e, u : TRow;
begin
  SetLength(e, Length(x));
  e[0] := 1.0;
  alpha := Norm(x);
  if x[0] > 0 then
    alpha := -alpha;
  u := VectorSubtract(x, ScalarMultiply(e, alpha));
  r := Norm(u);
  if r = 0 then
    Result := u
  else
    Result := ScalarMultiply(u, 1.0 / r);
end;

function ApplyHouseholder(const A: TMatrixObject; const v: TRow; k: Integer): TMatrixObject;
var
  i, j: Integer;
  beta: Double;
  w: TRow;
begin
  result := TMatrixObject.Create(A.numRows, A.numCols);
  for i := 0 to A.numRows - 1 do
    for j := 0 to A.numCols - 1 do
      Result[i,j] := A[i,j];

  beta := 2.0 / DotProduct(v, v);

  SetLength(w, A.numCols);
  for j := k to A.numCols - 1 do
  begin
    w[j] := 0.0;
    for i := k to A.numRows - 1 do
      w[j] := w[j] + v[i - k] * A[i,j];
    w[j] := beta * w[j];
  end;

  for i := k to A.numRows - 1 do
    for j := k to A.numCols - 1 do
      Result[i,j] := A[i,j] - v[i - k] * w[j];
end;


procedure QRFactorization(A: TMatrixObject; var Q, R: TMatrixObject);
var
  m, n, k, i, j: Integer;
  H : TMatrixObject;
  x, v: TRow;
begin
  m := A.numRows;
  n := A.numCols;
  R := TMatrixObject.Create(m, n);
  for i := 0 to m - 1 do
    for j := 0 to n - 1 do
      R[i,j] := A[i,j];

  Q := TMatrixObject.Create(m, m);
  for i := 0 to m - 1 do
    for j := 0 to m - 1 do
      if i = j then
        Q[i,j] := 1.0
      else
        Q[i,j] := 0.0;

  for k := 0 to Math.Min(m, n) - 1 do
  begin
    SetLength(x, m - k);
    for i := k to m - 1 do
      x[i - k] := R[i,k];

    v := HouseholderReflection(x);

    R := ApplyHouseholder(R, v, k);

    H := TMatrixObject.Create(m, m);
    for i := 0 to m - 1 do
      for j := 0 to m - 1 do
        if i = j then
          H[i,j] := 1.0
        else
          H[i,j] := 0.0;
    for i := k to m - 1 do
      for j := k to m - 1 do
        H[i,j] := H[i,j] - 2 * v[i - k] * v[j - k];

    Q := TMatrixObject.dotmult(Q, H);
  end;
end;


//function QRFactorization(const A: TMatrixObject; out Q, R: TMatrixObject): Boolean;
//var
//  m, n, i, j, k: Integer;
//  U, E: array of TRow;
//  NormU: Extended;
//begin
//  Result := False;
//  m := A.numRows;    // Number of rows
//  n := A.numCols; // Number of columns
//
//  // Ensure m >= n for the factorization to work
//  if m < n then
//    Exit;
//
//  SetLength(U, n);
//  SetLength(E, n);
//  Q := TMatrixObject.Create(m, n);
//  R := TMatrixObject.Create(n, n);
//
//  for i := 0 to n - 1 do
//  begin
//    SetLength(U[i], m);
//    SetLength(E[i], m);
//
//    // Copy the i-th column of A to U[i]
//    for j := 0 to m - 1 do
//      U[i,j] := A[j,i];
//
//    // Gram-Schmidt process
//    for k := 0 to i - 1 do
//    begin
//      R[k,i] := DotProduct(E[k], U[i]);
//      for j := 0 to m - 1 do
//        U[i,j] := U[i,j] - R[k,i] * E[k,j];
//    end;
//
//    NormU := VectorNorm(U[i]);
//    if NormU < 1e-10 then // Use a small threshold instead of exact zero
//      Exit;
//
//    R[i,i] := NormU;
//    for j := 0 to m - 1 do
//      E[i,j] := U[i,j] / NormU;
//
//    // Store the i-th column of Q
//    for j := 0 to m - 1 do
//      Q[j,i] := E[i,j];
//  end;
//
//  Result := True;
//end;




end.
