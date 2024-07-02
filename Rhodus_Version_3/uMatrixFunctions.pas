unit uMatrixFunctions;

// This source is distributed under Apache 2.0

// Copyright (C)  2019-2024 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

Uses SysUtils, uMatrixObject, uVMExceptions;

procedure determinant (m : TMatrixObject; var det : double);
procedure LU (m : TMatrixObject; var L, U, P : TMatrixObject; var pindex : TArray<integer>);
procedure GaussJordan(A : TMatrixObject; Lb, Ub1, Ub2 : Integer);
procedure solve (A, b, x : TMatrixObject);

implementation

Uses uRhodusTypes;

const
   MachEp = 2.220446049250313E-16;

// det = det (P) * det (U)
procedure determinant (m : TMatrixObject; var det : double);
var i : integer;
    nr, nc : integer;
    L, U, P : TMatrixObject;
    pindex : TArray<integer>;
begin
  nr := m.numRows;
  nc := m.numCols;
  if nr <> nc then
     raise ERuntimeException.Create('Matrix must be square to compute the determinant (' + inttostr (nr) + ', ' + inttostr (nc) + ')');

  try
    uMatrixFunctions.LU (m, L, U, P, pindex);
    det := 1;
    for i := 0 to nr - 1 do
        det := det * U[i,i];

    for i := 0 to nr-1 do
        begin
        if pindex[i] <> i then
           det := det * -1
         end;
    setlength (pindex, 0);
  finally

  end;
end;


// This function is based on the code fom tpmath
// https://www.unilim.fr/pages_perso/jean.debord/tpmath/tpmath.htm
procedure LU (m : TMatrixObject; var L, U, P : TMatrixObject; var pindex : TArray<integer>);
  var
    i, j, k, Imax : Integer;
    Pvt, T, Sum : double;
    v : TArray<double>;
    nr, nc : integer;
    cm : TMatrixObject;
    Lb, Ub, apindex : integer;
    tmp : double;
begin
  cm := m.clone();

  nr := m.numRows;
  nc := m.numCols;
  if nr <> nc then
     raise ERuntimeException.Create('LU decomposition for a non-square matrix is not supported (' + inttostr (nr) + ', ' + inttostr (nc) + ')');

  Lb := 0; Ub := nr;
  setlength (v, Ub);
  setlength (pindex, Ub);

   for i := Lb to Ub - 1 do
      begin
        Pvt := 0.0;
        for j := Lb to Ub - 1do
          if abs(cm[i,j]) > Pvt then
            Pvt := abs(cm[i,j]);
        if Pvt < MachEp then
           raise ERuntimeException.Create('Matrix is singular or close to being singular (' + inttostr (nr) + ', ' + inttostr (nc) + ')');
        v[i] := 1.0 / Pvt;
      end;

   for j := Lb to Ub - 1 do
      begin
        for i := Lb to Pred(j) do
          begin
            Sum := cm[i,j];
            for k := Lb to Pred(i) do
              Sum := Sum - cm[i,k] * cm[k,j];
            cm[i,j] := Sum;
          end;
        Imax := 0;
        Pvt := 0.0;
        for i := j to Ub - 1 do
          begin
            Sum := cm[i,j];
            for k := Lb to Pred(j) do
              Sum := Sum - cm[I,K] * cm[K,J];
            cm[I,J] := Sum;
            T := v[I] * abs(Sum);
            if T > Pvt then
              begin
                Pvt := T;
                Imax := I;
              end;
          end;
        if j <> Imax then
          begin
            for k := Lb to Ub - 1 do
                begin
                // swap elements
                tmp := cm[Imax,k];
                cm[Imax,k] := cm[J,K];
                cm[J,K] := tmp;
                end;
            V[Imax] := V[J];
          end;
        pIndex[j] := Imax;
        if cm[j,j] = 0.0 then
          cm[J,J] := MachEp;
        if J <> Ub then
          begin
            T := 1.0 / cm[j,j];
            for i := Succ(j) to Ub - 1 do
              cm[i,j] := cm[i,j] * T;
          end;
      end;

   // Pull out the L and U matrix
  L := cm.clone;
  U := cm.clone;

  for i := 0 to nr - 1 do
      L[i,i] := 0;

  for i := 0 to nr - 1 do
      for j := i + 1 to j - 1 do
            L[i,j] := 0;
  for i := 0 to nr - 1 do
      L[i,i] := 1;

  for i := 1 to nr - 1 do
      for j := 0 to i - 1 do
            U[i,j] := 0;

  // Create the pivot matrix
  // The permutation vector needs to be interpreted in sequence.
  // If piv=[1,2,2] then the following needs to be done in
  // sequence (with zero-based indexing):

  // Row 0 changes with Row 1
  // The new Row 1 changes with Row 2 and
  // The new Row 2 stays the same.

  P := TMatrixObject.CreateIdent(nr);
  for i := 0 to nr - 1 do
      begin
      apindex := pIndex[i];
      P.swapRows(i, apindex)
      end;
  setlength (v, 0);
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
  ik, jk     : Integer;     { Pivot's row and column }
  i, j, k    : Integer;     { Loop variables }
  T          : Double;       { Temporary variable }
  PRow, PCol : TIndexArray;  { Stores pivot's row and column }
  MCol       : TDoubleArray;     { Stores a column of matrix A }
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


procedure solve (A, b, x : TMatrixObject);
var pindex : TArray<integer>;
    m, L, U, P, LUM : TMatrixObject;
    i, j, K : integer;
    Ub, Ip : integer;
    sum : double;
begin
  Ub := A.numRows;
  m := A.clone;
  LU (m, L, U, P, pindex);
  for i := 0 to Ub - 1 do
      L[i,i] := 0;

  LUM := TMatrixObject.add(L, U);

  if A.numRows > 1 then
     begin
     // Make sure b is a row form
      if b.numRows > 1 then
         b := TMatrixObject.transpose (b);
     end;

  for i := 0 to Ub - 1 do
      x[0,i] := b[0,i];

    K := Pred(0);
    for i := 0 to Ub - 1 do
      begin
        Ip := pIndex[i];
        sum := x[0,Ip];
        x[0,Ip] := x[0,i];
        if K >= 0 then
          for J := K to Pred(i) do
            sum := sum - LUM[i,j] * X[0,j]
        else if sum <> 0.0 then
          K := i;
        x[0,i] := sum;
      end;

    for i := Ub - 1 downto 0 do
      begin
        Sum := x[0,i];
        if i < Ub then
          for j := Succ(i) to Ub - 1 do
            sum := Sum - LUM[i,j] * X[0,j];
        X[0,i] := Sum / LUM[i,i];
      end;
end;

end.
