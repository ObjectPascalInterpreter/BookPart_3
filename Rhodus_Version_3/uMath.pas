unit uMath;

{ ******************************************************************
  Solution of a system of linear equations by Gauss-Jordan method
  ****************************************************************** }

// Taken from TPMath Library
// https://www.unilim.fr/pages_perso/jean.debord/tpmath/tpmath.htm

interface

uses
  uArrayObject, uRhodusTypes, uVMExceptions;

procedure GaussJordan(A            : TArrayObject;
                      Lb, Ub1, Ub2 : Integer;
                      var Det      : Double);
{ ------------------------------------------------------------------
  Transforms a matrix according to the Gauss-Jordan method
  ------------------------------------------------------------------
  Input parameters : A        = system matrix
                     Lb       = lower matrix bound in both dim.
                     Ub1, Ub2 = upper matrix bounds
  ------------------------------------------------------------------
  Output parameters: A   = transformed matrix
                     Det = determinant of A
  ------------------------------------------------------------------
  Not Used:
  Possible results : MatOk     : No error
                     MatErrDim : Non-compatible dimensions
                     MatSing   : Singular matrix
  ------------------------------------------------------------------ }

implementation

Uses uVm;

// Double precision epsilon
const MachEp = 2.220446049250313e-16;

procedure FSwap(var X, Y : Double);
var
  Temp : Double;
begin
  Temp := X;
  X := Y;
  Y := Temp;
end;

procedure GaussJordan(A            : TArrayObject;
                      Lb, Ub1, Ub2 : Integer;
                      var Det      : double);
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

  Det := 1.0;

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

      // Update determinant
      Det := Det * Pvt;
      if ik <> k then Det := - Det;
      if jk <> k then Det := - Det;

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

//procedure matrixMult (vm : TObject; m1, m2 : TArrayObject);
//var nDim1, nDim2 :integer;
//    i, j,  n : integer;
//    sum : double;
//    ar : TArrayObject;
//begin
//  nDim1 := m1.getNumDimensions();
//  nDim2 := m2.getNumDimensions();
//  case nDim1 of
//     1 :
//       case nDim2 of
//          1 : begin
//              if m1.dim[0] <> m2.dim[0] then
//                 raise ERuntimeException.Create('Length of two arrays must be equal');
//              sum := 0;
//              for i := 0 to m1.dim[0] - 1 do
//                 sum := sum + m1.data[i]*m2.data[i];
//              TVM (vm).push(sum);
//              end;
//          2 : begin
//              if m1.dim[0] <> m2.dim[0] then
//                 raise ERuntimeException.Create('Length of vector and row dimension of matrix must be equal');
//
//              n := m2.dim[0];
//              ar := TArrayObject.Create ([n]);
//              for i := 0 to m1.dim[0] - 1 do
//                 begin
//                 sum := 0;
//                 for j := 0 to m1.dim[0] - 1 do
//                     begin
//                     sum := sum + m1.getValue([j]) * m2.getValue([j,i]);
//                     end;
//                     ar.setValue([i], sum);
//                     end;
//                 TVM (vm).push(ar);
//                 end;
//              end;
//
//     2 :
//       case nDim2 of
//          1 : begin
//              if m1.dim[0] <> x.aValue.dim[0] then
//                 raise ERuntimeException.Create('Length of vector and row dimension of matrix must be equal');
//
//              n := m2.dim[0];
//              ar := TArrayObject.Create ([n]);
//              for i := 0 to m1.dim[0] - 1 do
//                  begin
//                  sum := 0;
//                  for j := 0 to m1.dim[0] - 1 do
//                      begin
//                      sum := sum + m1.getValue([i,j]) * m2.getValue([j]);
//                      end;
//                  ar.setValue([i], sum);
//                  end;
//              TVM (vm).push(ar);
//              end;
//          2 : TVM (vm).push (matMatMult (m1, x.aValue));
//          end;
//       else
//          raise ERuntimeException.Create('Error Message');
//  end;
//end;



end.

