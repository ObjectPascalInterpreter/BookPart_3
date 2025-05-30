{
  Unit:    uBuiltInMatrix2.pas
  Author:  Herbert M sauro
  Date:    10/2021
  Purpose: This file implements the Matrix library that provide additional functions

  All the basic matri functions such as add, sub, and other manipulations are
  to be found in uMatrixObject.

  Ths source is distributed under Apache 2.0
  See https://www.apache.org/licenses/LICENSE-2.0.txt for further information

  Copyright (C)  2019-2024 Herbert M Sauro

  Author Contact Information:
  email: hsauro@gmail.com
}


unit uBuiltInMatrix;

interface

Uses SysUtils, Classes, uLibModule, uArrayObject, uMatrixObject, uMachineStack;

type
  TBuiltInMatrix = class (TModuleLib)

    private
    public
       //class function dotMatMatMult (m1, m2 : TArrayObject) : TArrayObject;
       //procedure   matrixGeneralMult (vm : TObject; m : TArrayObject; x : PMachineStackRecord);

       procedure   rndu(vm: TObject);
       procedure   rndi (vm : TObject);
       procedure   ident (vm : TObject);
       procedure   getCSV (vm : TObject);
       procedure   add (vm : TObject);
       procedure   sub (vm : TObject);
       procedure   inverse (vm : TObject);
       procedure   reducedechelon (vm : TObject);
       procedure   det (vm : TObject);
       procedure   transpose (vm : TObject);
       procedure   append (vm : TObject);
       procedure   LU (vm : TObject);
       procedure   QR (vm : TObject);
       procedure   solve (vm : TObject);
       procedure   createMatrix (vm : TObject);

       constructor Create;
       destructor  Destroy; override;
  end;

implementation

Uses  Math,
      uMath,
      uVM,
      uListObject,
      uStringObject,
      uRhodusTypes,
      uVMExceptions,
      uHelpUnit,
      uMatrixFunctions;


const outOfRangeMsg = 'Index out of range while accessing array element';
      sameDimensionsMsg = 'Arrays must have the same dimensions';

      MachEp = 2.220446049250313E-16;

constructor TBuiltInMatrix.Create;
begin
  inherited Create ('mat');

  addMethod(ident,          1, 'ident');
  addMethod(createMatrix,   2, 'matrix');
  addmethod(append,         1, 'append');
  addMethod(rndu,           2, 'rnd');
  addMethod(rndi,          -1, 'rndi');
  addMethod(getCSV,         1, 'csv');

  addmethod(inverse,        1, 'inv');
  addmethod(LU,             1, 'lu');
  addmethod(QR,             1, 'qr');
  addmethod(reducedechelon, 1, 'rref');
  addmethod(det,            1, 'det');
  addmethod(transpose,      1, 'tr');
  addmethod(solve,          2, 'solve');
end;


destructor TBuiltInMatrix.Destroy;
begin
  inherited;
end;


procedure TBuiltInMatrix.rndu(vm: TObject);
var mat : TMatrixObject;
    n, m : integer;
    i, j : integer;
begin
   m := TVM (vm).popInteger;
   n := TVM (vm).popInteger;

   mat := TMatrixObject.Create(n, m);
   for i := 0 to mat.numRows - 1 do
       for j := 0 to mat.numCols - 1 do
           mat.setval(i ,j, random ());
   TVM (vm).push (mat);
end;


procedure TBuiltInMatrix.rndi (vm : TObject);
var upper, lower : integer;
    n, m : integer;
    mat : TMatrixObject;
    i, j, nArgs : integer;
begin
  nArgs := TVM (vm).popInteger;
  case nArgs of
     0, 1 : raise ERuntimeException.Create('Insufficient arguments, need at least 2');

     2:
       begin
       lower := 1;
       upper := 10;
       m := TVM (vm).popInteger;
       n := TVM (vm).popInteger;
       end;
     3:
       begin
       upper := TVM (vm).popInteger;
       lower := 1;
       m := TVM (vm).popInteger;
       n := TVM (vm).popInteger;
       end;
     4:
       begin
       upper := TVM (vm).popInteger;
       lower := TVM (vm).popInteger;
       m := TVM (vm).popInteger;
       n := TVM (vm).popInteger;
       end;
     else
       raise ERuntimeException.Create('Too many arguments, number of arguments should be between 2 to 4');
  end;

  mat := TMatrixObject.Create (n, m);
  for i := 0 to mat.numRows - 1 do
      for j := 0 to mat.numCols - 1 do
          mat.setval(i ,j, RandomRange(lower, upper));

  TVM (vm).push (mat);
end;


// Identity matrix
procedure TBuiltInMatrix.ident (vm : TObject);
var n : integer;
    m : TMatrixObject;
begin
  n := TVM (vm).popInteger;
  m := TMatrixObject.CreateIdent(n);
  TVM (vm).push (m);
end;


procedure TBuiltInMatrix.getCSV (vm : TObject);
var m : TMatrixObject;
    result : TStringObject;
    astr : string;
    i, j : integer;
begin
  m := TVM (vm).popMatrix;
  astr := '';
  for i := 0 to m.numRows - 1 do
      begin
      astr := astr + floattostr (m[i,0]);
      for j := 1 to m.numCols - 1 do
          astr := astr + ',' + floattostr (m[i,j]);
      astr := astr + sLineBreak;
      end;


  result := TStringObject.Create(astr);
  TVM (vm).push (result);
end;


// Create an empty matrix of given size (m x n)
procedure TBuiltInMatrix.createMatrix (vm : TObject);
var nr, nc : integer;
begin
  nc := TVM (vm).popInteger;
  nr := TVM (vm).popInteger;

  TVM (vm).push (TMatrixObject.Create(nr, nc));
end;


procedure TBuiltInMatrix.transpose (vm : TObject);
var m1, m2 : TMatrixObject;
    r, c : integer;
    i, j : integer;
begin
  m1 := TVM (vm).popMatrix;

  r := m1.numRows;
  c := m1.numCols;

  m2 := TMatrixObject.Create (c, r);
  for i := 0 to r - 1 do
      for j := 0 to c - 1 do
          m2.setval(j,i, m1.getval(i,j));

  TVM (vm).push (m2);
end;


procedure TBuiltInMatrix.append (vm : TObject);
var m1, m2 : TMatrixObject;
    r, c : integer;
    i, j : integer;
begin
  m1 := TVM (vm).popMatrix;

  r := m1.numRows;
  c := m1.numCols;

//  m2 := TMatrixObject.Create (r, c+1);
//  for i := 0 to r - 1 do
//      for j := 0 to c - 1 do
//          m2.setval(j,i, m1.getval(i,j));

  TVM (vm).push (m1);
end;



// Not sure how relevant the add and sub are
// Can were intended to allow someone to do thngs like m.add (a)
procedure TBuiltInMatrix.add (vm : TObject);
var m1, m2 : TMatrixObject;
begin
  m2 := TVM (vm).popMatrix;
  m1 := TVM (vm).popMatrix;

  TVM (vm).push (TMatrixObject.add(m1, m2));
end;


procedure TBuiltInMatrix.sub (vm : TObject);
var m1, m2 : TMatrixObject;
begin
  m2 := TVM (vm).popMatrix;
  m1 := TVM (vm).popMatrix;

  TVM (vm).push (TMatrixObject.sub(m1, m2));
end;


//// This is used by the matrixGeneralMult below
//class function TBuiltInMatrix2.dotMatMatMult (m1, m2 : TArrayObject) : TArrayObject;
//var sum : double;
//begin
//  result := TArrayObject.Create ([m1.dim[0], m2.dim[1]]);
//  if (m1.dim[1] = m2.dim[0]) then  // if cols = row?
//     begin
//	   for var i := 0 to m1.dim[0] - 1 do
//		     for var j := 0 to m2.dim[1] - 1 do
//           begin
//           sum := 0;
//           for var k := 0 to m1.dim[1] - 1 do
//					      sum := sum + m1.getValue([i,k]) * m2.getValue([k,j]);
//           result.setValue([i,j], sum);
//					end;
//		end
//  else
//     raise ERuntimeException.Create ('Incompatible matrix operands to multiply');
//end;


// Different from ArrayObject mult which does term by term multiplication
// This does a dot product
// It deals with all cases
//procedure TBuiltInMatrix2.matrixGeneralMult (vm : TObject; m : TArrayObject; x : PMachineStackRecord);
//var nDim1, nDim2, n, i, j : integer;
//    sum : double;
//    ar : TArrayObject;
//begin
//   case x.stackType of
//        stInteger : TVM (vm).push (TArrayObject.arrayIntMult (m, x.iValue));
//        stDouble  : TVM (vm).push (TArrayObject.arrayDoubleMult (m, x.dValue));
//        stArray   : begin
//                    nDim1 := m.getNumDimensions();
//                    nDim2 := x.aValue.getNumDimensions();
//                    case nDim1 of
//                       1 :
//                          case nDim2 of
//                            1 : begin
//                                if m.dim[0] <> x.aValue.dim[0] then
//                                   raise ERuntimeException.Create('Length of two arrays must be equal');
//                                sum := 0;
//                                for i := 0 to m.dim[0] - 1 do
//                                     sum := sum + m.dataf[i]*x.aValue.dataf[i];
//                                TVM (vm).push(sum);
//                                end;
//                            2 : begin
//                                if m.dim[0] <> x.aValue.dim[0] then
//                                 raise ERuntimeException.Create('Length of vector and row dimension of matrix must be equal');
//
//                                n := x.aValue.dim[0];
//                                ar := TArrayObject.Create ([n]);
//	                              for i := 0 to m.dim[0] - 1 do
//                                    begin
//                                    sum := 0;
//                                    for j := 0 to m.dim[0] - 1 do
//                                        begin
//                                        sum := sum + m.getValue([j]) * x.aValue.getValue([j,i]);
//                                        end;
//                                    ar.setValue([i], sum);
//                                    end;
//                                TVM (vm).push(ar);
//                                end;
//                          end;
//                       2 :
//                          case nDim2 of
//                             1 : begin
//                                 if m.dim[0] <> x.aValue.dim[0] then
//                                   raise ERuntimeException.Create('Length of vector and row dimension of matrix must be equal');
//
//                                 n := x.aValue.dim[0];
//                                 ar := TArrayObject.Create ([n]);
//	                               for i := 0 to m.dim[0] - 1 do
//                                     begin
//                                     sum := 0;
//                                     for j := 0 to m.dim[0] - 1 do
//                                         begin
//                                         sum := sum + m.getValue([i,j]) * x.aValue.getValue([j]);
//                                         end;
//                     		 			       ar.setValue([i], sum);
//                                     end;
//                                 TVM (vm).push(ar);
//                                 end;
//                             2 : TVM (vm).push (dotMatMatMult (m, x.aValue));
//                          end;
//                    else
//                      raise ERuntimeException.Create('Error Message');
//                    end;
//        end
//   else
//      raise ERuntimeException.Create('Unsupported type in matrix multiply');
//  end;
//end;


procedure TBuiltInMatrix.inverse (vm : TObject);
var m, cpy : TMatrixObject;
    nr, nc : integer;
begin
  m := TVM (vm).popMatrix;
  cpy := m.clone() as TMatrixObject;

  nr := m.numRows;
  nc := m.numCols;
  if nr <> nc then
     raise ERuntimeException.Create('Matrix must be square to compute the inverse (' + inttostr (nr) + ', ' + inttostr (nc) + ')');

  GaussJordan(cpy, 0, nr-1, nr-1);

  TVM (vm).push(cpy);
end;


procedure TBuiltInMatrix.reducedechelon (vm : TObject);
var m, echelon : TMatrixObject;
begin
  m := TVM (vm).popMatrix;
  uMatrixFunctions.reducedRowEchelon (m, echelon);
  TVM (vm).push(echelon);
end;


procedure TBuiltInMatrix.LU (vm : TObject);
var
  m, L, U, P : TMatrixObject;
  res : TListObject;
  numSwaps : integer;
begin
  m := TVM (vm).popMatrix;

  if uMatrixFunctions.LU (m, L, U, P, numSwaps) then
     begin
     res := TListObject.Create (0);
     res.append(L);
     res.append(U);
     res.append(p);
     TVM (vm).push(res);
     end
  else
    raise ERuntimeException.Create('The matrix is singular');
end;


procedure TBuiltInMatrix.QR (vm : TObject);
var
  m, Q, R : TMatrixObject;
  res : TListObject;
begin
  m := TVM (vm).popMatrix;

  uMatrixFunctions.QRFactorization (m, Q, R);

  res := TListObject.Create (0);
  res.append(Q);
  res.append(R);
  TVM (vm).push(res);
end;


procedure TBuiltInMatrix.det (vm : TObject);
var m : TMatrixObject;
    det : double;
begin
  m := TVM (vm).popMatrix;

  determinant(m, det);

  TVM (vm).push(det);
end;


procedure  TBuiltInMatrix.solve (vm : TObject);
var A, b, x : TMatrixObject;
begin
  b := TVM (vm).popMatrix;
  A := TVM (vm).popMatrix;
  x := TMatrixObject.Create (1, A.numRows);

  uMatrixFunctions.solve (A, b, x);

  TVM (vm).push(x);
end;

end.
