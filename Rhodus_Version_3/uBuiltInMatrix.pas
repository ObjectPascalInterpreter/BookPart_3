unit uBuiltInMatrix;

interface

Uses SysUtils, Classes, uLibModule, uArrayObject, uMachineStack;

type
  TBuiltInMatrix = class (TModuleLib)

    private
       procedure matrixGeneralMult (vm : TObject; m : TArrayObject; x : PMachineStackRecord);
       class function matMatMult (m1, m2 : TArrayObject) : TArrayObject;
    public

       procedure   getRndu(vm: TObject);
       procedure   getRndi (vm : TObject);
       procedure   getIdent (vm : TObject);
       procedure   getAdd (vm : TObject);
       procedure   getSub (vm : TObject);
       procedure   getMult (vm : TObject);
       procedure   getInverse (vm : TObject);

       constructor Create;
       destructor  Destroy; override;
  end;

implementation

Uses  Math,
      uMath,
      uVM,
      uRhodusTypes,
      uVMExceptions;


const outOfRangeMsg = 'Index out of range while accessing array element';
      sameDimensionsMsg = 'Arrays must have the same dimensions';


constructor TBuiltInMatrix.Create;
begin
  inherited Create ('matrix', 'Matrix Module, deals with 2-dimensional arrays');

  addMethod(getIdent,          1, 'ident', 'Create an identity matrix: m = matrix.ident (4)');
  addMethod(getMult,           2, 'mult',  'Multiply two 2D matrices: m = matrix.mult (m1, m2)');
  addMethod(getAdd,            2, 'add',   'Add two 2D matrices: m = matrix.add (m1, m2)');
  addMethod(getSub,            2, 'sub',   'Subtract two 2D matrices: m = matrix.sub (m1, m2)');
  addmethod(getInverse,        1, 'inv',   'Compute inverse of matrix: m = matrix.inv (m)');

  addMethod(getRndu,           2, 'rand',  'Create an array of uniformly random numbers: m = matrix.rand (4,4)');
  addMethod(getRndi,           4, 'randi', 'Create a matrix of uniformly random integers: m = matrix.randi (3, 2, lower, upper)');
end;


destructor TBuiltInMatrix.Destroy;
begin
  inherited;
end;

procedure TBuiltInMatrix.getRndu(vm: TObject);
var ar : TArrayObject;
    n, m : integer;
begin
   m := TVM (vm).popInteger;
   n := TVM (vm).popInteger;

   ar := TArrayObject.Create([n,m]);
   for var i := 0 to length (ar.data) - 1 do
       ar.data[i] := random ();
   TVM (vm).push (ar);
end;


procedure TBuiltInMatrix.getRndi (vm : TObject);
var upper, lower : integer;
    n, m : integer;
    ar : TArrayObject;
    i : integer;
begin
  m := TVM (vm).popInteger;
  n := TVM (vm).popInteger;
  upper := TVM (vm).popInteger;
  lower := TVM (vm).popInteger;
  ar := TArrayObject.Create ([n,m]);
  for i := 0 to (n*m) - 1 do
      begin
      ar.data[i] := RandomRange(lower, upper);
      end;
  TVM (vm).push (ar);
end;


procedure TBuiltInMatrix.getIdent (vm : TObject);
var n : integer;
    ar : TArrayObject;
begin
  n := TVM (vm).popInteger;
  ar := TArrayObject.Create([n,n]);
  for var i := 0 to n - 1 do
      ar.setValue2D (i, i, 1.0);
  TVM (vm).push (ar);
end;


function arrayArrayMult (m1, m2: TArrayObject) : TArrayObject;
begin
  if (m1.getNumDimensions() > 2) or (m1.getNumDimensions() > 2) then
      raise ERuntimeException.Create('Matrix multiplcation not supported beyond 2D');

  result := TArrayObject.Create ([m1.dim[0], m2.dim[1]]);
  if (m1.dim[1] = m2.dim[0]) then  // if cols = row?
     begin
   for var i := 0 to m1.dim[1] - 1 do
		     for var j := 0 to m2.dim[0] - 1 do
           begin
           for var k := 0 to m1.dim[1] - 1 do
					      result.setValue([i,j], result.getValue([i,j]) + m1.getValue([i,k]) * m2.getValue([k,j]));
					end;
		end
  else
     raise ERuntimeException.Create ('Incompatible matrix operands to multiply');
end;


procedure TBuiltInMatrix.getAdd (vm : TObject);
var m1, m2, ar : TArrayObject;
begin
  m2 := TVM (vm).popArray;
  m1 := TVM (vm).popArray;

  if (m1.getNumDimensions () <> 2) or (m2.getNumDimensions() <> 2) then
     raise ERuntimeException.Create ('Matrices must be 2D in function add');
  TVM (vm).push (TArrayObject.add(m1, m2));
end;

procedure TBuiltInMatrix.getSub (vm : TObject);
var m1, m2, ar : TArrayObject;
begin
  m2 := TVM (vm).popArray;
  m1 := TVM (vm).popArray;

  if (m1.getNumDimensions () <> 2) or (m2.getNumDimensions() <> 2) then
     raise ERuntimeException.Create ('Matrices must be 2D in function sub');
  TVM (vm).push (TArrayObject.sub(m1, m2));
end;


class function TBuiltInMatrix.matMatMult (m1, m2 : TArrayObject) : TArrayObject;
begin
  result := TArrayObject.Create ([m1.dim[0], m2.dim[1]]);
  if (m1.dim[1] = m2.dim[0]) then  // if cols = row?
     begin
	   for var i := 0 to m1.dim[1] - 1 do
		     for var j := 0 to m2.dim[0] - 1 do
           begin
           for var k := 0 to m1.dim[1] - 1 do
					      result.setValue([i,j], result.getValue([i,j]) + m1.getValue([i,k]) * m2.getValue([k,j]));
					end;
		end
  else
     raise ERuntimeException.Create ('Incompatible matrix operands to multiply');
end;


// Different from ArrayObject mult which does term by term multiplication
// This does a dot product
// It deals with all cases
procedure TBuiltInMatrix.matrixGeneralMult (vm : TObject; m : TArrayObject; x : PMachineStackRecord);
var nDim1, nDim2, n, i, j : integer;
    sum : double;
    ar : TArrayObject;
begin
   case x.stackType of
        stInteger : TVM (vm).push (TArrayObject.arrayIntMult (m, x.iValue));
        stDouble  : TVM (vm).push (TArrayObject.arrayDoubleMult (m, x.dValue));
        stArray   : begin
                    nDim1 := m.getNumDimensions();
                    nDim2 := x.aValue.getNumDimensions();
                    case nDim1 of
                       1 :
                          case nDim2 of
                            1 : begin
                                if m.dim[0] <> x.aValue.dim[0] then
                                   raise ERuntimeException.Create('Length of two arrays must be equal');
                                sum := 0;
                                for i := 0 to m.dim[0] - 1 do
                                     sum := sum + m.data[i]*x.aValue.data[i];
                                TVM (vm).push(sum);
                                end;
                            2 : begin
                                if m.dim[0] <> x.aValue.dim[0] then
                                 raise ERuntimeException.Create('Length of vector and row dimension of matrix must be equal');

                                n := x.aValue.dim[0];
                                ar := TArrayObject.Create ([n]);
	                              for i := 0 to m.dim[0] - 1 do
                                    begin
                                    sum := 0;
                                    for j := 0 to m.dim[0] - 1 do
                                        begin
                                        sum := sum + m.getValue([j]) * x.aValue.getValue([j,i]);
                                        end;
                                    ar.setValue([i], sum);
                                    end;
                                TVM (vm).push(ar);
                                end;
                          end;
                       2 :
                          case nDim2 of
                             1 : begin
                                 if m.dim[0] <> x.aValue.dim[0] then
                                   raise ERuntimeException.Create('Length of vector and row dimension of matrix must be equal');

                                 n := x.aValue.dim[0];
                                 ar := TArrayObject.Create ([n]);
	                               for i := 0 to m.dim[0] - 1 do
                                     begin
                                     sum := 0;
                                     for j := 0 to m.dim[0] - 1 do
                                         begin
                                         sum := sum + m.getValue([i,j]) * x.aValue.getValue([j]);
                                         end;
                     		 			       ar.setValue([i], sum);
                                     end;
                                 TVM (vm).push(ar);
                                 end;
                             2 : TVM (vm).push (matMatMult (m, x.aValue));
                          end;
                    else
                      raise ERuntimeException.Create('Error Message');
                    end;
        end
   else
      raise ERuntimeException.Create('Unsupported type in matrix multiply');
  end;
end;


procedure TBuiltInMatrix.getMult (vm : TObject);
var m1, m2 : PMachineStackRecord;
    ar : TArrayObject;
    sum : double;
begin
   m2 := TVM (vm).pop;
   m1 := TVM (vm).pop;

   case m1.stackType of
      stInteger :
         case m2.stackType of
             stArray : TVM (vm).push (TArrayObject.arrayIntMult (m2.aValue, m1.iValue));
         else
             raise ERuntimeException.Create('Unsupported type in matrix multiply');
         end;
      stDouble :
        case m2.stackType of
             stArray : TVM (vm).push (TArrayObject.arrayDoubleMult (m2.aValue, m1.dValue));
         else
             raise ERuntimeException.Create('Unsupported type in matrix multiply');
         end;
      stArray :
         matrixGeneralMult (vm, m1.aValue, m2);
    end;
end;

procedure TBuiltInMatrix.getInverse (vm : TObject);
var m, cpy : TArrayObject;
    det : double;
    n : integer;
begin
  m := TVM (vm).popArray;
  cpy := m.clone();
  n := m.dim[0];
  GaussJordan(cpy, 0, n-1, n-1, det);

  TVM (vm).push(cpy);
end;

end.
