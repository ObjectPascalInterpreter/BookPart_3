unit uVMUnitTests;


interface

uses
  Classes, SysUtils, DUnitX.TestFramework, uVM;

type

  [TestFixture]
  TVM_UnitTests = class(TObject)
     vm : TVM;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]

    [TestCase('Case pushs','test1_push_String.asm,string')]
    procedure loadTestFileString (fileName : string; result : string);

    [TestCase('Case 1',  'test1_addii_Arithmetic.asm,7')]
    [TestCase('Case 2',  'test2_subii_Arithmetic.asm,-3')]
    [TestCase('Case 3',  'test3_multii_Arithmetic.asm,10')]
    [TestCase('Case 4',  'test4_divii_Arithmetic.asm,5')]
    [TestCase('Case 5',  'test5_mod_Arithmetic1.asm,3')]
    [TestCase('Case 6',  'test5_mod_Arithmetic2.asm,2')]
    [TestCase('Case 7',  'test5_mod_Arithmetic3.asm,0')]
    procedure loadTestFileArithmetic_ii (fileName : string; result : integer);

    [TestCase('Case 1',  'test1_adddd_Arithmetic.asm,8.3')]
    [TestCase('Case 2',  'test2_subdd_Arithmetic.asm,3.3')]
    [TestCase('Case 3',  'test3_multdd_Arithmetic.asm,10.036')]
    [TestCase('Case 4',  'test4_divdd_Arithmetic.asm,25')]
    procedure loadTestFileArithmetic_dd (fileName : string; result : double);

    [TestCase('Case 1',  'test1_Boolean.asm,true')]
    [TestCase('Case 2',  'test2_Boolean.asm,false')]

    [TestCase('Case 3',  'test3_not_1_Boolean.asm,true')]
    [TestCase('Case 3',  'test3_not_2_Boolean.asm,false')]

    [TestCase('Case 4',  'test4_and_Boolean.asm,true')]
    [TestCase('Case 5',  'test5_and_Boolean.asm,false')]
    [TestCase('Case 6',  'test6_and_Boolean.asm,false')]
    [TestCase('Case 7',  'test7_and_Boolean.asm,false')]

    [TestCase('Case 8',  'test8_or_Boolean.asm,false')]
    [TestCase('Case 9',  'test9_or_Boolean.asm,true')]
    [TestCase('Case 10', 'test10_or_Boolean.asm,true')]
    [TestCase('Case 11', 'test11_or_Boolean.asm,true')]

    [TestCase('Case 12', 'test12_xor_Boolean.asm,false')]
    [TestCase('Case 13', 'test13_xor_Boolean.asm,true')]
    [TestCase('Case 14', 'test14_xor_Boolean.asm,true')]
    [TestCase('Case 15', 'test15_xor_Boolean.asm,false')]

    [TestCase('Case 16', 'test16_isgt_Boolean.asm,true')]
    [TestCase('Case 17', 'test17_isgt_Boolean.asm,false')]
    [TestCase('Case 18', 'test18_isgte_Boolean.asm,false')]
    [TestCase('Case 19', 'test19_isgte_Boolean.asm,true')]
    [TestCase('Case 20', 'test20_isgte_Boolean.asm,true')]

    [TestCase('Case 21', 'test21_islt_Boolean.asm,false')]
    [TestCase('Case 22', 'test22_islt_Boolean.asm,true')]
    [TestCase('Case 23', 'test23_islte_Boolean.asm,true')]
    [TestCase('Case 24', 'test24_islte_Boolean.asm,true')]
    [TestCase('Case 25', 'test25_islte_Boolean.asm,false')]

    [TestCase('Case 26',  'test26_isEq_Boolean.asm,false')]
    [TestCase('Case 27',  'test27_isEq_Boolean.asm,true')]

    [TestCase('Case 28', 'test28_isNotEq_Boolean.asm,false')]
    [TestCase('Case 29', 'test29_isNotEq_Boolean.asm,true')]
    procedure loadTestFileConditionals (fileName : string; result : boolean);
   end;

implementation

Uses IOUtils, uSymbolTable, uAssembler, uMachineStack, uOpCodes;


procedure TVM_UnitTests.Setup;
begin
   vm := TVM.Create;
 end;


procedure TVM_UnitTests.TearDown;
begin
   vm.Free;
end;


procedure TVM_UnitTests.loadTestFileString (fileName : string; result : string);
var srcCode, parentDir, testDir : string;
    theProgram : TProgram;
    st : PMachineStackRecord;
    cwd : string;
begin
  parentDir := TDirectory.GetParent (TDirectory.GetCurrentDirectory());
  testDir :=  parentDir + '\\UnitTests\\String\\';
  if not FileExists (testDir + filename) then
     begin
       Assert.Fail ('Unable to locate file');
       exit;
       end;

    srcCode := TFile.ReadAllText(testDir + filename);
    theProgram := assembleCode(srcCode);
    vm.run (theProgram);
    st := vm.pop;
    if st.stackType <> stString then
       Assert.Fail('Expecting string result from: ' + fileName)
    else
       Assert.AreEqual(st.sValue.value, result);
end;


procedure TVM_UnitTests.loadTestFileArithmetic_ii (fileName : string; result : integer);
var srcCode, parentDir, testDir : string;
    theProgram : TProgram;
    st : PMachineStackRecord;
begin
  parentDir := TDirectory.GetParent (TDirectory.GetCurrentDirectory());
  testDir :=  parentDir + '\\UnitTests\\Arithmetic\\';
  if not FileExists (testDir + filename) then
     begin
       Assert.Fail ('Unable to locate file');
       exit;
       end;

    srcCode := TFile.ReadAllText(testDir + filename);
    theProgram := assembleCode(srcCode);
    vm.run (theProgram);
    st := vm.pop;
    if st.stackType <> stInteger then
       Assert.Fail('Expecting integer result from: ' + fileName)
    else
       Assert.AreEqual(st.iValue, result);
end;


procedure TVM_UnitTests.loadTestFileArithmetic_dd (fileName : string; result : double);
var srcCode, parentDir, testDir : string;
    theProgram : TProgram;
    st : PMachineStackRecord;
begin
  parentDir := TDirectory.GetParent (TDirectory.GetCurrentDirectory());
  testDir :=  parentDir + '\\UnitTests\\Arithmetic\\';
  if not FileExists (testDir + filename) then
     begin
       Assert.Fail ('Unable to locate file');
       exit;
       end;

    srcCode := TFile.ReadAllText(testDir + filename);
    theProgram := assembleCode(srcCode);
    vm.run (theProgram);
    st := vm.pop;
    if st.stackType <> stDouble then
       Assert.Fail('Expecting double result from: ' + fileName)
    else
       Assert.AreEqual(st.dValue, result);
end;


procedure TVM_UnitTests.loadTestFileConditionals (fileName : string; result : boolean);
var srcCode, parentDir, testDir : string;
    theProgram : TProgram;
    st : PMachineStackRecord;
begin
  parentDir := TDirectory.GetParent (TDirectory.GetCurrentDirectory());
  testDir :=  parentDir + '\\UnitTests\\Conditionals\\';
  if not FileExists (testDir + filename) then
     begin
       Assert.Fail ('Unable to locate file');
       exit;
       end;

    srcCode := TFile.ReadAllText(testDir + filename);
    theProgram := assembleCode(srcCode);
    vm.run (theProgram);
    st := vm.pop;
    if st.stackType <> stBoolean then
       Assert.Fail('Expecting boolean result from: ' + fileName)
    else
       Assert.AreEqual(st.bValue, result);
end;


initialization
  TDUnitX.RegisterTestFixture(TVM_UnitTests);
end.


// -------------------------------------------------------------------
// OLD Code

    //[TestCase('Case add','0,5')]
    //[TestCase('Case sub','0,-1')]
    //[TestCase('Case mult','2,6')]
    //[TestCase('Case divide','3,2')]
    //[TestCase('Case unary minus','4,-4')]
    //[TestCase('Case unary minus','5,4')]
    //[TestCase('Case power','6,64')]
    //procedure TestArithmetic(const index : integer; result : integer);


//    [TestCase('Case true','0,true')]
//    [TestCase('Case false','1,false')]
//    [TestCase('Case not','2,false')]
//    [TestCase('Case and t/f','3,false')]
//    [TestCase('Case and t/t','4,true')]
//    [TestCase('Case or, f/t','5,true')]
//    [TestCase('Case xor, t/f','6,true')]
//    procedure TestBoolean(const index : integer; result : boolean);

//    [TestCase('Case IsGt','0,false')]
//    [TestCase('Case IsLt','1,true')]
//    [TestCase('Case IsEq','2,false')]
//    [TestCase('Case IsEq','3,true')]
//    [TestCase('Case IsLE','4,true')]
//    [TestCase('Case IsNotEq','5,true')]
//    procedure TestCondtionals(const index : integer; result : boolean);

//  addProgram (testArithmetic_tests, TProgram.Create (
//              createByteCode (oPushi, 2),
//              createByteCode (oPushi, 3),
//              createByteCode (oAdd),
//              createByteCode (oPop),
//              createByteCode (oHalt)));
//
//   // Arithmetic tests
//    addProgram (testArithmetic_tests, TProgram.Create (
//              createByteCode (oPushi, 2),
//              createByteCode (oPushi, 3),
//              createByteCode (oSub),
//              createByteCode (oPop),
//              createByteCode (oHalt)));
//
//   addProgram (testArithmetic_tests, TProgram.Create (
//              createByteCode (oPushi, 2),
//              createByteCode (oPushi, 3),
//              createByteCode (oMult),
//              createByteCode (oPop),
//              createByteCode (oHalt)));
//
//   addProgram (testArithmetic_tests, TProgram.Create (
//              createByteCode (oPushi, 8),
//              createByteCode (oPushi, 4),
//              createByteCode (oDivide),
//              createByteCode (oPop),
//              createByteCode (oHalt)));
//
//   addProgram (testArithmetic_tests, TProgram.Create (
//              createByteCode (oPushi, 4),
//              createByteCode (oUmi),
//              createByteCode (oPop),
//              createByteCode (oHalt)));
//
//  addProgram (testArithmetic_tests, TProgram.Create (
//              createByteCode (oPushi, -4),
//              createByteCode (oUmi),
//              createByteCode (oPop),
//              createByteCode (oHalt)));
//
//  addProgram (testArithmetic_tests, TProgram.Create (
//              createByteCode (oPushi, 4),
//              createByteCode (oPushi, 3),
//              createByteCode (oPower),
//              createByteCode (oPop),
//              createByteCode (oHalt)));


//    // Boolean tests
//    addProgram (testBoolean_tests, TProgram.Create(
//             createByteCode (oPushb, true),
//             createByteCode (oPop),
//             createByteCode (oHalt)));
//
//   addProgram (testBoolean_tests, TProgram.Create(
//             createByteCode (oPushb, false),
//             createByteCode (oPop),
//             createByteCode (oHalt)));
//
//   addProgram (testBoolean_tests, TProgram.Create(
//             createByteCode (oPushb, true),
//             createByteCode (oNot),
//             createByteCode (oPop),
//             createByteCode (oHalt)));
//
//   addProgram (testBoolean_tests, TProgram.Create(
//             createByteCode (oPushb, false),
//             createByteCode (oPushb, true),
//             createByteCode (oAnd),
//             createByteCode (oPop),
//             createByteCode (oHalt)));
//
//   addProgram (testBoolean_tests, TProgram.Create(
//             createByteCode (oPushb, true),
//             createByteCode (oPushb, true),
//             createByteCode (oAnd),
//             createByteCode (oPop),
//             createByteCode (oHalt)));
//
//   addProgram (testBoolean_tests, TProgram.Create(
//             createByteCode (oPushb, false),
//             createByteCode (oPushb, true),
//             createByteCode (oOr),
//             createByteCode (oPop),
//             createByteCode (oHalt)));
//
//   addProgram (testBoolean_tests, TProgram.Create(
//             createByteCode (oPushb, true),
//             createByteCode (oPushb, false),
//             createByteCode (oXor),
//             createByteCode (oPop),
//             createByteCode (oHalt)));
//
//   // Conditionls
//   addProgram (testConditionals_tests, TProgram.Create(
//             createByteCode (oPushi, 4),
//             createByteCode (oPushi, 6),
//             createByteCode (oIsGT),
//             createByteCode (oPop),
//             createByteCode (oHalt)));
//
//   addProgram (testConditionals_tests, TProgram.Create(
//             createByteCode (oPushi, 4),
//             createByteCode (oPushi, 6),
//             createByteCode (oIsLT),
//             createByteCode (oPop),
//             createByteCode (oHalt)));
//
//   addProgram (testConditionals_tests, TProgram.Create(
//             createByteCode (oPushi, 4),
//             createByteCode (oPushi, 6),
//             createByteCode (oIsEq),
//             createByteCode (oPop),
//             createByteCode (oHalt)));
//
//   addProgram (testConditionals_tests, TProgram.Create(
//             createByteCode (oPushi, 6),
//             createByteCode (oPushi, 6),
//             createByteCode (oIsEq),
//             createByteCode (oPop),
//             createByteCode (oHalt)));
//
//   addProgram (testConditionals_tests, TProgram.Create(
//             createByteCode (oPushi, 6),
//             createByteCode (oPushi, 6),
//             createByteCode (oIsGte),
//             createByteCode (oPop),
//             createByteCode (oHalt)));
//
//   addProgram (testConditionals_tests, TProgram.Create(
//             createByteCode (oPushi, 6),
//             createByteCode (oPushi, 6),
//             createByteCode (oIsLte),
//             createByteCode (oPop),
//             createByteCode (oHalt)));
//
//   addProgram (testConditionals_tests, TProgram.Create(
//             createByteCode (oPushi, 2),
//             createByteCode (oPushi, 4),
//             createByteCode (oIsNotEq),
//             createByteCode (oPop),
//             createByteCode (oHalt)));
//
//procedure TVM_UnitTests.TestArithmetic (const index : integer; result : integer);
//var st : PMachineStackType;
//begin
//  st := vm.eval (testArithmetic_tests[index]);
//  if st.stackType = stInteger then
//     begin
//     Assert.AreEqual(st.iValue, result);
//     exit;
//     end;
//  if st.stackType = stDouble then
//     begin
//     Assert.AreEqual(integer (trunc (st.dValue)), result);
//     exit;
//     end;
//  Assert.Fail('Failed test ' + inttostr (index));
//end;


//procedure TVM_UnitTests.TestBoolean(const index : integer; result : boolean);
//var st : PMachineStackType;
//begin
//  st := vm.eval (testBoolean_tests[index]);
//  Assert.AreEqual(st.bValue, result);
//end;


//procedure TVM_UnitTests.TestCondtionals(const index : integer; result : boolean);
//var st : PMachineStackType;
//begin
//  st := vm.eval (testConditionals_tests[index]);
//  Assert.AreEqual(st.bValue, result);
//end;


