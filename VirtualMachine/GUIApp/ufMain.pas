unit ufMain;

{$DEFINE STACKCHECK}


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, uMachineStack, uModule;

type
  TfrmMain = class(TForm)
    Panel1: TPanel;
    plkBottom: TPanel;
    pnkMain: TPanel;
    btnClose: TButton;
    Panel2: TPanel;
    mnoCode: TMemo;
    btnAssemble: TButton;
    mnoOutput: TMemo;
    btnLoad: TButton;
    OpenDialog: TOpenDialog;
    lblFileName: TLabel;
    Label1: TLabel;
    lblTimeTaken: TLabel;
    Label2: TLabel;
    edtTimesToRun: TEdit;
    btnSaveData: TButton;
    SaveDialog: TSaveDialog;
    Label3: TLabel;
    procedure btnCloseClick(Sender: TObject);
    procedure btnAssembleClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveDataClick(Sender: TObject);
  private
    { Private declarations }
    procedure printValue (value : PMachineStackRecord);
    procedure printlnValue (value : PMachineStackRecord);
    procedure addUserTestFunctions  (module : TModule);
  public
    { Public declarations }
    collectedTimes : TArray<double>;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

Uses uAssembler, uSymbolTable, uVM, uListObject, IOUtils, uOpCodes;

procedure TfrmMain.printlnValue (value : PMachineStackRecord);
begin
  mnoOutput.Lines.Append ('');
 end;


procedure TfrmMain.addUserTestFunctions (module : TModule);
var f : TUserFunction;
begin
  f := TUserFunction.Create ('sayHello');
  f.nArgs := 1;
  f.funcCode.addByteCode(oPushs, 'Hello');
  f.funcCode.addByteCode(oPushi, 1);
  f.funcCode.addByteCode(oPrint);
  f.funcCode.addByteCode(oPushNone);
  f.funcCode.addByteCode(oRet);
  module.symbolTable.addSymbol(f);

  f := TUserFunction.Create ('myAdd');
  f.nArgs := 1;
  f.funcCode.addByteCode(oLoadLocal, 0);
  f.funcCode.addByteCode(oPushi, 2);
  f.funcCode.addByteCode(oAdd);
  f.funcCode.addByteCode(oPushi, 1);
  f.funcCode.addByteCode(oPrint);
  f.funcCode.addByteCode(oPushNone);
  f.funcCode.addByteCode(oRet);
  module.symbolTable.addSymbol(f);
end;

procedure TfrmMain.printValue (value : PMachineStackRecord);
begin
  case value.stackType of
      stInteger: mnoOutput.Lines[mnoOutput.Lines.Count-1] := mnoOutput.Lines[mnoOutput.Lines.Count-1] + inttostr (value.iValue);
      stDouble : mnoOutput.Lines[mnoOutput.Lines.Count-1] := mnoOutput.Lines[mnoOutput.Lines.Count-1] + floattostr (value.dValue);
      stString : mnoOutput.Lines[mnoOutput.Lines.Count-1] := mnoOutput.Lines[mnoOutput.Lines.Count-1] + value.sValue.value;
      stList   : mnoOutput.Lines[mnoOutput.Lines.Count-1] := mnoOutput.Lines[mnoOutput.Lines.Count-1] + value.lValue.listToString;
  else
      raise Exception.Create('Unrecognised data type in printValue');
  end;
end;


procedure TfrmMain.btnAssembleClick(Sender: TObject);
var theProgram : TProgram;
    vm : TVM;
    st : PMachineStackRecord;
    mainModule : TModule;
    timesToRun, i : integer;
    runningTotal : double;
begin
  timesToRun := strtoint (edtTimesToRun.Text);
  mainModule := TModule.Create ('_main_');
  addUserTestFunctions (mainModule);

  mnoOutput.Clear;
  mainModule.symbolTable.addSymbol('a');
  theProgram := assembleCode(mnoCode.text);
  mainModule.code := theProgram;
  vm := TVM.Create;
  vm.registerPrintCallBack(printValue);
  vm.registerPrintlnCallBack(printlnValue);
  try
    setLength (collectedTimes, timesToRun);
    screen.Cursor := crHourGlass;
    try
      runningTotal := 0;
      for i := 0 to timesToRun - 1 do
          begin
          vm_elapsedTime := 0;
          vm.runModule (mainModule);
          if vm.stackHasEntry then
             st := vm.pop
          else
             st := nil;
          runningTotal := runningTotal + vm_elapsedTime;
          collectedTimes[i] := vm_elapsedTime;
          end;
    finally
       screen.Cursor := crDefault;
    end;

    lblTimeTaken.Caption := floattostr (runningTotal/timesToRun);
    if st <> nil then
       case  st.stackType of
          stBoolean : if st.bValue then mnoOutput.Lines.Add ('True') else mnoOutput.Lines.Add  ('False');
          stInteger : mnoOutput.Lines.Add (inttostr (st.ivalue));
          stDouble  : mnoOutput.Lines.Add (floattostr (st.dValue));
          stString  : mnoOutput.Lines.Add (st.sValue.value);
          stList    : mnoOutput.Lines.Add (st.lValue.listToString);
       end;

  finally
    vm.Free;
    mainModule.Free;
  end;
end;


procedure TfrmMain.btnCloseClick(Sender: TObject);
begin
  Application.Terminate;
end;


procedure TfrmMain.btnLoadClick(Sender: TObject);
var dir : string;
begin
  dir := TDirectory.GetParent (TDirectory.GetCurrentDirectory());
  dir := TDirectory.GetParent(TDirectory.GetParent(dir));
  dir := dir + '\\Win32\\VMTests';

  OpenDialog.InitialDir := dir;
  if OpenDialog.Execute then
     begin
     lblFileName.Caption := ExtractFileName (OpenDialog.FileName);
     mnoCode.Lines.LoadFromFile(OpenDialog.FileName);
     end;
end;


procedure TfrmMain.btnSaveDataClick(Sender: TObject);
var f: TextFile; i : integer;
begin
  if SaveDialog.Execute then
     begin
     AssignFile (f, saveDialog.FileName);
     rewrite (f);
     for i := 0 to length (collectedTimes) - 1 do
         writeln (f, collectedTimes[i]:8:2);
     closeFile (f);
     end;
end;

end.
