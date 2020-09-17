unit uBuiltInOS;

interface

Uses SysUtils, Classes, uModule, uBuiltInModule;

const
   RHODUS_VERSION = '2.51';

type
  TBuiltInOS = class (TBuiltInModule)

     procedure   getVersion (vm : TObject);
     procedure   getPwd (vm : TObject);
     constructor Create;
  end;


implementation

Uses uSymboLTable, uVM, uStringObject, uListObject, uMemoryManager;


// --------------------------------------------------------------------------------------------

constructor TBuiltInOS.Create;
begin
  inherited Create ('os', 'Operating system module');

  addMethod (getpwd, 0, 'pwd', 'Return the path to the current working directory');
  addMethod (getversion, 0, 'version', 'Get the current version number for Rhodus');
end;


procedure TBuiltInOS.getPwd (vm : TObject);
begin
   TVM (vm).push(TStringObject.create (GetCurrentDir));
end;


procedure TBuiltInOs.getVersion (vm : TObject);
begin
  TVM (vm).push (TStringObject.create(RHODUS_VERSION));
end;




end.
