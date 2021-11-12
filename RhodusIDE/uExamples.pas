unit uExamples;

interface

Uses Classes, SysUtils, Dialogs, System.JSON, Generics.Collections;

type
   TExample = class
      name :string;
      src : string;
      constructor Create (name, src : string);
   end;

   TExamples = class (TList<TExample>)
      procedure loadExamples;
      constructor Create;
   end;


implementation

Uses IOUtils;

constructor TExample.Create (name, src : string);
begin
  inherited Create;
  self.name := name;
  self.src := src;
end;


constructor TExamples.Create;
begin
  inherited;
  loadExamples;
end;


//'{"colors":[{"name":"red", "hex":"#f00"}]}
procedure TExamples.loadExamples;
var jsonObj : TJSONObject;
    example: TJSONObject;
    examples : TJSONArray;
    jsonStr : string;
    i : integer;
begin
  jsonStr := TFile.ReadAllText('examples.json');
  try
    jsonObj := TJSONObject.ParseJSONValue(jsonStr) as TJSONObject;
    examples := TJSONArray(jsonObj.Get('examples').JSONValue);

    for i := 0 to examples.Size - 1 do
        begin
        example := examples.Get(i) as TJSONObject;
        add(TExample.Create(example.GetValue('name').Value, example.GetValue('src').Value));
        end;
  except
    on Exception do
       showmessage ('Json example file is not currectly formatted');
  end;
end;

end.
