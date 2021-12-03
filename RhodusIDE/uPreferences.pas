unit uPreferences;

interface

Uses IniFiles;

type
  TEditor = record
     fontSize : integer;
     fontName : string;
     theme : string;
  end;

  TIDE = record
    theme : string;
  end;

  TPreferences = record
     IDE : TIDE;
     editor : TEditor;
  end;

var
  preferences : TPreferences;

procedure savePreferences (iniFile  : TIniFile);
procedure loadPreferences (iniFile  : TIniFile);


implementation

const
  DEFAULT_FONT_SIZE = 15;
  DEFAULT_FONT_NAME = 'Consolas';
  DEFAULT_THEME_NAME = 'Windows';


procedure savePreferences (iniFile  : TIniFile);
begin
  iniFile.WriteInteger('FontPreferences', 'fontSize', preferences.editor.fontSize);
  iniFile.WriteString('FontPreferences', 'fontName', preferences.editor.fontName);

  iniFile.WriteString('IDE', 'theme', preferences.IDE.theme);
end;

procedure loadPreferences (iniFile  : TIniFile);
begin
  preferences.editor.fontSize := iniFile.ReadInteger('FontPreferences', 'fontSize', DEFAULT_FONT_SIZE);
  preferences.editor.fontName := iniFile.ReadString('FontPreferences', 'fontName', DEFAULT_FONT_NAME);
  preferences.IDE.theme := iniFile.ReadString('IDE', 'theme', DEFAULT_THEME_NAME);
end;

initialization
  preferences.editor.fontSize := DEFAULT_FONT_SIZE;
  preferences.editor.fontName := DEFAULT_FONT_NAME;
end.
