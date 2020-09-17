unit uInitialize;

interface

Uses SysUtils, IniFiles, IOUtils;

procedure setUpEnvironment (appExe : string);

var
  launchDir : string;
  homeDir : string;
  rhodusConfigDir : string;
  docsDir : string;

  RHODUSPATH : string;

implementation


procedure setUpEnvironment (appExe : string);
var Ini: TIniFile;
begin
   homeDir := TPath.GetHomePath;
   rhodusConfigDir := homeDir + '\rhodus';
   // Read the ini file to get the rhoduspath
   Ini := TIniFile.Create (rhodusConfigDir + '\rhodus.ini');
   try
     RHODUSPATH := Ini.ReadString('Path', 'RHODUSPATH', '');
     if RHODUSPATH = '' then
        begin
        writeln ('Failed to locate Rhodus path, defaulting to Documents');
        RHODUSPATH := docsDir;
        end
     else
        begin
        if not TDirectory.Exists(RHODUSPATH) then
           begin
           writeln (RHODUSPATH + ' does not exist, using instead ' + docsDir + sLineBreak);
           SetCurrentDir(docsDir);
           RHODUSPATH := docsDir
           end
        else
           SetCurrentDir(RHODUSPATH);
        end;
   finally
     Ini.Free;
   end;
end;


// Decprecated, now handled by installer
//procedure setUpDirectories (appExe : string);
//var ans, contents : string;
//    Ini: TIniFile;
//begin
//   try
//  launchDir := ExtractFileDir (ParamStr (0));
//  homeDir := TPath.GetHomePath;
//  docsDir := TPath.GetDocumentsPath;
//  rhodusConfigDir := homeDir + '\rhodus';
//  if not TDirectory.Exists(rhodusConfigDir) then
//     begin
//     writeln;
//     writeln ('This looks like this is the first time Rhodus has been started' + sLineBreak);
//     write   ('Can I create a configuration file in: ' + rhodusConfigDir, ' ? <y/n>');
//     readln (ans);
//     if ans = 'y' then
//        begin
//        writeln ('The initial working directory will be: \Documents\rhodus' + sLineBreak);
//        if not TDirectory.Exists (docsDir + '\rhodus') then
//           TDirectory.CreateDirectory(docsDir + '\rhodus');
//        RHODUSPATH := docsDir + '\rhodus';
//        contents := '[Path]' + sLineBreak + 'RHODUSPATH=' + RHODUSPATH;
//       if not TDirectory.Exists (rhodusConfigDir) then
//          TDirectory.CreateDirectory(rhodusConfigDir);
//        TFile.WriteAllText(rhodusConfigDir + '\rhodus.ini', contents);
//        end
//     else
//       begin
//       writeln ('You answered no'); writeln;
//       SetCurrentDir(docsDir);
//       end;
//     end
//  else
//     begin
//     // Read the ini file to get the rhoduspath
//     Ini := TIniFile.Create (rhodusConfigDir + '\rhodus.ini');
//     try
//       RHODUSPATH := Ini.ReadString('Path', 'RHODUSPATH', '');
//       if RHODUSPATH = '' then
//          begin
//          writeln ('Failed to locate Rhodus path, defaulting to Documents');
//          RHODUSPATH := docsDir;
//          end
//       else
//          begin
//          writeln('Rhodus path = ', RHODUSPATH);
//          if not TDirectory.Exists(RHODUSPATH) then
//             begin
//             writeln (RHODUSPATH + ' does not exist, using instead ' + docsDir + sLineBreak);
//             SetCurrentDir(docsDir);
//             end;
//          end;
//     finally
//       Ini.Free;
//     end;
//     end;
//    SetCurrentDir(RHODUSPATH);
//  except
//    on e:Exception do
//       begin
//       writeln ('Error setting up: ' + e.Message);
//       readln;
//       end;
//  end;
//
//end;


end.
