unit uInitialize;


// This source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

Uses SysUtils, IniFiles, IOUtils, uEnvironment;


procedure setUpEnvironment (appExe : string);

var
  launchDir : string;
  homeDir : string;
  rhodusConfigDir : string;
  docsDir : string;

  //RHODUSPATH : string;

implementation


procedure setUpEnvironment (appExe : string);
var Ini: TIniFile;
    s1, s2 : string;
begin
   launchEnvironment.executionPath := ExtractFilePath (appExe);

   // Look for the modules directory
   if TDirectory.Exists(launchEnvironment.executionPath + '\\Modules') then
      launchEnvironment.moduleDir := launchEnvironment.executionPath + '\\Modules'
   else
      // I'm guaess this is a dev enviornment and the directory is two levels up
      begin
      if TDirectory.Exists('..\\..\\Modules') then
         begin
         s1 := TDirectory.GetParent(ExcludeTrailingPathDelimiter(launchEnvironment.executionPath));
         s1 := TDirectory.GetParent(ExcludeTrailingPathDelimiter(s1));
         launchEnvironment.moduleDir := s1 + '\\Modules';
         launchEnvironment.basePath := s1;
         end
      else
         launchEnvironment.moduleDir := launchEnvironment.executionPath;
     end;
   exit;




//   // Look for the ini file in the launch directory first, if its not here then look for it in AppData
//   s1 := ExtractFilePath (Paramstr(0));
//   if not FileExists(s1 + '\rhodus.ini') then
//      begin
//      RHODUSPATH := s1;
//      writeln ('Setting current directory to ' + RHODUSPATH + sLineBreak);
//      SetCurrentDir(RHODUSPATH);
//      exit;
//      end
//   else
//      begin
//      homeDir := TPath.GetHomePath;
//      rhodusConfigDir := homeDir;
//      if not FileExists(rhodusConfigDir + '\rhodus\rhodus.ini') then
//         rhodusConfigDir := '.';
//      end;
//
//   // Read the ini file to get the rhoduspath
//   Ini := TIniFile.Create (rhodusConfigDir + '\rhodus\rhodus.ini');
//   try
//     RHODUSPATH := Ini.ReadString('Path', 'RHODUSPATH', '');
//     if RHODUSPATH = '' then
//        begin
//        writeln ('Failed to locate Rhodus path, defaulting to Documents');
//
//        s2 := TDirectory.GetParent(ExcludeTrailingPathDelimiter(s1));
//        s2 := TDirectory.GetParent(ExcludeTrailingPathDelimiter(s2));
//
//        RHODUSPATH := ExtractFilePath(ExtractFilePath(ParamStr(0)));
//        end
//     else
//        begin
//        if not TDirectory.Exists(RHODUSPATH) then
//           begin
//           writeln (RHODUSPATH + ' does not exist, using instead ' + docsDir + sLineBreak);
//           SetCurrentDir(docsDir);
//           RHODUSPATH := docsDir
//           end
//        else
//           SetCurrentDir(RHODUSPATH);
//        end;
//   finally
//     Ini.Free;
//   end;
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
