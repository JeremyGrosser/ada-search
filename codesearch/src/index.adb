with Ada.Strings.Fixed;

with Codesearch.HTTP;
with Codesearch.Database;
with Codesearch.Strings;
with Codesearch.File;

with Codesearch_Config;
with Resources;

with URI;

procedure Index is
   package Share is new Resources
      (Crate_Name => Codesearch_Config.Crate_Name);

   package HTTP renames Codesearch.HTTP;
   package DB renames Codesearch.Database;
   package Str renames Codesearch.Strings;
   package File renames Codesearch.File;

   P : constant String := URI.Normalize_Path (HTTP.Path);
   Q : constant String := HTTP.Query_Parameter ("q");
begin
   if P = "/" and then Q'Length > 0 then
      declare
         Results  : DB.Search_Results (1 .. 250);
         Last     : Natural;
      begin
         DB.Open (Read_Only => True);
         DB.Search
            (Query   => Str.Decode (Str.UTF8 (Q)),
             Results => Results,
             Last    => Last);
         DB.Close;
         if Last > 0 then
            HTTP.Set_Status (200, "OK");
            HTTP.Set_Header ("Content-Type", "text/plain;charset=utf-8");

            for Result of Results (1 .. Last) loop
               HTTP.Put (String (Str.Encode (Result.Crate)));
               HTTP.Put (" ");
               HTTP.Put (String (Str.Encode (Result.Filename)));
               HTTP.Put (" ");
               HTTP.Put (String (Str.Encode (Result.Path)));
               HTTP.Put ("" & ASCII.LF);
            end loop;
         else
            HTTP.Set_Status (404, "Not Found");
            HTTP.Set_Header ("Content-Type", "text/plain;charset=utf-8");
            HTTP.Put ("404 Not Found" & ASCII.LF);
         end if;
      end;
   elsif P = "/" then
      declare
         Filename : constant String := Share.Resource_Path & "static/index.html";
         Text     : String (1 .. File.Length (Filename));
      begin
         File.Read (Filename, Text);
         HTTP.Set_Status (200, "OK");
         HTTP.Set_Header ("Content-Type", "text/html;charset=utf-8");
         HTTP.Put (Text);
      end;
   else
      HTTP.Set_Status (404, "Not Found");
      HTTP.Set_Header ("Content-Type", "text/plain;charset=utf-8");
      HTTP.Put ("404 Not Found" & ASCII.LF);
   end if;
end Index;
