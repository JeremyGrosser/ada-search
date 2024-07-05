with Ada.Strings.UTF_Encoding;
with Codesearch.HTTP;
with Codesearch.Database;
with Codesearch.Strings;
with Codesearch.File;
with Codesearch.Template;
with URI;

procedure Index is
   package HTTP renames Codesearch.HTTP;
   package DB renames Codesearch.Database;
   package Str renames Codesearch.Strings;
   package File renames Codesearch.File;
   package Template renames Codesearch.Template;

   P : constant String := URI.Normalize_Path (HTTP.Path);
   Q : constant String := HTTP.Query_Parameter ("q");
begin
   Codesearch.File.Set_Working_Directory;
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
            HTTP.Set_Header ("Content-Type", "text/html;charset=utf-8");
            HTTP.Set_Header ("Cache-Control", "max-age=86400");
            declare
               use Str.Unicode_Maps;
               Values : Map := Empty_Map;
               Head : constant Str.Unicode := File.Read_Resource ("head.html");
               Tail : constant Str.Unicode := File.Read_Resource ("tail.html");
            begin
               Insert (Values, "query", Str.Decode (Str.UTF8 (Q)));
               HTTP.Put (Template.Render (Head, Values));
               for Result of Results (1 .. Last) loop
                  HTTP.Put ("<div class=""result"">");
                  HTTP.Put (Str.To_Unicode (Result.Crate));
                  HTTP.Put (" <a href=""");
                  HTTP.Put (Str.To_Unicode (Result.Path));
                  HTTP.Put (".html"">");
                  HTTP.Put (Str.To_Unicode (Result.Filename));
                  HTTP.Put ("</a></div>" & Str.LF);
               end loop;
               HTTP.Put (Tail);
            end;
         else
            HTTP.Set_Status (404, "Not Found");
            HTTP.Set_Header ("Content-Type", "text/plain;charset=utf-8");
            HTTP.Put ("404 Not Found" & Str.LF);
         end if;
      exception
         when Ada.Strings.UTF_Encoding.Encoding_Error =>
            HTTP.Set_Status (400, "Bad Request");
            HTTP.Set_Header ("Content-Type", "text/plain;charset=utf-8");
            HTTP.Put ("400 Bad Request" & Str.LF);
      end;
   elsif P = "/" then
      HTTP.Set_Status (200, "OK");
      HTTP.Set_Header ("Content-Type", "text/html;charset=utf-8");
      HTTP.Set_Header ("Cache-Control", "max-age=86400");
      HTTP.Put (File.Read_Resource ("static/index.html"));
   else
      HTTP.Set_Status (404, "Not Found");
      HTTP.Set_Header ("Content-Type", "text/plain;charset=utf-8");
      HTTP.Put ("404 Not Found" & Str.LF);
   end if;
end Index;
