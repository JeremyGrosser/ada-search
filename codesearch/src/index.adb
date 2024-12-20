with Ada.Strings.UTF_Encoding;
with Codesearch.HTTP;
with Codesearch.Database;
with Codesearch.Strings;
with Codesearch.File;
with Codesearch.Template;
with URI;

procedure Index
   (Request  : Codesearch.HTTP.Request;
    Response : in out Codesearch.HTTP.Response;
    DB       : Codesearch.Database.Session)
is
   package HTTP renames Codesearch.HTTP;
   package Str renames Codesearch.Strings;
   package File renames Codesearch.File;
   package Template renames Codesearch.Template;

   P : constant String := URI.Normalize_Path (HTTP.Path (Request));
   Q : constant String := HTTP.Query_Parameter (Request, "q");
begin
   Codesearch.File.Set_Working_Directory;
   if P = "/" and then Q'Length > 0 then
      declare
         Results  : Codesearch.Database.Search_Results (1 .. 250);
         Last     : Natural;
      begin
         Codesearch.Database.Search
            (This    => DB,
             Query   => Str.Decode (Str.UTF8 (Q)),
             Results => Results,
             Last    => Last);
         if Last > 0 then
            HTTP.Set_Status (Response, 200, "OK");
            HTTP.Set_Header (Response, "Content-Type", "text/html;charset=utf-8");
            HTTP.Set_Header (Response, "Cache-Control", "max-age=86400");
            declare
               use Str.Unicode_Maps;
               Values : Map := Empty_Map;
               Head : constant Str.Unicode := File.Read_Resource ("head.html");
               Tail : constant Str.Unicode := File.Read_Resource ("tail.html");
            begin
               Insert (Values, "query", Str.Decode (Str.UTF8 (Q)));
               HTTP.Put (Response, Template.Render (Head, Values));
               for Result of Results (1 .. Last) loop
                  HTTP.Put (Response, "<div class=""result"">");
                  HTTP.Put (Response, Str.To_Unicode (Result.Crate));
                  HTTP.Put (Response, " <a href=""");
                  HTTP.Put (Response, Str.To_Unicode (Result.Path));
                  HTTP.Put (Response, ".html"">");
                  HTTP.Put (Response, Str.To_Unicode (Result.Filename));
                  HTTP.Put (Response, "</a></div>" & Str.LF);
               end loop;
               HTTP.Put (Response, Tail);
            end;
         else
            HTTP.Set_Status (Response, 404, "Not Found");
            HTTP.Set_Header (Response, "Content-Type", "text/plain;charset=utf-8");
            HTTP.Put (Response, "404 Not Found" & Str.LF);
         end if;
      exception
         when Ada.Strings.UTF_Encoding.Encoding_Error =>
            HTTP.Set_Status (Response, 400, "Bad Request");
            HTTP.Set_Header (Response, "Content-Type", "text/plain;charset=utf-8");
            HTTP.Put (Response, "400 Bad Request" & Str.LF);
      end;
   elsif P = "/" then
      HTTP.Set_Status (Response, 200, "OK");
      HTTP.Set_Header (Response, "Content-Type", "text/html;charset=utf-8");
      HTTP.Set_Header (Response, "Cache-Control", "max-age=86400");
      HTTP.Put (Response, File.Read_Resource ("static/index.html"));
   else
      HTTP.Set_Status (Response, 404, "Not Found");
      HTTP.Set_Header (Response, "Content-Type", "text/plain;charset=utf-8");
      HTTP.Put (Response, "404 Not Found" & Str.LF);
   end if;
end Index;
