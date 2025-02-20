--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: AGPL-3.0-or-later
--
with Codesearch.Strings; use Codesearch.Strings;
with Codesearch.HTTP;
with Codesearch.File;
with Codesearch.Template;
with Codesearch.Database;
with URI;

procedure Source_Highlight
   (Request  : Codesearch.HTTP.Request;
    Response : in out Codesearch.HTTP.Response;
    DB       : Codesearch.Database.Session)
is
   package HTTP renames Codesearch.HTTP;
   package File renames Codesearch.File;
   package Template renames Codesearch.Template;

   P        : constant Unicode := Decode (UTF8 (URI.Normalize_Path (HTTP.Path (Request))));
   Basename : constant Unicode := Remove_Prefix (P, "/");
begin
   declare
      Text : constant String := Codesearch.Database.Get_Text (DB, Basename);
   begin
      if Text = "" then
         HTTP.Set_Status (Response, 404, "Not Found");
         HTTP.Set_Header (Response, "Content-Type", "text/plain;charset=utf-8");
         HTTP.Put (Response, "404 Not Found" & Codesearch.Strings.LF);
         return;
      elsif Ends_With (P, ".html") then
         declare
            use Codesearch.Strings.Unicode_Maps;
            Env      : Map := Empty_Map;
            T        : constant Unicode := File.Read_Resource ("highlight.html");
            Prefix   : constant String := "source/alire-20241219/";
            Filename : constant Unicode := Basename (Basename'First + Prefix'Length .. Basename'Last);
            Title    : constant Unicode := Remove_Suffix (Filename, ".html");
            Link     : constant Unicode := "/" & Remove_Suffix (Basename, ".html");
         begin
            if Ends_With (Basename, ".ads.html") and then Codesearch.Database.Exists (DB, Remove_Suffix (Basename, ".ads.html") & ".adb") then
               Insert (Env, "related", "<a href=""" & Remove_Suffix (Link, ".ads") & ".adb.html"">go to body</a>");
            elsif Ends_With (Basename, ".adb.html") and then Codesearch.Database.Exists (DB, Remove_Suffix (Basename, ".adb.html") & ".ads") then
               Insert (Env, "related", "<a href=""" & Remove_Suffix (Link, ".adb") & ".ads.html"">go to spec</a>");
            else
               Insert (Env, "related", "");
            end if;
            Insert (Env, "title", Title);
            Insert (Env, "href", Link);
            Insert (Env, "code", Decode (UTF8 (Text)));
            HTTP.Set_Status (Response, 200, "OK");
            HTTP.Set_Header (Response, "Content-Type", "text/html;charset=utf-8");
            HTTP.Set_Header (Response, "Cache-Control", "max-age=2592000");
            HTTP.Put (Response, Template.Render (T, Env));
         end;
      else
         HTTP.Set_Status (Response, 200, "OK");
         HTTP.Set_Header (Response, "Content-Type", "text/plain;charset=utf-8");
         HTTP.Set_Header (Response, "Cache-Control", "max-age=2592000");
         HTTP.Put_Raw (Response, Text);
      end if;
   end;
end Source_Highlight;
