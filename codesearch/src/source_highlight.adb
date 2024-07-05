with Codesearch.HTTP;
with Codesearch.Strings;
with Codesearch.File;
with Codesearch.Template;
with Codesearch.Syntax;
with Codesearch.Database;
with Codesearch.Blobstore;

with Codesearch_Config;
with Resources;

with Ada.Directories;

with URI;

procedure Source_Highlight is
   package Share is new Resources
      (Crate_Name => Codesearch_Config.Crate_Name);

   package HTTP renames Codesearch.HTTP;
   package Str renames Codesearch.Strings;
   package File renames Codesearch.File;
   package Template renames Codesearch.Template;

   P        : constant String := URI.Normalize_Path (HTTP.Path);
   Filename : constant String := P (P'First + 1 .. P'Last - 5);
begin
   Ada.Directories.Set_Directory ("/home/synack/src/ada-search/codesearch");

   Codesearch.Database.Open (Read_Only => True);
   declare
      Hash : constant String := Codesearch.Database.Get_Hash (Filename);
   begin
      if Hash = "" then
         HTTP.Set_Status (404, "Not Found");
         HTTP.Set_Header ("Content-Type", "text/plain;charset=utf-8");
         HTTP.Put ("404 Not Found" & Codesearch.Strings.LF);
         return;
      else
         declare
            use Str.Unicode_Maps;
            Env  : Map := Empty_Map;
            T    : constant Str.Unicode := File.Read_Unicode (Share.Resource_Path & "highlight.html");
            HL   : constant String := Codesearch.Syntax.Highlight (Codesearch.Blobstore.Get (Hash));
         begin
            Codesearch.Database.Close;
            Insert (Env, "filename", Str.Decode (Str.UTF8 (Filename)));
            Insert (Env, "raw", Str.Decode (Str.UTF8 (Filename)));
            Insert (Env, "code", Str.Decode (Str.UTF8 (HL)));
            HTTP.Set_Status (200, "OK");
            HTTP.Set_Header ("Content-Type", "text/html;charset=utf-8");
            HTTP.Put (Template.Render (T, Env));
         end;
      end if;
   end;
end Source_Highlight;
