with Codesearch.Strings; use Codesearch.Strings;
with Codesearch.HTTP;
with Codesearch.File;
with Codesearch.Template;
with Codesearch.Syntax;
with Codesearch.Database;
with Codesearch.Blobstore;

with Codesearch_Config;
with Resources;

with Ada.Directories;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;

with URI;

procedure Source_Highlight is
   package Share is new Resources
      (Crate_Name => Codesearch_Config.Crate_Name);

   package HTTP renames Codesearch.HTTP;
   package File renames Codesearch.File;
   package Template renames Codesearch.Template;

   P        : constant Unicode := Decode (UTF8 (URI.Normalize_Path (HTTP.Path)));
   Basename : constant Unicode := Remove_Prefix (Remove_Suffix (P, ".html"), "/");
begin
   Ada.Directories.Set_Directory ("/home/synack/src/ada-search/codesearch");

   Put_Line (Standard_Error, Basename);

   Codesearch.Database.Open (Read_Only => True);
   declare
      Hash : constant String := Codesearch.Database.Get_Hash (Basename);
   begin
      Codesearch.Database.Close;
      if Hash = "" then
         HTTP.Set_Status (404, "Not Found");
         HTTP.Set_Header ("Content-Type", "text/plain;charset=utf-8");
         HTTP.Put ("404 Not Found" & Codesearch.Strings.LF);
         return;
      elsif Ends_With (P, ".html") then
         declare
            use Codesearch.Strings.Unicode_Maps;
            Env  : Map := Empty_Map;
            T    : constant Unicode := File.Read_Unicode (Share.Resource_Path & "highlight.html");
            HL   : constant String := Codesearch.Syntax.Highlight (Codesearch.Blobstore.Get (Hash));
            Prefix : constant String := "source/alire-20240227/";
            Title  : constant Unicode := Basename (Basename'First + Prefix'Length .. Basename'Last);
         begin
            Insert (Env, "title", Title);
            Insert (Env, "href", "/" & Basename);
            Insert (Env, "code", Decode (UTF8 (HL)));
            HTTP.Set_Status (200, "OK");
            HTTP.Set_Header ("Content-Type", "text/html;charset=utf-8");
            HTTP.Put (Template.Render (T, Env));
         end;
      else
         HTTP.Set_Status (200, "OK");
         HTTP.Set_Header ("Content-Type", "text/plain;charset=utf-8");
         HTTP.Put_Raw (Codesearch.Blobstore.Get (Hash));
      end if;
   end;
end Source_Highlight;
