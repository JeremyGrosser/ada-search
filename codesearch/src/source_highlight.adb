with Codesearch.HTTP;
with Codesearch.Strings;
with Codesearch.File;
with Codesearch.Template;
with Codesearch.Syntax;

with Codesearch_Config;
with Resources;

with URI;

procedure Source_Highlight is
   package Share is new Resources
      (Crate_Name => Codesearch_Config.Crate_Name);

   package HTTP renames Codesearch.HTTP;
   package Str renames Codesearch.Strings;
   package File renames Codesearch.File;
   package Template renames Codesearch.Template;

   P : constant String := URI.Normalize_Path (HTTP.Path);
   Filename : constant String := "/home/synack/src/codesearch" & P (P'First .. P'Last - 5);
begin
   if File.Exists (Filename) then
      declare
         use Str.Unicode_Maps;
         Env : Map := Empty_Map;
         T   : constant Str.Unicode := File.Read_Unicode (Share.Resource_Path & "highlight.html");
         HL  : constant String := Codesearch.Syntax.Highlight (Filename);
      begin
         Insert (Env, "filename", Str.Decode (Str.UTF8 (Filename)));
         Insert (Env, "raw", Str.Decode (Str.UTF8 (Filename)));
         Insert (Env, "code", Str.Decode (Str.UTF8 (HL)));
         HTTP.Set_Status (200, "OK");
         HTTP.Set_Header ("Content-Type", "text/html;charset=utf-8");
         HTTP.Put (Template.Render (T, Env));
      end;
   else
      HTTP.Set_Status (404, "Not Found");
      HTTP.Set_Header ("Content-Type", "text/plain;charset=utf-8");
      HTTP.Put ("404 Not Found" & Str.LF);
      HTTP.Put (Str.Decode (Str.UTF8 (Filename)));
   end if;
end Source_Highlight;
