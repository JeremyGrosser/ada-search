with Codesearch.Strings; use Codesearch.Strings;
with Codesearch.HTTP;
with Codesearch.File;
with Codesearch.Template;
with Codesearch.Syntax;
with Codesearch.Database;
with Codesearch.Blobstore;
with URI;

procedure Source_Highlight is
   package HTTP renames Codesearch.HTTP;
   package File renames Codesearch.File;
   package Template renames Codesearch.Template;

   P        : constant Unicode := Decode (UTF8 (URI.Normalize_Path (HTTP.Path)));
   Basename : constant Unicode := Remove_Prefix (Remove_Suffix (P, ".html"), "/");
begin
   Codesearch.File.Set_Working_Directory;
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
            T    : constant Unicode := File.Read_Resource ("highlight.html");
            HL   : constant String := Codesearch.Syntax.Highlight (Codesearch.Blobstore.Get (Hash));
            Prefix : constant String := "source/alire-20240227/";
            Title  : constant Unicode := Basename (Basename'First + Prefix'Length .. Basename'Last);
         begin
            Insert (Env, "title", Title);
            Insert (Env, "href", "/" & Basename);
            Insert (Env, "code", Decode (UTF8 (HL)));
            HTTP.Set_Status (200, "OK");
            HTTP.Set_Header ("Content-Type", "text/html;charset=utf-8");
            HTTP.Set_Header ("Cache-Control", "max-age=2592000");
            HTTP.Put (Template.Render (T, Env));
         end;
      else
         HTTP.Set_Status (200, "OK");
         HTTP.Set_Header ("Content-Type", "text/plain;charset=utf-8");
         HTTP.Set_Header ("Cache-Control", "max-age=2592000");
         HTTP.Put_Raw (Codesearch.Blobstore.Get (Hash));
      end if;
   end;
end Source_Highlight;
