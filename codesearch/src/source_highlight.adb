with Codesearch.Strings; use Codesearch.Strings;
with Codesearch.HTTP;
with Codesearch.File;
with Codesearch.Template;
with Codesearch.Syntax;
with Codesearch.Database;
with Codesearch.Blobstore;
with URI;

procedure Source_Highlight
   (Request  : Codesearch.HTTP.Request;
    Response : in out Codesearch.HTTP.Response)
is
   package HTTP renames Codesearch.HTTP;
   package File renames Codesearch.File;
   package Template renames Codesearch.Template;

   P        : constant Unicode := Decode (UTF8 (URI.Normalize_Path (HTTP.Path (Request))));
   Basename : constant Unicode := Remove_Prefix (Remove_Suffix (P, ".html"), "/");
begin
   declare
      Hash : constant String := Codesearch.Database.Get_Hash (Basename);
   begin
      if Hash = "" then
         HTTP.Set_Status (Response, 404, "Not Found");
         HTTP.Set_Header (Response, "Content-Type", "text/plain;charset=utf-8");
         HTTP.Put (Response, "404 Not Found" & Codesearch.Strings.LF);
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
            HTTP.Set_Status (Response, 200, "OK");
            HTTP.Set_Header (Response, "Content-Type", "text/html;charset=utf-8");
            HTTP.Set_Header (Response, "Cache-Control", "max-age=2592000");
            HTTP.Put (Response, Template.Render (T, Env));
         end;
      else
         HTTP.Set_Status (Response, 200, "OK");
         HTTP.Set_Header (Response, "Content-Type", "text/plain;charset=utf-8");
         HTTP.Set_Header (Response, "Cache-Control", "max-age=2592000");
         HTTP.Put_Raw (Response, Codesearch.Blobstore.Get (Hash));
      end if;
   end;
end Source_Highlight;
