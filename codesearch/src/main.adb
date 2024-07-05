with Codesearch.Strings; use Codesearch.Strings;
with Codesearch.HTTP;
with Index;
with Source_Highlight;
with URI;

with Codesearch.HTTP.Server;

procedure Main is
   package HTTP renames Codesearch.HTTP;

   Request  : HTTP.Request;
   Response : HTTP.Response;
   P : constant Unicode := Decode (UTF8 (URI.Normalize_Path (HTTP.Path (Request))));
begin
   HTTP.Server.Bind;
   HTTP.Server.Run;

   if P = "/" then
      Index (Request, Response);
   elsif Starts_With (P, "/source/") then
      Source_Highlight (Request, Response);
   else
      HTTP.Set_Status (Response, 404, "Not Found");
      HTTP.Set_Header (Response, "Content-Type", "text/plain;charset=utf-8");
      HTTP.Put (Response, "404 Not Found");
   end if;
end Main;
