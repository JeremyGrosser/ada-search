pragma Extensions_Allowed (On);
with Codesearch.Strings; use Codesearch.Strings;
with Index;
with Source_Highlight;
with Static_File;

package body Codesearch.Service is

   procedure Handle_Request
      (Request  : HTTP.Request;
       Response : in out HTTP.Response;
       DB       : Codesearch.Database.Session)
   is
      P : constant Unicode := Decode (UTF8 (Request.Path));
   begin
      if P = "/" then
         Index (Request, Response, DB);
      elsif Starts_With (P, "/source/") then
         Source_Highlight (Request, Response, DB);
      elsif Starts_With (P, "/static/") then
         Static_File (Request, Response);
      else
         Response.Set_Status (404, "Not Found");
         Response.Set_Header ("Content-Type", "text/plain;charset=utf-8");
         Response.Put ("404 Not Found" & LF);
      end if;
   end Handle_Request;

end Codesearch.Service;
