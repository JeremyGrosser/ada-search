pragma Extensions_Allowed (On);
with Codesearch.Strings; use Codesearch.Strings;
with Index;
with Source_Highlight;

package body Codesearch.Service is

   procedure Handle_Request
      (Request  : HTTP.Request;
       Response : in out HTTP.Response)
   is
      P : constant Unicode := Decode (UTF8 (Request.Path));
   begin
      if P = "/" then
         Index (Request, Response);
      elsif Starts_With (P, "/source/") then
         Source_Highlight (Request, Response);
      else
         Response.Set_Status (404, "Not Found");
         Response.Set_Header ("Content-Type", "text/plain;charset=utf-8");
         Response.Put ("404 Not Found" & LF);
      end if;
   end Handle_Request;

end Codesearch.Service;
