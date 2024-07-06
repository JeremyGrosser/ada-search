pragma Extensions_Allowed (On);
with Codesearch.Strings; use Codesearch.Strings;
with Codesearch.HTTP;
with Codesearch.File;

procedure Static_File
   (Request  : Codesearch.HTTP.Request;
    Response : in out Codesearch.HTTP.Response)
is
   package File renames Codesearch.File;

   function Ends_With
      (Source : String;
       Suffix : String)
       return Boolean
   is
   begin
      if Source'Length < Suffix'Length then
         return False;
      else
         return Source (Source'Last - Suffix'Length + 1 .. Source'Last) = Suffix;
      end if;
   end Ends_With;

   Path : constant String := Request.Path;
begin
   if not File.Resource_Exists (Path) then
      Response.Set_Status (404, "Not Found");
      Response.Set_Header ("Content-Type", "text/plain;charset=utf-8");
      Response.Put ("404 Not Found" & LF);
   else
      Response.Set_Status (200, "OK");
      if Ends_With (Path, ".svg") then
         Response.Set_Header ("Content-Type", "image/svg+xml");
      elsif Ends_With (Path, ".css") then
         Response.Set_Header ("Content-Type", "text/css");
      elsif Ends_With (Path, ".txt") then
         Response.Set_Header ("Content-Type", "text/plain;charset=utf-8");
      elsif Ends_With (Path, ".html") then
         Response.Set_Header ("Content-Type", "text/html;charset=utf-8");
      end if;
      Response.Put (File.Read_Resource (Path));
   end if;
end Static_File;
