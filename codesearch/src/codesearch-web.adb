with VSS.Strings; use VSS.Strings;
with Codesearch.Strings; use Codesearch.Strings;
with Codesearch.Database;
with Codesearch.HTTP;

package body Codesearch.Web is
   package HTTP renames Codesearch.HTTP;

   procedure Not_Found is
   begin
      HTTP.Set_Status (404, "Not Found");
      HTTP.Set_Header ("Content-Type", "text/html;charset=utf-8");
      HTTP.Put ("<h1>404 Not Found</h1>" & ASCII.LF);
   end Not_Found;

   procedure Do_Search is
      package DB renames Codesearch.Database;
      Query    : constant Virtual_String := UTF8_Decode (HTTP.Query_Parameter ("q"));
      Results  : DB.Search_Results (1 .. 250);
      Last     : Natural;
   begin
      DB.Search (Query, Results, Last);
      if Last = 0 then
         Not_Found;
         HTTP.Put ("no results for query: ");
         HTTP.Put (UTF8_Encode (Query));
      else
         HTTP.Set_Status (200, "OK");
         HTTP.Set_Header ("Content-Type", "text/html;charset=utf-8");
         for I in 1 .. Last loop
            HTTP.Put ("<p>");
            HTTP.Put (UTF8_Encode (Results (I).Filename));
            HTTP.Put ("</p>" & ASCII.LF);
         end loop;
      end if;
   end Do_Search;

   procedure Do_Index is
   begin
      HTTP.Set_Status (200, "OK");
      HTTP.Set_Header ("Content-Type", "text/html;charset=utf-8");

      HTTP.Put ("<form action=""/"" method=""GET"">");
      HTTP.Put ("<input type=""text"" name=""q"" placeholder=""Search"" />");
      HTTP.Put ("<input type=""submit"" />");
      HTTP.Put ("</form>");
   end Do_Index;

   procedure Do_Request is
      Path : constant String := HTTP.Path;
   begin
      if Path = "" or else Path = "/" then
         if HTTP.Query_Parameter ("q") /= "" then
            Do_Search;
         else
            Do_Index;
         end if;
      else
         Not_Found;
      end if;
   end Do_Request;

end Codesearch.Web;
