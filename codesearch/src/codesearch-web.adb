with Codesearch.Strings; use Codesearch.Strings;
with Codesearch.Database;
with Codesearch.HTTP;
with Codesearch.File;
with URI;

package body Codesearch.Web is
   package HTTP renames Codesearch.HTTP;

   function Left_Strip
      (Str    : String;
       Prefix : String)
       return String
   is
   begin
      if Prefix'Length <= Str'Length and then Str (Str'First .. Str'First + Prefix'Length - 1) = Prefix then
         return Str (Str'First + Prefix'Length .. Str'Last);
      else
         return Str;
      end if;
   end Left_Strip;

   procedure Not_Found is
   begin
      HTTP.Set_Status (404, "Not Found");
      HTTP.Set_Header ("Content-Type", "text/html;charset=utf-8");
      HTTP.Put ("<h1>404 Not Found</h1>" & ASCII.LF);
   end Not_Found;

   procedure Do_Search is
      package DB renames Codesearch.Database;
      Query    : constant String := HTTP.Query_Parameter ("q");
      Results  : DB.Search_Results (1 .. 250);
      Last     : Natural;
   begin
      DB.Search (Query, Results, Last);
      if Last = 0 then
         Not_Found;
         HTTP.Set_Status (404, "Not Found");
         HTTP.Set_Header ("Content-Type", "text/plain;charset=utf-8");
         HTTP.Put ("no results for query: ");
         HTTP.Put (Query);
      else
         HTTP.Set_Status (200, "OK");
         HTTP.Set_Header ("Content-Type", "text/html;charset=utf-8");
         for R of Results (1 .. Last) loop
            HTTP.Put ("<p>");
            HTTP.Put (UTF8_Encode (R.Crate));
            HTTP.Put (" <a href=""/source/");
            HTTP.Put (Left_Strip (UTF8_Encode (R.Path), "../"));
            HTTP.Put (""">");
            HTTP.Put (UTF8_Encode (R.Filename));
            HTTP.Put ("</a></p>" & ASCII.LF);
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

   function Starts_With
      (Str : String;
       Prefix : String)
       return Boolean
   is
      I : Integer := Str'First;
   begin
      if Prefix'Length > Str'Length then
         return False;
      end if;

      for Ch of Prefix loop
         if Str (I) /= Ch then
            return False;
         end if;
         I := I + 1;
      end loop;

      return True;
   end Starts_With;

   procedure Do_Source_File
      (Path : String)
   is
      Source_Root : constant String := "/home/synack/src/ada-search";
      Full_Path   : constant String := URI.Normalize_Path (Source_Root & Path);
   begin
      if Codesearch.File.Exists (Full_Path) then
         declare
            Text : String (1 .. Codesearch.File.Length (Full_Path));
         begin
            Codesearch.File.Read (Full_Path, Text);
            HTTP.Set_Status (200, "OK");
            HTTP.Set_Header ("Content-Type", "text/plain;charset=utf-8");
            HTTP.Put (Text);
         end;
      else
         Not_Found;
      end if;
   end Do_Source_File;

   procedure Do_Request is
   begin
      if HTTP.Path = "/" then
         if HTTP.Query_Parameter ("q") /= "" then
            Do_Search;
         else
            Do_Index;
         end if;
      elsif Starts_With (HTTP.Path, "/source/") then
         Do_Source_File (HTTP.Path);
      else
         Not_Found;
      end if;
   end Do_Request;

end Codesearch.Web;
