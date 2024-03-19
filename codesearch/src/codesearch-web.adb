with Codesearch.Strings; use Codesearch.Strings;
with Codesearch.File; use Codesearch.File;
with Codesearch.Database;
with Codesearch.HTTP;
with Codesearch.Syntax;
with Codesearch_Config;

with Resources;
with URI;
with AAA.Strings;

with HTML;

package body Codesearch.Web is
   package Share is new Resources
      (Crate_Name => Codesearch_Config.Crate_Name);

   Static_Root : constant String := URI.Normalize_Path (Share.Resource_Path & "static/");

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

   procedure Do_Static_File
      (Path : String;
       Content_Type : String;
       Root : String := Static_Root)
   is
      Full_Path   : constant String := URI.Normalize_Path (Root & Path);
   begin
      if AAA.Strings.Has_Prefix (Full_Path, Root) and then Codesearch.File.Exists (Full_Path) then
         declare
            Data : String (1 .. Codesearch.File.Length (Full_Path));
         begin
            Codesearch.File.Read (Full_Path, Data);
            HTTP.Set_Status (200, "OK");
            HTTP.Set_Header ("Content-Type", Content_Type);
            HTTP.Put (Data);
         end;
      else
         Not_Found;
      end if;
   end Do_Static_File;

   procedure Do_Search is
      package DB renames Codesearch.Database;
      Query    : constant Unicode := Decode (UTF8 (HTTP.Query_Parameter ("q")));
      Results  : DB.Search_Results (1 .. 250);
      Last     : Natural;

      Head_Template : constant Unicode := Read_Unicode (UTF8 (Share.Resource_Path & "head.html"));
      Tail_Template : constant Unicode := Read_Unicode (UTF8 (Share.Resource_Path & "tail.html"));

      Head : constant Unicode := Replace_All
         (Str   => Head_Template,
          Match => "{query}",
          Subst => HTML.Escape (Query));
   begin
      DB.Search (Query, Results, Last);
      if Last = 0 then
         HTTP.Set_Status (404, "Not Found");
         HTTP.Set_Header ("Content-Type", "text/html;charset=utf-8");
         HTTP.Put (Encode (Head));
         HTTP.Put ("no results");
         HTTP.Put (Encode (Tail_Template));
      else
         HTTP.Set_Status (200, "OK");
         HTTP.Set_Header ("Content-Type", "text/html;charset=utf-8");
         HTTP.Put (String (Encode (Head)));
         for R of Results (1 .. Last) loop
            HTTP.Put ("<div class=""result"">");
            HTTP.Put (Encode (R.Crate));
            HTTP.Put (" <a href=""/source/");
            HTTP.Put (Left_Strip (Encode (R.Path), "../"));
            HTTP.Put (".html"">");
            HTTP.Put (Encode (R.Filename));
            HTTP.Put ("</a></div>" & ASCII.LF);
         end loop;
         HTTP.Put (String (Encode (Tail_Template)));
      end if;
   end Do_Search;

   procedure Do_Highlight
      (Full_Path : String;
       Raw_Link  : String)
   is
      Code : constant Unicode := Decode (UTF8 (Codesearch.Syntax.Highlight (Full_Path)));

      Template : constant Unicode := Read_Unicode (UTF8 (Share.Resource_Path & "highlight.html"));
      HTML_1 : constant Unicode := Replace
         (Str   => Template,
          Match => "{filename}",
          Subst => Decode (UTF8 (Full_Path)));
      HTML_2 : constant Unicode := Replace
         (Str   => HTML_1,
          Match => "{raw}",
          Subst => Decode (UTF8 (Raw_Link)));
      HTML_3 : constant Unicode := Replace
         (Str   => HTML_2,
          Match => "{code}",
          Subst => Code);
   begin
      HTTP.Set_Status (200, "OK");
      HTTP.Set_Header ("Content-Type", "text/html;charset=utf-8");
      HTTP.Put (Encode (HTML_3));
   end Do_Highlight;

   procedure Do_Source_File
      (Path : String)
   is
      Source_Root : constant String := "/home/synack/src/ada-search";
      Full_Path   : constant String := URI.Normalize_Path (Source_Root & Path);
   begin
      if AAA.Strings.Has_Suffix (Full_Path, ".html") then
         declare
            Source_Path : constant String := Full_Path (Full_Path'First .. Full_Path'Last - 5);
            Raw_Link : constant String := Path (Path'First .. Path'Last - 5);
         begin
            if Codesearch.File.Exists (Source_Path) then
               Do_Highlight (Source_Path, Raw_Link);
            else
               Not_Found;
            end if;
         end;
      else
         Do_Static_File
            (Path => Path,
             Content_Type => "text/plain;charset=utf-8",
             Root => Source_Root);
      end if;
   end Do_Source_File;

   procedure Do_Request is
   begin
      if HTTP.Path = "/" then
         if HTTP.Query_Parameter ("q") /= "" then
            Do_Search;
         else
            Do_Static_File ("index.html", "text/html;charset=utf-8");
         end if;
      elsif AAA.Strings.Has_Prefix (HTTP.Path, "/source/") then
         Do_Source_File (HTTP.Path);
      elsif HTTP.Path = "/search.css" then
         Do_Static_File (HTTP.Path, "text/css");
      elsif HTTP.Path = "/ada.svg" then
         Do_Static_File (HTTP.Path, "image/svg+xml");
      elsif HTTP.Path = "/ada-dark.svg" then
         Do_Static_File (HTTP.Path, "image/svg+xml");
      elsif HTTP.Path = "/robots.txt" then
         Do_Static_File (HTTP.Path, "text/plain");
      else
         Not_Found;
      end if;
   end Do_Request;
end Codesearch.Web;
