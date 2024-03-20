with Codesearch.Strings; use Codesearch.Strings;
with Codesearch.File; use Codesearch.File;
with Codesearch.Database;
with Codesearch.HTTP;
with Codesearch.Syntax;
with Codesearch.Template;
with Codesearch_Config;

with Resources;

with HTML;

package body Codesearch.Web is

   function Resource_Path
      (Name : Path)
      return Path
   is
      package Share is new Resources
         (Crate_Name => Codesearch_Config.Crate_Name);
      P : constant Path := Path (Decode (UTF8 (Share.Resource_Path)));
   begin
      return Join (P, Name);
   end Resource_Path;

   function Resource_Text
      (Name : Path)
      return Unicode
   is (Read_Unicode (Resource_Path (Name)));

   package HTTP renames Codesearch.HTTP;

   procedure Not_Found is
   begin
      HTTP.Set_Status (404, "Not Found");
      HTTP.Set_Header ("Content-Type", "text/html;charset=utf-8");
      HTTP.Put ("<h1>404 Not Found</h1>" & ASCII.LF);
   end Not_Found;

   Static_Root : constant Path := Resource_Path ("static/");

   procedure Do_Static_File
      (Name         : Path;
       Content_Type : String;
       Root         : Path := Static_Root)
   is
      Full_Path : constant Path := Normalize (Join (Root, Name));
   begin
      if Starts_With (Unicode (Full_Path), Unicode (Root)) and then Exists (Full_Path) then
         HTTP.Set_Status (200, "OK");
         HTTP.Set_Header ("Content-Type", Content_Type);
         HTTP.Put (String (Encode (Read_Unicode (Full_Path))));
      else
         Not_Found;
      end if;
   end Do_Static_File;

   procedure Do_Search is
      package DB renames Codesearch.Database;
      Query    : constant Unicode := Decode (UTF8 (HTTP.Query_Parameter ("q")));
      Results  : DB.Search_Results (1 .. 250);
      Last     : Natural;

      Env : Unicode_Maps.Map := Unicode_Maps.Empty_Map;

      Head_Template   : constant Unicode := Resource_Text ("head.html");
      Tail            : constant Unicode := Resource_Text ("tail.html");
   begin
      Unicode_Maps.Insert (Env, "query", HTML.Escape (Query));

      DB.Search (Query, Results, Last);
      if Last = 0 then
         HTTP.Set_Status (404, "Not Found");
         HTTP.Set_Header ("Content-Type", "text/html;charset=utf-8");
         HTTP.Put (String (Encode (Codesearch.Template.Render (Head_Template, Env))));
         HTTP.Put ("no results");
         HTTP.Put (String (Encode (Tail)));
      else
         HTTP.Set_Status (200, "OK");
         HTTP.Set_Header ("Content-Type", "text/html;charset=utf-8");
         HTTP.Put (String (Encode (Codesearch.Template.Render (Head_Template, Env))));
         for R of Results (1 .. Last) loop
            HTTP.Put ("<div class=""result"">");
            HTTP.Put (String (Encode (R.Crate)));
            HTTP.Put (" <a href=""/source/source/");
            HTTP.Put (String (Encode (R.Path)));
            HTTP.Put (".html"">");
            HTTP.Put (String (Encode (R.Filename)));
            HTTP.Put ("</a></div>" & ASCII.LF);
         end loop;
         HTTP.Put (String (Encode (Tail)));
      end if;
   end Do_Search;

   procedure Do_Highlight
      (Full_Path : Path)
   is
      Template : constant Unicode := Resource_Text ("highlight.html");
      Env  : Unicode_Maps.Map := Unicode_Maps.Empty_Map;
      Code : constant Unicode := Decode (Codesearch.Syntax.Highlight (Full_Path));
      Base : constant Path := Remove_Extension (Basename (Full_Path));
   begin
      Unicode_Maps.Insert (Env, "filename", Unicode (Full_Path));
      Unicode_Maps.Insert (Env, "raw", Unicode (Base));
      Unicode_Maps.Insert (Env, "code", Code);
      HTTP.Set_Status (200, "OK");
      HTTP.Set_Header ("Content-Type", "text/html;charset=utf-8");
      HTTP.Put (String (Encode (Codesearch.Template.Render (Template, Env))));
   end Do_Highlight;

   procedure Do_Source_File
      (Name : Unicode)
   is
      Source_Root : constant Path := "/home/synack/src/ada-search";
      Source_Path : constant Path := Remove_Extension (Normalize (Join (Source_Root, Path (Name))));
   begin
      if Starts_With (Unicode (Source_Root), Unicode (Source_Path)) and then Exists (Source_Path) then
         if Ends_With (Name, ".html") then
            Do_Highlight (Source_Path);
         else
            Do_Static_File
               (Name => Source_Path,
                Content_Type => "text/plain;charset=utf-8",
                Root => Source_Root);
         end if;
      else
         Not_Found;
      end if;
   end Do_Source_File;

   procedure Do_Request is
      HPath : constant Unicode := Decode (UTF8 (HTTP.Path));
   begin
      if HPath = "/" then
         if HTTP.Query_Parameter ("q") /= "" then
            Do_Search;
         else
            Do_Static_File ("index.html", "text/html;charset=utf-8");
         end if;
      elsif Starts_With (HPath, "/source/") then
         Do_Source_File (HPath);
      elsif HPath = "/search.css" then
         Do_Static_File (Path (HPath), "text/css");
      elsif HPath = "/ada.svg" then
         Do_Static_File (Path (HPath), "image/svg+xml");
      elsif HPath = "/ada-dark.svg" then
         Do_Static_File (Path (HPath), "image/svg+xml");
      elsif HPath = "/robots.txt" then
         Do_Static_File (Path (HPath), "text/plain");
      else
         Not_Found;
      end if;
   end Do_Request;
end Codesearch.Web;
