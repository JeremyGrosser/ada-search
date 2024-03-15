with Codesearch.Database;
with Codesearch.Strings;
with Ada.Strings.Fixed;

package body Codesearch.Web is

   function Get_Query
      (Q : String)
      return String
   is
      use Ada.Strings.Fixed;
   begin
      if Head (Q, 2) = "q=" then
         return Q (Q'First + 2 .. Q'Last);
      else
         return "";
      end if;
   end Get_Query;

   procedure Request
      (Path         : String;
       Query_String : String;
       Response     : out Response_Type)
   is
   begin
      Response.Status := 404;

      if Path = "/" then
         declare
            Query : constant String := Get_Query (Query_String);
         begin
            if Query'Length = 0 then
               Response.Status := 200;
               Response.Data := To_Unbounded_String ("index.html");
            else
               declare
                  use Codesearch.Database;
                  Results  : Search_Results (1 .. 250);
                  Last     : Natural;
               begin
                  Codesearch.Database.Search
                     (Query   => Codesearch.Strings.UTF8_Decode (Query),
                      Results => Results,
                      Last    => Last);
                  if Last > 0 then
                     Response.Status := 200;
                     for I in 1 .. Last loop
                        Append (Response.Data, "<p>");
                        Append (Response.Data, Codesearch.Strings.UTF8_Encode (Results (I).Filename));
                        Append (Response.Data, "</p>");
                        Append (Response.Data, ASCII.LF);
                     end loop;
                  end if;
               end;
            end if;
         end;
      end if;
   end Request;

end Codesearch.Web;
