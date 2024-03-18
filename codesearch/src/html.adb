with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body HTML is

   function Escape
      (Text  : String;
       Quote : Boolean := True)
      return String
   is
      S : Unbounded_String := Null_Unbounded_String;
   begin
      for I in Text'Range loop
         case Text (I) is
            when '&' => Append (S, "&amp;");
            when '<' => Append (S, "&lt;");
            when '>' => Append (S, "&gt;");
            when '"' =>
               if Quote then
                  Append (S, "&quot;");
               end if;
            when ''' =>
               if Quote then
                  Append (S, "&#x27;");
               end if;
            when others =>
               Append (S, Text (I));
         end case;
      end loop;
      return To_String (S);
   end Escape;

   function Unescape
      (Text : String)
      return String
   is
   begin
      return Text;
   end Unescape;

end HTML;
