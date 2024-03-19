package body HTML is

   function Escape
      (Text  : Unicode;
       Quote : Boolean := True)
      return Unicode
   is
      S : Unbounded_Unicode;
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
      return To_Unicode (S);
   end Escape;

end HTML;
