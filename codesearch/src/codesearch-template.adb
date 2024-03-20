package body Codesearch.Template is

   function Render
      (Template : Unicode;
       Values   : Unicode_Maps.Map)
       return Unicode
   is
      Result : Unbounded_Unicode;
      First, Last : Natural;
      I : Natural := Template'First;
   begin
      while I <= Template'Last loop
         if I < Template'Last and then Template (I .. I + 1) = "{{" then
            First := I;
            Last := Index (Template, "}}", First + 2) + 1;
            if Last = 0 then
               raise Template_Error with "Unclosed {{, expected }}";
            end if;

            declare
               use Unicode_Maps;
               Name : constant Unicode := Trim (Template (First + 2 .. Last - 2), ' ');
            begin
               if Contains (Values, Name) then
                  Append (Result, Element (Values, Name));
               else
                  raise Template_Error with String (Encode (Name)) & " not in values map";
               end if;
            end;

            I := Last;
         else
            Append (Result, Template (I));
         end if;

         I := I + 1;
      end loop;

      return To_Unicode (Result);
   end Render;

end Codesearch.Template;
