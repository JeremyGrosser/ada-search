with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Strings.Wide_Wide_Fixed;

package body Codesearch.Strings is

   function Trim
      (Str : Unicode;
       Ch  : Wide_Wide_Character)
       return Unicode
   is
   begin
      if Str'Length > 0 and then Str (Str'First) = Ch then
         return Str (Str'First + 1 .. Str'Last);
      else
         return Str;
      end if;
   end Trim;

   function Replace
      (Str   : Unicode;
       Match : Unicode;
       Subst : Unicode)
       return Unicode
   is
      use Ada.Strings.Wide_Wide_Fixed;
      First : constant Natural := Index (Str, Match);
   begin
      if First = 0 then
         return Str;
      else
         declare
            Head : constant Unicode := Str (Str'First .. First - 1);
            Tail : constant Unicode := Str (First + Match'Length .. Str'Last);
         begin
            return Head & Subst & Tail;
         end;
      end if;
   end Replace;

   function Index
      (Source : Unicode;
       Pattern : Unicode;
       From : Positive)
       return Natural
   is
      First : Positive := From;
   begin
      while First <= Source'Last - Pattern'Length - 1 loop
         if Source (First .. First + Pattern'Length - 1) = Pattern then
            return First;
         else
            First := First + 1;
         end if;
      end loop;

      return 0;
   end Index;

   function Replace_All
      (Str, Match, Subst : Unicode)
      return Unicode
   is
      use Ada.Strings.Wide_Wide_Unbounded;
      S : Unbounded_Unicode := To_Unbounded (Str);
      First : Natural := 1;
   begin
      loop
         First := Index (Source => Str, Pattern => Match, From => First);
         exit when First = 0;
         Delete (S, First, First + Match'Length - 1);
         Insert (S, First, Subst);
         First := First + Subst'Length;
      end loop;
      return To_Unicode (S);
   end Replace_All;

   function Decode
      (Str : UTF8)
      return Unicode
   is (Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Decode (Str));

   function Encode
      (Str : Unicode)
      return UTF8
   is (Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode (Str));

   function Encode
      (Str : Unbounded_Unicode)
      return UTF8
   is (Encode (To_Unicode (Str)));

end Codesearch.Strings;
