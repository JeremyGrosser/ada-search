with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.UTF_Encoding;
with Ada.Strings;

package Codesearch.Strings is

   subtype UTF8 is Ada.Strings.UTF_Encoding.UTF_8_String;
   subtype Unicode is Wide_Wide_String;
   subtype Unbounded_Unicode is Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;

   function Trim
      (Str : Unicode;
       Ch  : Wide_Wide_Character)
      return Unicode;

   function Index
      (Source  : Unicode;
       Pattern : Unicode;
       From    : Positive)
       return Natural;

   function Replace
      (Str, Match, Subst : Unicode)
       return Unicode;

   function Replace_All
      (Str, Match, Subst : Unicode)
       return Unicode;

   procedure Append
      (Str : in out Unbounded_Unicode;
       Ch  : Wide_Wide_Character)
   renames Ada.Strings.Wide_Wide_Unbounded.Append;

   procedure Append
      (Left  : in out Unbounded_Unicode;
       Right : Unbounded_Unicode)
   renames Ada.Strings.Wide_Wide_Unbounded.Append;

   procedure Append
      (Left  : in out Unbounded_Unicode;
       Right : Unicode)
   renames Ada.Strings.Wide_Wide_Unbounded.Append;

   function Length
      (Str : Unbounded_Unicode)
      return Natural
   renames Ada.Strings.Wide_Wide_Unbounded.Length;

   function To_Unbounded
      (Str : Unicode)
      return Unbounded_Unicode
   renames Ada.Strings.Wide_Wide_Unbounded.To_Unbounded_Wide_Wide_String;

   function To_Unicode
      (Str : Unbounded_Unicode)
      return Unicode
   renames Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String;

   function Decode
      (Str : UTF8)
      return Unicode;

   function Encode
      (Str : Unicode)
      return UTF8;

   function Encode
      (Str : Unbounded_Unicode)
      return UTF8;

end Codesearch.Strings;
