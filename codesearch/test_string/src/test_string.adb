with Codesearch.Strings; use Codesearch.Strings;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;

procedure Test_String is
   Haystack : constant Unicode := "hello my name is {name}";
   Needle   : constant Unicode := "{name}";
begin
   Put_Line (Replace_All (Haystack, Needle, "needle"));
end Test_String;
