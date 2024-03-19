with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

package body Codesearch.Strings is

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
