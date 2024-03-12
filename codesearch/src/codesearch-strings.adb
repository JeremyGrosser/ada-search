with Ada.Strings.UTF_Encoding;
with VSS.Strings.Conversions;

package body Codesearch.Strings is

   function UTF8_Decode
      (Str : String)
      return VSS.Strings.Virtual_String
   is (VSS.Strings.Conversions.To_Virtual_String (Ada.Strings.UTF_Encoding.UTF_8_String (Str)));

   function UTF8_Encode
      (Str : VSS.Strings.Virtual_String)
      return String
   is (String (VSS.Strings.Conversions.To_UTF_8_String (Str)));

end Codesearch.Strings;
