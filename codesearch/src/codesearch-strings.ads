with VSS.Strings;

package Codesearch.Strings is
   function UTF8_Decode
      (Str : String)
      return VSS.Strings.Virtual_String;

   function UTF8_Encode
      (Str : VSS.Strings.Virtual_String)
      return String;
end Codesearch.Strings;
