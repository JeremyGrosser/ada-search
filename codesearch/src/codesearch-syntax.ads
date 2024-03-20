with Codesearch.File;
with Codesearch.Strings;

package Codesearch.Syntax is

   function Highlight
      (Filename : Codesearch.File.Path)
      return Codesearch.Strings.UTF8;

end Codesearch.Syntax;
