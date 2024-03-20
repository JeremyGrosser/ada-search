with Codesearch.Strings; use Codesearch.Strings;

package Codesearch.Template is

   Template_Error : exception;

   function Render
      (Template : Unicode;
       Values   : Unicode_Maps.Map)
       return Unicode;

end Codesearch.Template;
