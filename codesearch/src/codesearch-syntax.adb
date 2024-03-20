with AAA.Processes;
with AAA.Strings;

package body Codesearch.Syntax is

   function Highlight
      (Filename : Codesearch.File.Path)
      return Codesearch.Strings.UTF8
   is
      use AAA.Strings;
      Cmd : Vector := Empty_Vector;
      Result : AAA.Processes.Result;
   begin
      Cmd := Cmd & "pygmentize" &
         "-f" & "html" &
         "-l" & "ada" &
         "-O" & "style=tango,linenos=1,cssclass=source" &
         Codesearch.File.To_String (Filename);
      Result := AAA.Processes.Run (Cmd);
      return Codesearch.Strings.UTF8 (Flatten (Result.Output, ASCII.LF));
   end Highlight;
end Codesearch.Syntax;
