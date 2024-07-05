with AAA.Processes;
with AAA.Strings;

package body Codesearch.Syntax is

   function Highlight
      (Text : String)
      return String
   is
      use AAA.Strings;
      Cmd : Vector := Empty_Vector;
      Result : AAA.Processes.Result;
   begin
      Cmd := Cmd & "pygmentize" &
         "-f" & "html" &
         "-l" & "ada" &
         "-O" & "style=tango,linenos=1,cssclass=source";
      Result := AAA.Processes.Run (Cmd, Input => Text);
      return Flatten (Result.Output, ASCII.LF);
   end Highlight;
end Codesearch.Syntax;
