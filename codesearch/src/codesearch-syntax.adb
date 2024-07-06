with AAA.Processes;
with AAA.Strings;

with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Directories;

package body Codesearch.Syntax is

   function Highlight
      (Text : String)
      return String
   is
      use AAA.Strings;
      Tmpfile : constant String := "highlight.tmp";
      Cmd : Vector := Empty_Vector;
      Result : AAA.Processes.Result;
   begin
      Cmd := Cmd & "pygmentize" &
         "-f" & "html" &
         "-l" & "ada" &
         "-O" & "style=tango,linenos=1,cssclass=source" &
         "-o" & Tmpfile;
      Result := AAA.Processes.Run (Cmd, Input => Text);
      if Result.Exit_Code /= 0 then
         return "";
      end if;

      declare
         use Ada.Text_IO;
         use Ada.Text_IO.Text_Streams;
         Data : String (1 .. Natural (Ada.Directories.Size (Tmpfile)));
         File : File_Type;
      begin
         Open (File, In_File, Tmpfile);
         String'Read (Stream (File), Data);
         Close (File);
         return Data;
      end;
   end Highlight;
end Codesearch.Syntax;
