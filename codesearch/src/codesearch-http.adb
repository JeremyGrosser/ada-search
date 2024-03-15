with Ada.Environment_Variables;
with Ada.Text_IO;
with GNAT.CGI;

package body Codesearch.HTTP is
   package Env renames Ada.Environment_Variables;

   function Method
      return String
   is (Env.Value ("REQUEST_METHOD", ""));

   function Path
      return String
   is (Env.Value ("PATH_INFO", ""));

   function Query_Parameter
      (Key     : String;
       Default : String := "")
       return String
   is (GNAT.CGI.Value (Key));

   procedure Set_Header
      (Key, Value : String)
   is
   begin
      GNAT.CGI.Put_Header (Key & ": " & Value);
   end Set_Header;

   procedure Set_Status
      (Code    : HTTP_Status;
       Message : String)
   is
   begin
      GNAT.CGI.Put_Header ("Status:" & Code'Image & " " & Message);
   end Set_Status;

   procedure Put
      (Data : String)
   is
   begin
      Ada.Text_IO.Put (Data);
   end Put;

end Codesearch.HTTP;
