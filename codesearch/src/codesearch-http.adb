with Ada.Environment_Variables;
with Ada.Text_IO;
with GNAT.CGI;
with URI;

package body Codesearch.HTTP is
   package Env renames Ada.Environment_Variables;

   End_Headers : Boolean := False;

   function Method
      return String
   is (Env.Value ("REQUEST_METHOD", ""));

   function Path
      return String
   is (URI.Normalize_Path (Env.Value ("DOCUMENT_URI", "")));

   function Query_Parameter
      (Key     : String;
       Default : String := "")
       return String
   is (GNAT.CGI.Value (Key));

   procedure Set_Header
      (Key, Value : String)
   is
   begin
      Ada.Text_IO.Put (Key);
      Ada.Text_IO.Put (": ");
      Ada.Text_IO.Put (Value);
      Ada.Text_IO.New_Line;
   end Set_Header;

   procedure Set_Status
      (Code    : HTTP_Status;
       Message : String)
   is
   begin
      End_Headers := False;
      Ada.Text_IO.Put ("Status:");
      Ada.Text_IO.Put (Code'Image);
      Ada.Text_IO.Put (' ');
      Ada.Text_IO.Put (Message);
      Ada.Text_IO.New_Line;
   end Set_Status;

   procedure Put
      (Data : String)
   is
   begin
      if not End_Headers then
         Ada.Text_IO.New_Line;
         End_Headers := True;
      end if;
      Ada.Text_IO.Put (Data);
   end Put;

end Codesearch.HTTP;
