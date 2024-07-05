with Codesearch.Strings;
private with Ada.Streams;
private with GNAT.Sockets;

package Codesearch.HTTP
   with Elaborate_Body
is

   type Request is private;

   --  Request
   function Method
      (This : Request)
      return String;

   function Path
      (This : Request)
      return String;

   function Query_Parameter
      (This    : Request;
       Key     : String;
       Default : String := "")
       return String;

   type Response is private;

   procedure Set_Header
      (This : in out Response;
       Key, Value : String);

   type HTTP_Status is new Integer range 100 .. 599;

   procedure Set_Status
      (This    : in out Response;
       Code    : HTTP_Status;
       Message : String);

   procedure Put
      (This : in out Response;
       Data : Codesearch.Strings.Unicode);

   procedure Put_Raw
      (This : in out Response;
       Data : String);

private

   Max_Request_Length : constant := 65536;

   type Request is record
      Item : Ada.Streams.Stream_Element_Array (1 .. Max_Request_Length);
      Last : Ada.Streams.Stream_Element_Offset := 0;
      End_Headers : Ada.Streams.Stream_Element_Offset := 0;
   end record;

   type Response is record
      Socket : GNAT.Sockets.Socket_Type;
   end record;

end Codesearch.HTTP;
