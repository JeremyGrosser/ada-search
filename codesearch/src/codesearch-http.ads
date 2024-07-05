with Codesearch.Strings;

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

   type Request is null record;
   type Response is null record;

end Codesearch.HTTP;
