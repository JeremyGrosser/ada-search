with Codesearch.Strings;

package Codesearch.HTTP is

   --  Request
   function Method
      return String;

   function Path
      return String;

   function Query_Parameter
      (Key     : String;
       Default : String := "")
       return String;

   --  Response
   procedure Set_Header
      (Key, Value : String);

   type HTTP_Status is new Integer range 100 .. 599;

   procedure Set_Status
      (Code    : HTTP_Status;
       Message : String);

   procedure Put
      (Data : Codesearch.Strings.Unicode);

   procedure Put_Raw
      (Data : String);

end Codesearch.HTTP;
