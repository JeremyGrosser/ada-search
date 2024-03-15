with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Ordered_Maps;

package Codesearch.Web is

   package String_Maps is new Ada.Containers.Indefinite_Ordered_Maps
      (Key_Type     => String,
       Element_Type => String);

   type Response_Type is record
      Status   : Natural := 500;
      Headers  : String_Maps.Map := String_Maps.Empty_Map;
      Data     : Unbounded_String;
   end record;

   procedure Request
      (Path         : String;
       Query_String : String;
       Response     : out Response_Type);

end Codesearch.Web;
