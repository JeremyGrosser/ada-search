private with Ada.Containers.Ordered_Maps;
private with Epoll;

with Codesearch.Sockets;
with System;

package Codesearch.IO is

   type IO_Context is private;

   subtype Descriptor is Codesearch.Sockets.Socket_Type;

   type Event_Callback is access procedure
      (Desc : Descriptor;
       User_Context : System.Address);

   procedure Initialize
      (This : out IO_Context);

   procedure Register
      (This          : in out IO_Context;
       Desc          : Descriptor;
       Readable      : Event_Callback;
       Writable      : Event_Callback;
       Error         : Event_Callback;
       User_Context  : System.Address);

   procedure Set_Triggers
      (This    : in out IO_Context;
       Desc    : Descriptor;
       Readable, Writable, Error : Boolean);

   procedure Unregister
      (This : in out IO_Context;
       Desc : Descriptor);

   procedure Run
      (This : in out IO_Context);

private

   type Event_Callbacks is record
      Readable, Writable, Error : Event_Callback;
      User_Context : System.Address;
   end record;

   use type Codesearch.Sockets.Socket_Type;
   package Descriptor_Maps is new Ada.Containers.Ordered_Maps
      (Key_Type      => Descriptor,
       Element_Type  => Event_Callbacks);

   type IO_Context is record
      EP          : Epoll.Epoll_Descriptor;
      Descriptors : Descriptor_Maps.Map;
   end record;

end Codesearch.IO;
