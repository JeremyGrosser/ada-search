private with Ada.Containers.Ordered_Maps;
private with Ada.Containers.Ordered_Sets;
private with Ada.Calendar;
private with Epoll;

with Codesearch.Sockets;

package Codesearch.IO is

   type IO_Context is private;

   subtype Descriptor is Codesearch.Sockets.Socket_Type;

   type Event_Callback is access procedure
      (This : in out IO_Context;
       Desc : Descriptor);

   procedure Initialize
      (This : out IO_Context);

   procedure Register
      (This     : in out IO_Context;
       Desc     : Descriptor;
       Readable : Event_Callback;
       Writable : Event_Callback;
       Error    : Event_Callback);

   procedure Set_Triggers
      (This    : in out IO_Context;
       Desc    : Descriptor;
       Readable, Writable, Error : Boolean);

   procedure Unregister
      (This : in out IO_Context;
       Desc : Descriptor);

   procedure Set_Timeout
      (This     : in out IO_Context;
       After    : Duration;
       Callback : Event_Callback;
       Desc     : Descriptor);

   procedure Run
      (This : in out IO_Context);

private

   type Event_Callbacks is record
      Readable, Writable, Error : Event_Callback;
   end record;

   use type Codesearch.Sockets.Socket_Type;
   package Descriptor_Maps is new Ada.Containers.Ordered_Maps
      (Key_Type      => Descriptor,
       Element_Type  => Event_Callbacks);

   type Timer is record
      Expires_At  : Ada.Calendar.Time;
      Callback    : Event_Callback;
      Desc        : Descriptor;
   end record;

   function "<" (Left, Right : Timer)
      return Boolean;

   package Timer_Sets is new Ada.Containers.Ordered_Sets
      (Element_Type => Timer);

   type IO_Context is record
      EP          : Epoll.Epoll_Descriptor;
      Descriptors : Descriptor_Maps.Map;
      Timers      : Timer_Sets.Set;
   end record;

end Codesearch.IO;
