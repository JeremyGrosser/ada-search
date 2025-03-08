with GNAT.Sockets;

package Codesearch.IO is

   subtype Descriptor is GNAT.Sockets.Socket_Type;

   type Event_Callback is access procedure
      (Desc : Descriptor);

   type Timer_Callback is access procedure;

   procedure Register
      (Desc     : Descriptor;
       Readable : Event_Callback;
       Writable : Event_Callback;
       Error    : Event_Callback);

   procedure Unregister
      (Desc : Descriptor);

   procedure Set_Timeout
      (After    : Duration;
       Callback : Timer_Callback);

   procedure Run;

end Codesearch.IO;
