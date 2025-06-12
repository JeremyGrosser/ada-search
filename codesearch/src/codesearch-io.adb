with Interfaces;

package body Codesearch.IO is

   procedure Initialize
      (This : out IO_Context)
   is
   begin
      This.EP := Epoll.Create;
   end Initialize;

   procedure Register
      (This          : in out IO_Context;
       Desc          : Descriptor;
       Readable      : Event_Callback;
       Writable      : Event_Callback;
       Error         : Event_Callback;
       User_Context  : System.Address)
   is
      use Descriptor_Maps;
      Event : aliased Epoll.Epoll_Event;
   begin
      Event.Flags :=
         (Readable => Readable /= null,
          Writable => Writable /= null,
          Error    => Error /= null,
          Hang_Up  => Error /= null,
          others   => False);
      Event.Data := Interfaces.Unsigned_64 (Desc);
      Epoll.Control (This.EP, Desc, Epoll.Add, Event'Access);

      if Contains (This.Descriptors, Desc) then
         Reference (This.Descriptors, Desc) := Event_Callbacks'
            (Readable      => Readable,
             Writable      => Writable,
             Error         => Error,
             User_Context  => User_Context);
      else
         Insert (This.Descriptors, Desc,
            (Readable      => Readable,
             Writable      => Writable,
             Error         => Error,
             User_Context  => User_Context));
      end if;
   end Register;

   procedure Set_Triggers
      (This    : in out IO_Context;
       Desc    : Descriptor;
       Readable, Writable, Error : Boolean)
   is
      Event : aliased Epoll.Epoll_Event :=
         (Flags =>
            (Readable => Readable,
             Writable => Writable,
             Error    => Error,
             Hang_Up  => Error,
             others   => False),
          Data => Interfaces.Unsigned_64 (Desc));
   begin
      Epoll.Control (This.EP, Desc, Epoll.Modify, Event'Access);
   end Set_Triggers;

   procedure Unregister
      (This : in out IO_Context;
       Desc : Descriptor)
   is
   begin
      Epoll.Control (This.EP, Desc, Epoll.Delete, null);
   end Unregister;

   procedure Poll_Events
      (This : in out IO_Context)
   is
      use Descriptor_Maps;
   begin
      for Event of Epoll.Wait (This.EP, Timeout => 1, Max_Events => 8) loop
         declare
            Desc : constant Descriptor := Descriptor
               (Integer (Event.Data));
            Callbacks : Event_Callbacks;
         begin
            if Contains (This.Descriptors, Desc) then
               Callbacks := Element (This.Descriptors, Desc);
               if Event.Flags.Readable and then
                  Callbacks.Readable /= null
               then
                  Callbacks.Readable.all (Desc, Callbacks.User_Context);
               end if;
               if Event.Flags.Writable and then
                  Callbacks.Writable /= null
               then
                  Callbacks.Writable.all (Desc, Callbacks.User_Context);
               end if;
               if (Event.Flags.Error or else Event.Flags.Hang_Up) and then
                  Callbacks.Error /= null
               then
                  Callbacks.Error.all (Desc, Callbacks.User_Context);
               end if;
            else
               raise Program_Error with "epoll_wait returned event for unknown descriptor";
            end if;
         end;
      end loop;
   end Poll_Events;

   procedure Run
      (This : in out IO_Context)
   is
   begin
      loop
         Poll_Events (This);
      end loop;
   end Run;

end Codesearch.IO;
