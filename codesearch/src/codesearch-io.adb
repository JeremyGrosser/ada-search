with Ada.Containers.Ordered_Maps;
with Interfaces;
with Epoll;

package body Codesearch.IO is

   EP : constant Epoll.Epoll_Descriptor := Epoll.Create;

   type Event_Callbacks is record
      Readable, Writable, Error : Event_Callback;
   end record;

   function "<" (Left, Right : Descriptor)
      return Boolean
   is
      use GNAT.Sockets;
   begin
      return To_C (Left) < To_C (Right);
   end "<";

   package Descriptor_Maps is new Ada.Containers.Ordered_Maps
      (Key_Type      => Descriptor,
       Element_Type  => Event_Callbacks);

   Descriptors : Descriptor_Maps.Map;

   procedure Register
      (Desc     : Descriptor;
       Readable : Event_Callback;
       Writable : Event_Callback;
       Error    : Event_Callback)
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
      Event.Data := Interfaces.Unsigned_64 (GNAT.Sockets.To_C (Desc));
      Epoll.Control (EP, Desc, Epoll.Add, Event'Access);

      if Contains (Descriptors, Desc) then
         Reference (Descriptors, Desc) := Event_Callbacks'
            (Readable => Readable,
             Writable => Writable,
             Error    => Error);
      else
         Insert (Descriptors, Desc,
            (Readable => Readable,
             Writable => Writable,
             Error    => Error));
      end if;
   end Register;

   procedure Set_Triggers
      (Desc : Descriptor;
       Readable, Writable, Error : Boolean)
   is
      Event : aliased Epoll.Epoll_Event :=
         (Flags =>
            (Readable => Readable,
             Writable => Writable,
             Error    => Error,
             Hang_Up  => Error,
             others   => False),
          Data => Interfaces.Unsigned_64 (GNAT.Sockets.To_C (Desc)));
   begin
      Epoll.Control (EP, Desc, Epoll.Modify, Event'Access);
   end Set_Triggers;

   procedure Unregister
      (Desc : Descriptor)
   is
   begin
      Epoll.Control (EP, Desc, Epoll.Delete, null);
   end Unregister;

   procedure Poll_Events is
      use Descriptor_Maps;
   begin
      for Event of Epoll.Wait (EP, Timeout => 1, Max_Events => 8) loop
         declare
            Desc : constant Descriptor := Descriptor
               (GNAT.Sockets.To_Ada (Integer (Event.Data)));
            Callbacks : Event_Callbacks;
         begin
            if Contains (Descriptors, Desc) then
               Callbacks := Element (Descriptors, Desc);
               if Event.Flags.Readable and then
                  Callbacks.Readable /= null
               then
                  Callbacks.Readable.all (Desc);
               end if;
               if Event.Flags.Writable and then
                  Callbacks.Writable /= null
               then
                  Callbacks.Writable.all (Desc);
               end if;
               if (Event.Flags.Error or else Event.Flags.Hang_Up) and then
                  Callbacks.Error /= null
               then
                  Callbacks.Error.all (Desc);
               end if;
            else
               raise Program_Error with "epoll_wait returned event for unknown descriptor";
            end if;
         end;
      end loop;
   end Poll_Events;

   procedure Run is
   begin
      loop
         Poll_Events;
      end loop;
   end Run;

end Codesearch.IO;
