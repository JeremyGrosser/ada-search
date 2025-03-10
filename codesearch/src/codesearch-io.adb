with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Calendar;
with Ada.Text_IO;
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

   type Timer is record
      Expires_At  : Ada.Calendar.Time;
      Callback    : Event_Callback;
      Desc        : Descriptor;
   end record;

   function "<" (Left, Right : Timer)
      return Boolean
   is
      use Ada.Calendar;
   begin
      return Left.Expires_At < Right.Expires_At;
   end "<";

   package Timer_Sets is new Ada.Containers.Ordered_Sets
      (Element_Type => Timer);
   Timers : Timer_Sets.Set;

   procedure Set_Timeout
      (After    : Duration;
       Callback : Event_Callback;
       Desc     : Descriptor)
   is
      use Ada.Calendar;
      T : Timer;
   begin
      T.Expires_At := Clock + After;
      T.Callback := Callback;
      T.Desc := Desc;
      Timer_Sets.Include (Timers, T);
   end Set_Timeout;

   procedure Poll_Timers is
      use Timer_Sets;
      use Ada.Calendar;
      Now : constant Time := Clock;
      T : Timer;
   begin
      loop
         exit when Is_Empty (Timers);
         T := First_Element (Timers);
         exit when T.Expires_At > Now;
         Delete_First (Timers);
         T.Callback.all (T.Desc);
      end loop;
   end Poll_Timers;

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
         Poll_Timers;
         Poll_Events;
      end loop;
   end Run;

   procedure Print_Status is
      use Ada.Text_IO;
   begin
      Put ("Descriptors: ");
      Put (Descriptor_Maps.Length (Descriptors)'Image);
      New_Line;

      Put ("Timers: ");
      Put (Timer_Sets.Length (Timers)'Image);
      New_Line;
   end Print_Status;

end Codesearch.IO;
