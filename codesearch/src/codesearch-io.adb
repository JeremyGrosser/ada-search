with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Maps;
with Ada.Real_Time;
with Interfaces;
with Epoll;

package body Codesearch.IO is

   EP : constant Epoll.Epoll_Descriptor := Epoll.Create;

   type Timer is record
      Callback    : Timer_Callback;
      Expires_At  : Ada.Real_Time.Time;
   end record;

   type Event_Callbacks is record
      Readable, Writable, Error : Event_Callback;
   end record;

   function "<" (Left, Right : Timer)
      return Boolean
   is
      use Ada.Real_Time;
   begin
      return Left.Expires_At < Right.Expires_At;
   end "<";

   function "<" (Left, Right : Descriptor)
      return Boolean
   is
      use GNAT.Sockets;
   begin
      return To_C (Left) < To_C (Right);
   end "<";

   package Timer_Sets is new Ada.Containers.Ordered_Sets
      (Element_Type  => Timer);

   package Descriptor_Maps is new Ada.Containers.Ordered_Maps
      (Key_Type      => Descriptor,
       Element_Type  => Event_Callbacks);

   Timers      : Timer_Sets.Set;
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

   procedure Unregister
      (Desc : Descriptor)
   is
   begin
      Epoll.Control (EP, Desc, Epoll.Delete, null);
   end Unregister;

   procedure Set_Timeout
      (After    : Duration;
       Callback : Timer_Callback)
   is
      use Ada.Real_Time;
   begin
      Timer_Sets.Insert (Timers, Timer'(
         Callback   => Callback,
         Expires_At => Clock + To_Time_Span (After)));
   end Set_Timeout;

   procedure Run_Timers is
      use Ada.Real_Time;
      use Timer_Sets;
      Now : constant Time := Clock;
      T : Timer;
   begin
      loop
         exit when Is_Empty (Timers);
         T := First_Element (Timers);
         exit when T.Expires_At > Now;
         Delete_First (Timers);
         T.Callback.all;
      end loop;
   end Run_Timers;

   function Next_Event
      return Ada.Real_Time.Time
   is
      use Ada.Real_Time;
      use Timer_Sets;
   begin
      if Is_Empty (Timers) then
         return Time_Last;
      else
         return First_Element (Timers).Expires_At;
      end if;
   end Next_Event;

   procedure Poll_Events is
      use Ada.Real_Time;
      use Descriptor_Maps;
      Next    : Time;
      Timeout : Duration;
   begin
      Next := Next_Event;
      if Next = Time_Last then
         Timeout := 1.0;
      else
         Timeout := To_Duration (Next - Clock);
      end if;

      for Event of Epoll.Wait (EP, Timeout => Integer (Timeout), Max_Events => 64) loop
         declare
            Desc : constant Descriptor := Descriptor
               (GNAT.Sockets.To_Ada (Integer (Event.Data)));
            Callbacks : Event_Callbacks;
         begin
            if Contains (Descriptors, Desc) then
               Callbacks := Element (Descriptors, Desc);
               if Event.Flags.Readable and then Callbacks.Readable /= null then
                  Callbacks.Readable.all (Desc);
               end if;
               if Event.Flags.Writable and then Callbacks.Writable /= null then
                  Callbacks.Writable.all (Desc);
               end if;
               if Event.Flags.Error and then Callbacks.Error /= null then
                  Callbacks.Error.all (Desc);
               end if;
            end if;
         end;
      end loop;
   end Poll_Events;

   procedure Run is
   begin
      loop
         Run_Timers;
         Poll_Events;
      end loop;
   end Run;

end Codesearch.IO;
