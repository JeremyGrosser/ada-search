package body Codesearch.Timers is

   use Timer_Vectors;

   procedure Initialize
      (Wheel : in out Timer_Wheel)
   is
   begin
      --  Clear all slots
      for I in Wheel.Slots'Range loop
         Clear (Wheel.Slots (I));
      end loop;

      Wheel.Current_Slot := 0;
      Wheel.Start_Time := Ada.Real_Time.Clock;
   end Initialize;

   procedure Set_Timeout
     (Wheel       : in out Timer_Wheel;
      Timeout_Sec : Natural;
      Callback    : Timer_Callback;
      Context     : Context_Type)
   is
      Slot : constant Slot_Index := Wheel.Current_Slot + Slot_Index (Timeout_Sec);
      New_Timer : constant Timer :=
        (Callback => Callback,
         Context  => Context);
   begin
      Append (Wheel.Slots (Slot), New_Timer);
   end Set_Timeout;

   procedure Tick
      (Wheel : in out Timer_Wheel)
   is
      Current_Slot_Timers : Timer_Vectors.Vector renames Wheel.Slots (Wheel.Current_Slot);
   begin
      --  Process all timers in current slot
      for Cursor in Iterate (Current_Slot_Timers) loop
         declare
            T : constant Reference_Type := Reference (Current_Slot_Timers, Cursor);
         begin
            T.Callback.all (T.Context);
         end;
      end loop;
      Clear (Current_Slot_Timers);

      --  Clear processed timers from current slot
      Clear (Current_Slot_Timers);

      --  Advance to next slot
      Wheel.Current_Slot := Wheel.Current_Slot + 1;
   end Tick;

end Codesearch.Timers;
