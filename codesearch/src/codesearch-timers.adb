package body Codesearch.Timers is
   use Timer_Vectors;

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
