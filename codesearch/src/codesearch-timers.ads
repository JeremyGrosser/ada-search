private with Ada.Containers.Vectors;
private with Ada.Real_Time;
with Codesearch.Sockets;

package Codesearch.Timers is

   subtype Context_Type is Codesearch.Sockets.Socket_Type;

   type Timer_Callback is access procedure
      (Context : Context_Type);

   type Timer_Wheel is private;

   procedure Set_Timeout
     (Wheel       : in out Timer_Wheel;
      Timeout_Sec : Natural;
      Callback    : Timer_Callback;
      Context     : Context_Type);

   procedure Tick
      (Wheel : in out Timer_Wheel);

private

   type Timer is record
      Callback : Timer_Callback;
      Context  : Context_Type;
   end record;

   type Timer_Id is new Positive;

   package Timer_Vectors is new Ada.Containers.Vectors
      (Timer_Id, Timer);

   Wheel_Size : constant := 64;
   type Slot_Index is mod Wheel_Size;
   type Slots_Array is array (Slot_Index) of Timer_Vectors.Vector;

   type Timer_Wheel is record
      Slots : Slots_Array := (others => Timer_Vectors.Empty_Vector);
      Current_Slot : Slot_Index := 0;
      Start_Time : Ada.Real_Time.Time;
   end record;
end Codesearch.Timers;
