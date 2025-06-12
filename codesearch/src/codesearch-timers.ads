with Ada.Real_Time;
with Ada.Containers.Vectors;
with Codesearch.Sockets;

package Codesearch.Timers is

   subtype Context_Type is Codesearch.Sockets.Socket_Type;

   --  Maximum timeout in seconds
   Wheel_Size : constant := 64;

   type Timer_Callback is access procedure
      (Context : Context_Type);
   subtype Time is Ada.Real_Time.Time;

   type Timer is record
      Callback : Timer_Callback;
      Context  : Context_Type;
   end record;

   type Timer_Id is new Positive;

   package Timer_Vectors is new Ada.Containers.Vectors (Timer_Id, Timer);

   type Slot_Index is mod Wheel_Size;
   type Slots_Array is array (Slot_Index) of Timer_Vectors.Vector;

   type Timer_Wheel is record
      Slots        : Slots_Array;
      Current_Slot : Slot_Index := 0;
      Start_Time   : Time;
   end record;

   procedure Initialize
      (Wheel : in out Timer_Wheel);

   procedure Set_Timeout
     (Wheel       : in out Timer_Wheel;
      Timeout_Sec : Natural;
      Callback    : Timer_Callback;
      Context     : Context_Type);

   procedure Tick
      (Wheel : in out Timer_Wheel);

end Codesearch.Timers;
