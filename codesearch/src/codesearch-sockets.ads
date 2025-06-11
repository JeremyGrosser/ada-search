with Ada.Streams;
with Interfaces.C;

package Codesearch.Sockets is

   Socket_Error : exception;

   subtype Socket_Type is Interfaces.C.int;

   type UInt8 is mod 2 ** 8
      with Size => 8;
   type UInt16 is mod 2 ** 16
      with Size => 16;
   type UInt32 is mod 2 ** 32
      with Size => 32;

   type Inet_Addr is array (1 .. 16) of UInt8
      with Size => 128;
   type Inet_Port is mod 2 ** 16
      with Size => 16;

   AF_INET6 : constant := 10;

   type Sock_Addr is record
      Family   : UInt16 := AF_INET6;
      Port     : Inet_Port := 0;
      Flowinfo : UInt32 := 0;
      Addr     : Inet_Addr := (others => 0);
      Scope_Id : UInt32 := 0;
   end record
      with Convention => C;

   type Socket_Option is
      (Reuse_Address, Reuse_Port);
   for Socket_Option use
      (Reuse_Address => 2,
       Reuse_Port => 15);

   procedure Create_Socket
      (Sock : out Socket_Type);

   procedure Set_Socket_Option
      (Sock   : Socket_Type;
       Option : Socket_Option;
       Set    : Boolean);

   procedure Bind_Socket
      (Sock : Socket_Type;
       Addr : Sock_Addr);

   procedure Listen_Socket
      (Sock : Socket_Type;
       Backlog : Natural);

   procedure Accept_Socket
      (Server : Socket_Type;
       Sock   : out Socket_Type;
       Addr   : out Sock_Addr);

   procedure Close_Socket
      (Sock : Socket_Type);

   procedure Receive_Socket
      (Sock : Socket_Type;
       Data : out Ada.Streams.Stream_Element_Array;
       Last : out Ada.Streams.Stream_Element_Offset);

   procedure Send_Socket
      (Sock : Socket_Type;
       Data : Ada.Streams.Stream_Element_Array;
       Last : out Ada.Streams.Stream_Element_Offset);

end Codesearch.Sockets;
