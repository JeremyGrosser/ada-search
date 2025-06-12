with System;

package body Codesearch.Sockets is
   use Interfaces.C;

   function Errno
      return Integer
   with Import, Convention => C, External_Name => "__get_errno";
   --  implemented by libgnarl

   subtype ssize_t is int;

   function htons
      (Host_Short : Inet_Port)
      return Inet_Port
   with Import, Convention => C, External_Name => "htons";

   procedure Create_Socket
      (Sock : out Socket_Type)
   is
      SOCK_STREAM : constant := 1;
      function C_socket
         (domain, typ, protocol : int)
         return int
      with Import, Convention => C, External_Name => "socket";
   begin
      Sock := Socket_Type (C_socket (AF_INET6, SOCK_STREAM, 0));
   end Create_Socket;

   procedure Set_Socket_Option
      (Sock   : Socket_Type;
       Option : Socket_Option;
       Set    : Boolean)
   is
      SOL_SOCKET : constant := 1;

      function C_setsockopt
         (sockfd  : Socket_Type;
          level   : int;
          optname : int;
          optval  : System.Address;
          optlen  : size_t)
          return int
      with Import, Convention => C, External_Name => "setsockopt";

      Val : aliased int := (if Set then 1 else 0);
      Result : int;
   begin
      Result := C_setsockopt
         (sockfd  => Sock,
          level   => SOL_SOCKET,
          optname => int (Socket_Option'Enum_Rep (Option)),
          optval  => Val'Address,
          optlen  => size_t (Val'Size / 8));
      if Result = -1 then
         raise Socket_Error with Errno'Image;
      end if;
   end Set_Socket_Option;

   procedure Bind_Socket
      (Sock : Socket_Type;
       Addr : Sock_Addr)
   is
      function C_bind
         (sockfd   : Socket_Type;
          sockaddr : access Sock_Addr;
          addrlen  : UInt32)
          return int
      with Import, Convention => C, External_Name => "bind";

      Copy : aliased Sock_Addr := Addr;
      Result : int;
   begin
      Copy.Port := htons (Copy.Port);
      Result := C_bind (Sock, Copy'Access, Copy'Size / 8);
      if Result /= 0 then
         raise Socket_Error with Errno'Image;
      end if;
   end Bind_Socket;

   procedure Listen_Socket
      (Sock : Socket_Type;
       Backlog : Natural)
   is
      function C_listen
         (sockfd  : Socket_Type;
          backlog : int)
          return int
      with Import, Convention => C, External_Name => "listen";

      Result : int;
   begin
      Result := C_listen (Sock, int (Backlog));
      if Result /= 0 then
         raise Socket_Error with Errno'Image;
      end if;
   end Listen_Socket;

   procedure Accept_Socket
      (Server : Socket_Type;
       Sock   : out Socket_Type;
       Addr   : out Sock_Addr)
   is
      function C_accept
         (sockfd  : Socket_Type;
          addr    : System.Address;
          addrlen : System.Address)
          return int
      with Import, Convention => C, External_Name => "accept";

      Copy : aliased Sock_Addr;
      Len  : aliased int;
      Result : int;
   begin
      Result := C_accept (Server, Copy'Address, Len'Address);
      if Result = -1 then
         raise Socket_Error with Errno'Image;
      else
         Sock := Socket_Type (Result);
         Addr := Copy;
      end if;
   end Accept_Socket;

   procedure Close_Socket
      (Sock : Socket_Type)
   is
      function C_close
         (fd : Socket_Type)
         return int
      with Import, Convention => C, External_Name => "close";

      Result : int with Unreferenced;
   begin
      Result := C_close (Sock);
      --  We don't actually care if close failed.
      --
      --  if Result /= 0 then
      --     raise Socket_Error with Errno'Image;
      --  end if;
   end Close_Socket;

   procedure Receive_Socket
      (Sock : Socket_Type;
       Data : out Ada.Streams.Stream_Element_Array;
       Last : out Ada.Streams.Stream_Element_Offset)
   is
      function C_recv
         (sockfd : Socket_Type;
          buf    : System.Address;
          len    : size_t;
          flags  : int)
          return ssize_t
      with Import, Convention => C, External_Name => "recv";

      use type Ada.Streams.Stream_Element_Offset;
      Result : int;
   begin
      Result := C_recv (Sock, Data'Address, size_t (Data'Length), 0);
      if Result = -1 then
         raise Socket_Error with Errno'Image;
      else
         Last := Data'First + Ada.Streams.Stream_Element_Offset (Result) - 1;
      end if;
   end Receive_Socket;

   procedure Send_Socket
      (Sock : Socket_Type;
       Data : Ada.Streams.Stream_Element_Array;
       Last : out Ada.Streams.Stream_Element_Offset)
   is
      MSG_NOSIGNAL : constant := 16#4000#; --  no SIGPIPE if connection is closed

      function C_send
         (sockfd : Socket_Type;
          buf    : System.Address;
          len    : size_t;
          flags  : int)
          return int
      with Import, Convention => C, External_Name => "send";

      use type Ada.Streams.Stream_Element_Offset;
      Result : int;
   begin
      Result := C_send
         (sockfd => Sock,
          buf    => Data'Address,
          len    => size_t (Data'Length),
          flags  => MSG_NOSIGNAL);
      if Result = -1 then
         Last := Data'First - 1;
         if Errno = 32 then
            --  Client closed connection
            return;
         else
            raise Socket_Error with Errno'Image;
         end if;
      else
         Last := Data'First + Ada.Streams.Stream_Element_Offset (Result);
      end if;
   end Send_Socket;

end Codesearch.Sockets;
