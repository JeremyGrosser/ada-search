with Ada.Streams; use Ada.Streams;
with GNAT.Sockets; use GNAT.Sockets;
with Codesearch.Service;

package body Codesearch.HTTP.Server is

   Listen_Sock : Socket_Type;

   procedure Bind is
      Addr : constant Sock_Addr_Type :=
         (Addr => <>,
          Port => 9999,
          Family => Family_Inet);
   begin
      Create_Socket (Listen_Sock);
      Set_Socket_Option (Listen_Sock, Socket_Level, (Reuse_Address, True));
      Bind_Socket (Listen_Sock, Addr);
      Listen_Socket (Listen_Sock);
   end Bind;

   procedure Parse_Request
      (Req : in out Request)
   is
      CRLFCRLF : constant Stream_Element_Array := (16#0D#, 16#0A#, 16#0D#, 16#0A#);
   begin
      for I in Req.Item'First + 3 .. Req.Last loop
         if Req.Item (I - 3 .. I) = CRLFCRLF then
            Req.End_Headers := I;
            exit;
         end if;
      end loop;
   end Parse_Request;

   procedure Serve_Connection
      (Sock : Socket_Type)
   is
      Req  : Request;
      Last : Stream_Element_Offset;
   begin
      loop
         Receive_Socket (Sock, Req.Item (Req.Last + 1 .. Req.Item'Last), Last);
         exit when Last = 0 or else Req.Last = Req.Item'Last;
         Req.Last := Last;
         Parse_Request (Req);
         exit when Req.End_Headers > 0;
      end loop;

      declare
         Resp : Response;
      begin
         Resp.Socket := Sock;
         Codesearch.Service.Handle_Request (Req, Resp);
      end;
      Close_Socket (Sock);
   end Serve_Connection;

   procedure Run is
      Addr        : Sock_Addr_Type;
      Client_Sock : Socket_Type;
   begin
      loop
         Accept_Socket (Listen_Sock, Client_Sock, Addr);
         Serve_Connection (Client_Sock);
      end loop;
   end Run;

end Codesearch.HTTP.Server;
