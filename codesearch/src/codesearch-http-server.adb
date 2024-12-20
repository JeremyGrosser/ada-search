with Ada.Streams; use Ada.Streams;
with GNAT.Sockets; use GNAT.Sockets;
with Codesearch.Service;

package body Codesearch.HTTP.Server is

   Listen_Sock : Socket_Type;
   Running : Boolean := True;

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

   function Index
      (Req   : Request;
       First : Positive;
       Str   : String)
       return Natural
   is
   begin
      if Str'Length > (Req.End_Headers - First + 1) then
         return 0;
      end if;

      for I in First .. Req.End_Headers - Str'Length + 1 loop
         if Req.Item (I .. I + Str'Length - 1) = Str then
            return I;
         end if;
      end loop;
      return 0;
   end Index;

   Parse_Error : exception;

   procedure Parse_Request
      (Req : in out Request)
   is
      CRLF : constant String := ASCII.CR & ASCII.LF;
   begin
      Req.End_Headers := Req.Item'Last;
      Req.End_Headers := Index (Req, Req.Item'First, CRLF & CRLF);
      if Req.End_Headers = 0 then
         --  Don't start parsing the request until we have all the headers
         return;
      else
         Req.End_Headers := Req.End_Headers + 2;
         --  Leave one CRLF in the buffer to make header parsing easier
      end if;

      Req.Method.First := Req.Item'First;
      Req.Method.Last := Index (Req, Req.Method.First, " ") - 1;

      Req.Target.First := Req.Method.Last + 2;
      Req.Target.Last := Index (Req, Req.Target.First, " ") - 1;

      Req.Protocol.First := Req.Target.Last + 2;
      Req.Protocol.Last := Index (Req, Req.Protocol.First, CRLF) - 1;

      declare
         Name, Value : Span;
         I : Natural := Req.Protocol.Last + 3;
      begin
         loop
            Name.First := I;
            I := Index (Req, Name.First, ":");
            exit when I = 0;
            Name.Last := I - 1;

            Value.First := I + 1;
            if Req.Item (Value.First) = ' ' then --  consume optional whitespace
               Value.First := Value.First + 1;
            end if;
            I := Index (Req, Value.First, CRLF);
            if I = 0 then
               raise Parse_Error with "Missing CRLF after header value";
            end if;
            Value.Last := I - 1;
            I := I + 2;
            Request_Header_Maps.Include (Req.Headers, Get_String (Req, Name), Value);
         end loop;
      end;
   end Parse_Request;

   procedure Serve_Connection
      (Sock : Socket_Type;
       DB   : Codesearch.Database.Session)
   is
      Req  : Request;
      Item : Stream_Element_Array (1 .. Stream_Element_Offset (Req.Item'Last))
         with Address => Req.Item'Address;
      Last : Stream_Element_Offset := 0;
   begin
      loop
         Receive_Socket (Sock, Item (Last + 1 .. Item'Last), Last);
         exit when Last = 0;
         Req.Last := Natural (Last);
         Parse_Request (Req);
         exit when Req.End_Headers > 0;
      end loop;

      declare
         Resp : Response;
      begin
         Resp.Socket := Sock;
         Codesearch.Service.Handle_Request (Req, Resp, DB);
      end;
      Close_Socket (Sock);
   exception
      when Parse_Error =>
         Close_Socket (Sock);
   end Serve_Connection;

   procedure Run
      (DB : Codesearch.Database.Session)
   is
      Addr        : Sock_Addr_Type;
      Client_Sock : Socket_Type;
   begin
      while Running loop
         begin
            Accept_Socket (Listen_Sock, Client_Sock, Addr);
            Serve_Connection (Client_Sock, DB);
         exception
            when Socket_Error =>
               null;
         end;
      end loop;
   end Run;

   procedure Stop is
   begin
      Close_Socket (Listen_Sock);
      Running := False;
   end Stop;

end Codesearch.HTTP.Server;
