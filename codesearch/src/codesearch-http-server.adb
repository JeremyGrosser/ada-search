with Ada.Streams; use Ada.Streams;
with Ada.Containers.Vectors;
with GNAT.Sockets; use GNAT.Sockets;
with Codesearch.Service;
with Codesearch.Database;
with Ada.Text_IO;
with Ada.Real_Time;

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
      Listen_Socket (Listen_Sock, 256);
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

   package Socket_Vectors is new Ada.Containers.Vectors (Positive, Socket_Type);

   protected Queue is
      procedure Put
         (Socket : Socket_Type);
      entry Get
         (Socket : out Socket_Type);

      procedure Start;
      procedure Stop;

      function Is_Running
         return Boolean;
   private
      Q : Socket_Vectors.Vector;
      Running : Boolean := False;
   end Queue;

   protected body Queue is
      procedure Put
         (Socket : Socket_Type)
      is
      begin
         Socket_Vectors.Append (Q, Socket);
      end Put;

      entry Get
         (Socket : out Socket_Type)
      when not Socket_Vectors.Is_Empty (Q)
      is
      begin
         Socket := Socket_Vectors.First_Element (Q);
         Socket_Vectors.Delete_First (Q);
      end Get;

      procedure Start is
      begin
         Running := True;
      end Start;

      procedure Stop is
      begin
         Running := False;
      end Stop;

      function Is_Running return Boolean is (Running);
   end Queue;

   task type Worker;

   task body Worker is
      DB   : Codesearch.Database.Session;
      Sock : Socket_Type;

      use Ada.Real_Time;
      Start : Time;
      Elapsed : Duration;
   begin
      while not Queue.Is_Running loop
         delay 0.1;
      end loop;

      DB := Codesearch.Database.Open (Read_Only => True);

      while Queue.Is_Running loop
         begin
            Queue.Get (Sock);
            Start := Clock;
            Serve_Connection (Sock, DB);
            Elapsed := To_Duration (Clock - Start);
            Ada.Text_IO.Put_Line ("Complete in " & Elapsed'Image);
         exception
            when Socket_Error =>
               null;
         end;
      end loop;
      Codesearch.Database.Close (DB);
   end Worker;

   procedure Run is
      Pool : array (1 .. 4) of Worker;
      Addr : Sock_Addr_Type;
      Sock : Socket_Type;
   begin
      Queue.Start;
      loop
         Accept_Socket (Listen_Sock, Sock, Addr);
         Queue.Put (Sock);
      end loop;
   exception
      when Socket_Error =>
         null;
   end Run;

   procedure Stop is
   begin
      Close_Socket (Listen_Sock);
      Queue.Stop;
   end Stop;

end Codesearch.HTTP.Server;
