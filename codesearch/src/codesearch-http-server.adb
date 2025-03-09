--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: AGPL-3.0-or-later
--
with Ada.Streams; use Ada.Streams;
with GNAT.Sockets; use GNAT.Sockets;
with Ada.Containers.Ordered_Maps;
with Codesearch.Service;
with Codesearch.Database;
with Codesearch.IO;

package body Codesearch.HTTP.Server is

   DB : Codesearch.Database.Session;

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
      I, Next : Natural;
   begin
      --  Find the end of headers (double CRLF)
      Req.End_Headers := Req.Item'Last;
      Req.End_Headers := Index (Req, Req.Item'First, CRLF & CRLF);
      if Req.End_Headers = 0 then
         --  Don't start parsing the request until we have all the headers
         return;
      else
         Req.End_Headers := Req.End_Headers + 2;
         --  Leave one CRLF in the buffer to make header parsing easier
      end if;

      --  Parse request line (METHOD TARGET PROTOCOL)
      Req.Method.First := Req.Item'First;
      I := Index (Req, Req.Method.First, " ");
      if I = 0 then
         raise Parse_Error with "Invalid request line: missing space after method";
      end if;
      Req.Method.Last := I - 1;

      --  Parse target
      if I + 1 > Req.End_Headers then
         raise Parse_Error with "Invalid request line: unexpected end after method";
      end if;
      Req.Target.First := I + 1;
      I := Index (Req, Req.Target.First, " ");
      if I = 0 then
         raise Parse_Error with "Invalid request line: missing space after target";
      end if;
      Req.Target.Last := I - 1;

      --  Parse protocol
      if I + 1 > Req.End_Headers then
         raise Parse_Error with "Invalid request line: unexpected end after target";
      end if;
      Req.Protocol.First := I + 1;
      I := Index (Req, Req.Protocol.First, CRLF);
      if I = 0 then
         raise Parse_Error with "Invalid request line: missing CRLF after protocol";
      end if;
      Req.Protocol.Last := I - 1;

      --  Parse headers
      I := Req.Protocol.Last + CRLF'Length + 1;
      while I <= Req.End_Headers - CRLF'Length loop
         --  Check if we've reached the end of headers (empty line)
         if I + CRLF'Length - 1 <= Req.End_Headers and then
            Req.Item (I .. I + CRLF'Length - 1) = CRLF
         then
            exit;
         end if;

         --  Parse header name
         declare
            Name, Value : Span;
         begin
            Name.First := I;
            Next := Index (Req, Name.First, ":");
            if Next = 0 or else Next >= Req.End_Headers then
               raise Parse_Error with "Invalid header: missing colon";
            end if;
            Name.Last := Next - 1;

            --  Parse header value (skipping optional whitespace)
            Value.First := Next + 1;
            if Value.First <= Req.End_Headers and then
               Req.Item (Value.First) = ' '
            then
               Value.First := Value.First + 1;
            end if;

            Next := Index (Req, Value.First, CRLF);
            if Next = 0 or else Next > Req.End_Headers then
               raise Parse_Error with "Invalid header: missing CRLF after value";
            end if;
            Value.Last := Next - 1;

            --  Only add the header if both name and value are valid
            if Name.First <= Name.Last and then
               Value.First <= Value.Last and then
               Name.Last < Req.Item'Last and then
               Value.Last < Req.Item'Last
            then
               Request_Header_Maps.Include (
                  Req.Headers,
                  Get_String (Req, Name),
                  Value
               );
            end if;

            --  Move to the next header
            I := Next + CRLF'Length;
         end;
      end loop;
   exception
      when Constraint_Error =>
         raise Parse_Error with "Index out of bounds during request parsing";
   end Parse_Request;

   function "<" (Left, Right : Socket_Type)
      return Boolean
   is (To_C (Left) < To_C (Right));

   type Session_Type is record
      Req  : Request := (others => <>);
      Resp : Response := (others => <>);
   end record;

   package Session_Maps is new Ada.Containers.Ordered_Maps
      (Key_Type     => Socket_Type,
       Element_Type => Session_Type);
   Sessions : Session_Maps.Map;

   procedure On_Readable
      (Sock : Socket_Type);
   procedure On_Writable
      (Sock : Socket_Type);
   procedure On_Error
      (Sock : Socket_Type);

   procedure On_Request
      (Session : in out Session_Type;
       Sock    : Socket_Type)
   is
   begin
      Session.Resp.Socket := Sock;
      Codesearch.Service.Handle_Request (Session.Req, Session.Resp, DB);
      if Response_Buffers.Length (Session.Resp.Buffer) > 0 then
         Codesearch.IO.Unregister (Sock);
         Codesearch.IO.Register (Sock,
            Readable => On_Readable'Access,
            Writable => On_Writable'Access,
            Error    => On_Error'Access);
      end if;
   end On_Request;

   procedure On_Readable
      (Sock : Socket_Type)
   is
      Session : constant Session_Maps.Reference_Type := Session_Maps.Reference (Sessions, Sock);
      Item : Stream_Element_Array (1 .. Stream_Element_Offset (Session.Req.Item'Last))
         with Address => Session.Req.Item'Address;
      Last : Stream_Element_Offset := 0;
   begin
      Receive_Socket (Sock, Item (Last + 1 .. Item'Last), Last);
      Session.Req.Last := Natural (Last);
      if Session.Req.Last = 0 then
         On_Error (Sock);
         return;
      end if;

      Parse_Request (Session.Req);

      if Session.Req.End_Headers > 0 then
         On_Request (Session, Sock);
      end if;
   exception
      when Parse_Error =>
         On_Error (Sock);
   end On_Readable;

   procedure On_Writable
      (Sock : Socket_Type)
   is
      Session : constant Session_Maps.Reference_Type := Session_Maps.Reference (Sessions, Sock);
      Str  : constant String := Response_Buffers.To_String (Session.Resp.Buffer);
      Item : Stream_Element_Array (1 .. Stream_Element_Offset (Str'Length))
         with Address => Str'Address;
      Last : Stream_Element_Offset := 0;
   begin
      Send_Socket (Sock, Item, Last);
      Response_Buffers.Delete (Session.Resp.Buffer, 1, Natural (Last));
      if Response_Buffers.Length (Session.Resp.Buffer) = 0 then
         Close_Socket (Sock);
      end if;
   exception
      when Socket_Error =>
         On_Error (Sock);
   end On_Writable;

   procedure On_Error
      (Sock : Socket_Type)
   is
   begin
      Close_Socket (Sock);
   end On_Error;

   procedure On_Connect
      (Listen_Sock : Socket_Type)
   is
      Sock : Socket_Type;
      Addr : Sock_Addr_Type;
   begin
      Accept_Socket (Listen_Sock, Sock, Addr);

      if not Session_Maps.Contains (Sessions, Sock) then
         declare
            New_Session : constant Session_Type := (others => <>);
         begin
            Session_Maps.Insert (Sessions, Sock, New_Session);
         end;
      end if;

      declare
         Session : constant Session_Maps.Reference_Type := Session_Maps.Reference (Sessions, Sock);
      begin
         Reset (Session.Req);
         Reset (Session.Resp);
      end;

      Codesearch.IO.Register (Sock,
         Readable => On_Readable'Access,
         Writable => null,
         Error    => On_Error'Access);
   end On_Connect;

   procedure Bind is
      Addr : constant Sock_Addr_Type :=
         (Addr => <>,
          Port => 9999,
          Family => Family_Inet);
      Listen_Sock : Socket_Type;
   begin
      Create_Socket (Listen_Sock);
      Set_Socket_Option (Listen_Sock, Socket_Level, (Reuse_Address, True));
      Bind_Socket (Listen_Sock, Addr);
      Listen_Socket (Listen_Sock, 256);
      Codesearch.IO.Register (Listen_Sock,
         Readable => On_Connect'Access,
         Writable => null,
         Error    => null);
   end Bind;

   procedure Run is
   begin
      DB := Codesearch.Database.Open (Read_Only => True);
      Codesearch.IO.Run;
   end Run;

end Codesearch.HTTP.Server;
