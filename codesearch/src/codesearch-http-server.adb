pragma Ada_2022;
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
with Ada.Text_IO;

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

   function "<" (Left, Right : Socket_Type)
      return Boolean
   is (To_C (Left) < To_C (Right));

   package Request_Maps is new Ada.Containers.Ordered_Maps
      (Key_Type     => Socket_Type,
       Element_Type => Request);
   Requests : Request_Maps.Map;

   package Response_Maps is new Ada.Containers.Ordered_Maps
      (Key_Type => Socket_Type,
       Element_Type => Response);
   Responses : Response_Maps.Map;

   procedure On_Readable
      (Sock : Socket_Type);
   procedure On_Writable
      (Sock : Socket_Type);
   procedure On_Error
      (Sock : Socket_Type);

   procedure On_Request
      (Req  : Request;
       Sock : Socket_Type)
   is
      Resp : constant Response_Maps.Reference_Type := Response_Maps.Reference (Responses, Sock);
   begin
      Resp.Socket := Sock;
      Codesearch.Service.Handle_Request (Req, Resp, DB);
      if Response_Buffers.Length (Resp.Buffer) > 0 then
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
      use Request_Maps;
      Req : constant Reference_Type := Reference (Requests, Sock);
      Item : Stream_Element_Array (1 .. Stream_Element_Offset (Req.Item'Last))
         with Address => Req.Item'Address;
      Last : Stream_Element_Offset := 0;
   begin
      Ada.Text_IO.Put_Line ("recv " & Sock'Image);
      Receive_Socket (Sock, Item (Last + 1 .. Item'Last), Last);
      Req.Last := Natural (Last);
      if Req.Last = 0 then
         On_Error (Sock);
         return;
      end if;

      Parse_Request (Req);

      if Req.End_Headers > 0 then
         On_Request (Req, Sock);
      end if;
   exception
      when Parse_Error =>
         On_Error (Sock);
   end On_Readable;

   procedure On_Writable
      (Sock : Socket_Type)
   is
      Resp : constant Response_Maps.Reference_Type := Response_Maps.Reference (Responses, Sock);
      Str  : constant String := Response_Buffers.To_String (Resp.Buffer);
      Item : Stream_Element_Array (1 .. Stream_Element_Offset (Str'Length))
         with Address => Str'Address;
      Last : Stream_Element_Offset := 0;
   begin
      Send_Socket (Sock, Item, Last);
      Ada.Text_IO.Put_Line ("send " & Sock'Image & Last'Image);
      Response_Buffers.Delete (Resp.Buffer, 1, Natural (Last));
      if Response_Buffers.Length (Resp.Buffer) = 0 then
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
      Ada.Text_IO.Put_Line ("error " & Sock'Image);
   end On_Error;

   procedure On_Connect
      (Listen_Sock : Socket_Type)
   is
      Sock : Socket_Type;
      Addr : Sock_Addr_Type;
   begin
      Accept_Socket (Listen_Sock, Sock, Addr);

      if not Request_Maps.Contains (Requests, Sock) then
         declare
            New_Request : Request;
         begin
            Request_Maps.Insert (Requests, Sock, New_Request);
         end;
      else
         Reset (Request_Maps.Reference (Requests, Sock));
      end if;

      if not Response_Maps.Contains (Responses, Sock) then
         declare
            New_Response : Response;
         begin
            Response_Maps.Insert (Responses, Sock, New_Response);
         end;
      else
         Reset (Response_Maps.Reference (Responses, Sock));
      end if;

      Codesearch.IO.Register (Sock,
         Readable => On_Readable'Access,
         Writable => null,
         Error    => On_Error'Access);
      Ada.Text_IO.Put_Line ("Connect " & Image (Addr));
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
      Ada.Text_IO.Put_Line ("Bind " & Image (Addr));
   end Bind;

   procedure Run is
   begin
      DB := Codesearch.Database.Open (Read_Only => True);
      Codesearch.IO.Run;
   end Run;

end Codesearch.HTTP.Server;
