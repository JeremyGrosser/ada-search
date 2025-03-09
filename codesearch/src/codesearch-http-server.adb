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
         Codesearch.IO.Set_Triggers (Sock, Readable => True, Writable => True, Error => True);
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
         Writable => On_Writable'Access,
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
