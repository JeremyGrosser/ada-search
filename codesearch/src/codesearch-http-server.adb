--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: AGPL-3.0-or-later
--
with Ada.Streams; use Ada.Streams;
with Codesearch.Sockets; use Codesearch.Sockets;
with Ada.Containers.Ordered_Maps;
with Ada.Calendar;
with Ada.Exceptions;
with Codesearch.Service;
with Codesearch.Database;
with Codesearch.File;
with Codesearch.IO;
with Ada.Text_IO;

package body Codesearch.HTTP.Server is

   Request_Timeout   : constant Duration := 3.0;
   Response_Timeout  : constant Duration := 3.0;
   Idle_Timeout      : constant Duration := 3.0;

   DB_Session : access Codesearch.Database.Session := null
      with Thread_Local_Storage;

   function DB
      return Codesearch.Database.Session
   is
   begin
      if DB_Session = null then
         Ada.Text_IO.Put_Line ("New database session");
         DB_Session := new Codesearch.Database.Session;
         DB_Session.all := Codesearch.Database.Open (Read_Only => True);
      end if;
      return DB_Session.all;
   end DB;

   type Server_Context is record
      IOC : Codesearch.IO.IO_Context;
   end record;

   type Session_Type is record
      Req            : Request := (others => <>);
      Resp           : Response := (others => <>);
      Expires_At     : Ada.Calendar.Time := Ada.Calendar.Clock;
      Timers_Enabled : Boolean := True;
   end record;

   use type Codesearch.Sockets.Socket_Type;
   package Session_Maps is new Ada.Containers.Ordered_Maps
      (Key_Type     => Socket_Type,
       Element_Type => Session_Type);
   Sessions : Session_Maps.Map;

   procedure On_Readable
      (Context : in out Codesearch.IO.IO_Context;
       Sock    : Socket_Type);
   procedure On_Writable
      (Context : in out Codesearch.IO.IO_Context;
       Sock    : Socket_Type);
   procedure On_Error
      (Context : in out Codesearch.IO.IO_Context;
       Sock    : Socket_Type);
   procedure On_Timeout
      (Context : in out Codesearch.IO.IO_Context;
       Sock    : Socket_Type);

   procedure Set_Timeout
      (Context : in out Codesearch.IO.IO_Context;
       Session : in out Session_Type;
       Sock    : Socket_Type;
       After   : Duration)
   is
      use Ada.Calendar;
   begin
      Session.Expires_At := Clock + After;
      Codesearch.IO.Set_Timeout
         (This     => Context,
          Desc     => Sock,
          After    => After,
          Callback => On_Timeout'Access);
   end Set_Timeout;

   procedure On_Timeout
      (Context : in out Codesearch.IO.IO_Context;
       Sock    : Socket_Type)
   is
      pragma Unreferenced (Context);
      use Ada.Calendar;
      Session : constant Session_Maps.Reference_Type := Session_Maps.Reference (Sessions, Sock);
   begin
      if Session.Timers_Enabled and then Clock > Session.Expires_At then
         Session.Timers_Enabled := False;
         Close_Socket (Sock);
      end if;
   end On_Timeout;

   procedure On_Request
      (Context : in out Codesearch.IO.IO_Context;
       Session : in out Session_Type;
       Sock    : Socket_Type)
   is
   begin
      Session.Resp.Socket := Sock;

      Codesearch.Service.Handle_Request (Session.Req, Session.Resp, DB);

      Set_Header (Session.Resp,
         Key   => "Content-Length",
         Value => Codesearch.Strings.To_String
            (Payload_Length (Session.Resp)));

      if Response_Buffers.Length (Session.Resp.Buffer) > 0 then
         Set_Timeout (Context, Session, Sock, Response_Timeout);
         Codesearch.IO.Set_Triggers
            (This     => Context,
             Desc     => Sock,
             Readable => False,
             Writable => True,
             Error    => True);
      end if;
   end On_Request;

   procedure On_Readable
      (Context : in out Codesearch.IO.IO_Context;
       Sock    : Socket_Type)
   is
      Session : constant Session_Maps.Reference_Type := Session_Maps.Reference (Sessions, Sock);
      Item : Stream_Element_Array (1 .. Stream_Element_Offset (Session.Req.Item'Last))
         with Address => Session.Req.Item'Address;
      Last : Stream_Element_Offset := 0;
   begin
      Receive_Socket (Sock, Item (Last + 1 .. Item'Last), Last);
      Session.Req.Last := Natural (Last);
      if Session.Req.Last = 0 then
         Session.Timers_Enabled := False;
         Close_Socket (Sock);
         return;
      end if;

      Parse_Request (Session.Req);

      if Session.Req.End_Headers > 0 then
         On_Request (Context, Session, Sock);
      end if;
   exception
      when Socket_Error =>
         null;
      when Parse_Error =>
         Ada.Text_IO.Put_Line ("Parse_Error");
         --  On_Error (Sock);
   end On_Readable;

   procedure On_Writable
      (Context : in out Codesearch.IO.IO_Context;
       Sock    : Socket_Type)
   is
      pragma Unreferenced (Context);
      Session : constant Session_Maps.Reference_Type := Session_Maps.Reference (Sessions, Sock);
      Str  : constant String := Response_Buffers.To_String (Session.Resp.Buffer);
      Item : Stream_Element_Array (1 .. Stream_Element_Offset (Str'Length))
         with Address => Str'Address;
      Last : Stream_Element_Offset := 0;
   begin
      if Item'Length = 0 then
         return;
      end if;
      Send_Socket (Sock, Item, Last);
      Response_Buffers.Delete (Session.Resp.Buffer, 1, Natural (Last));
      if Response_Buffers.Length (Session.Resp.Buffer) = 0 then
         Session.Timers_Enabled := False;
         Reset (Session.Req);
         Reset (Session.Resp);
         Close_Socket (Sock);
         --  Codesearch.IO.Set_Triggers
         --     (Context  => Context,
         --      Desc     => Sock,
         --      Readable => True,
         --      Writable => False,
         --      Error    => True);
         --  Set_Timeout (Context, Session, Sock, Idle_Timeout);
      end if;
   exception
      when E : Socket_Error =>
         Ada.Text_IO.Put_Line ("Socket_Error in On_Writable");
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         --  On_Error (Sock);
   end On_Writable;

   procedure On_Error
      (Context : in out Codesearch.IO.IO_Context;
       Sock    : Socket_Type)
   is
      pragma Unreferenced (Context);
      Session : constant Session_Maps.Reference_Type := Session_Maps.Reference (Sessions, Sock);
   begin
      Session.Timers_Enabled := False;
      --  Close_Socket (Sock);
   end On_Error;

   procedure On_Connect
      (Context     : in out Codesearch.IO.IO_Context;
       Listen_Sock : Socket_Type)
   is
      Sock : Socket_Type;
      Addr : Sock_Addr;
   begin
      Accept_Socket (Listen_Sock, Sock, Addr);

      if not Session_Maps.Contains (Sessions, Sock) then
         declare
            New_Session : Session_Type := (others => <>);
         begin
            Session_Maps.Insert (Sessions, Sock, New_Session);
            Set_Timeout (Context, New_Session, Sock, Request_Timeout);
         end;
      else
         declare
            Session : constant Session_Maps.Reference_Type := Session_Maps.Reference (Sessions, Sock);
         begin
            Reset (Session.Req);
            Reset (Session.Resp);
            Set_Timeout (Context, Session, Sock, Request_Timeout);
         end;
      end if;

      Codesearch.IO.Register
         (This     => Context,
          Desc     => Sock,
          Readable => On_Readable'Access,
          Writable => On_Writable'Access,
          Error    => On_Error'Access);
   end On_Connect;

   procedure Bind
      (Context : out Server_Context)
   is
      Addr : constant Sock_Addr :=
         (Port   => 9999,
          others => <>);
      Listen_Sock : Socket_Type;
   begin
      Codesearch.IO.Initialize (Context.IOC);
      Create_Socket (Listen_Sock);
      Set_Socket_Option (Listen_Sock, Reuse_Address, True);
      Set_Socket_Option (Listen_Sock, Reuse_Port, True);
      Bind_Socket (Listen_Sock, Addr);
      Listen_Socket (Listen_Sock, 256);
      Codesearch.IO.Register (Context.IOC, Listen_Sock,
         Readable => On_Connect'Access,
         Writable => null,
         Error    => null);
   end Bind;

   procedure Run
      (Context : in out Server_Context)
   is
   begin
      Ada.Text_IO.Put_Line ("Server IO Running");
      Codesearch.IO.Run (Context.IOC);
   end Run;

   task body Worker is
      Context : Server_Context;
   begin
      Codesearch.File.Set_Working_Directory;
      Bind (Context);
      Run (Context);
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
   end Worker;
end Codesearch.HTTP.Server;
