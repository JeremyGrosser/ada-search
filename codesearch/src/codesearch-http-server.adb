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
with System;
with Ada.Text_IO;

package body Codesearch.HTTP.Server is

   Request_Timeout   : constant Duration := 10.0;
   Response_Timeout  : constant Duration := 10.0;
   Idle_Timeout      : constant Duration := 10.0;

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

   type Server_Context is record
      Sessions : Session_Maps.Map := Session_Maps.Empty_Map;
      IOC      : Codesearch.IO.IO_Context;
      DB       : Codesearch.Database.Session;
   end record;

   procedure On_Readable
      (Context : in out Codesearch.IO.IO_Context;
       Sock    : Socket_Type;
       User_Context : System.Address);
   procedure On_Writable
      (Context : in out Codesearch.IO.IO_Context;
       Sock    : Socket_Type;
       User_Context : System.Address);
   procedure On_Error
      (Context : in out Codesearch.IO.IO_Context;
       Sock    : Socket_Type;
       User_Context : System.Address);
   procedure On_Timeout
      (Context : in out Codesearch.IO.IO_Context;
       Sock    : Socket_Type;
       User_Context : System.Address);

   procedure Set_Timeout
      (Context : in out Codesearch.IO.IO_Context;
       Session : in out Session_Type;
       Sock    : Socket_Type;
       After   : Duration;
       User_Context : System.Address)
   is
      use Ada.Calendar;
   begin
      Session.Expires_At := Clock + After;
      Codesearch.IO.Set_Timeout
         (This     => Context,
          Desc     => Sock,
          After    => After,
          Callback => On_Timeout'Access,
          User_Context => User_Context);
   end Set_Timeout;

   procedure On_Timeout
      (Context : in out Codesearch.IO.IO_Context;
       Sock    : Socket_Type;
       User_Context : System.Address)
   is
      pragma Unreferenced (Context);
      use Session_Maps;
      use Ada.Calendar;
      Server : Server_Context with Import, Address => User_Context;
   begin
      Ada.Text_IO.Put_Line ("Timeout");
      if Contains (Server.Sessions, Sock) then
         declare
            Session : constant Reference_Type := Reference (Server.Sessions, Sock);
         begin
            if Session.Timers_Enabled and then Clock > Session.Expires_At then
               Close_Socket (Sock);
               Session.Timers_Enabled := False;
            end if;
         end;
      end if;
   end On_Timeout;

   procedure On_Request
      (Context : in out Codesearch.IO.IO_Context;
       Session : in out Session_Type;
       Sock    : Socket_Type;
       User_Context : System.Address)
   is
      Server : Server_Context with Import, Address => User_Context;
   begin
      Session.Resp.Socket := Sock;

      Codesearch.Service.Handle_Request (Session.Req, Session.Resp, Server.DB);

      Set_Header (Session.Resp,
         Key   => "Content-Length",
         Value => Codesearch.Strings.To_String
            (Payload_Length (Session.Resp)));

      if not Is_Empty (Session.Resp) then
         --  Set_Timeout (Context, Session, Sock, Response_Timeout, User_Context);
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
       Sock    : Socket_Type;
       User_Context : System.Address)
   is
      Server : Server_Context with Import, Address => User_Context;
      Session : constant Session_Maps.Reference_Type := Session_Maps.Reference (Server.Sessions, Sock);
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
         On_Request (Context, Session, Sock, User_Context);
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
       Sock    : Socket_Type;
       User_Context : System.Address)
   is
      Server : Server_Context with Import, Address => User_Context;
      Session : constant Session_Maps.Reference_Type := Session_Maps.Reference (Server.Sessions, Sock);
      Str  : constant String := Response_Buffers.To_String (Session.Resp.Buffer);
      Item : Stream_Element_Array (1 .. Stream_Element_Offset (Str'Length))
         with Address => Str'Address;
      Last : Stream_Element_Offset := 0;
   begin
      if Item'Length = 0 then
         return;
      end if;
      Send_Socket (Sock, Item, Last);
      if Last = 0 then
         --  Client closed connection
         Close_Socket (Sock);
         Session.Timers_Enabled := False;
      else
         Response_Buffers.Delete (Session.Resp.Buffer, 1, Natural (Last));
         if Is_Empty (Session.Resp) then
            --  No more data to send, wait for another request
            Reset (Session.Req);
            Reset (Session.Resp);
            Codesearch.IO.Set_Triggers
               (This     => Context,
                Desc     => Sock,
                Readable => True,
                Writable => False,
                Error    => True);
            --  Set_Timeout (Context, Session, Sock, Idle_Timeout, User_Context);
         end if;
      end if;
   exception
      when E : Socket_Error =>
         Ada.Text_IO.Put_Line ("Socket_Error in On_Writable");
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         --  On_Error (Sock);
   end On_Writable;

   procedure On_Error
      (Context : in out Codesearch.IO.IO_Context;
       Sock    : Socket_Type;
       User_Context : System.Address)
   is
      pragma Unreferenced (Context);
      Server : Server_Context with Import, Address => User_Context;
      Session : constant Session_Maps.Reference_Type := Session_Maps.Reference (Server.Sessions, Sock);
   begin
      Session.Timers_Enabled := False;
      --  Close_Socket (Sock);
   end On_Error;

   procedure On_Connect
      (Context     : in out Codesearch.IO.IO_Context;
       Listen_Sock : Socket_Type;
       User_Context : System.Address)
   is
      Server : Server_Context with Import, Address => User_Context;
      Sock : Socket_Type;
      Addr : Sock_Addr;
   begin
      Accept_Socket (Listen_Sock, Sock, Addr);

      if not Session_Maps.Contains (Server.Sessions, Sock) then
         declare
            New_Session : Session_Type := (others => <>);
         begin
            Session_Maps.Insert (Server.Sessions, Sock, New_Session);
            --  Set_Timeout (Context, New_Session, Sock, Request_Timeout, User_Context);
         end;
      else
         declare
            Session : constant Session_Maps.Reference_Type := Session_Maps.Reference (Server.Sessions, Sock);
         begin
            Reset (Session.Req);
            Reset (Session.Resp);
            --  Set_Timeout (Context, Session, Sock, Request_Timeout, User_Context);
         end;
      end if;

      Codesearch.IO.Register
         (This     => Context,
          Desc     => Sock,
          Readable => On_Readable'Access,
          Writable => On_Writable'Access,
          Error    => On_Error'Access,
          User_Context => User_Context);
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
      Context.DB := Codesearch.Database.Open (Read_Only => True);
      Create_Socket (Listen_Sock);
      Set_Socket_Option (Listen_Sock, Reuse_Address, True);
      Set_Socket_Option (Listen_Sock, Reuse_Port, True);
      Bind_Socket (Listen_Sock, Addr);
      Listen_Socket (Listen_Sock, 256);
      Codesearch.IO.Register (Context.IOC, Listen_Sock,
         Readable => On_Connect'Access,
         Writable => null,
         Error    => null,
         User_Context => Context'Address);
   end Bind;

   task body Worker is
      Context : Server_Context;
   begin
      accept Start;
      Codesearch.File.Set_Working_Directory;
      Bind (Context);
      Ada.Text_IO.Put_Line ("Server IO Running");
      accept Wait_Ready;
      Codesearch.IO.Run (Context.IOC);
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         accept Wait_Error;
   end Worker;
end Codesearch.HTTP.Server;
