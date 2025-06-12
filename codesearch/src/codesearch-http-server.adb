--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: AGPL-3.0-or-later
--
pragma Ada_2022;
with Ada.Streams; use Ada.Streams;
with Ada.Real_Time;
with Codesearch.Sockets; use Codesearch.Sockets;
with Ada.Containers.Ordered_Maps;
with Ada.Exceptions;
with Codesearch.Service;
with Codesearch.Database;
with Codesearch.File;
with Codesearch.IO;
with Codesearch.Timers;
with System;
with Ada.Text_IO;

package body Codesearch.HTTP.Server is

   Request_Timeout : constant := 10;

   type Session_Type is record
      Req    : Request := (others => <>);
      Resp   : Response := (others => <>);
   end record;

   use type Codesearch.Sockets.Socket_Type;
   package Session_Maps is new Ada.Containers.Ordered_Maps
      (Key_Type     => Socket_Type,
       Element_Type => Session_Type);

   type Server_Context is record
      Sessions    : Session_Maps.Map := Session_Maps.Empty_Map;
      IOC         : Codesearch.IO.IO_Context;
      DB          : Codesearch.Database.Session;
      Timers      : Codesearch.Timers.Timer_Wheel;
   end record;

   procedure On_Timeout
      (Context : Socket_Type)
   is
   begin
      Close_Socket (Context);
   exception
      when Socket_Error =>
         null;
   end On_Timeout;

   procedure On_Request
      (Session      : in out Session_Type;
       Sock         : Socket_Type;
       User_Context : System.Address)
   is
      Server : Server_Context
         with Import, Address => User_Context;
   begin
      Session.Resp.Socket := Sock;

      Codesearch.Service.Handle_Request (Session.Req, Session.Resp, Server.DB);

      Set_Header (Session.Resp,
         Key   => "Content-Length",
         Value => Codesearch.Strings.To_String
            (Payload_Length (Session.Resp)));

      if not Is_Empty (Session.Resp) then
         Codesearch.IO.Set_Triggers
            (This     => Server.IOC,
             Desc     => Sock,
             Readable => False,
             Writable => True,
             Error    => False);
      end if;
   end On_Request;

   procedure On_Readable
      (Sock : Socket_Type;
       User_Context : System.Address)
   is
      Server : Server_Context
         with Import, Address => User_Context;
      Session : constant Session_Maps.Reference_Type := Session_Maps.Reference (Server.Sessions, Sock);
      Item : Stream_Element_Array (1 .. Stream_Element_Offset (Session.Req.Item'Last))
         with Address => Session.Req.Item'Address;
      Last : Stream_Element_Offset := 0;
   begin
      Receive_Socket (Sock, Item, Last);
      if Last = 0 then
         Close_Socket (Sock);
      else
         Session.Req.Last := Natural (Last);
         Parse_Request (Session.Req);
         if Session.Req.End_Headers > 0 then
            On_Request (Session, Sock, User_Context);
         end if;
      end if;
   exception
      when Socket_Error =>
         null;
      when Parse_Error =>
         Ada.Text_IO.Put_Line ("Parse_Error");
         Close_Socket (Sock);
   end On_Readable;

   procedure On_Writable
      (Sock : Socket_Type;
       User_Context : System.Address)
   is
      Server : Server_Context
         with Import, Address => User_Context;
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
      else
         Response_Buffers.Delete (Session.Resp.Buffer, 1, Natural (Last));
         if Is_Empty (Session.Resp) then
            --  No more data to send, wait for another request
            Reset (Session.Req);
            Reset (Session.Resp);
            Codesearch.IO.Set_Triggers
               (This     => Server.IOC,
                Desc     => Sock,
                Readable => True,
                Writable => False,
                Error    => False);
         end if;
      end if;
   exception
      when E : Socket_Error =>
         Ada.Text_IO.Put_Line ("Socket_Error in On_Writable");
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         Close_Socket (Sock);
   end On_Writable;

   procedure On_Connect
      (Listen_Sock  : Socket_Type;
       User_Context : System.Address)
   is
      Server : Server_Context
         with Import, Address => User_Context;
      Sock : Socket_Type;
      Addr : Sock_Addr;
   begin
      Accept_Socket (Listen_Sock, Sock, Addr);

      if not Session_Maps.Contains (Server.Sessions, Sock) then
         declare
            New_Session : constant Session_Type := (others => <>);
         begin
            Session_Maps.Insert (Server.Sessions, Sock, New_Session);
         end;
      end if;

      declare
         Session : constant Session_Maps.Reference_Type := Session_Maps.Reference (Server.Sessions, Sock);
      begin
         Reset (Session.Req);
         Reset (Session.Resp);
         Codesearch.Timers.Set_Timeout
            (Wheel         => Server.Timers,
             Timeout_Sec   => Request_Timeout,
             Callback      => On_Timeout'Access,
             Context       => Sock);
      end;

      Codesearch.IO.Register
         (This     => Server.IOC,
          Desc     => Sock,
          Readable => On_Readable'Access,
          Writable => On_Writable'Access,
          Error    => null,
          User_Context => User_Context);
   end On_Connect;

   procedure Bind
      (Server : out Server_Context)
   is
      Addr : constant Sock_Addr :=
         (Port   => 9999,
          others => <>);
      Listen_Sock : Socket_Type;
   begin
      Codesearch.IO.Initialize (Server.IOC);
      Codesearch.Timers.Initialize (Server.Timers);
      Server.DB := Codesearch.Database.Open (Read_Only => True);
      Create_Socket (Listen_Sock);
      Set_Socket_Option (Listen_Sock, Reuse_Address, True);
      Set_Socket_Option (Listen_Sock, Reuse_Port, True);
      Bind_Socket (Listen_Sock, Addr);
      Listen_Socket (Listen_Sock, 256);
      Codesearch.IO.Register
         (This          => Server.IOC,
          Desc          => Listen_Sock,
          Readable      => On_Connect'Access,
          Writable      => null,
          Error         => null,
          User_Context  => Server'Address);
   end Bind;

   task body Worker is
      use Ada.Real_Time;
      Next_Tick : Time := Clock;
      Server : Server_Context;
   begin
      accept Start;
      Codesearch.File.Set_Working_Directory;
      Bind (Server);
      accept Wait_Ready;
      Ada.Text_IO.Put_Line ("Server IO Running");

      loop
         Codesearch.IO.Poll (Server.IOC);
         if Clock >= Next_Tick then
            Codesearch.Timers.Tick (Server.Timers);
            Ada.Text_IO.Put_Line ("Tick");
            Next_Tick := Next_Tick + Seconds (1);
         end if;
      end loop;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         accept Wait_Error;
   end Worker;
end Codesearch.HTTP.Server;
