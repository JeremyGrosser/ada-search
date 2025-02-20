--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: AGPL-3.0-or-later
--
with Codesearch.Strings;
private with Ada.Strings.Equal_Case_Insensitive;
private with Ada.Strings.Hash_Case_Insensitive;
private with Ada.Strings.Bounded;
private with Ada.Containers.Indefinite_Hashed_Maps;
private with GNAT.Sockets;

package Codesearch.HTTP
   with Elaborate_Body
is

   type Request is private;

   --  Request
   function Method
      (This : Request)
      return String;

   function Path
      (This : Request)
      return String;

   function Query_Parameter
      (This    : Request;
       Key     : String;
       Default : String := "")
       return String;

   type Response is private;

   procedure Set_Header
      (This : in out Response;
       Key, Value : String);

   type HTTP_Status is new Integer range 100 .. 599;

   procedure Set_Status
      (This    : in out Response;
       Code    : HTTP_Status;
       Message : String);

   procedure Put
      (This : in out Response;
       Data : Codesearch.Strings.Unicode);

   procedure Put_Raw
      (This : in out Response;
       Data : String);

private

   Max_Request_Length   : constant := 4096;
   Max_Response_Length  : constant := 4096;
   --  Status line and headers only, does not include payload

   type Span is record
      First, Last : Natural := 0;
   end record;

   package Request_Header_Maps is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type         => String,
       Element_Type     => Span,
       Equivalent_Keys  => Ada.Strings.Equal_Case_Insensitive,
       Hash             => Ada.Strings.Hash_Case_Insensitive);

   type Request is record
      Item        : String (1 .. Max_Request_Length);
      Last        : Natural := 0;
      End_Headers : Natural := 0;
      Method      : Span;
      Target      : Span;
      Protocol    : Span;
      Headers     : Request_Header_Maps.Map := Request_Header_Maps.Empty_Map;
   end record;

   package Response_Buffers is new Ada.Strings.Bounded.Generic_Bounded_Length
      (Max_Response_Length);

   type Response is record
      Socket   : GNAT.Sockets.Socket_Type;
      Started  : Boolean := False; --  Set True after status and headers are sent
      Buffer   : Response_Buffers.Bounded_String := Response_Buffers.Null_Bounded_String;
   end record;

   function Get_String
      (Req : Request;
       Sp  : Span)
       return String;

end Codesearch.HTTP;
