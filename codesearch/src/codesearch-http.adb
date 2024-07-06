with Ada.Strings.Fixed;
with URI;

package body Codesearch.HTTP is

   CRLF : constant String := ASCII.CR & ASCII.LF;

   function Method
      (This : Request)
      return String
   is (Get_String (This, This.Method));

   function Path
      (This : Request)
      return String
   is
      use Ada.Strings.Fixed;
      Target : constant String := Get_String (This, This.Target);
      Query : constant Natural := Index
         (Source  => Target,
          Pattern => "?",
          From    => Target'First);
      Fragment : constant Natural := Index
         (Source  => Target,
          Pattern => "#",
          From    => Target'First);
      Last : Natural := Target'Last;
   begin
      if Query in Target'First .. Last then
         Last := Query - 1;
      end if;
      if Fragment in Target'First .. Last then
         Last := Fragment - 1;
      end if;
      return URI.Normalize_Path (Target (Target'First .. Last));
   end Path;

   function Query_Parameter
      (This    : Request;
       Key     : String;
       Default : String := "")
       return String
   is
      use Ada.Strings.Fixed;
      Target : constant String := Get_String (This, This.Target);
      Query  : constant Natural := Index
         (Source  => Target,
          Pattern => "?",
          From    => Target'First);
      First : Natural;
   begin
      if Query = 0 then
         return Default;
      end if;
      for I in Query + 1 .. Target'Last loop
         exit when Target (I) = '#';
         First := Index (Target (I .. Target'Last), Key & '=');
         if First > 0 then
            First := First + Key'Length + 1;
            for J in First .. Target'Last loop
               if Target (J) = '&' or else Target (J) = '#' then
                  return Target (First .. J - 1);
               end if;
            end loop;
            return Target (First .. Target'Last);
         end if;
      end loop;
      return Default;
   end Query_Parameter;

   procedure Set_Header
      (This : in out Response;
       Key, Value : String)
   is
      use Response_Buffers;
   begin
      Append (This.Buffer, Key);
      Append (This.Buffer, ": ");
      Append (This.Buffer, Value);
      Append (This.Buffer, CRLF);
   end Set_Header;

   procedure Set_Status
      (This    : in out Response;
       Code    : HTTP_Status;
       Message : String)
   is
      use Response_Buffers;
   begin
      if This.Started or else Length (This.Buffer) /= 0 then
         raise Program_Error with "Set_Status cannot be called at this time";
      end if;
      --  This.Started := False;
      Append (This.Buffer, "HTTP/1.1");
      Append (This.Buffer, Code'Image);
      Append (This.Buffer, ' ');
      Append (This.Buffer, Message);
      Append (This.Buffer, CRLF);
   end Set_Status;

   procedure Put_Raw
      (This : in out Response;
       Data : String)
   is
      Stream : constant GNAT.Sockets.Stream_Access := GNAT.Sockets.Stream (This.Socket);
   begin
      if not This.Started then
         Response_Buffers.Append (This.Buffer, CRLF);
         String'Write (Stream, Response_Buffers.To_String (This.Buffer));
         This.Started := True;
      end if;
      String'Write (Stream, Data);
   end Put_Raw;

   procedure Put
      (This : in out Response;
       Data : Codesearch.Strings.UTF8)
   is
   begin
      Put_Raw (This, String (Data));
   end Put;

   procedure Put
      (This : in out Response;
       Data : Codesearch.Strings.Unicode)
   is
   begin
      Put (This, Codesearch.Strings.Encode (Data));
   end Put;

   function Get_String
      (Req : Request;
       Sp  : Span)
       return String
   is
   begin
      if Sp.Last = 0 then
         return "";
      else
         return Req.Item (Sp.First .. Sp.Last);
      end if;
   end Get_String;

end Codesearch.HTTP;
