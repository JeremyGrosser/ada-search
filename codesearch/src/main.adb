--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: AGPL-3.0-or-later
--
pragma Ada_2022;
with Codesearch.HTTP.Server;
with Ada.Text_IO;

procedure Main is
   package Server renames Codesearch.HTTP.Server;
   Workers : array (1 .. 1) of Server.Worker;
begin
   for W of Workers loop
      W.Start;
      W.Wait_Ready;
   end loop;

   for W of Workers loop
      W.Wait_Error;
      Ada.Text_IO.Put (W'Image);
      Ada.Text_IO.Put ("terminated with error");
      Ada.Text_IO.New_Line;
   end loop;
end Main;
