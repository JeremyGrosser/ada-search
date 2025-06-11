--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: AGPL-3.0-or-later
--
with Codesearch.HTTP.Server;
with Codesearch.File;
with Ada.Text_IO;
with Ada.Exceptions;

procedure Main is
   package Server renames Codesearch.HTTP.Server;

   task type Worker;

   task body Worker is
      Context : Server.Server_Context;
   begin
      Codesearch.File.Set_Working_Directory;
      Server.Bind (Context);
      Server.Run (Context);
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
   end Worker;

   Workers : array (1 .. 1) of Worker;
begin
   loop
      delay 10.0;
   end loop;
end Main;
