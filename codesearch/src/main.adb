--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: AGPL-3.0-or-later
--
with Codesearch.HTTP.Server;
with Codesearch.File;
with GNAT.Ctrl_C;

procedure Main is
   package Server renames Codesearch.HTTP.Server;

   procedure Stop is
   begin
      Server.Stop;
   end Stop;
begin
   Codesearch.File.Set_Working_Directory;
   Server.Bind;
   GNAT.Ctrl_C.Install_Handler (Stop'Unrestricted_Access);
   Server.Run;
end Main;
