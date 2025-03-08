--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: AGPL-3.0-or-later
--
with Codesearch.HTTP.Server;
with Codesearch.File;

procedure Main is
   package Server renames Codesearch.HTTP.Server;
begin
   Codesearch.File.Set_Working_Directory;
   Server.Bind;
   Server.Run;
end Main;
