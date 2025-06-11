--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: AGPL-3.0-or-later
--
with Codesearch.HTTP.Server;

procedure Main is
   package Server renames Codesearch.HTTP.Server;
   Workers : array (1 .. 32) of Server.Worker;
begin
   for W of Workers loop
      W.Start;
      W.Wait_Ready;
   end loop;
   loop
      delay 10.0;
   end loop;
end Main;
