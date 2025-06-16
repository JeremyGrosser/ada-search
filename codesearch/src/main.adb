--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: AGPL-3.0-or-later
--
pragma Ada_2022;
with Eva.HTTP.Server;
with Codesearch.Service;

procedure Main is
   package Server is new Eva.HTTP.Server
      (Handle_Request => Codesearch.Service.Handle_Request);
begin
   Server.Run;
end Main;
