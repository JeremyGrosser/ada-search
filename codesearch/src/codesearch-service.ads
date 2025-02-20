--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: AGPL-3.0-or-later
--
with Codesearch.HTTP;
with Codesearch.Database;

package Codesearch.Service is

   package HTTP renames Codesearch.HTTP;

   procedure Handle_Request
      (Request  : HTTP.Request;
       Response : in out HTTP.Response;
       DB       : Codesearch.Database.Session);

end Codesearch.Service;
