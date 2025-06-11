--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: AGPL-3.0-or-later
--
private with Codesearch.IO;

package Codesearch.HTTP.Server is

   type Server_Context is private;

   procedure Bind
      (Context : out Server_Context);
   procedure Run
      (Context : in out Server_Context);

private

   type Server_Context is record
      IOC : Codesearch.IO.IO_Context;
   end record;

end Codesearch.HTTP.Server;
