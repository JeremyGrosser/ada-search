--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: AGPL-3.0-or-later
--
package Codesearch.HTTP.Server is
   task type Worker is
      entry Start;
      entry Wait_Ready;
      entry Wait_Error;
   end Worker;
end Codesearch.HTTP.Server;
