--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: AGPL-3.0-or-later
--
package Codesearch.Path
   with Pure
is

   --  Ignore the previous parts if a part is absolute
   --  Insert a '/' unless the first part is empty or already ends in '/'
   function Join
      (Left, Right : String)
      return String;

end Codesearch.Path;
