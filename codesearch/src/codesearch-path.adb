--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: AGPL-3.0-or-later
--
package body Codesearch.Path is

   Sep : constant Character := '/';

   function Join
      (Left, Right : String)
      return String
   is
   begin
      if Right'Length > 0 and then Right (Right'First) = Sep then
         return Right;
      elsif Left'Length = 0 or else Left (Left'Last) = Sep then
         return Left & Right;
      else
         return Left & Sep & Right;
      end if;
   end Join;

end Codesearch.Path;
