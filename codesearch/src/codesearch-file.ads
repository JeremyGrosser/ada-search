--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: AGPL-3.0-or-later
--
with Codesearch.Strings; use Codesearch.Strings;
with Codesearch_Config;
with Resources;

package Codesearch.File
   with Elaborate_Body
is

   package Share is new Resources
      (Crate_Name => Codesearch_Config.Crate_Name);

   procedure Set_Working_Directory;

   function Resource_Exists
      (Filename : String)
      return Boolean;

   function Read_Resource
      (Filename : String)
      return Unicode;

end Codesearch.File;
