--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: AGPL-3.0-or-later
--
with Eva.Strings; use Eva.Strings;

package Codesearch.Template
   with Preelaborate
is

   Template_Error : exception;

   function Render
      (Template : Unicode;
       Values   : Unicode_Maps.Map)
       return Unicode;

end Codesearch.Template;
