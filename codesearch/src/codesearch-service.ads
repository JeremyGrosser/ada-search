--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: AGPL-3.0-or-later
--
with Eva.HTTP;

package Codesearch.Service is

   package HTTP renames Eva.HTTP;

   procedure Handle_Request
      (Request  : HTTP.Request;
       Response : in out HTTP.Response);

end Codesearch.Service;
