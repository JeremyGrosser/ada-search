--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: AGPL-3.0-or-later
--
pragma Extensions_Allowed (On);
with Codesearch.Database;
with Eva.Strings; use Eva.Strings;
with Index;
with Source_Highlight;
with Static_File;

package body Codesearch.Service is

   DB : constant Codesearch.Database.Session := Codesearch.Database.Open (Read_Only => True);

   procedure Handle_Request
      (Request  : Eva.HTTP.Request;
       Response : in out Eva.HTTP.Response)
   is
      P : constant Unicode := Decode (UTF8 (Request.Path));
   begin
      if P = "/" then
         Index (Request, Response, DB);
      elsif Starts_With (P, "/source/") then
         Source_Highlight (Request, Response, DB);
      elsif Starts_With (P, "/static/") then
         Static_File (Request, Response);
      else
         Response.Set_Status (404, "Not Found");
         Response.Set_Header ("Content-Type", "text/plain;charset=utf-8");
         Response.Put ("404 Not Found" & LF);
      end if;
   end Handle_Request;

end Codesearch.Service;
