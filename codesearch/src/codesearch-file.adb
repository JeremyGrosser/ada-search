--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: AGPL-3.0-or-later
--
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

package body Codesearch.File is

   package Content_Maps is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type        => String,
       Element_Type    => Unicode,
       Hash            => Ada.Strings.Hash,
       Equivalent_Keys => "=");

   Cache : Content_Maps.Map := Content_Maps.Empty_Map;

   protected Resource_Cache is
      function Get_Content
         (Filename : String)
         return Unicode;
   end Resource_Cache;

   protected body Resource_Cache is
      function Get_Content_From_File
         (Filename : String)
         return Unicode
      is
         use Ada.Streams.Stream_IO;
         F : File_Type;
         Data : UTF8 (1 .. Natural (Ada.Directories.Size (Filename)));
      begin
         Open (F, In_File, Filename);
         UTF8'Read (Stream (F), Data);
         Close (F);
         return Decode (Data);
      end Get_Content_From_File;

      function Get_Content
         (Filename : String)
         return Unicode
      is
         use Content_Maps;
      begin
         if not Contains (Cache, Filename) then
            Insert (Cache, Filename, Get_Content_From_File (Filename));
         end if;
         return Element (Cache, Filename);
      end Get_Content;
   end Resource_Cache;

   procedure Set_Working_Directory is
   begin
      Ada.Directories.Set_Directory (Share.Prefix_Path);
   end Set_Working_Directory;

   function Read_Unicode
      (Filename : String)
      return Unicode
   is (Resource_Cache.Get_Content (Filename));

   function Read_Resource
      (Filename : String)
      return Unicode
   is
   begin
      return Read_Unicode (Share.Resource_Path & Filename);
   end Read_Resource;

   function Resource_Exists
      (Filename : String)
      return Boolean
   is (Ada.Directories.Exists (Share.Resource_Path & Filename));

end Codesearch.File;
