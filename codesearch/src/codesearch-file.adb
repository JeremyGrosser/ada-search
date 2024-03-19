with Ada.Containers.Indefinite_Vectors;
with Ada.Directories;
with Ada.Streams.Stream_IO;

package body Codesearch.File is
   package Path_Vectors is new Ada.Containers.Indefinite_Vectors
      (Index_Type   => Positive,
       Element_Type => Unicode);

   function Join
      (Left, Right : Relative_Path)
      return Relative_Path
   is (Relative_Path (Unicode (Left) & Trim (Unicode (Right), '/')));

   function Normalize
      (Path : Relative_Path)
      return Absolute_Path
   is
      use Path_Vectors;
      Stack, Final : Path_Vectors.Vector := Empty_Vector;
      First : Natural := Path'First;
      Last : Natural := Path'First;
   begin
      while Last <= Path'Last loop
         if Path (Last) = '/' then
            Append (Stack, Unicode (Path (First .. Last)));
            First := Last + 1;
            Last := First;
         else
            Last := Last + 1;
         end if;
      end loop;
      if (Last - First) > 0 then
         Append (Stack, Unicode (Path (First .. Last)));
      end if;

      while not Is_Empty (Stack) loop
         if First_Element (Stack) = ".." then
            Delete_First (Final);
         elsif First_Element (Stack) = "."  or else First_Element (Stack) = "" then
            null;
         else
            Append (Final, First_Element (Stack));
         end if;
         Delete_First (Stack);
      end loop;

      declare
         Result : Unbounded_Unicode;
      begin
         for Component of Final loop
            Append (Result, '/');
            Append (Result, To_Unbounded (Component));
         end loop;
         return Absolute_Path (To_Unicode (Result));
      end;
   end Normalize;

   function Exists
      (Filename : String)
      return Boolean
   is (Ada.Directories.Exists (Filename));

   function Length
      (Filename : String)
      return Natural
   is (Natural (Ada.Directories.Size (Filename)));

   procedure Read
      (Filename : String;
       Data : out String)
   is
      use Ada.Streams.Stream_IO;
      F : File_Type;
   begin
      Open (F, In_File, Filename);
      String'Read (Stream (F), Data);
      Close (F);
   end Read;

   function Read_Unicode
      (Filename : UTF8)
      return Unicode
   is
      use Ada.Streams.Stream_IO;
      F : File_Type;
      Data : UTF8 (1 .. Length (Filename));
   begin
      Open (F, In_File, String (Filename));
      UTF8'Read (Stream (F), Data);
      Close (F);
      return Decode (Data);
   end Read_Unicode;

end Codesearch.File;
