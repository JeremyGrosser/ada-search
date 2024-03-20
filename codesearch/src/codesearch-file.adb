with Ada.Containers.Indefinite_Vectors;
with Ada.Directories;
with Ada.Streams.Stream_IO;

with Ada.Strings.Wide_Wide_Fixed;
with Ada.Strings;

package body Codesearch.File is

   package Path_Vectors is new Ada.Containers.Indefinite_Vectors
      (Index_Type   => Positive,
       Element_Type => Path);

   function Split
      (Name : Path)
      return Path_Vectors.Vector
   is
      use Path_Vectors;
      V : Vector := Empty_Vector;
      Current : Unbounded_Unicode;
   begin
      for Ch of Name loop
         if Ch = '/' then
            Append (V, Path (To_Unicode (Current)));
            Delete (Current, 1, Length (Current) - 1);
         else
            Append (Current, Ch);
         end if;
      end loop;
      Append (V, Path (To_Unicode (Current)));
      return V;
   end Split;

   function To_String
      (Name : Path)
      return String
   is (String (Encode (Unicode (Name))));

   function Join
      (Left, Right : Path)
      return Path
   is (Path (Unicode (Left) & Unicode (Right)));

   function Normalize
      (Name : Path)
      return Path
   is
      use Path_Vectors;
      V : Vector := Empty_Vector;
      Result : Unbounded_Unicode;
   begin
      for Component of Split (Name) loop
         if Component = ".." then
            Delete_Last (V);
         else
            Append (V, Component);
         end if;
      end loop;

      for Component of V loop
         Append (Result, Component);
   end Normalize;

   function Basename
      (Name : Path)
      return Path
   is
      use Ada.Strings.Wide_Wide_Fixed;
      use Ada.Strings;

      I : constant Natural := Index
         (Source  => Wide_Wide_String (Name),
          Pattern => "/",
          Going   => Backward);
   begin
      if I = 0 then
         return Name;
      else
         return Name (I + 1 .. Name'Last);
      end if;
   end Basename;

   function Exists
      (Name : Path)
      return Boolean
   is (Ada.Directories.Exists (To_String (Name)));

   function Length
      (Name : Path)
      return Natural
   is (Natural (Ada.Directories.Size (To_String (Name))));

   function Read_Unicode
      (Name : Path)
      return Unicode
   is
      use Ada.Streams.Stream_IO;
      F : Ada.Streams.Stream_IO.File_Type;
      Data : UTF8 (1 .. Codesearch.File.Length (Name));
   begin
      Open (F, In_File, To_String (Name));
      UTF8'Read (Stream (F), Data);
      Close (F);
      return Decode (Data);
   end Read_Unicode;

   function Remove_Extension
      (Name : Path)
      return Path
   is
      Dot : constant Natural := Ada.Strings.Wide_Wide_Fixed.Index
         (Source  => Wide_Wide_String (Name),
          Pattern => ".",
          Going   => Ada.Strings.Backward);
   begin
      if Dot = 0 then
         return Name;
      else
         return Name (Name'First .. Dot - 1);
      end if;
   end Remove_Extension;

end Codesearch.File;
