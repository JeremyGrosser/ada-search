with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Fixed;
with Ada.Strings;

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

   function RFind
      (Haystack : String;
       Needle   : Character)
       return Natural
   is (Ada.Strings.Fixed.Index
         (Source  => Haystack,
          Pattern => "" & Needle,
          From    => Haystack'Last,
          Going   => Ada.Strings.Backward));

   function Basename
      (P : String)
      return String
   is
      First : constant Natural := RFind (P, Sep);
   begin
      if First = 0 then
         return P;
      else
         return P (First + 1 .. P'Last);
      end if;
   end Basename;

   function "*"
      (Left  : Character;
       Right : Natural)
       return String
   is
      S : constant String (1 .. Right) := (others => Left);
   begin
      return S;
   end "*";

   function Dirname
      (P : String)
      return String
   is
      Last : Natural := RFind (P, Sep);
   begin
      if Last = 0 then
         return P;
      else
         declare
            Head : constant String := P (P'First .. Last);
         begin
            if Head /= (Sep * Head'Length) then
               while Last >= Head'First and then Head (Last) = Sep loop
                  Last := Last - 1;
               end loop;
               return Head (Head'First .. Last);
            else
               return Head;
            end if;
         end;
      end if;
   end Dirname;

   package String_Vectors is new Ada.Containers.Indefinite_Vectors
      (Index_Type   => Positive,
       Element_Type => String);

   function Split
      (P : String)
      return String_Vectors.Vector
   is
      use String_Vectors;
      V : Vector;
      First : Natural := P'First;
   begin
      for I in P'Range loop
         if P (I) = Sep then
            Append (V, P (First .. I - 1));
            First := I + 1;
         end if;
      end loop;
      if First <= P'Last then
         Append (V, P (First .. P'Last));
      end if;
      return V;
   end Split;

   function Join
      (V : String_Vectors.Vector;
       Delimiter : Character)
       return String
   is
      Length : Natural := 0;
   begin
      for Component of V loop
         Length := Length + Component'Length + 1;
      end loop;
      declare
         S : String (1 .. Length);
         I : Natural := S'First;
      begin
         for Component of V loop
            S (I .. I + Component'Length - 1) := Component;
            S (I + Component'Length) := Sep;
            I := I + Component'Length + 1;
         end loop;
         return S;
      end;
   end Join;

end Codesearch.Path;
