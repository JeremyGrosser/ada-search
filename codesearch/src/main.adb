pragma Ada_2022;
with Codesearch.Database;
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   package DB renames Codesearch.Database;

   Results : DB.Search_Results (1 .. 250);
   Last    : Natural;
begin
   DB.Initialize;

   DB.Search ("synack", Results, Last);
   for Row of Results (1 .. Last) loop
      Put_Line (Row'Image);
   end loop;
   Put_Line (Last'Image & " results returned");

   DB.Search ("package Ada.Containers.Bounded_Vectors", Results, Last);
   for Row of Results (1 .. Last) loop
      Put_Line (Row'Image);
   end loop;
   Put_Line (Last'Image & " results returned");

   DB.Search ("WNM", Results, Last);
   for Row of Results (1 .. Last) loop
      Put_Line (Row'Image);
   end loop;
   Put_Line (Last'Image & " results returned");

   DB.Close;
end Main;
