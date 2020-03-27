with Ada.Strings.Unbounded;

private package Nazar.Names is

   function Next_Name return String;
   function Next_Name return Ada.Strings.Unbounded.Unbounded_String;

end Nazar.Names;
