package Nazar.Colors is

   type Nazar_Color is
      record
         Red, Green, Blue : Nazar_Unit_Float;
         Alpha            : Nazar_Unit_Float := 1.0;
      end record;

end Nazar.Colors;
