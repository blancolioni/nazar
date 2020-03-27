package Nazar.Measurements is

   type Nazar_Unit is (Absolute, Proportional);

   type Nazar_Size is new Nazar_Float;

   type Nazar_Measurement is
      record
         Unit : Nazar_Unit;
         Size : Nazar_Size;
      end record;

end Nazar.Measurements;
