package Nazar.Interfaces.Numeric is

   type Nazar_Float_Interface is interface;

   function Minimum (Model : Nazar_Float_Interface) return Nazar_Float
                     is abstract;

   function Maximum (Model : Nazar_Float_Interface) return Nazar_Float
                     is abstract;

   function Step (Model : Nazar_Float_Interface) return Nazar_Float
                  is abstract;

   function Current (Model : Nazar_Float_Interface) return Nazar_Float
                     is abstract;

end Nazar.Interfaces.Numeric;
