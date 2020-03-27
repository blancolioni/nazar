with WL.Guids;

package Nazar is

   type Nazar_Float is new Long_Float range
     Long_Float'First .. Long_Float'Last;

   type Nazar_Unit_Float is new Nazar_Float range 0.0 .. 1.0;

   type Rectangle is
      record
         X, Y : Nazar_Float := 0.0;
         W, H : Nazar_Float := 1.0;
      end record;

   type Nazar_Object_Interface is interface;

   function Guid (Object : Nazar_Object_Interface)
                  return WL.Guids.Guid
                  is abstract;

   function Name (Object : Nazar_Object_Interface) return String
                  is abstract;

   function Class_Name (Object : Nazar_Object_Interface) return String
                        is abstract;

   procedure Set_Name
     (Object : in out Nazar_Object_Interface;
      Name   : String)
   is abstract;

   procedure Destroy
     (Object : in out Nazar_Object_Interface)
   is null;

end Nazar;
