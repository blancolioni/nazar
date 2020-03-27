with WL.String_Maps;

package body Nazar.Signal_Store is

   package Signal_Maps is
     new WL.String_Maps (Nazar.Signals.Signal_Type, Nazar.Signals."=");

   Signal_Map : Signal_Maps.Map;

   ---------
   -- Add --
   ---------

   procedure Add
     (Signal_Name : String;
      Signal      : Nazar.Signals.Signal_Type)
   is
   begin
      Signal_Map.Insert (Signal_Name, Signal);
   end Add;

   ------------
   -- Exists --
   ------------

   function Exists (Signal_Name : String) return Boolean is
   begin
      return Signal_Map.Contains (Signal_Name);
   end Exists;

   ---------
   -- Get --
   ---------

   function Get (Signal_Name : String) return Nazar.Signals.Signal_Type is
   begin
      return Signal_Map.Element (Signal_Name);
   end Get;

end Nazar.Signal_Store;
