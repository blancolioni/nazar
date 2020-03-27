with Nazar.Signals;

private package Nazar.Signal_Store is

   function Exists (Signal_Name : String) return Boolean;
   function Get (Signal_Name : String) return Nazar.Signals.Signal_Type;

   procedure Add
     (Signal_Name : String;
      Signal      : Nazar.Signals.Signal_Type);

end Nazar.Signal_Store;
