private with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package Nazar.Interfaces.Observable is

   type Observer_Interface is interface;

   procedure Notify (Observer : Observer_Interface)
   is abstract;

   type Observable_Interface is interface;

   procedure Add_Observer
     (Observable : in out Observable_Interface;
      Observer   : Observer_Interface'Class)
   is abstract;

   procedure Remove_Observer
     (Observable : in out Observable_Interface;
      Observer   : Observer_Interface'Class)
   is abstract;

   procedure Notify_Observers
     (Observable : Observable_Interface)
   is abstract;

   type Nazar_Observable_Record is
     new Observable_Interface with private;

   overriding procedure Add_Observer
     (Observable : in out Nazar_Observable_Record;
      Observer   : Observer_Interface'Class);

   overriding procedure Remove_Observer
     (Observable : in out Nazar_Observable_Record;
      Observer   : Observer_Interface'Class);

   overriding procedure Notify_Observers
     (Observable : Nazar_Observable_Record);

private

   package Observer_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Observer_Interface'Class);

   type Nazar_Observable_Record is
     new Observable_Interface with
      record
         List : Observer_Lists.List;
      end record;

end Nazar.Interfaces.Observable;
