package body Nazar.Interfaces.Observable is

   ------------------
   -- Add_Observer --
   ------------------

   overriding procedure Add_Observer
     (Observable : in out Nazar_Observable_Record;
      Observer   :        Observer_Interface'Class)
   is
   begin
      Observable.List.Append (Observer);
   end Add_Observer;

   ----------------------
   -- Notify_Observers --
   ----------------------

   overriding procedure Notify_Observers (Observable : Nazar_Observable_Record)
   is
   begin
      for Observer of Observable.List loop
         Observer.Notify;
      end loop;
   end Notify_Observers;

   ---------------------
   -- Remove_Observer --
   ---------------------

   overriding procedure Remove_Observer
     (Observable : in out Nazar_Observable_Record;
      Observer   :        Observer_Interface'Class)
   is
      Position : Observer_Lists.Cursor :=
        Observable.List.Find (Observer);
   begin
      if Observer_Lists.Has_Element (Position) then
         Observable.List.Delete (Position);
      else
         raise Constraint_Error with
           "observer not found";
      end if;
   end Remove_Observer;

end Nazar.Interfaces.Observable;
