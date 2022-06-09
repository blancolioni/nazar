with Nazar.Signals;

package Nazar.Events is

   type Event_Signal_Data is
     new Nazar.Signals.Signal_Data_Interface with private;

private

   type Event_Signal_Data is
     new Nazar.Signals.Signal_Data_Interface with
      record
         null;
      end record;

end Nazar.Events;
