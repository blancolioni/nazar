with Nazar.Logging;
with Nazar.Signals;

package body Nazar.Main is

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      Nazar.Logging.Start_Logging;
      Nazar.Signals.Create_Signal ("signal-command");
   end Init;

   ----------
   -- Stop --
   ----------

   procedure Stop is
   begin
      Nazar.Logging.Stop_Logging;
   end Stop;

end Nazar.Main;
