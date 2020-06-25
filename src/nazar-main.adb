with Nazar.Logging;
with Nazar.Signals;

package body Nazar.Main is

   protected Render_Lock is
      procedure Execute (Proc : not null access procedure);
   end Render_Lock;

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      Nazar.Logging.Start_Logging;
      Nazar.Signals.Create_Signal ("signal-command");
      Nazar.Signals.Create_Signal ("signal-activate");
   end Init;

   -----------------
   -- Render_Lock --
   -----------------

   protected body Render_Lock is

      procedure Execute (Proc : not null access procedure) is
      begin
         Proc.all;
      end Execute;

   end Render_Lock;

   ----------
   -- Stop --
   ----------

   procedure Stop is
   begin
      Nazar.Logging.Stop_Logging;
   end Stop;

   ----------------------
   -- With_Render_Lock --
   ----------------------

   procedure With_Render_Lock
     (Process : not null access procedure)
   is
   begin
      Render_Lock.Execute (Process);
   end With_Render_Lock;

end Nazar.Main;
