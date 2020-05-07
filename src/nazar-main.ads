package Nazar.Main is

   procedure Init;
   procedure Stop;

   procedure With_Render_Lock
     (Process : not null access procedure);

end Nazar.Main;
