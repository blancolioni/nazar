package Nazar.Interfaces.Work is

   type Work_Interface is synchronized interface;

   procedure Step (Work : in out Work_Interface) is abstract;

   procedure Get_State
     (Work    : in out Work_Interface;
      Current : out Natural;
      Total   : out Natural)
   is abstract;

end Nazar.Interfaces.Work;
