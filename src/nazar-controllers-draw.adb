package body Nazar.Controllers.Draw is

   -------------------
   -- Start_Draw --
   -------------------

   procedure Start_Draw
     (Controller : in out Nazar_Draw_Controller_Record;
      Model      : not null access Controller_Model;
      View       : not null access Controller_View)
   is
   begin
      Controller.Model := Model_Access (Model);
      Controller.View  := View_Access (View);
      View.Set_Model (Model);
      View.Show;
   end Start_Draw;

end Nazar.Controllers.Draw;
