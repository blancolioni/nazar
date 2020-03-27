with Nazar.Signals;

with Nazar.Models.Draw;
with Nazar.Views.Draw;

package Nazar.Controllers.Draw is

   subtype Controller_Model is
     Nazar.Models.Draw.Root_Draw_Model'Class;

   subtype Controller_View is
     Nazar.Views.Draw.Draw_View_Interface'Class;

   type Nazar_Draw_Controller_Record is
     new Nazar.Signals.User_Data_Interface with private;

   procedure Start_Draw
     (Controller : in out Nazar_Draw_Controller_Record;
      Model      : not null access Controller_Model;
      View       : not null access Controller_View);

private

   type Model_Access is access all Controller_Model;
   type View_Access is access all Controller_View;

   type Nazar_Draw_Controller_Record is
     new Nazar.Signals.User_Data_Interface with
      record
         Model : Model_Access;
         View  : View_Access;
      end record;

end Nazar.Controllers.Draw;
