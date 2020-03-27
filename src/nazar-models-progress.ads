with Nazar.Interfaces.Work;

package Nazar.Models.Progress is

   type Root_Progress_Model is
     new Root_Model_Type with private;

   type Nazar_Progress_Model is access all Root_Progress_Model'Class;

   procedure Clear
     (Model      : in out Root_Progress_Model'Class);

   procedure Add_Work_Item
     (Model : in out Root_Progress_Model'Class;
      Work  : Nazar.Interfaces.Work.Work_Interface'Class);

   procedure Tick (Model : in out Root_Progress_Model'Class);

   function Status
     (Model : Root_Progress_Model'Class)
      return Nazar_Unit_Float;

private

   type Root_Progress_Model is
     new Root_Model_Type with
      record
         Step_Count   : Natural := 0;
         Current_Step : Natural := 0;
      end record;

end Nazar.Models.Progress;
