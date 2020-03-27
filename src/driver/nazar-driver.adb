with Nazar.Controllers.Console;
with Nazar.Models.Console;
with Nazar.Views.Text_Console;

with Nazar.Models.Environment;
with Nazar.Models.Directories;

with Nazar.Main;

with Nazar.Paths;

procedure Nazar.Driver is
   Controller : Nazar.Controllers.Console.Nazar_Console_Controller_Record;
   Model      : constant Nazar.Models.Console.Nazar_Console_Model :=
     new Nazar.Models.Console.Root_Console_Model;
   Env        : constant Nazar.Models.Environment.Nazar_Environment_Model :=
     new Nazar.Models.Environment.Root_Environment_Model;
begin

   Nazar.Main.Init;

   Model.Initialize
     (Root          =>
        Nazar.Models.Directories.Directory_Model
          (Nazar.Paths.Config_Path),
      Environment   => Env,
      Default_Scope => "/");

   Controller.Start_Console
     (Model => Model,
      View  => Nazar.Views.Text_Console.Text_Console_View (Model));

   Nazar.Main.Stop;

end Nazar.Driver;
