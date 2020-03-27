with Tropos.Reader;

with Nazar.Views.Container;

package body Nazar.Builder is

   procedure Create_View_Tree
     (Builder     : in out Nazar_Builder_Record'Class;
      Parent      : Nazar.Views.Container.Nazar_Container_View;
      Creator     : Nazar_Creator_Interface'Class;
      Top_Config  : Tropos.Configuration);

   --------------
   -- Add_View --
   --------------

   procedure Add_View
     (Builder : in out Nazar_Builder_Record'Class;
      Name    : String;
      View    : not null access Nazar.Views.Nazar_View_Record'Class)
   is
   begin
      Builder.Map.Insert (Name, Nazar.Views.Nazar_View (View));
   end Add_View;

   ----------------------
   -- Create_View_Tree --
   ----------------------

   procedure Create_View_Tree
     (Builder     : in out Nazar_Builder_Record'Class;
      Parent      : Nazar.Views.Container.Nazar_Container_View;
      Creator     : Nazar_Creator_Interface'Class;
      Top_Config  : Tropos.Configuration)
   is
      use type Nazar.Views.Container.Nazar_Container_View;
      Name : constant String := Top_Config.Attribute ("name");
      View : constant Nazar.Views.Nazar_View :=
        Creator.Create_View (Top_Config.Config_Name);

      procedure Process_Attribute
        (Name, Value : String);

      -----------------------
      -- Process_Attribute --
      -----------------------

      procedure Process_Attribute
        (Name, Value : String)
      is
      begin
         if View.Has_Property (Name) then
            View.Set_Property (Name, Value);
         else
            raise Constraint_Error with
            View.Class_Name &  " " & View.Name
              & ": unknown property: " & Name;
         end if;
      end Process_Attribute;

   begin
      if Name /= "" then
         View.Set_Name (Name);
      end if;

      Top_Config.Iterate_Attributes (Process_Attribute'Access);

      if Parent /= null then
         Parent.Append (View);
      end if;

      Builder.Map.Insert (View.Name, View);

      for Child_Config of Top_Config loop
         if View.all not in
           Nazar.Views.Container.Nazar_Container_View_Interface'Class
         then
            raise Constraint_Error with
            View.Class_Name & " " & View.Name & " is not a container";
         end if;

         declare
            Container : constant Nazar.Views.Container.Nazar_Container_View :=
              Nazar.Views.Container.Nazar_Container_View (View);
         begin
            Builder.Create_View_Tree (Container, Creator, Child_Config);
         end;
      end loop;

   end Create_View_Tree;

   --------------
   -- Get_View --
   --------------

   function Get_View
     (Builder : Nazar_Builder_Record'Class;
      Name    : String)
      return Nazar.Views.Nazar_View
   is
   begin
      if Builder.Map.Contains (Name) then
         return Builder.Map.Element (Name);
      else
         return null;
      end if;
   end Get_View;

   --------------
   -- Has_View --
   --------------

   function Has_View
     (Builder : Nazar_Builder_Record'Class;
      Name    : String)
      return Boolean
   is
   begin
      return Builder.Map.Contains (Name);
   end Has_View;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Builder     : in out Nazar_Builder_Record'Class;
      Creator     : Nazar_Creator_Interface'Class;
      Config_Path : String)
   is
      Config : constant Tropos.Configuration :=
        Tropos.Reader.Read_Config (Config_Path);
   begin
      for Top_Level_Config of Config loop
         Builder.Create_View_Tree
           (Parent     => null,
            Creator    => Creator,
            Top_Config => Top_Level_Config);
      end loop;
   end Initialize;

   -----------------------
   -- Nazar_Builder_New --
   -----------------------

   function Nazar_Builder_New
     (Creator     : Nazar_Creator_Interface'Class;
      Config_Path : String)
      return Nazar_Builder
   is
   begin
      return Builder : constant Nazar_Builder := new Nazar_Builder_Record do
         Builder.Initialize (Creator, Config_Path);
      end return;
   end Nazar_Builder_New;

end Nazar.Builder;
