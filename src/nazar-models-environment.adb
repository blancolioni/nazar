pragma Ada_2012;
package body Nazar.Models.Environment is

   ---------------------------
   -- Get_Environment_Value --
   ---------------------------

   function Get_Environment_Value
     (Model : Root_Environment_Model;
      Name  : String)
      return String
   is
   begin
      return Root_Environment_Model'Class (Model).Get_Value (Name);
   end Get_Environment_Value;

   ----------------------
   -- Get_String_Value --
   ----------------------

   overriding function Get_Value
     (From : Root_Environment_Model;
      Name : String)
      return String
   is
   begin
      if From.Map.Contains (Name) then
         return From.Map.Element (Name);
      else
         return "";
      end if;
   end Get_Value;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Model   : Root_Environment_Model;
      Process : not null access procedure (Name, Value : String))
   is
   begin
      for Position in Model.Map.Iterate loop
         Process (String_Maps.Key (Position), String_Maps.Element (Position));
      end loop;
   end Iterate;

   ---------------------------
   -- Set_Environment_Value --
   ---------------------------

   procedure Set_Environment_Value
     (Model : in out Root_Environment_Model;
      Name  : String;
      Value : String)
   is
   begin
      Root_Environment_Model'Class (Model).Set_Value (Name, Value);
   end Set_Environment_Value;

   ---------------
   -- Set_Value --
   ---------------

   overriding procedure Set_Value
     (To   : in out Root_Environment_Model;
      Name  : String;
      Value : String)
   is
   begin
      if To.Map.Contains (Name) then
         To.Map.Replace (Name, Value);
      else
         To.Map.Insert (Name, Value);
      end if;
   end Set_Value;

end Nazar.Models.Environment;
