package body Nazar.Models.Scope is

   function Split_Path
     (Path : String)
      return String_Vectors.Vector;

   function Join_Path
     (Path : String_Vectors.Vector)
      return String;

   function Follow_Path
     (Root : Nazar.Interfaces.Hierarchy.Node_Reference_Class;
      Path : String_Vectors.Vector)
      return Nazar.Interfaces.Hierarchy.Node_Reference_Class;

   ------------------
   -- Change_Scope --
   ------------------

   function Change_Scope
     (Model : in out Root_Scope_Model;
      Path  : String)
      return Boolean
   is
      Old_Path : constant String_Vectors.Vector := Model.Active_Path;
      New_Path : constant String_Vectors.Vector := Split_Path (Path);
   begin
      for Item of New_Path loop
         if Item = "." then
            null;
         elsif Item = ".." then
            Model.Set_Parent_Scope;
         elsif Model.Current_Node.Get.Is_Leaf then
            Model.Active_Path := Old_Path;
            return False;
         elsif Model.Current_Node.Get.Has_Child (Item) then
            Model.Set_Child_Scope (Item);
         else
            Model.Active_Path := Old_Path;
            return False;
         end if;
      end loop;
      return True;
   end Change_Scope;

   ------------------
   -- Current_Node --
   ------------------

   function Current_Node
     (Model : Root_Scope_Model)
      return Nazar.Interfaces.Hierarchy.Node_Reference_Class
   is
   begin
      return Follow_Path (Model.Root.Element, Model.Active_Path);
   end Current_Node;

   -------------------
   -- Current_Scope --
   -------------------

   function Current_Scope (Model : Root_Scope_Model) return String is
   begin
      return Join_Path (Model.Active_Path);
   end Current_Scope;

   -----------------
   -- Follow_Path --
   -----------------

   function Follow_Path
     (Root : Nazar.Interfaces.Hierarchy.Node_Reference_Class;
      Path : String_Vectors.Vector)
      return Nazar.Interfaces.Hierarchy.Node_Reference_Class
   is

      function Go
        (Current : Nazar.Interfaces.Hierarchy.Node_Reference_Class;
         Index   : Positive)
         return Nazar.Interfaces.Hierarchy.Node_Reference_Class;

      --------
      -- Go --
      --------

      function Go
        (Current : Nazar.Interfaces.Hierarchy.Node_Reference_Class;
         Index   : Positive)
         return Nazar.Interfaces.Hierarchy.Node_Reference_Class
      is
      begin
         if Index > Path.Last_Index then
            return Current;
         else
            declare
               Element : constant String := Path.Element (Index);
            begin
               if Current.Get.Has_Child (Element) then
                  return Go
                    (Current.Get.Get_Child (Element), Index + 1);
               else
                  return Nazar.Interfaces.Hierarchy.No_Node;
               end if;
            end;
         end if;
      end Go;

   begin
      return Go (Root, 1);
   end Follow_Path;

   --------------
   -- Get_Node --
   --------------

   function Get_Node
     (Model : Root_Scope_Model;
      Path  : String)
      return Nazar.Interfaces.Hierarchy.Node_Reference_Interface'Class
   is
      V : constant String_Vectors.Vector := Split_Path (Path);
   begin
      return Follow_Path (Model.Root.Element, V);
   end Get_Node;

   ---------------
   -- Join_Path --
   ---------------

   function Join_Path
     (Path : String_Vectors.Vector)
      return String
   is
      function Join (Index : Positive) return String
      is ("/" & Path.Element (Index)
          & (if Index < Path.Last_Index
             then Join (Index + 1)
             else ""));

   begin
      if Path.Is_Empty then
         return "/";
      else
         return Join (1);
      end if;
   end Join_Path;

   -----------------
   -- Scope_Model --
   -----------------

   function Scope_Model
     (Root_Node     : Nazar.Interfaces.Hierarchy.Node_Reference_Class;
      Default_Scope : String)
      return Nazar_Scope_Model
   is
   begin
      return Model : constant Nazar_Scope_Model :=
        new Root_Scope_Model'
          (Nazar_Model_Record with
           Is_Valid     => True,
           Root         => Node_Reference_Holders.To_Holder (Root_Node),
           Default_Path => Split_Path (Default_Scope),
           Active_Path  => Split_Path (Default_Scope))
      do
         Model.Initialize;
      end return;
   end Scope_Model;

   ---------------------
   -- Set_Child_Scope --
   ---------------------

   procedure Set_Child_Scope
     (Model      : in out Root_Scope_Model;
      Child_Name : String)
   is
   begin
      Model.Active_Path.Append (Child_Name);
   end Set_Child_Scope;

   ----------------------
   -- Set_Parent_Scope --
   ----------------------

   procedure Set_Parent_Scope
     (Model : in out Root_Scope_Model)
   is
   begin
      if not Model.Active_Path.Is_Empty then
         Model.Active_Path.Delete_Last;
      end if;
   end Set_Parent_Scope;

   ----------------
   -- Split_Path --
   ----------------

   function Split_Path
     (Path : String)
      return String_Vectors.Vector
   is
      P : constant String :=
        (if Path (Path'Last) = '/' then Path else Path & '/');
      Start : Positive := P'First;
      Index : Positive := Start;
   begin
      return Vector : String_Vectors.Vector do
         for Ch of P loop
            if Ch = '/' then
               if Index > Start then
                  Vector.Append (P (Start .. Index - 1));
               end if;

               Start := Index + 1;
            end if;
            Index := Index + 1;
         end loop;
      end return;
   end Split_Path;

end Nazar.Models.Scope;
