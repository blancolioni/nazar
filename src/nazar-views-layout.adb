package body Nazar.Views.Layout is

   --------------
   -- Contains --
   --------------

   function Contains
     (Layout : Layout_View_Interface'Class;
      Item   : not null access constant Nazar_View_Record'Class)
      return Boolean
   is
      use type WL.Guids.Guid;
   begin
      for Child of Layout.Container.Contents loop
         if Child.View /= null
           and then Child.View.Guid = Item.Guid
         then
            return True;
         end if;
      end loop;
      return False;
   end Contains;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Layout : in out Layout_View_Interface'Class;
      Item   : not null access Nazar_View_Record'Class)
   is
      use type WL.Guids.Guid;

      Position : Cell_Content_Lists.Cursor :=
        Cell_Content_Lists.No_Element;

      procedure Delete (Container : in out Layout_Container);

      ------------
      -- Delete --
      ------------

      procedure Delete (Container : in out Layout_Container) is
      begin
         Container.Contents.Delete (Position);
      end Delete;

   begin
      for It in Layout.Container.Contents.Iterate loop
         if Cell_Content_Lists.Element (It).View.Guid
           = Item.Guid
         then
            Position := It;
            exit;
         end if;
      end loop;

      Layout.Update_Container (Delete'Access);

   end Delete;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Layout : in out Layout_View_Interface'Class;
      Item   :        not null access Nazar_View_Record'Class)
   is
      procedure Insert
        (Container : in out Layout_Container);

      ------------
      -- Insert --
      ------------

      procedure Insert
        (Container : in out Layout_Container)
      is
      begin
         Container.Contents.Append ((View => Nazar_View (Item)));
      end Insert;

   begin
      Layout.Update_Container (Insert'Access);

   end Insert;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Layout  : Layout_View_Interface'Class;
      Process : not null access procedure (Item : Nazar_View))
   is
   begin
      for Item of Layout.Container.Contents loop
         Process (Item.View);
      end loop;
   end Iterate;

   ----------------------
   -- Set_Column_Width --
   ----------------------

   procedure Set_Column_Width
     (Layout       : in out Layout_View_Interface'Class;
      Column_Index : Positive;
      Width        : Nazar.Measurements.Nazar_Measurement)
   is
      procedure Set (Container : in out Layout_Container);

      ---------
      -- Set --
      ---------

      procedure Set (Container : in out Layout_Container) is
      begin
         while Column_Index > Container.Column_Sizes.Last_Index loop
            Container.Column_Sizes.Append
              ((Nazar.Measurements.Proportional, 1.0));
         end loop;
         Container.Column_Sizes (Column_Index) := Width;
      end Set;

   begin
      Layout.Update_Container (Set'Access);
   end Set_Column_Width;

   --------------------
   -- Set_Row_Height --
   --------------------

   procedure Set_Row_Height
     (Layout    : in out Layout_View_Interface'Class;
      Row_Index : Positive;
      Height    : Nazar.Measurements.Nazar_Measurement)
   is
      procedure Set (Container : in out Layout_Container);

      ---------
      -- Set --
      ---------

      procedure Set (Container : in out Layout_Container) is
      begin
         while Row_Index > Container.Row_Sizes.Last_Index loop
            Container.Row_Sizes.Append
              ((Nazar.Measurements.Proportional, 1.0));
         end loop;
         Container.Row_Sizes (Row_Index) := Height;
      end Set;

   begin
      Layout.Update_Container (Set'Access);
   end Set_Row_Height;

end Nazar.Views.Layout;
