package body Nazar.Names is

   Current_Name : String := "AA-A0-0A0A";

   ---------------
   -- Next_Name --
   ---------------

   function Next_Name return String is
   begin
      return Name : constant String := Current_Name do
         for Ch of reverse Current_Name loop
            if Ch in 'A' .. 'Z' then
               if Ch = 'Z' then
                  Ch := 'A';
               else
                  Ch := Character'Succ (Ch);
                  exit;
               end if;
            elsif Ch in '0' .. '9' then
               if Ch = '9' then
                  Ch := '0';
               else
                  Ch := Character'Succ (Ch);
                  exit;
               end if;
            end if;
         end loop;
      end return;
   end Next_Name;

   ---------------
   -- Next_Name --
   ---------------

   function Next_Name return Ada.Strings.Unbounded.Unbounded_String is
   begin
      return Ada.Strings.Unbounded.To_Unbounded_String (Next_Name);
   end Next_Name;

end Nazar.Names;
