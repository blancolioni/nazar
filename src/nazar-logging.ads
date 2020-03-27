package Nazar.Logging is

   type Message_Priority is (Low, Medium, High);

   Default_Minimum_Priority : constant Message_Priority := Low;

   procedure Start_Logging
     (Priority : Message_Priority := Default_Minimum_Priority);

   procedure Stop_Logging;

   procedure Set_Log_Priority
     (Minimum_Priority : Message_Priority);

   procedure Log
     (Message : String);

   procedure Log
     (Priority : Message_Priority;
      Message  : String);

   procedure Log
     (Object  : Nazar_Object_Interface'Class;
      Message : String);

   procedure Log
     (Object   : Nazar_Object_Interface'Class;
      Priority : Message_Priority;
      Message  : String);

end Nazar.Logging;
