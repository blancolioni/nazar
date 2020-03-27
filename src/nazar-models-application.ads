package Nazar.Models.Application is

   type Nazar_Application_Model_Record is
     new Nazar_Model_Record with private;

   subtype Nazar_Application_Model_Class is
     Nazar_Application_Model_Record'Class;

   type Nazar_Application_Model is
     access all Nazar_Application_Model_Record'Class;

private

   type Nazar_Application_Model_Record is
     new Nazar_Model_Record with
      record
         null;
      end record;

   overriding function Class_Name
     (Model : Nazar_Application_Model_Record)
      return String
   is ("nazar-application-model");

end Nazar.Models.Application;
