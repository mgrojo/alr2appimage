package Rpm_Package is

   -- Convert an Alire architecture name to an RPM architecture name.
   --
   function Rpm_Architecture_Of (Arch : String) return String;

   -- Convert a crate version into RPM Version and Release fields.
   -- The Version field cannot contain '-'.
   --
   function Rpm_Version_Of (Version : String) return String;

   function Rpm_Release_Of (Version : String) return String;

   -- Write an RPM spec file in SPECS/<name>.spec and return its path.
   --
   function Write_Spec
     (App_Dir  : String;
      Name     : String;
      Version  : String;
      Summary  : String;
      License  : String) return String;

end Rpm_Package;
