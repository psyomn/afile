with Ada.Text_Io; use Ada.Text_Io;
package body Headers is
   procedure Print_File_Info (F : File_Signature) is
   begin
      Put_Line (F.Description.all);
   end Print_File_Info;
end Headers;
