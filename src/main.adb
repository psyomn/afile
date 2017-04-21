with Ada.Text_IO; use Ada.Text_Io;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions; use Ada.Exceptions;

with Identify; use Identify;

procedure Main is

   Argument_Error : exception;
   Arg_Count : Natural := Argument_Count;

   procedure Print_Help is
   begin
      Put_Line ("usage: ");
      Put_Line ("   afile <file>+");
   end Print_Help;

begin

   if Arg_Count < 1 then
      raise Argument_Error with "you need to provide more than one parameter";
   else
      for I in 1 .. Arg_Count loop
         Identify.Identify_File (Argument (I));
      end loop;
   end if;

exception

   when E : Argument_Error =>
      Put_Line ("Error: " & Exception_Name (E) & ": " & Exception_Message (E));
      Print_Help;

   when E : others =>
      Put_Line ("Unknown Error: " & Exception_Name (E) & ": " & Exception_Message (E));

end Main;
