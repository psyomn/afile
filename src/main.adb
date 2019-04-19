--  Copyright 2017-2019 Simon Symeonidis (psyomn)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--    http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.with Interfaces; use Interfaces;
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
