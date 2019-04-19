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
with Interfaces; use Interfaces;
with Headers; use Headers;

package Identify is
   function Try (H : Unsigned_64) return Boolean;
   procedure Identify_File (Filename : String);
end Identify;
