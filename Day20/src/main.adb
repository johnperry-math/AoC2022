-- Advent of Code 2022, Day 20
--
-- John Perry
--
-- Grove Positioning System
--
-- part 1: reorder a circular list depending on the number's value
--
-- part 2: multiply the values by a ginormous number, then reorder 10 times
--

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Containers.Doubly_Linked_Lists;

procedure Main is

   package Text_IO renames Ada.Text_IO;
   package Integer_IO is new Ada.Text_IO.Integer_IO ( Num => Integer );
   package Natural_IO is new Ada.Text_IO.Integer_IO ( Num => Natural );

   Doing_Example: constant Boolean := False;

   type Part_Number is ( First, Second );

   -- SECTION
   -- global types and variables

   package Integer_Lists is new Ada.Containers.Doubly_Linked_Lists
      (
       Element_Type => Integer
      );

   Input_List: Integer_Lists.List;

   -- SECTION
   -- I/O

   Filename: constant String
      := ( if Doing_Example then "example.txt" else "input.txt" );

   Input_File: Text_IO.File_Type;

   procedure Get_Integer(S: String; Result: out Integer; Pos: in out Positive)
   is
   -- Ada.Text_IO.Integer_IO has issues with "(\d)*:",
   -- so I have to roll my own

      Is_Negative: Boolean := S(Pos) = '-';

   begin

      if Is_Negative then Pos := Pos + 1; end if;

      Result := 0;
      while Pos <= S'Length and then S(Pos) in '0'..'9' loop
         Result := Result * 10;
         Result := Result + Character'Pos(S(Pos)) - Character'Pos('0');
         Pos := Pos + 1;
      end loop;

      if Is_Negative then Result := -Result; end if;

   end Get_Integer;

   procedure Read_Input is
   -- read the blueprints

      Blueprints_Read: Natural := 0;

   begin

      Text_IO.Open(Input_File, Text_IO.In_File, Filename);

      while not Text_IO.End_Of_File(Input_File) loop
         declare
            Input_String: String := Text_IO.Get_Line(Input_File);
            Value       : Integer;
            Pos         : Positive := 1;
         begin
            Get_Integer(Input_String, Value, Pos);
            Input_List.Append(Value);
         end;
      end loop;

      Text_IO.Close(Input_File);

   end Read_Input;

   procedure Put_List(List: Integer_Lists.List) is

   begin

      for E of List loop
         Text_IO.Put(" " & E'Image);
      end loop;
      Text_IO.New_Line;

   end Put_List;

   -- SECTION
   -- PART 1

   function Sum_Of_Coordinates return Integer is
   -- determines the result of sorting Input_List
   -- by shifting each value around the same number of places as its value,
   -- then returns the sum of the 1000th, 2000th, and 3000th entries
   -- of the final list

      use type Ada.Containers.Count_Type;
      use type Integer_Lists.Cursor;

      Result       : Integer := 0;
      New_List: Integer_Lists.List;
      Position_List: array(1 .. Input_List.Length) of Integer_Lists.Cursor;
      Cursor: Integer_Lists.Cursor;
      I: Ada.Containers.Count_Type := 1;

   begin

      -- set up the position list
      Cursor := Input_List.First;
      while Cursor /= Integer_Lists.No_Element loop
         New_List.Append(Integer_Lists.Element(Cursor));
         Position_List(I) := New_List.Last;
         I := I + 1;
         Integer_Lists.Next(Cursor);
      end loop;

      -- reorder it
      Through_Values: for K in Position_List'First .. Position_List'Last loop

         -- if the value at position k is larger than the list's length,
         -- there's no point cycling around however many times;
         -- might as well work out the resulting position after all those cycles

         Cursor := Position_List(K);

         declare
            Value: Integer := Integer_Lists.Element(Cursor);
            Move_Left: Boolean := Value < 0;
            New_Cursor: Integer_Lists.Cursor;
            Net_Motion: Integer := abs(Value) rem
               ( Positive(Position_List'Length) - 1 ); -- how far it moves

         begin

            if Move_Left then
               if Cursor = New_List.First then
                  New_Cursor := New_List.Last;
               else
                  New_Cursor := Integer_Lists.Previous(Cursor);
               end if;
               Net_Motion := Net_Motion - 1;
            else
               if Cursor = New_List.Last then
                  New_Cursor := New_List.First;
               else
                  New_Cursor := Integer_Lists.Next(Cursor);
               end if;
            end if;

            New_List.Delete(Cursor);

            -- perform the shift

            Through_Shift: for L in 1 .. Net_Motion loop

               if Move_Left then
                  if New_Cursor = New_List.First then
                     New_Cursor := New_List.Last;
                  else
                     Integer_Lists.Previous(New_Cursor);
                  end if;

               else
                  if New_Cursor = New_List.Last then
                     New_Cursor := New_List.First;
                  else
                     Integer_Lists.Next(New_Cursor);
                  end if;

               end if;

            end loop Through_Shift;

            New_List.Insert(New_Cursor, Value);

         end;

      end loop Through_Values;

      declare
         C: Integer_Lists.Cursor := New_List.Find(0);
      begin

         for I in 1 .. 3000 loop
            if C = New_List.Last then C := New_List.First;
            else Integer_Lists.Next(C);
            end if;
            if I rem 1000 = 0 then
               --  Text_IO.Put_Line(Integer_Lists.Element(C)'image);
               Result := Result + New_List(C);
            end if;
         end loop;

      end;

      return Result;

   end Sum_of_Coordinates;

   -- SECTION
   -- PART 2

   function Sum_Of_New_Coordinates return Long_Long_Integer is
   -- same as Sum_of_Coordinates, except that
   -- now we have to multiply the values by some ludicrously large number,
   -- then sort it 10 times
   --
   -- since the solution to part 1 already uses remainders,
   -- this doesn't slow us down that much here
   --
   -- I thought about taking the remainder of Key by the list's length
   -- so as to avoid using Long_Long_Integer, but
   -- (a) that would complicate things, and
   -- (b) I was already using a remainder for the part where it matters most,
   --     and
   -- (c) the result is (probably) a Long_Long_Integer anyway,
   --     so I can't avoid it even then

      Key: constant Long_Long_Integer := 811589153;

      package Long_Integer_Lists is new Ada.Containers.Doubly_Linked_Lists
            ( Element_Type => Long_Long_Integer
             );

      use type Integer_Lists.Cursor;
      use type Ada.Containers.Count_Type;
      use type Long_Integer_Lists.Cursor;

      Result       : Long_Long_Integer := 0;
      New_List     : Long_Integer_Lists.List;
      Position_List: array(1 .. Input_List.Length) of Long_Integer_Lists.Cursor;
      Cursor       : Integer_Lists.Cursor;
      Long_Cursor  : Long_Integer_Lists.Cursor;
      I            : Ada.Containers.Count_Type := 1;

   begin

      -- set up the lists
      Cursor := Input_List.First;
      while Cursor /= Integer_Lists.No_Element loop
         New_List.Append(Long_Long_Integer(Integer_Lists.Element(Cursor)) * Key);
         Position_List(I) := New_List.Last;
         I := I + 1;
         Integer_Lists.Next(Cursor);
      end loop;

      Loop_10: for Each in 1 .. 10 loop

         -- reorder it
         Through_Values: for K in Position_List'First .. Position_List'Last loop

            -- if the value at position k is larger than the list's length,
            -- there's no point cycling around however many times;
            -- might as well work out the resulting position after all those cycles

            Long_Cursor := Position_List(K);

            declare
               Value     : Long_Long_Integer
                  := Long_Integer_Lists.Element(Long_Cursor);
               Move_Left : Boolean := Value < 0;
               New_Cursor: Long_Integer_Lists.Cursor;
               Net_Motion: Long_Long_Integer := abs(Value) rem
                  ( Long_Long_Integer(Position_List'Length) - 1 ); -- how far it moves

            begin

               if Move_Left then
                  if Long_Cursor = New_List.First then
                     New_Cursor := New_List.Last;
                  else
                     New_Cursor := Long_Integer_Lists.Previous(Long_Cursor);
                  end if;
                  Net_Motion := Net_Motion - 1;
               else
                  if Long_Cursor = New_List.Last then
                     New_Cursor := New_List.First;
                  else
                     New_Cursor := Long_Integer_Lists.Next(Long_Cursor);
                  end if;
               end if;

               New_List.Delete(Long_Cursor);

               -- perform the shift

               Through_Shift: for L in 1 .. Net_Motion loop

                  if Move_Left then
                     if New_Cursor = New_List.First then
                        New_Cursor := New_List.Last;
                     else
                        Long_Integer_Lists.Previous(New_Cursor);
                     end if;

                  else
                     if New_Cursor = New_List.Last then
                        New_Cursor := New_List.First;
                     else
                        Long_Integer_Lists.Next(New_Cursor);
                     end if;

                  end if;

               end loop Through_Shift;

               New_List.Insert(New_Cursor, Value);

            end;

         end loop Through_Values;

      end loop Loop_10;

      declare
         C: Long_Integer_Lists.Cursor := New_List.Find(0);
      begin

         for I in 1 .. 3000 loop
            if C = New_List.Last then C := New_List.First;
            else Long_Integer_Lists.Next(C);
            end if;
            if I rem 1000 = 0 then
               Result := Result + New_List(C);
            end if;
         end loop;

      end;

      return Result;

   end Sum_Of_New_Coordinates;

begin

   Read_Input;

   Text_IO.Put_Line("part 1:" & Sum_Of_Coordinates'Image);
   Text_IO.Put_Line("part 2:" & Sum_Of_New_Coordinates'Image);

end Main;
