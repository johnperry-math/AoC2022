-- Advent of Code 2022, Day 5
--
-- John Perry
--
-- Supply Stacks
--
-- part 1: Rearrange the stacks moving only one crate at a time.
--
-- part 2: Rearrange the stacks moving all indicated crates at a time.

with Ada.Text_IO;

procedure Main is

   -- SECTION
   -- input-related, part 1

   Filename: constant String := "input.txt";
   package ATIO renames Ada.Text_IO;
   Input_File    : ATIO.File_Type;

   -- SECTION
   -- utility types and subprograms

   subtype Crate_Range is Natural range 0 .. 55;
   -- I came up with 55 because there are 9 stacks,
   -- with at most 8 crates in any stack,
   -- and by subtracting the missing number (16) I get 56;
   --
   type Crate_Array is array ( Crate_Range ) of Character;

   type Stack_Record is record
      Num: Crate_Range := 0; -- number of crates on the stack
      Crate: Crate_Array;
   end record;

   subtype Stack_Range is Positive range 1 .. 9;
   type Dock is Array ( Stack_Range ) of Stack_Record;

   Stacks: Dock;

   procedure Clear_Stacks is
      -- resets Stacks to an "empty" state
   begin
      for Stack of Stacks loop
         Stack.Num := 0;
      end loop;
   end Clear_Stacks;

   procedure Print_Stacks is
   -- prints the crates, but in a left-to-right order,
   -- rather than the bottom-to-top order shown in the input
   -- this is useful for debugging
      Stack_Number: Positive := 1;
   begin
      for Stack of Stacks loop
         ATIO.Put(Stack_Number'Image & ": ");
         for I in 0 .. Stack.Num - 1 loop
            ATIO.Put(Stack.Crate(I)'Image & " ");
         end loop;
         ATIO.New_Line;
         Stack_Number := Stack_Number + 1;
      end loop;
   end Print_Stacks;

   procedure Print_Tops_Of_Stacks is
   -- prints the tops of the stacks, which is what the puzzle asks
   begin
      ATIO.Put("Tops of stacks:");
      for Stack of Stacks loop
         ATIO.Put(Stack.Crate(Stack.Num - 1));
      end loop;
      ATIO.New_Line;
   end Print_Tops_Of_Stacks;

   -- SECTION
   -- crane behavior
   -- we define a crane type and two cranes of that type

   type Crane is not null access procedure (Number, From, To: Positive);

   procedure Crate_Mover_9000(Number, From, To: Positive) is
   -- moves `Number` crates from `From` to `To`, one at a time
   begin
      for I in 1 .. Number loop
         Stacks(To).Crate(Stacks(To).Num) := Stacks(From).Crate(Stacks(From).Num - 1);
         Stacks(To).Num := Stacks(To).Num + 1;
         Stacks(From).Num := Stacks(From).Num - 1;
      end loop;
   end Crate_Mover_9000;

   procedure Crate_Mover_9001(Number, From, To: Positive) is
   -- moves `Number` crates from `From` to `To`, all at once
   begin
      Stacks(To).Crate(Stacks(To).Num .. Stacks(To).Num + Number)
         := Stacks(From).Crate(Stacks(From).Num - Number .. Stacks(From).Num);
      Stacks(To).Num := Stacks(To).Num + Number;
      Stacks(From).Num := Stacks(From).Num - Number;
   end Crate_Mover_9001;

   -- SECTION
   -- I/O
   -- this section takes care of both reading and rearranging the crates

   procedure Read_Crates is
   -- reads the stack setup from disk
   begin
      loop
         declare
            Level: String := ATIO.Get_Line(Input_File);
         begin
            -- make sure we're not at the line which labels the stacks
            if Level(2) = '1' then exit; end if;
            -- thankfully the stacks are spaced regularly
            for I in Stack_Range loop
               if Level(2 + (I - 1) * 4) /= ' ' then
                  Stacks(I).Crate(1..Stacks(I).Num) := Stacks(I).Crate(0..Stacks(I).Num - 1);
                  Stacks(I).Crate(0) := Level(2 + (I - 1) * 4);
                  Stacks(I).Num := Stacks(I).Num + 1;
               end if;
            end loop;
         end;
      end loop;
   end Read_Crates;

   procedure Rearrange_Crates(Use_Crane: Crane) is
   -- rearranges the crates according to the input instructions
   begin
      while not ATIO.End_Of_File(Input_File) loop
         declare
            Position   : Positive := 6; -- start after "move "
            Instruction: String := ATIO.Get_Line(Input_File);
            Number_To_Move,
               From_Stack,
               To_Stack   : Natural := 0;
         begin
            if Instruction'Length /= 0 then
               while Instruction(Position) in '0'..'9' loop
                  Number_To_Move := Number_To_Move * 10;
                  Number_To_Move := Number_To_Move
                     + Natural'Value((1 => Instruction(Position)));
                  Position := Position + 1;
               end loop;
               Position := Position + 6; -- move past " from "
               From_Stack := Natural'Value((1 => Instruction(Position)));
               Position := Position + 5; -- move past " to "
               To_Stack := Natural'Value((1 => Instruction(Position)));
               Use_Crane(Number_To_Move, From_Stack, To_Stack);
            end if;
         end;
      end loop;
   end Rearrange_Crates;

begin

   -- part 1

   ATIO.Open(Input_File, ATIO.In_File, Filename);
   Read_Crates;
   Rearrange_Crates(Crate_Mover_9000'access);
   ATIO.Close(Input_File);
   Print_Tops_Of_Stacks;

   -- part 2

   Clear_Stacks;

   ATIO.Open(Input_File, ATIO.In_File, Filename);
   Read_Crates;
   Rearrange_Crates(Crate_Mover_9001'access);
   ATIO.Close(Input_File);
   Print_Tops_Of_Stacks;

end Main;
