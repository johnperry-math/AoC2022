-- Advent of Code 2022, Day 4 -- this time, with SPARK
--
-- John Perry
--
-- Camp Cleanup
--
-- part 1: determine how many pairs of elves have assignments that render
-- one of the elves redundant
--
-- part 2: determine how many pairs of elves have assignments that overlap

with Ada.Text_IO; use all type Ada.Text_IO.File_Mode;

procedure Main with SPARK_Mode => On is

   -- SECTION
   -- input-related, part 1

   Filename: constant String := "input.txt";
   package ATIO renames Ada.Text_IO;
   Input_File    : ATIO.File_Type;

   -- SECTION
   -- utility subprograms and types

   subtype Input_String is String( 1 .. 30 );
   -- SPARK wants a form of ATIO.Get_Line that precisely defines string length,
   -- so we define this type for it

   type Number_And_Position is record
      -- see Next_Number
      Value   : Natural;
      Position: Positive;
   end record;

   function Next_Number(S: Input_String; Pos: Positive) return Number_And_Position
   -- returns the next natural number found in S at or after position Pos,
   -- along with the first position that follows that number;
   -- if none is found, the number returns is 0;
   -- the maximum position returned is S'Length

      with
         Pre => ( S'Length >= 1 and Pos >= S'First and Pos <= S'Length ),
         Post => ( Next_Number'Result.Position <= S'Length )

   is
      Running_Value: Natural := 0;
      New_Pos      : Positive := Pos;

   begin

      -- find position of first digit
      while New_Pos < Positive'Last
         and then New_Pos <= S'Length
         and then S(New_Pos) not in '0'..'9'
      loop
         New_Pos := New_Pos + 1;
      end loop;

      -- read the digit, updating Running_Value
      while New_Pos < Positive'Last
         and then New_Pos <= S'Length
         and then S(New_Pos) in '0'..'9'
         and then Running_Value < 100
      loop
         Running_Value := Running_Value * 10;
         Running_Value := Running_Value + Character'Pos(S(New_Pos)) - Character'Pos('0');
         New_Pos := New_Pos + 1;
      end loop;

      -- adjust New_Pos; needed for the prover so that we can reuse it
      -- while this makes the prover happy, it's bad design,
      -- and I wouldn't do this in real life
      if New_Pos > S'Length then New_Pos := S'Length; end if;

      return ( Value => Running_Value, Position => New_Pos );

   end Next_Number;

   procedure Count_Redundant(Result: out Natural; Input_File: ATIO.File_Type)
      -- find the number of elf pairs where one elf's work
      -- fully encompasses the other's
      --
      -- SPARK doesn't like to read ATIO.File_Types inside a function,
      -- since functions shouldn't affect global state,
      -- so I had to make this a procedure :-(

      with Pre => (
                      ATIO.Is_Open(Input_File)
                   and then ATIO.Mode(Input_File) = In_File
                  )

   is
      Pair : Input_String;
      Blank: Input_String := ( others => ' ' );
      Last : Natural;

   begin

      Result := 0;

      loop

         ATIO.Get_Line(Input_File, Pair, Last);

         declare
            Location    : Positive := 1;
            -- get the four numbers on the line
            NAP_1a      : Number_And_Position := Next_Number(Pair, Location);
            NAP_1b      : Number_And_Position := Next_Number(Pair, NAP_1a.Position);
            NAP_2a      : Number_And_Position := Next_Number(Pair, NAP_1b.Position);
            NAP_2b      : Number_And_Position := Next_Number(Pair, NAP_2a.Position);
            -- extract the numbers from the results
            First_Start : Natural := NAP_1a.Value;
            First_End   : Natural := NAP_1b.Value;
            Second_Start: Natural := NAP_2a.Value;
            Second_End  : Natural := NAP_2b.Value;

            Frighteningly_Large_Assignment: exception;

         begin

            -- in real life this should raise an exception or mark an error,
            -- because we don't expect the input to have such large values,
            -- but SPARK doesn't allow exceptions
            if Result >= Natural'Last then exit; end if;

            if (First_Start <= Second_Start and First_End >= Second_End)
               or (First_Start >= Second_Start and First_End <= Second_End)
            then
               Result := Result + 1;
            end if;

            Pair := Blank;

         end;

         if ATIO.End_Of_File(Input_File) then exit; end if;

      end loop;

   end Count_Redundant;

   procedure Count_Overlapping(Result: out Natural; Input_File: ATIO.File_Type)
      -- find the number of elf pairs where one elf's work
      -- fully encompasses the other's
      --
      -- SPARK doesn't like to read ATIO.File_Types inside a function,
      -- since functions shouldn't affect global state,
      -- so I had to make this a procedure :-(
      --
      -- this mimics the previous procedure, so I won't comment further

      with Pre => (
                      ATIO.Is_Open(Input_File)
                   and then ATIO.Mode(Input_File) = In_File
                  )

   is
      Pair : Input_String;
      Blank: Input_String := ( others => ' ' );
      Last : Natural;

   begin

      Result := 0;

      loop

         if Result >= Natural'Last then exit; end if;

         ATIO.Get_Line(Input_File, Pair, Last);

         declare
            Location    : Positive := 1;
            NAP_1a      : Number_And_Position := Next_Number(Pair, Location);
            NAP_1b      : Number_And_Position := Next_Number(Pair, NAP_1a.Position);
            NAP_2a      : Number_And_Position := Next_Number(Pair, NAP_1b.Position);
            NAP_2b      : Number_And_Position := Next_Number(Pair, NAP_2a.Position);
            First_Start : Natural := NAP_1a.Value;
            First_End   : Natural := NAP_1b.Value;
            Second_Start: Natural := NAP_2a.Value;
            Second_End  : Natural := NAP_2b.Value;

         begin

            if (First_Start <= Second_Start and First_End >= Second_Start)
               or (First_Start <= Second_End and First_End >= Second_End)
               or (First_Start >= Second_Start and First_Start <= Second_End)
               or (First_End >= Second_Start and First_End <= Second_End)
            then
               Result := Result + 1;
            end if;

         end;

         if ATIO.End_Of_File(Input_File) then exit; end if;
         Pair := Blank;

      end loop;

   end Count_Overlapping;

   Elves, Assignments: Natural;

begin

   -- part 1
   ATIO.Open(Input_File, ATIO.In_File, Filename);
   pragma Assert(ATIO.Is_Open(Input_File));
   Count_Redundant(Elves, Input_File);
   ATIO.CLose(Input_File);
   ATIO.Put_Line
      (
       "Part 1: There are" & Elves'Image & " redundant elves"
      );

   -- part 2
   ATIO.Open(Input_File, ATIO.In_File, Filename);
   pragma Assert(ATIO.Is_Open(Input_File));
   Count_Overlapping(Assignments, Input_File);
   ATIO.CLose(Input_File);
   ATIO.Put_Line
      (
       "Part 2: There are" & Assignments'Image & " overlapping assignments"
      );

end Main;
