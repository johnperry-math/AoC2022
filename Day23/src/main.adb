-- Advent of Code 2022, Day 23
--
-- John Perry
--
-- Unstable Diffusion
--
-- part 1: determine the number of open spaces after 10 rounds of elf diffusion
--
-- part 2: determine the number of rounds needed
--    for the elf diffusion to reach equilibrium
--

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Main is

   package Text_IO renames Ada.Text_IO;
   package Integer_IO is new Ada.Text_IO.Integer_IO ( Num => Integer );
   package Natural_IO is new Ada.Text_IO.Integer_IO ( Num => Natural );

   Doing_Example: constant Boolean := False;

   type Part_Number is ( First, Second );

   -- SECTION
   -- global types and variables

   -- SUBSECTION
   -- rounds and ranges

   Rounds: constant Positive := ( if Doing_Example then 30 else 1000 );

   subtype Row_Range
      is Integer range -Rounds .. ( if Doing_Example then 7 + Rounds
                                    else 70 + Rounds );
   subtype Col_Range
      is Integer range -Rounds .. ( if Doing_Example then 7 + Rounds
                                    else 70 + Rounds );

   -- SUBSECTION
   -- elves

   type Elf is record
      Row, Prop_Row : Row_Range;
      Col, Prop_Col : Col_Range;
   end record;
   -- row, col are current position
   -- prop_row, prop_col are proposed position

   package Elf_Vectors is new Ada.Containers.Vectors
      ( Index_Type => Positive,
        Element_Type => Elf
       );

   Elves: Elf_Vectors.Vector;

   -- SUBSECTION
   -- map information

   type Filling is ( Fill_Space, Fill_Elf );

   type Map_Array is array( Row_Range, Col_Range ) of Filling;

   Map, Clear_Map: Map_Array := ( others => ( others => Fill_Space ) );
   -- `Clear_Map` is used to clear the map after each round,
   -- before replacing the elves

   -- SUBSECTION
   -- motion

   type Check_Direction is ( North, South, West, East );

   type Prefs_Array is array( Row_Range, Col_Range ) of Natural;

   Map_Choices, Clear_Choices: Prefs_Array := ( others => ( others => 0 ) );
   -- indicates how many elves choose a given position;
   -- `Clear_Choices` is used to clear the map after each round

   procedure Propose_Move(E: in out Elf; Round: Positive) is
   -- applies the rules to move `E` in the given round

      Proposed : Boolean := False; -- has elf proposed a move?
      Checks   : Natural := 0;     -- number of checks elf performed
      Direction: Check_Direction;  -- direction being checked

   begin

      -- if neighbors are all empty, stay put
      if ( for all Row in E.Row - 1 .. E.Row + 1
            => ( for all Col in E.Col - 1 .. E.Col + 1
                  => ( Row = E.Row and then Col = E.Col )
                  or else Map(Row, Col) = Fill_Space
                 )
           )
      then
         Proposed := True;
         E.Prop_Row := E.Row;
         E.Prop_Col := E.Col;
      end if;

      -- check surroundings according to cycling system
      while not Proposed and then Checks < 4 loop

         Direction := Check_Direction'Val( ( Round + Checks - 1 ) rem 4 );

         case Direction is

            when North =>
               if ( for all Col in E.Col - 1 .. E.Col + 1
                     => Map(E.Row - 1, Col) = Fill_Space )
               then
                  Proposed := True;
                  E.Prop_Row := E.Row - 1;
                  E.Prop_Col := E.Col;
               end if;

            when South =>
               if ( for all Col in E.Col - 1 .. E.Col + 1
                     => Map(E.Row + 1, Col) = Fill_Space )
               then
                  Proposed := True;
                  E.Prop_Row := E.Row + 1;
                  E.Prop_Col := E.Col;
               end if;

            when West =>
               if ( for all Row in E.Row - 1 .. E.Row + 1
                     => Map(Row, E.Col - 1) = Fill_Space )
               then
                  Proposed := True;
                  E.Prop_Row := E.Row;
                  E.Prop_Col := E.Col - 1;
               end if;

            when East =>
               if ( for all Row in E.Row - 1 .. E.Row + 1
                     => Map(Row, E.Col + 1) = Fill_Space )
               then
                  Proposed := True;
                  E.Prop_Row := E.Row;
                  E.Prop_Col := E.Col + 1;
               end if;

         end case;

         Checks := Checks + 1;

      end loop;

      -- if no valid move was possible, stay put
      if not Proposed then
         E.Prop_Row := E.Row;
         E.Prop_Col := E.Col;
      end if;

      -- update map choices
      Map_Choices(E.Prop_Row, E.Prop_Col) := @ + 1;

   end Propose_Move;

   procedure Move_Elf(E: in out Elf) is
   -- move elf according to proposal,
   -- so long as no other elf proposes to move there
   begin

      if Map_Choices(E.Prop_Row, E.Prop_Col) < 2 then
         E.Row := E.Prop_Row;
         E.Col := E.Prop_Col;
      end if;

   end Move_Elf;

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
   -- read the map, set up the elves

   begin

      Text_IO.Open(Input_File, Text_IO.In_File, Filename);

      for Row in 1 .. Row_Range'Last - Rounds loop

         declare
            Input_String: String := Text_IO.Get_Line(Input_File);
         begin

            for Col in 1 .. Col_Range'Last - Rounds loop
               if Input_String(Col) = '.' then
                  Map(Row, Col) := Fill_Space;
               else
                  Map(Row, Col) := Fill_Elf;
                  Elves.Append( Elf'( Row, Row, Col, Col ) );
                  -- the previous line looks funny because of how `Elf` is defined
               end if;
            end loop;

         end;

      end loop;

      Text_IO.Close(Input_File);

   end Read_Input;

   procedure Put_Map is
   -- used for debugging; turned out to be useful
   -- because `Read_Input` was originally very, very wrong
   begin

      for Row in Row_Range loop
         for Col in Col_Range loop
            Text_IO.Put( ( if Map(Row, Col) = Fill_Space then '.' else '#' ) );
         end loop;
         Text_IO.New_Line;
      end loop;

   end Put_Map;

   -- SECTION
   -- PART 1

   procedure Perform_10_Rounds is
   -- performs 10 rounds of elf diffusion
   begin

      for Round in 1 .. 10 loop

         -- clear the choices
         Map_Choices := Clear_Choices;

         -- make proposals
         for E of Elves loop
            Propose_Move(E, Round);
         end loop;

         -- clear the map
         Map := Clear_Map;

         -- make moves and draw into map
         for E of Elves loop
            Move_Elf(E);
            Map(E.Row, E.Col) := Fill_Elf;
         end loop;

      end loop;

   end Perform_10_Rounds;

   function Score_Map return Natural is
   -- returns the number of empty spaces in the smallest north-up rectangle
   -- that contains all the elves

      Result: Natural := 0;

      Min_Row, Max_Row: Row_Range;
      Min_Col, Max_Col: Col_Range;

   begin

      -- find coordinates of smallest rectangle containing all the elves

      -- rows first
      Min_Row := Row_Range'Last;
      Max_Row := Row_Range'First;
      for Row in Row_Range loop
         if ( for some Col in Col_Range => Map(Row, Col) = Fill_Elf ) then
            Min_Row := Row_Range'Min(Min_Row, Row);
            Max_Row := Row_Range'Max(Max_Row, Row);
         end if;
      end loop;

      -- now cols
      Min_Col := Col_Range'Last;
      Max_Col := Col_Range'First;
      for Col in Col_Range loop
         if ( for some Row in Row_Range => Map(Row, Col) = Fill_Elf ) then
            Min_Col := Col_Range'Min(Min_Col, Col);
            Max_Col := Col_Range'Max(Max_Col, Col);
         end if;
      end loop;

      -- count the spaces
      for Row in Min_Row .. Max_Row loop
         for Col in Min_Col .. Max_Col loop
            if Map(Row, Col) = Fill_Space then
               Result := Result + 1;
            end if;
         end loop;
      end loop;

      return Result;

   end Score_Map;

   -- SECTION
   -- PART 2

   procedure Report_Equilibrium is
   -- run, run, and run, until the elves stop diffusing

      Round    : Natural := 10;
      -- start at round 10, since we've already done that

   begin

      while Round < Rounds loop

         Round := Round + 1;

         -- clear choices
         Map_Choices := Clear_Choices;

         -- make proposals
         for E of Elves loop
            Propose_Move(E, Round);
         end loop;

         -- clear map
         Map := Clear_Map;

         -- check for an end to motion
         if ( for all E of Elves
               => E.Prop_Row = E.Row and then E.Prop_Col = E.Col )
         then
            exit;
         end if;

         -- make moves and draw into map
         for E of Elves loop
            Move_Elf(E);
            Map(E.Row, E.Col) := Fill_Elf;
         end loop;

      end loop;

      Text_IO.Put_Line("the elves stop moving on round" & Round'Image);

   end Report_Equilibrium;

begin

   Read_Input;

   Perform_10_Rounds;
   Text_IO.Put_Line("number of empty ground tiles is" & Score_Map'Image);

   Report_Equilibrium;

end Main;
