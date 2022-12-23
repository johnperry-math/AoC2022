-- Advent of Code 2022, Day 22
--
-- John Perry
--
-- Monkey Map
--
-- part 1: determine my final location and orientation
--    after following directions on a map given by monkeys,
--    observing rules that respect walls and wrap-around
--
-- part 2: the same, but wrap-around is now governed by
--    the rules of moving around a cube
--

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;

procedure Main is

   package Text_IO renames Ada.Text_IO;
   package Integer_IO is new Ada.Text_IO.Integer_IO ( Num => Integer );
   package Natural_IO is new Ada.Text_IO.Integer_IO ( Num => Natural );

   Doing_Example: constant Boolean := False;
   -- cannot do part 2 with the example,
   -- as the map topology (?) differs between the example and my input

   type Part_Number is ( First, Second );

   -- SECTION
   -- global types and variables

   -- SUBSECTION
   -- the monkey map

   type Filling is ( Void, Space, Wall );

   Map_Rows: constant Positive := ( if Doing_Example then 12 else 200 );
   Map_Cols: constant Positive := ( if Doing_Example then 16 else 150 );

   Map: array( 1 .. Map_Rows, 1 .. Map_Cols ) of Filling;

   -- SUBSECTION
   -- instructions

   type Instruction_Type is ( Left, Right, Forward );

   type Instruction(Kind: Instruction_Type) is record
      case Kind is
         when Left => null;
         when Right => null;
         when Forward => Distance: Positive;
      end case;
   end record;

   package Instruction_Vectors is new Ada.Containers.Indefinite_Vectors
         ( Index_Type => Positive,
           Element_Type => Instruction
         );

   Instructions: Instruction_Vectors.Vector;

   -- SUBSECTION
   -- orientation and how turns change them

   type Orientation is ( Left, Right, Up, Down );

   procedure Turn_Left(From: in out Orientation) is
   begin
      From := (case From is
                  when Left  => Down,
                  when Right => Up,
                  when Up    => Left,
                  when Down  => Right
              );
   end Turn_Left;

   procedure Turn_Right(From: in out Orientation) is
   begin
      From := ( case From is
                   when Left => Up,
                   when Right => Down,
                   when Up => Right,
                   when Down => Left
               );
   end Turn_Right;

   -- SUBSECTION
   -- path taken (for ppm visualization)

   type Position is record
      Row, Col: Positive;
   end record;

   package Position_Vectors is new Ada.Containers.Vectors
      ( Index_Type => Positive,
        Element_Type => Position
       );

   Path_Taken: Position_Vectors.Vector;

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

   Bad_Instruction: exception;

   procedure Read_Input is
   begin

      Text_IO.Open(Input_File, Text_IO.In_File, Filename);

      -- get map

      for Row in 1 .. Map_Rows loop

         declare
            Input_String: String := Text_IO.Get_Line(Input_File);
            Pos: Positive := Input_String'Last;
         begin

            for Col in 1 .. Pos loop

               Map(Row, Col) := ( case Input_String(Col) is
                                     when ' ' => Void,
                                     when '.' => Space,
                                     when '#' => Wall,
                                     when others => raise Bad_Instruction
                                 );

            end loop;

            for Col in Pos + 1 .. Map_Cols loop
               Map(Row, Col) := Void;
            end loop;

         end;

      end loop;

      -- skip blank line
      declare Blank_Line: String := Text_IO.Get_Line(Input_File);
      begin
         null;
      end;

      -- get instructions
      declare
         Input_String: String := Text_IO.Get_Line(Input_File);
         Pos         : Positive := Input_String'First;
         Value       : Positive;
      begin

         -- get first distance
         Get_Integer(Input_String, Value, Pos);
         Instructions.Append( Instruction'( Kind => Forward, Distance => Value ) );

         while Pos <= Input_String'Last loop
            case Input_String(Pos) is
            when 'L' => Instructions.Append( Instruction'( Kind => Left ) );
            when 'R' => Instructions.Append( Instruction'( Kind => Right ) );
            when others => raise Bad_Instruction;
            end case;

            Pos := Pos + 1;

            Get_Integer(Input_String, Value, Pos);
            Instructions.Append( Instruction'(Kind => Forward, Distance => Value ) );

         end loop;

      end;


      Text_IO.Close(Input_File);

   end Read_Input;

   procedure Put_Map is
   -- didn't actually use this much at all...
   begin
      for Row in 1 .. Map_Rows loop
         for Col in 1 .. Map_Cols loop
            Text_IO.Put( case Map(Row, Col) is
                            when Void => ' ',
                            when Space => '.',
                            when Wall => '#'
                        );
         end loop;
         Text_IO.New_Line;
      end loop;
   end Put_Map;

   procedure Put_Instructions is
   -- didn't actually use this much at all...
   begin
      for Instruction of Instructions loop
         case Instruction.Kind is
            when Left => Text_IO.Put('L');
            when Right => Text_IO.Put('R');
            when Forward => Integer_IO.Put(Instruction.Distance, 0);
         end case;
      end loop;
   end Put_Instructions;

   procedure Write_Path_To_Ppm(Part: Part_Number) is
   --writes the path taken out to a ppm

      Output_File  : Text_IO.File_Type;
      Num_Steps    : Natural := Natural(Path_Taken.Length);
      Tmp_Suffix   : String := ( if Num_Steps = 0 then " 0000"
                                 else Num_Steps'Image );
      Suffix       : array (1..6) of Character := ( others => '0' );

      type Pixel is record
         Red, Green, Blue: Natural;
      end record;

      Space_Value: pixel := ( Red | Green | Blue => 224 );
      Wall_Value : pixel := ( Red | Green | Blue => 128 );

      Bad_Coords: exception;

      First_Step_Drawn: Natural
         := Integer'Max(1, Integer(Path_Taken.Length) - 255);
      Last_Step_Drawn: Natural := Natural(Path_Taken.Length);

   begin

      if Num_Steps < 100 then
         for I in 5..6 loop
            Suffix(I) := Tmp_Suffix(I - 3);
         end loop;
      elsif Num_Steps < 1_000 then
         for I in 4..6 loop
            Suffix(I) := Tmp_Suffix(I - 2);
         end loop;
      elsif Num_Steps < 10_000 then
         for I in 3..6 loop
            Suffix(I) := Tmp_Suffix(I - 1);
         end loop;
      else
         for I in 2..6 loop
            Suffix(I) := Tmp_Suffix(I);
         end loop;
      end if;

      --  Text_IO.Put_Line("printing" & Num_Steps'Image & " " & String(Suffix));

      Text_IO.Create(Output_File,
                     Name =>
                        Part'Image & "/path_" & String(Suffix) & ".ppm"
                    );
      Text_IO.Put(Output_File, "P3");
      Text_IO.Put(Output_File, Map_Cols'Image);
      Text_IO.Put(Output_File, Map_Rows'Image);
      Text_IO.Put(Output_File, " 255"); -- max color
      Text_IO.New_Line(Output_File);

      declare
         Raster       : array( 1..Map_Rows, 1..Map_Cols ) of Pixel;
      begin

         -- read the map
         for I in 1 .. Map_Rows loop
            for J in 1 .. Map_Cols loop
               Raster(I,J) := ( if Map(I,J) = Void then
                                   ( Num_Steps, Num_Steps, Num_Steps )
                                elsif Map(I,J) = Space then
                                   Space_Value
                                elsif Map(I,J) = Wall then
                                   Wall_Value
                                else raise Bad_Coords
                                   with I'Image & J'Image
                               );
            end loop;
         end loop;

         for I in First_Step_Drawn .. Last_Step_Drawn loop
            declare
               Pos: Position := Path_Taken(I);
            begin
               Raster(Pos.Row, Pos.Col)
                  := ( Red  => 255,
                       Green => Last_Step_Drawn - I,
                       Blue  => 0 );
            end;
         end loop;

         for Row in 1 .. Map_Rows loop
            for Col in 1 .. Map_Cols loop
               Text_IO.Put_Line(Output_File,
                                Raster(Row, Col).Red'Image
                                & Raster(Row, Col).Green'Image
                                & Raster(Row, Col).Blue'Image
                               );
               Text_IO.New_Line(Output_File);
            end loop;
            Text_IO.New_Line(Output_File);
         end loop;

      end;

      Text_IO.Close(Output_File);
   end Write_Path_To_Ppm;

   -- SECTION
   -- PART 1

   procedure Set_Start(Row, Col: out Positive) is
   -- sets the start position;
   -- essentially, it's looking for the top `Space`
   begin
      Row := 1;
      Col := 1;
      while Map(Row, Col) /= Space loop
         Col := Col + 1;
      end loop;
   end Set_Start;

   procedure Wrap_Right(Row: Natural; Col: in out Natural) is
   -- wraps the player right, by moving left willy-nilly,
   -- even through walls, until further movement would fall into the void
      Tmp_Col: Natural := Col;
   begin
      while Tmp_Col > 1 and then Map(Row, Tmp_Col - 1) /= Void loop
         Tmp_Col := Tmp_Col - 1;
      end loop;
      if Map(Row, Tmp_Col) /= Wall then
         Col := Tmp_Col;
      end if;
   end Wrap_Right;

   procedure Wrap_Left(Row: Natural; Col: in out Natural) is
   -- wraps the player left, by moving right willy-nilly,
   -- even through walls, until further movement would fall into the void
      Tmp_Col: Natural := Col;
   begin
      while Tmp_Col < Map'Last(2) and then Map(Row, Tmp_Col + 1) /= Void loop
         Tmp_Col := Tmp_Col + 1;
      end loop;
      if Map(Row, Tmp_Col) /= Wall then
         Col := Tmp_Col;
      end if;
   end Wrap_Left;

   procedure Wrap_Up(Row: in out Natural; Col: Natural) is
   -- wraps the player up, by moving down willy-nilly,
   -- even through walls, until further movement would fall into the void
      Tmp_Row: Natural := Row;
   begin
      while Tmp_Row < Map'Last(1) and then Map(Tmp_Row + 1, Col) /= Void loop
         Tmp_Row := Tmp_Row + 1;
      end loop;
      if Map(Tmp_Row, Col) /= Wall then
         Row := Tmp_Row;
      end if;
   end Wrap_Up;

   procedure Wrap_Down(Row: in out Natural; Col: Natural) is
   -- wraps the player down, by moving up willy-nilly,
   -- even through walls, until further movement would fall into the void
      Tmp_Row: Natural := Row;
   begin
      while Tmp_Row > 1 and then Map(Tmp_Row - 1, Col) /= Void loop
         Tmp_Row := Tmp_Row - 1;
      end loop;
      if Map(Tmp_Row, Col) /= Wall then
         Row := Tmp_Row;
      end if;
   end Wrap_Down;

   procedure Move( Row, Col: in out Natural;
                   Facing  : Orientation;
                   Distance: Positive
                  )
   is
   -- performs the requested move, observing the wrapping rules
   -- described by Wrap_Up, Wrap_Down, Wrap_Left, Wrap_Right

      Tmp_Row, Tmp_Col: Positive; -- temporary positions

      Row_Delta: constant array( Orientation ) of Integer
         := ( Left | Right => 0, Up => -1, Down => 1 );
      -- how current orientation changes the row

      Col_Delta: constant array( Orientation ) of Integer
         := ( Left => -1, Right => 1, Up | Down => 0 );
      -- how current orientation changes the column

   begin

      for I in 1 .. Distance loop

         -- in the normal map range?
         if Row + Row_Delta(Facing) in Map'Range(1)
            and then Col + Col_Delta(Facing) in Map'Range(2)
         then

            -- yes; test movement validity
            Tmp_Row := Row + Row_Delta(Facing);
            Tmp_Col := Col + Col_Delta(Facing);

            case Map(Tmp_Row, Tmp_Col) is

               when Void =>
                  -- need to wrap
                  case Facing is
                  when Up => Wrap_Up(Row, Col);
                  when Down => Wrap_Down(Row, Col);
                  when Left => Wrap_Left(Row, Col);
                  when Right => Wrap_Right(Row, Col);
                  end case;

               when Space =>
                  -- yay! move
                  Row := Tmp_Row;
                  Col := Tmp_Col;

               when Wall =>
                  -- no can do
                  null;

            end case;

         elsif Row + Row_Delta(Facing) = 0 then
            -- we've gone too far up; wrap up
            Wrap_Up(Row, Col);

         elsif Row + Row_Delta(Facing) > Map'Last(1) then
            -- we've gone too far down; wrap down
            Wrap_Down(Row, Col);

         elsif Col + Col_Delta(Facing) = 0 then
            -- we've gone too far left; wrap left
            Wrap_Left(Row, Col);

         elsif Col + Col_Delta(Facing) > Map'Last(2) then
            -- we've gone too far right; wrap right
            Wrap_Right(Row, Col);

         end if;

         Path_Taken.Append( Position'( Row, Col ) );
         if Positive(Path_Taken.Length) rem 50 = 0 then
            Write_Path_To_Ppm(First);
         end if;

      end loop;

   end Move;

   function Final_Password return Natural is
   -- determine the final password by following the instructions
   -- to move around the map

      Row, Col: Natural;
      Facing  : Orientation := Right;

   begin

      Set_Start(Row, Col);

      Path_Taken.Clear;
      Path_Taken.Append( Position'( Row, Col ) );

      for Instruction of Instructions loop

         case Instruction.Kind is
            when Left =>
               Turn_Left(Facing);
               Path_Taken.Append( Position'( Row, Col ) );
            when Right =>
               Turn_Right(Facing);
               Path_Taken.Append( Position'( Row, Col ) );
            when Forward =>
               Move(Row, Col, Facing, Instruction.Distance);
               -- appending to path handled in Move
         end case;

         if Positive(Path_Taken.Length) rem 50 = 0 then
            Write_Path_To_Ppm(First);
         end if;

      end loop;

      return 1000 * Row + 4 * Col
         + ( case Facing is
                when Left => 2,
                when Right => 0,
                when Up => 3,
                when Down => 1
            );

   end Final_Password;

   -- SECTION
   -- PART 2

   -- I refer to my cube's sectors as
   --     111 222
   --     111 222
   --     111 222
   --
   --     333
   --     333
   --     333
   --
   -- 444 555
   -- 444 555
   -- 444 555
   --
   -- 666
   -- 666   <- lol
   -- 666

   Bad_Coords     : exception;
   -- raised when a sector is requested for coordinates
   -- that do not correspond to a known sector

   function In_Sector(Row, Col: Positive) return Positive is
   -- returns the sector containing the given row and column;
   -- if no such sector exists, raises `Bad_Coords`

      (if Row in 1..50 then
          ( if Col in 51..100 then 1
           elsif Col in 101..150 then 2
           else raise Bad_Coords
          )

       elsif Row in 51..100 then
          ( if Col in 51..100 then 3
           else raise Bad_Coords
          )

       elsif Row in 101..150 then
          ( if Col in 1..50 then 4
           elsif Col in 51..100 then 5
           else raise Bad_Coords
          )

       elsif Row in 151..200 then
          ( if Col in 1..50 then 6
           else raise Bad_Coords
          )

       else raise Bad_Coords
      );

   Bad_Sector: exception;

   procedure Wrap_Up_Cube(Row, Col: in out Natural; Facing: out Orientation) is
   -- wraps the player up according to the rules of motion on the given cube,
   -- modifying the orientation accordingly

   begin

      case In_Sector(Row, Col) is

         when 1 =>
            -- must be on row 1,
            -- moves to sector 6 by exchange row, col and shift down
            if Map(100 + Col, 1) /= Wall then
               Row := 100 + Col;
               Col := 1;
               Facing := Right;
            else
               Facing := Up;
            end if;

         when 2 =>
            -- must be on row 1,
            -- moves to sector 6 by shift down and left
            if Map(200, Col - 100) /= Wall then
               Row := 200;
               Col := Col - 100;
            end if;
            Facing := Up;

         when 4 =>
            -- must be on row 101,
            -- moves to sector 3 by exchange row, col and shift left, down
            if Map(Col + 50, 51) /= Wall then
               Row := Col + 50;
               Col := 51;
               Facing := Right;
            else
               Facing := Up;
            end if;

         when others =>
            -- should not wrap up from 3, 5, or 6
            raise Bad_Sector
               with "cannot wrap up from" & Row'Image & Col'Image;

      end case;

   end Wrap_Up_Cube;

   procedure Wrap_Down_Cube(Row, Col: in out Natural; Facing: out Orientation)
   is
   -- wraps the player up according to the rules of motion on the given cube,
   -- modifying the orientation accordingly

   begin

      case In_Sector(Row, Col) is

         when 2 =>
            -- must be on row 50,
            -- moves to sector 3 by exchange row, col and shift right, up
            if Map(Col - 50, 100) /= Wall then
               Row := Col - 50;
               Col := 100;
               Facing := Left;
            else
               Facing := Down;
            end if;

         when 5 =>
            -- must be on row 150,
            -- moves to sector 6 by exchange row, col and shift up, right
            if Map(Col + 100, 50) /= Wall then
               Row := Col + 100;
               Col := 50;
               Facing := Left;
            else
               Facing := Down;
            end if;

         when 6 =>
            -- must be on row 200,
            -- moves to sector 2 by shift up, right
            if Map(1, Col + 100) /= Wall then
               Row := 1;
               Col := Col + 100;
            end if;
            Facing := Down;

         when others =>
            raise Bad_Sector
               with "cannot wrap down from" & Row'Image & Col'Image;

      end case;

   end Wrap_Down_Cube;

   procedure Wrap_Left_Cube(Row, Col: in out Natural; Facing: out Orientation)
   is
   -- wraps the player left according to the rules of motion on the given cube,
   -- modifying the orientation accordingly

   begin

      case In_Sector(Row, Col) is

         when 1 =>
            -- must be on column 51,
            -- moves to sector 4 by reversing rows
            if Map(150 - Row + 1, 1) /= Wall then
               Row := 150 - Row + 1;
               Col := 1;
               Facing := Right;
            else
               Facing := Left;
            end if;

         when 3 =>
            -- must be on column 51,
            -- moves to sector 4 by exchange row, col and shift left, down
            if Map(101, Row - 50) /= Wall then
               Col := Row - 50;
               Row := 101;
               Facing := Down;
            else
               Facing := Left;
            end if;

         when 4 =>
            -- must be on column 1,
            -- moves to sector 1 by reversing rows
            if Map(150 + 1 - Row, 51) /= Wall then
               Row := 150 + 1 - Row;
               Col := 51;
               Facing := Right;
            else
               Facing := Left;
            end if;

         when 6 =>
            -- must be on column 1,
            -- moves to sector 1 by exchange row, col and shift right
            if Map(1, Row - 100) /= Wall then
               Col := Row - 100;
               Row := 1;
               Facing := Down;
            else
               Facing := Left;
            end if;

         when others =>
            raise Bad_Sector
               with "cannot wrap left from" & Row'Image & Col'Image;
      end case;
   end Wrap_Left_Cube;

   procedure Wrap_Right_Cube(Row, Col: in out Natural; Facing: out Orientation)
   is
   -- wraps the player right according to the rules of motion on the given cube,
   -- modifying the orientation accordingly

   begin

      case In_Sector(Row, Col) is

         when 2 =>
            -- must be on column 150,
            -- moves to sector 5 by reversing rows
            if Map(50 - Row + 101, 100) /= Wall then
               Row := 50 - Row + 101;
               Col := 100;
            end if;
            Facing := Right;

         when 3 =>
            -- must be on column 100,
            -- moves to sector 2 by exchange row, col and shift right, up
            if Map(50, Row + 50) /= Wall then
               Col := Row + 50;
               Row := 50;
               Facing := Up;
            else
               Facing := Right;
            end if;

         when 5 =>
            -- must be on column 100,
            -- moves to sector 2 by reversing rows
            if Map(150 - Row + 1, 150) /= Wall then
               Row := 150 - Row + 1;
               Col := 150;
               Facing := Left;
            else
               Facing := Right;
            end if;

         when 6 =>
            -- must be on column 50,
            -- moves to sector 5 by exchange row, col and shift left,down
            if Map(150, Row - 100) /= Wall then
               Col := Row - 100;
               Row := 150;
               Facing := Up;
            else
               Facing := Right;
            end if;

         when others =>
            raise Bad_Sector
               with "cannot wrap right from" & Row'Image & Col'Image;

      end case;

   end Wrap_Right_Cube;

   procedure Move_Cube( Row, Col: in out Natural;
                   Facing  : in out Orientation;
                   Distance: Positive
                  )
   is
   -- performs the requested move, observing the wrapping rules
   -- described by Wrap_Up_Cube, Wrap_Down_Cube, Wrap_Left_Cube, Wrap_Right_Cube

      Tmp_Row, Tmp_Col: Positive;

      Row_Delta: constant array( Orientation ) of Integer
         := ( Left | Right => 0, Up => -1, Down => 1 );
      -- how current orientation changes the row

      Col_Delta: constant array( Orientation ) of Integer
         := ( Left => -1, Right => 1, Up | Down => 0 );
      -- how current orientation changes the column

   begin

      for I in 1 .. Distance loop

         -- in the normal map range?
         if Row + Row_Delta(Facing) in Map'Range(1)
            and then Col + Col_Delta(Facing) in Map'Range(2)
         then

            -- yes; test movement validity
            Tmp_Row := Row + Row_Delta(Facing);
            Tmp_Col := Col + Col_Delta(Facing);

            case Map(Tmp_Row, Tmp_Col) is

               when Void =>
                  -- need to wrap
                  case Facing is
                  when Up => Wrap_Up_Cube(Row, Col, Facing);
                  when Down => Wrap_Down_Cube(Row, Col, Facing);
                  when Left => Wrap_Left_Cube(Row, Col, Facing);
                  when Right => Wrap_Right_Cube(Row, Col, Facing);
                  end case;

               when Space =>
                  -- yay! move
                  Row := Tmp_Row;
                  Col := Tmp_Col;

               when Wall =>
                  -- no can do
                  null;
            end case;

         elsif Row + Row_Delta(Facing) = 0 then
            -- we've gone too far up; wrap up
            Wrap_Up_Cube(Row, Col, Facing);

         elsif Row + Row_Delta(Facing) > Map'Last(1) then
            -- we've gone too far down; wrap down
            Wrap_Down_Cube(Row, Col, Facing);

         elsif Col + Col_Delta(Facing) = 0 then
            -- we've gone too far left; wrap left
            Wrap_Left_Cube(Row, Col, Facing);

         elsif Col + Col_Delta(Facing) > Map'Last(2) then
            -- we've gone too far right; wrap right
            Wrap_Right_Cube(Row, Col, Facing);

         end if;

         Path_Taken.Append( Position'( Row, Col ) );

         if Positive(Path_Taken.Length) rem 50 = 0 then
            Write_Path_To_Ppm(Second);
         end if;

      end loop;

   end Move_Cube;

   function Final_Password_Cube return Natural is
   -- determine the final password by following the instructions
   -- to move around the **cube** map

      Row, Col: Natural;
      Facing  : Orientation := Right;

   begin

      Set_Start(Row, Col);

      Path_Taken.Clear;
      Path_Taken.Append( Position'( Row, Col ) );

      for Instruction of Instructions loop
         case Instruction.Kind is
            when Left =>
               Turn_Left(Facing);
               Path_Taken.Append( Position'( Row, Col ) );
            when Right =>
               Turn_Right(Facing);
               Path_Taken.Append( Position'( Row, Col ) );
            when Forward =>
               Move_Cube(Row, Col, Facing, Instruction.Distance);
               -- appending to path taken care of by `Move_Cube`
         end case;
         Path_Taken.Append( Position'( Row, Col ) );
         if Positive(Path_Taken.Length) rem 50 = 0 then
            Write_Path_To_Ppm(Second);
         end if;
      end loop;

      return 1000 * Row + 4 * Col
         + ( case Facing is
                when Left => 2,
                when Right => 0,
                when Up => 3,
                when Down => 1
            );

   end Final_Password_Cube;

begin

   Read_Input;

   Write_Path_To_Ppm(First);
   Text_IO.Put_Line("final password on the map is" & Final_Password'Image);
   Write_Path_To_Ppm(First);

   Path_Taken.Clear;
   Write_Path_To_Ppm(Second);
   Text_IO.Put_Line("final password on the cube is"
                    & Final_Password_Cube'Image);
   Write_Path_To_Ppm(Second);

end Main;
