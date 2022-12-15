-- Advent of Code 2022, Day 15
--
-- John Perry
--
-- Beacon Exclusion Zone
--
-- part 1: find how many positions are detected by a sensor on row 2_000_000
--
-- part 2: find the location of the only position that cannot be detected
--    by a sensor in the range 0..4_000_000, 0..4_000_000
--

with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;

procedure Main is

   package Text_IO renames Ada.Text_IO;
   package Integer_IO is new Ada.Text_IO.Integer_IO ( Num => Integer );

   Doing_Example: constant Boolean := False;

   -- SECTION
   -- global types and variables

   type Position is record
      Row: Integer;
      Col: Integer;
   end record;

   function "<"(Left, Right: Position) return Boolean is
   -- lexicographic ordering
      ( Left.Row < Right.Row
        or else ( Left.Row = Right.Row and then Left.Col < Right.Col )
       );

   package Position_Sets is new Ada.Containers.Ordered_Sets
      (
       Element_Type => Position
      );

   type Sensor_And_Beacon is record
      Sensor, Beacon: Position;
   end record;

   package Sensor_Vectors is new Ada.Containers.Vectors
      (
       Index_Type => Positive,
       Element_Type => Sensor_And_Beacon
      );

   Beacon_Positions: Position_Sets.Set;

   Sensors: Sensor_Vectors.Vector;

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

   procedure Read_Sensors is
   -- read the sensor / beacon data

   begin

      Text_IO.Open(Input_File, Text_IO.In_File, Filename);

      while not Text_IO.End_Of_File(Input_File) loop

         declare
            Input_String: String := Text_IO.Get_Line(Input_File);
            Sensor_X, Sensor_Y, Beacon_X, Beacon_Y: Integer;
            Pos: Positive := 13;
         begin

            Get_Integer(Input_String, Sensor_X, Pos);
            Pos := Pos + 4;
            Get_Integer(Input_String, Sensor_Y, Pos);
            Pos := Pos + 25;
            Get_Integer(Input_String, Beacon_X, Pos);
            Pos := Pos + 4;
            Get_Integer(Input_String, Beacon_Y, Pos);

            Sensors.Append
               (
                   (
                Sensor => (Col => Sensor_X, Row => Sensor_Y),
                Beacon => (Col => Beacon_X, Row => Beacon_Y)
               )
               );

            Beacon_Positions.Include( ( Col => Beacon_X, Row => Beacon_Y ) );

         end;

      end loop;

      Text_IO.Close(Input_File);

   end Read_Sensors;

   procedure Beaconless_Positions_At_Goal_Row is
   -- part 1: find all the positions a on the goal row
   --    that cannot contain a beacon

      Goal_Row         : constant Integer
         := ( if Doing_Example then 10 else 2_000_000 );

      Sensor, Beacon   : Position;

      Dist             : Natural;
      Dist_To_Goal     : Natural;

      Reached_Positions: Position_Sets.Set;
      -- positions on the goal row that we detect with _some_ sensor

   begin

      for Sb of Sensors loop

         Sensor := Sb.Sensor;
         Beacon := Sb.Beacon;

         -- Manhattan distance from the sensor to its beacon
         Dist := abs(Sensor.Row - Beacon.Row) + abs(Sensor.Col - Beacon.Col);

         -- is it close enough?
         if Dist >= abs(Sensor.Row - Goal_Row) then

            -- yes; determine how far to the goal row
            Dist_To_Goal := abs(Sensor.Row - Goal_Row);

            -- include the positions on the goal row that this sensor can detect
            for Col in Sensor.Col - (Dist - Dist_To_Goal) ..
               Sensor.Col + (Dist - Dist_To_Goal)
            loop

               if not Beacon_Positions.Contains((Row => Goal_Row, Col => Col))
               then
                  Reached_Positions.Include((Row => Goal_Row, Col => Col));
               end if;

            end loop;

         end if;

      end loop;

      Text_IO.Put_Line("No beacon for" & Reached_Positions.Length'Image
                      & " positions at row" & Goal_Row'Image
                      );

   end Beaconless_Positions_At_Goal_Row;

   procedure Beaconless_Position_In_Box is
   -- part 2: find the one position the sensors cannot detect
   -- in a 4_000_000 square box whose top left corner is at the origin
   --
   -- I find it amazing that we can pull this off...

      Sensor, Beacon      : Position;

      Start_Row, Start_Col: constant Integer := 0;
      End_Row, End_Col    : constant Integer
         := ( if Doing_Example then 20 else 4_000_000 );

      Dist                : Natural;
      Dist_To_Goal        : Natural;
      Row, Col, Old_Col   : Integer;
      -- we use Old_Col to remember where Col was
      -- when a sensor can detect the position (Row, Col),
      -- we can skedaddle Col over to the end of the sensor's range,
      -- and we use Old_Col to detect this motion

   begin

      Row := Start_Row;

      Row_Loop:
      while Row <= End_Row loop

         -- after part 1, I wanted to make sure this approach
         -- wasn't taking too long
         -- it wasn't! so I should probably rewrite part 1
         if Row rem 1000 = 0 then
            Text_IO.Put_Line("on row" & Row'Image);
         end if;

         Col := Start_Col;
         while Col <= End_Col loop

            Old_Col := Col;
            -- remember column to test for motion

            for Sb of Sensors loop

               Sensor := Sb.Sensor;
               Beacon := Sb.Beacon;
               Dist := abs(Sensor.Row - Beacon.Row)
                  + abs(Sensor.Col - Beacon.Col);

               -- can this sensor detect things on this row?
               if Dist >= abs(Sensor.Row - Row) then

                  -- can it detect this column, as well?
                  Dist_To_Goal := abs(Sensor.Row - Row);
                  -- gotta love Ada's ranges!
                  if Col in Sensor.Col - (Dist - Dist_To_Goal) ..
                     Sensor.Col + (Dist - Dist_To_Goal)
                  then
                     Col := Sensor.Col + (Dist - Dist_To_Goal) + 1;
                  end if;

               end if;

            end loop;

            -- I LOVE THIS
            if Col = Old_Col then exit Row_Loop; end if;

         end loop;

         Row := Row + 1;

      end loop Row_Loop;

      Text_IO.Put_Line("row" & Row'Image & " col" & Col'Image);
      Text_IO.Put_Line("Tuning frequency is"
                       & Long_Long_Integer'Image
                          ( Long_Long_Integer(Col) * 4_000_000
                           + Long_Long_Integer(Row) )
                      );

   end Beaconless_Position_In_Box;

begin

   Read_Sensors;

   Beaconless_Positions_At_Goal_Row;
   Beaconless_Position_In_Box;

end Main;
