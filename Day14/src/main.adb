-- Advent of Code 2022, Day 14
--
-- John Perry
--
-- Regolith Reservoir
--
-- part 1: count the number of sand particles that don't fall into the void
--
-- part 2: add a ledge; count the number of sand particles that fall before
--    piling up to the source

with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;

procedure Main is

   package Text_IO renames Ada.Text_IO;
   package Natural_IO is new Ada.Text_IO.Integer_IO ( Num => Natural );

   Doing_Example: constant Boolean := False;
   type Part_Type is ( First, Second );

   -- SECTION
   -- global types and variables

   type Filling is ( Rock, Sand, Source, None );
   type Position is record
      X, Y: Natural;
   end record;

   function Position_Hash(P: Position) return Ada.Containers.Hash_Type is
      ( Ada.Containers.Hash_Type( P.X * 1073 + P.Y ) );

   package Reservoir_Maps is new Ada.Containers.Hashed_Maps
      (
       Key_Type     => Position,
       Element_Type    => Filling,
       Hash            => Position_Hash,
       Equivalent_Keys => "="
      );

   Reservoir_Map, Old_Reservoir: Reservoir_Maps.Map;

   Min_X, Min_Y: Natural := Natural'Last;
   Max_X, Max_Y: Natural := Natural'First;

   type Sand_State is ( Moving, Producing, Void );

   -- SECTION
   -- I/O

   -- SUBSECTION
   -- input

   Filename: constant String
      := ( if Doing_Example then "example.txt" else "input.txt" );

   Input_File: Text_IO.File_Type;

   Bad_Structure: exception;

   procedure Read_Chain(Chain: String) is
   -- reads the chain of rock structures from `Chain`

      Prev: Position; -- previous rock structure
      X, Y: Natural;  -- current rock structure
      Pos : Positive := Chain'First; -- current position in `Chain`
      Last: Positive := Chain'Last;  -- last possible position in `Chain`

   begin

      -- get first position
      Natural_IO.Get(Chain, Prev.X, Pos);
      Pos := Pos + 2;
      Natural_IO.Get(Chain(Pos..Last), Prev.Y, Pos);
      Pos := Pos + 5;

      -- get remaining positions and record rock
      while Pos < Last loop

         -- get position
         Natural_IO.Get(Chain(Pos..Last), X, Pos);
         Pos := Pos + 2;
         Natural_IO.Get(Chain(Pos..Last), Y, Pos);
         Pos := Pos + 5;

         -- build rock
         if X = Prev.X and Y /= Prev.Y then
            -- vertical
            for Row in Natural'Min(Prev.Y, Y) .. Natural'Max(Prev.Y, Y) loop
               Reservoir_Map.Include
                  (
                   Key      => ( X, Row ),
                   New_Item => Rock
                  );
            end loop;

         elsif Y = Prev.Y and then X /= Prev.x then
            -- horizontal
            for Col in Natural'Min(Prev.X, X) .. Natural'Max(Prev.X, X) loop
               Reservoir_Map.Include
                  (
                   Key      => ( Col, Y ),
                   New_Item => Rock
                  );
            end loop;

         else
            -- eek
            raise Bad_Structure;

         end if;

         -- update
         Prev.X := X; Prev.Y := Y;
         Min_X := Natural'Min(X, Min_X);
         Min_Y := Natural'Min(Y, Min_Y);
         Max_X := Natural'Max(X, Max_X);
         Max_Y := Natural'Max(Y, Max_Y);

      end loop;

   end Read_Chain;

   procedure Read_Structures is
   -- reads the input from disk

   begin

      Min_X := 500; Max_X := 500;
      Min_Y := 0;   Max_Y := 0;
      Reservoir_Map.Include((500, 0), Source);

      Text_IO.Open(Input_File, Text_IO.In_File, Filename);

      while not Text_IO.End_Of_File(Input_File) loop

         declare
            Input_String: String := Text_IO.Get_Line(Input_File);
         begin
            Read_Chain(Input_String);
         end;

      end loop;

      Text_IO.Close(Input_File);

   end Read_Structures;

   -- SUBSECTION
   -- output

   Display_Char: array( Filling ) of Character
      := ( Rock => '#', Sand => 'o', Source => '+', None => '.' );
   -- text to display for each filling

   procedure Print_Reservoir is
   -- prints the reservoir to standard output
   begin

      for Y in Min_Y .. Max_Y loop
         for X in Min_X .. Max_X loop

            if Reservoir_Map.Contains((X,Y)) then
               Text_IO.Put(Display_Char(Reservoir_Map.Constant_Reference((X,Y))));
            else
               Text_IO.Put('.');
            end if;

         end loop;
         Text_IO.New_Line;
      end loop;

   end Print_Reservoir;

   procedure Write_Ppm(Part: Part_Type := First) is
   -- writes the reservoir out as a ppm

      Output_File  : Text_IO.File_Type;
      Width, Height: Integer;

   begin

      Width := Max_X - Min_X;
      Height := Max_Y - Min_Y;
      Text_IO.Create(Output_File, Name =>"pile_" & Part'Image & ".ppm");
      Text_IO.Put(Output_File, "P3");
      Text_IO.Put(Output_File, Natural'Image(Width + 3)); -- extra for last line
      Text_IO.Put(Output_File, Natural'Image(Height + 1));-- extra for last line
      Text_IO.Put(Output_File, " 255"); -- max color
      Text_IO.New_Line(Output_File);

      declare
         Raster: array(0 .. Height + 1, -1 .. Width + 1 ) of Filling
            := ( others => ( others => None ) );
         Pts_Processed: Natural := 0;
      begin

         -- read the map
         for Pos in Reservoir_Map.Iterate loop
            Raster(Reservoir_Maps.Key(Pos).Y - Min_Y, Reservoir_Maps.Key(Pos).X - Min_X)
               := Reservoir_Maps.Element(Pos);
         end loop;

         -- add bottom ledge
         for X in -1 .. Width + 1 loop
            Raster(Height, X) := Rock;
         end loop;

         for Row in 0 .. Height + 1 loop
            for Col in -1 .. Width + 1 loop
               case Raster(Row,Col) is
                  when Rock => Text_IO.Put_Line(Output_File, "128 128 128");
                  when Sand => Text_IO.Put_Line(Output_File, "224 224 0");
                  when None => Text_IO.Put_Line(Output_File, "255 255 255");
                  when Source => Text_IO.Put_Line(Output_File, "0 0 0");
               end case;
            end loop;
            Text_IO.New_Line(Output_File);
         end loop;

      end;

      Text_IO.Close(Output_File);

   end Write_Ppm;

   -- SECTION
   -- puzzle logic

   procedure Update_Pos_And_State
      (Particle: in out Position; State: in out Sand_State)
   is
   -- updates position and state according to what's in the map
   begin

      if not Reservoir_Map.Contains((Particle.X, Particle.Y + 1)) then
         Particle.Y := Particle.Y + 1;
      elsif not Reservoir_Map.Contains((Particle.X - 1, Particle.Y + 1)) then
         Particle.X := Particle.X - 1;
         Particle.Y := Particle.Y + 1;
      elsif not Reservoir_Map.Contains((Particle.X + 1, Particle.Y + 1)) then
         Particle.X := Particle.X + 1;
         Particle.Y := Particle.Y + 1;
      else
         Reservoir_Map.Include((Particle.X, Particle.Y), Sand);
         State := Producing;
      end if;

   end Update_Pos_And_State;

   Bad_State: exception;

   function Count_Sand(Part: Part_Type := First) return Natural is
   -- counts the particles of sand that occur through the entire puzzle

      Particle: Position;
      Particles: Natural := 0;
      State: Sand_State := Producing;

   begin

      while State /= Void loop

         -- have we filled up in part 2?
         if Part = Second
            and then Reservoir_Map.Constant_Reference((500, 0)) = Sand
         then exit;
         end if;

         case State is
         when Producing =>
            -- new particle
            Particle := ( 500, 0 );
            Particles := Particles + 1;
            State := Moving;

         when Moving =>

            Update_Pos_And_State(Particle, State);

            -- what we do now depends on the puzzle part
            case Part is

            when First =>
               if Particle.Y > Max_Y then
                  State := Void;
               end if;

            when Second =>
               Min_X := Natural'Min(Particle.X, Min_X);
               Max_X := Natural'Max(Particle.X, Max_X);
               if Particle.Y = Max_Y then
                  Reservoir_Map.Include((Particle.X, Particle.Y), Sand);
                  State := Producing;
               end if;
            end case;

         when Void =>
            raise Bad_State;

         end case;

      end loop;

      Max_Y := Max_Y + 1;
      return ( if Part = First then Particles - 1 else Particles );

   end Count_Sand;

begin

   Read_Structures;
   --  Print_Reservoir;

   Old_Reservoir := Reservoir_Map;
   Ada.Text_IO.Put_Line(Count_Sand'Image & " particles were produced");
   --  Print_Reservoir;
   Write_Ppm;

   Reservoir_Map := Old_Reservoir;
   Ada.Text_IO.Put_Line(Count_Sand(Second)'Image & " particles were produced");
   --  Print_Reservoir;
   Write_Ppm(Second);

end Main;
