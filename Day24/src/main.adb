-- Advent of Code 2022, Day 24
--
-- John Perry
--
-- Blizzard Basin
--
-- part 1: how long will it take to cross a valley while avoiding blizzards?
--
-- part 2: an idiot elf forgot its snacks. how long will the entire journey of
--    crossing the valley, returning to retrieve the snacks, then crossing anew,
--    take?
--

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;

use type Ada.Containers.Count_Type;

procedure Main is

   package Text_IO renames Ada.Text_IO;
   package Integer_IO is new Ada.Text_IO.Integer_IO ( Num => Integer );
   package Natural_IO is new Ada.Text_IO.Integer_IO ( Num => Natural );

   Doing_Example: constant Boolean := False;

   type Part_Number is ( First, Second );

   With_Visualization: constant Boolean := False;

   -- SECTION
   -- global types and variables

   -- SUBSECTION
   -- map locations

   subtype Row_Range is Natural range 1 .. ( if Doing_Example then 6 else 27 );
   subtype Col_Range is Natural range 1 .. ( if Doing_Example then 8 else 122 );

   type Location is record
      Row: Row_Range;
      Col: Col_Range;
   end record;

   -- SUBSECTION
   -- blizzard data

   type Direction is ( Up, Down, Left, Right );

   type Blizzard is record
      Loc: Location;  -- starting location
      Dir: Direction; -- direction it blows
   end record;

   package Blizzard_Vectors is new Ada.Containers.Vectors
      ( Index_Type => Positive,
        Element_Type => Blizzard
       );

   Blizzards: Blizzard_Vectors.Vector;

   -- SUBSECTION
   -- caching the blizzards' locations at each minute

   function Location_Hash(L: Location) return Ada.Containers.Hash_Type is
      ( Ada.Containers.Hash_Type( L.Row * Col_Range'Last + L.Col ) );

   package Location_Sets is new Ada.Containers.Hashed_Maps
      ( Key_Type            => Location,
        Element_Type        => Natural,
        Hash                => Location_Hash,
        Equivalent_Keys     => "="
       );

   function "="(Left, Right: Location_Sets.Map) return Boolean is
      ( Left.Length = Right.Length and then
           ( for all Cursor in Left.Iterate =>
                   Right.Contains(Location_Sets.Key(Cursor)) )
       );
   package Blizzard_Location_Vectors is new Ada.Containers.Vectors
      ( Index_Type  => Positive,
        Element_Type => Location_Sets.Map
       );
   Blizzard_Locations: Blizzard_Location_Vectors.Vector;

   -- SUSBECTION
   -- expedition state

   package Location_Vectors is new Ada.Containers.Vectors
      ( Index_Type => Positive,
        Element_Type => Location
      );

   type State is record
      Min: Natural;
      Path: Location_Vectors.Vector;
      Loc : Location;
   end record;

   Empty_State_Vec: Location_Vectors.Vector;

   package Queue_Interfaces
   is new Ada.Containers.Synchronized_Queue_Interfaces
      ( Element_Type => State );

   package Queues is new Ada.Containers.Unbounded_Synchronized_Queues
      ( Queue_Interfaces => Queue_Interfaces);

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

   Invalid_Character: exception; -- pointless, btu just in case

   procedure Read_Input is
   -- read the map, set up the blizzards

   begin

      Text_IO.Open(Input_File, Text_IO.In_File, Filename);

      -- valley wall
      declare Input_String: String := Text_IO.Get_Line(Input_File);
      begin
         null;
      end;

      for Row in 2 .. Row_Range'Last - 1 loop
         declare Input_String: String := Text_IO.Get_Line(Input_File);
         begin
            for Col in 2 .. Col_Range'Last - 1 loop
               case Input_String(Col) is
                  when '.' => null;
                  when '>' => Blizzards.Append( Blizzard'
                                                   ( ( Row, Col ), Right ) );
                  when '<' => Blizzards.Append( Blizzard'
                                                   ( ( Row, Col ), Left ) );
                  when '^' => Blizzards.Append( Blizzard'
                                                   ( ( Row, Col ), Up ) );
                  when 'v' => Blizzards.Append( Blizzard'
                                                   ( ( Row, Col ), Down ) );
                  when others => raise Invalid_Character;
               end case;
            end loop;
         end;
      end loop;

      Text_IO.Close(Input_File);

   end Read_Input;

   procedure Put_Map(Min: Natural) is
   -- proved useful during debugging

   begin

      -- top wall
      Text_IO.Put('#'); Text_IO.Put('.');
      for Col in 3 .. Col_Range'Last loop
         Text_IO.Put('#');
      end loop;
      Text_IO.New_Line;

      -- next row - 2 walls
      for Row in 2 .. Row_Range'Last - 1 loop
         Text_IO.Put('#');
         for Col in 2 .. Col_Range'Last - 1 loop
            Text_IO.Put(( if Blizzard_Locations(Min).Contains(( Row, Col ))
                        then 'X'
                        else '.'
                       ));
         end loop;
         Text_IO.Put_Line("#");
      end loop;

      -- bottom wall
      for Col in 1 .. Col_Range'Last - 2 loop
         Text_IO.Put('#');
      end loop;
      Text_IO.Put('.'); Text_IO.Put_Line("#");

   end Put_Map;

   procedure Write_State_As_Ppm(Path: Location_Vectors.Vector) is
   --writes the map of blizzards and expedition out to a ppm

      Output_File  : Text_IO.File_Type;
      Suffix       : array (1..4) of Character := ( others => '0' );

      type Pixel is record
         Red, Green, Blue: Natural;
      end record;

      Exped_Pixel: Pixel := ( Red | Blue => 64, Green => 192 );
      Empty_Pixel: Pixel := ( others => 255 );
      Wall_Pixel : Pixel := ( others => 0 );

      Bliz_Val   : array( Direction ) of Natural := ( 64, 96, 128, 160 );

      function Bliz_Pixel(I: Positive) return Pixel is
         ( ( Red => Bliz_Val(Blizzards(I).Dir),
             Green => Bliz_Val(Blizzards(I).Dir),
             Blue => Bliz_Val(Blizzards(I).Dir)
            )
         );

      Current_Blizzards: Location_Sets.Map;
      Loc: Location;

   begin

      for Min in 1 .. Positive(Path.Length) loop

         declare Tmp_Suffix: String := Min'Image;
         begin
            if Min < 10 then
               Suffix(4) := Tmp_Suffix(2);
            elsif Min < 100 then
               Suffix(3) := Tmp_Suffix(2);
               Suffix(4) := Tmp_Suffix(3);
            else
               for I in 2..4 loop
                  Suffix(I) := Tmp_Suffix(I);
               end loop;
            end if;
         end;

         Text_IO.Create(Output_File,
                        Name =>
                           "min_" & String(Suffix) & ".ppm"
                       );
         Text_IO.Put(Output_File, "P3");
         Text_IO.Put(Output_File, Natural'Image(Col_Range'Last - Col_Range'First + 1));
         Text_IO.Put(Output_File, Natural'Image(Row_Range'Last - Row_Range'First + 1));
         Text_IO.Put(Output_File, " 255"); -- max color
         Text_IO.New_Line(Output_File);

         if Min > 1 then
            Current_Blizzards := Blizzard_Locations(Min - 1);
         else
            for I in 1 .. Positive(Blizzards.Length) loop
               Current_Blizzards.Include( Blizzards(I).Loc, I );
            end loop;
         end if;

         Loc := Path(Min);

         for Row in Row_Range loop
            for Col in Col_Range loop
               declare
                  Write_Pixel: Pixel
                     := ( if Loc = ( Row, Col ) then Exped_Pixel
                          elsif Row = 1 then
                             ( if Col /= 2 then Wall_Pixel
                              else Empty_Pixel )
                          elsif Row = Row_Range'Last then
                             ( if Col /= Col_Range'Last - 1 then Wall_Pixel
                              else Empty_Pixel )
                          elsif ( Col = 1 or else Col = Col_Range'Last ) then Wall_Pixel
                          elsif Current_Blizzards.Contains( ( Row, Col ) )
                          then Bliz_Pixel( Current_Blizzards( ( Row, Col ) ) )
                          else Empty_Pixel );
               begin
                  Text_IO.Put(Output_File, Write_Pixel.Red'Image);
                  Text_IO.Put(Output_File, Write_Pixel.Green'Image);
                  Text_IO.Put(Output_File, Write_Pixel.Blue'Image);
                  Text_IO.New_Line(Output_File);
               end;
            end loop;
            Text_IO.New_Line(Output_File);
         end loop;

         Text_IO.Close(Output_File);

      end loop;

   end Write_State_As_Ppm;

   -- SECTION
   -- PARTS 1 AND 2

   -- SUBSECTION
   -- subprograms common to both types

   function Valid_Location(Row, Col: Natural) return Boolean is
   -- returns True when `(Row, Col)` is a valid map location for the expedition
      ( Row in Row_Range and then
        Col in 2 .. Col_Range'Last - 1 and then
           ( if Row = 1 then Col = 2 ) and then
              ( if Row = Row_Range'Last then Col = Col_Range'Last - 1 )
       );

   procedure Determine_Blizzards_At_Time(Min: Positive) is
   -- determines the blizzards at the given time, and stores them in the cache

      Ht     : constant Natural := Row_Range'Last - 2; -- navigable height
      Wd     : constant Natural := Col_Range'Last - 2; -- navigable width

      Min_Row: Natural := Min rem Ht; -- minute offset for rows
      Min_Col: Natural := Min rem Wd; -- minute offset for columns

      Row    : Row_Range; -- current location under consideration
      Col    : Col_Range;

      Result : Location_Sets.Map; -- the blizzards' locations; will be stored

   begin

      for I in 1 .. Positive(Blizzards.Length) loop

         case Blizzards(I).Dir is

            when Up =>
               Row := ( ( Blizzards(I).Loc.Row - 2 ) - Min_Row + Ht ) rem Ht + 2;
               if Row = 1 then Row := Ht - 1; end if;
               Result.Include( ( Row, Blizzards(I).Loc.Col ), I );

            when Down =>
               Row := ( ( Blizzards(I).Loc.Row - 2 + Min_Row ) rem Ht ) + 2;
               if Row = 1 then Row := Ht - 1; end if;
               Result.Include( ( Row, Blizzards(I).Loc.Col ), I );

            when Left =>
               Col := ( ( Blizzards(I).Loc.Col - 2 ) - Min_Col + Wd ) rem Wd + 2;
               if Col = 1 then Col := Wd - 1; end if;
               Result.Include( ( Blizzards(I).Loc.Row, Col ), I );

            when Right =>
               Col := ( ( Blizzards(I).Loc.Col - 2 ) + Min_Col ) rem Wd + 2;
               if Col = 1 then Col := Wd - 1; end if;
               Result.Include( ( Blizzards(I).Loc.Row, Col ), I );

         end case;

      end loop;

      Blizzard_Locations.Append(Result);

   end Determine_Blizzards_At_Time;

   function Minimize_Steps_To_Goal(Q: in out Queues.Queue; Goal: Location)
                                   return State
   is
   -- performs a breadth-first search
   -- to determine the number of steps necessary to reach the requested goal,
   -- given an initial set of position in the queue,
   -- which MUST be non-empty at the outset; otherwise this loops for ever

      type Remembered_State is record
         Min: Natural;
         Loc: Location;
      end record;

      function State_Hash(S: Remembered_State) return Ada.Containers.Hash_Type
      is
         ( Ada.Containers.Hash_Type
              ( Row_Range'Last * ( S.Loc.Row + Col_Range'Last * S.Loc.Col )
               + S.Min
              )
          );

      package Explored_State_Sets is new Ada.Containers.Hashed_Sets
         ( Element_Type        => Remembered_State,
           Hash                => State_Hash,
           Equivalent_Elements => "="
          );

      Explored_States: Explored_State_Sets.Set;
      New_Path: Location_Vectors.Vector;

   begin

      loop -- this loop better not be infinite...

         declare
            S: State;
         begin

            Q.Dequeue(S);

            -- "are we there yet?"
            if S.Loc = Goal then
               return S;
            end if;

            -- make sure our blizzard cache works here
            if S.Min + 1 > Blizzard_Locations.Last_Index then
               Determine_Blizzards_At_Time(S.Min + 1);
            end if;

            -- check all neighboring locations
            for Row in
               S.Loc.Row - 1 .. S.Loc.Row + 1
            loop
               for Col in
                  S.Loc.Col - 1 .. S.Loc.Col + 1
               loop

                  if ( Row = S.Loc.Row or else Col = S.Loc.Col )
                     and then Valid_Location(Row, Col)
                     and then
                        not Blizzard_Locations(S.Min + 1).Contains( (Row, Col) )
                     and then
                        not Explored_States.Contains
                           ( ( S.Min + 1, ( Row, Col ) ) )
                  then
                     if With_Visualization then
                        New_Path := Location_Vectors.Copy(S.Path);
                        New_Path.Append( Location'( Row, Col ) );
                        Q.Enqueue( ( S.Min + 1, New_Path, ( Row, Col ) ) );
                        Explored_States.Include( ( S.Min + 1, ( Row, Col ) ) );
                     else
                        Q.Enqueue( ( S.Min + 1, Empty_State_Vec, ( Row, Col ) ) );
                        Explored_States.Include( ( S.Min + 1, ( Row, Col ) ) );
                     end if;
                  end if;

               end loop;
            end loop;

         end;

      end loop;

   end Minimize_Steps_To_Goal;

   -- SUBSECTION
   -- PART 1

   function Minutes_To_Extraction_Point return State is
   -- how long will it take to cross the valley from the start
   -- to the extraction point?

      Q: Queues.Queue;
      Goal: Location := ( Row_Range'Last, Col_Range'Last - 1 );

      Path: Location_Vectors.Vector;

   begin

      Path.Append( Location'( 1, 2 ) );
      Q.Enqueue( ( 0, Path, ( 1, 2 ) ) );
      return Minimize_Steps_To_Goal(Q, Goal);

   end Minutes_To_Extraction_Point;

   -- SUBSECTION
   -- PART 2

   Part_1_Result: State;

   function Time_To_Recover_Snacks return State is
   -- how long will it take to do part 1, then return back,
   -- then cross the valley again?

      Q, R: Queues.Queue;

      First_Goal: Location := ( 1, 2 );
      Second_Goal: Location := ( Row_Range'Last, Col_Range'Last - 1 );

      Result: State;

   begin

      Q.Enqueue( ( Part_1_Result.Min, Part_1_Result.Path, Part_1_Result.Loc ) );
      Result := Minimize_Steps_To_Goal(Q, First_Goal);

      R.Enqueue( ( Result.Min, Result.Path, Result.Loc ) );
      Result := Minimize_Steps_To_Goal(R, Second_Goal);

      return Result;

   end Time_To_Recover_Snacks;

   Part_2_Result: State;

begin

   Read_Input;

   Part_1_Result := Minutes_To_Extraction_Point;
   Text_IO.Put_Line("at least" & Part_1_Result.Min'Image
                    & " minutes to the extraction point!");

   Part_2_Result := Time_To_Recover_Snacks;
   Text_IO.Put_Line("at least" & Part_2_Result.Min'Image
                    & " minutes to reach goal, return to start,"
                    & " then return to goal!");

   if With_Visualization then
      Text_IO.Put_Line("writing images");
      Write_State_As_Ppm(Part_2_Result.Path);
   end if;

end Main;
