-- Advent of Code 2022, Day 12
--
-- John Perry
--
-- Hill Climbing Algorithm
--
-- part 1: find the shortest route from start to apex
--
-- part 2: find the shortest route from any ground level to apex
--

with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;

procedure Main is

   package Text_IO renames Ada.Text_IO;

   Doing_Example: constant Boolean := False;

   type Part_Number is ( First, Second );

   -- SECTION
   -- global types and variables

   -- SUBSECTION
   -- the map

   subtype Elevation is Character range 'a' .. 'z';
   subtype Row_Range is Positive range 1 .. ( if Doing_Example then 5 else 41 );
   subtype Col_Range is Positive range 1 .. ( if Doing_Example then 8 else 161 );
   type Elevation_Map_Array is array( Row_Range, Col_Range ) of Elevation;
   Elevation_Map: Elevation_Map_Array;

   -- SUBSECTION
   -- my position

   type Position is record
      Row: Row_Range;
      Col: Col_Range;
   end record;

   -- SUBSECTION
   -- allowed motion

   function Good_Climb(From, To: Position; Part: Part_Number) return Boolean is
      (
       case Part is
          when First => Character'Pos(Elevation_Map(To.Row, To.Col))
       - Character'Pos(Elevation_Map(From.Row, From.Col))
       <= 1,
       when Second => Character'Pos(Elevation_Map(To.Row, To.Col))
       - Character'Pos(Elevation_Map(From.Row, From.Col))
       >= -1
      );

   Start, Apex: Position;

   -- SECTION
   -- I/O

   Filename: constant String
      := ( if Doing_Example then "example.txt" else "input.txt" );

   Input_File: Text_IO.File_Type;

   Unexpected_Character: exception;

   procedure Read_Map is
   begin

      Text_IO.Open(Input_File, Text_IO.In_File, Filename);

      for Row in Row_Range loop

         declare
            Input_String: String := Text_IO.Get_Line(Input_File);
         begin

            for Col in Col_Range loop

               case Input_String(Col) is

                  when Elevation =>
                     Elevation_Map(Row, Col) := Input_String(Col);

                  when 'S' => -- start position
                     Elevation_Map(Row, Col) := 'a';
                     Start := ( Row => Row, Col => Col );

                  when 'E' => -- end position ("apex")
                     Elevation_Map(Row, Col) := 'z';
                     Apex := ( Row => Row, Col => Col );

                  when others => raise Unexpected_Character
                        with "at" & Row'Image & Col'Image;

               end case;

            end loop;

         end;

      end loop;

      Text_IO.Close(Input_File);

   end Read_Map;

   -- SECTION
   -- breadth-first search

   -- SUBSECTION
   -- more motion

   type Direction is ( Up, Down, Left, Right );
   type Delta_Array is array ( Direction ) of Integer;
   Delta_Col: Delta_Array := ( 0, 0, -1, 1 );
   Delta_Row: Delta_Array := ( -1, 1, 0, 0 );

   function New_Position(From: Position; Into: Direction) return Position is
   -- returns the position one step from `From` in the direction `Into`
      (
       if From.Row + Delta_Row(Into) in Row_Range
       and then From.Col + Delta_Col(Into) in Col_Range
       then
          ( Row => From.Row + Delta_Row(Into),
            Col => From.Col + Delta_Col(Into)
           )
       else
          From
      );

   -- SUBSECTION
   -- remembering the positions we've passed

   function "<"(Left, Right: Position) return Boolean is
      (
       Left.Row < Right.Row
       or else
          ( Left.Row = Right.Row and then Left.Col < Right.Col )
      );

   package Position_Sets is new Ada.Containers.Ordered_Sets
      (
       Element_Type => Position
      );

   package Position_Vectors is new Ada.Containers.Vectors
      (
       Index_Type => Positive,
       Element_Type => Position
      );

   Did_Not_Find_Path: exception;

   function Find_Shortest_Route(Part: Part_Number := First)
                                return Position_Vectors.Vector
   is
   -- uses breadth-first search to find the shortest route
   -- from the starting point to the ending point;
   -- determining the start and end depends on `Part`:
   -- * when `Part = First`, we start at `Start` and end at `Apex`;
   -- * when `Part = Second`, we start at `Apex` and end at any position
   --   that has an `a` on the map

      use all type Ada.Containers.Count_Type;
      -- needed for testing for empty

      Explored: Position_Sets.Set;
      -- positions explored

      package Position_Queue_Interfaces
      is new Ada.Containers.Synchronized_Queue_Interfaces
         (
          Element_Type => Position_Vectors.Vector
         );
      package Position_Queues is new Ada.Containers.Unbounded_Synchronized_Queues
         (
          Queue_Interfaces => Position_Queue_Interfaces
         );
      Position_Queue: Position_Queues.Queue;
      -- positions we still want to explore

      Current, Empty_Path: Position_Vectors.Vector;
      -- current path
      Starting_Point: Position := ( if Part = First then Start else Apex );
      -- starting position; depends on which part!

   begin

      Current.Append(Starting_Point);
      Explored.Insert(Starting_Point);
      Position_Queue.Enqueue(Current);

      while Position_Queue.Current_Use > 0 loop

         Position_Queue.Dequeue(Current);
         --  Text_IO.Put_Line("exploring " & Current.Last_Element.Row'Image
         --                   & Current.Last_Element.Col'Image);

         if ( Part = First and then Current.Last_Element = Apex )
            or else
               ( Part = Second
                  and then
                  Elevation_Map
                     (Current.Last_Element.Row, Current.Last_Element.Col) = 'a'
                 )
         then -- found it!
            return Current;

         else -- keep looking, in each direction
            for Dir in Direction loop
               declare
                  New_Pos: Position := New_Position(Current.Last_Element, Dir);
               begin
                  -- can we go here? did we already?
                  if Good_Climb(Current.Last_Element, New_Pos, Part)
                     and then not Explored.Contains(New_Pos)
                  then
                     Explored.Insert(New_Pos);
                     declare
                        New_Path: Position_Vectors.Vector := Current.Copy;
                     begin
                        New_Path.Append(New_Pos);
                        Position_Queue.Enqueue( New_Path );
                     end;
                  end if;
               end;
            end loop;
         end if;

      end loop;

      raise Did_Not_Find_Path;

   end Find_Shortest_Route;

   procedure Print_Map(Route: Position_Vectors.Vector) is
      Temp: array( Elevation_Map_Array'Range(1), Elevation_Map_Array'Range(2) )
         of Character := ( others => ( others => '.' ) );
   begin
      Temp(Route.Last_Element.Row, Route.Last_Element.Col) := 'E';
      for I in Route.First_Index .. Route.Last_Index - 1 loop
         declare
            Current: Position := Route(I + 1);
            Previous: Position := Route(I);
         begin
            if Current.Row > Previous.Row then
               Temp(Previous.Row, Previous.Col) := 'v';
            elsif Current.Row < Previous.Row then
               Temp(Previous.Row, Previous.Col) := '^';
            elsif Current.Col > Previous.Col then
               Temp(Previous.Row, Previous.Col) := '>';
            elsif Current.Col < Previous.Col then
               Temp(Previous.Row, Previous.Col) := '<';
            end if;
         end;
      end loop;
      for row in Temp'Range(1) loop
         for Col in Temp'Range(2) loop
            Text_IO.Put(Temp(Row, Col));
         end loop;
         Text_IO.New_Line;
      end loop;
   end Print_Map;

   Shortest_Route: Position_Vectors.Vector;

begin

   Read_Map;

   Shortest_Route := Find_Shortest_Route;
   --  Text_IO.Put_Line("shortest route from start to apex takes"
   --                   & Natural'Image(Natural(Shortest_Route.Length) - 1)
   --                  );
   --  Print_Map(Shortest_Route);

   Shortest_Route := Find_Shortest_Route(Second);
   --  Shortest_Route.Reverse_Elements;
   --  Text_IO.Put_Line("shortest route from ground to apex takes"
   --                   & Natural'Image(Natural(Shortest_Route.Length) - 1)
   --                  );
   --  Print_Map(Shortest_Route);

end Main;
