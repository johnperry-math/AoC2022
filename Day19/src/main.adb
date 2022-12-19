-- Advent of Code 2022, Day 19
--
-- John Perry
--
-- Not Enough Minerals
--
-- part 1: maximize the quality levels of the blueprints
--
-- part 2: same, but with fewer blueprints and more time
--

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;

procedure Main is

   package Text_IO renames Ada.Text_IO;
   package Integer_IO is new Ada.Text_IO.Integer_IO ( Num => Integer );
   package Natural_IO is new Ada.Text_IO.Integer_IO ( Num => Natural );

   Doing_Example: constant Boolean := False;

   type Part_Number is ( First, Second );

   -- SECTION
   -- global types and variables

   function Quality_Level(ID, Geodes: Natural) return Natural is
      ( ID * Geodes );

   type Robot is record
      Ore_Cost, Clay_Cost, Obsidian_Cost: Natural;
   end record;

   type Blueprint is record
      Orebot, Claybot, Obsidibot, Geodebot: Robot;
   end record;

   type Path is record
      Id: Positive;
      Time: Positive := 1;
      Orebots: Natural := 1;
      Claybots, Obsidibots, Geodebots: Natural := 0;
      Ore, Clay, Obsidian, Geode       : Natural := 0;
   end record;

   Default_Path: Path;

   Num_Blueprints: constant Positive := ( if Doing_Example then 2 else 30 );

   Blueprints: array( 1 .. Num_Blueprints ) of Blueprint;

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

      while Blueprints_Read < Num_Blueprints loop

         Blueprints_Read := Blueprints_Read + 1;

         declare
            Input_String  : String := Text_IO.Get_Line(Input_File);
            Pos           : Positive := ( if Blueprints_Read < 10 then 35 else 36 );
            Ore1, Ore2,
               Ore3, Ore4,
               Clay,
               Obsidian      : Positive;
         begin

            -- one day I'll get these numbers right the first time...
            Get_Integer(Input_String, Ore1, Pos); Pos := Pos + 28;
            Get_Integer(Input_String, Ore2, Pos); Pos := Pos + 32;
            Get_Integer(Input_String, Ore3, Pos); Pos := Pos + 9;
            Get_Integer(Input_String, Clay, Pos); Pos := Pos + 30;
            Get_Integer(Input_String, Ore4, Pos); Pos := Pos + 9;
            Get_Integer(Input_String, Obsidian, Pos);
            Blueprints(Blueprints_Read)
               := (
                   Orebot => ( Ore1, 0, 0 ),
                   Claybot => ( Ore2, 0, 0 ),
                   Obsidibot => ( Ore3, Clay, 0 ),
                   Geodebot => ( Ore4, 0, Obsidian )
                  );

         end;

      end loop;

      Text_IO.Close(Input_File);

   end Read_Input;

   -- SECTION
   -- PARTS 1 AND 2

   function Path_Hash(Route: Path) return Ada.Containers.Hash_Type is
   -- computes the hash of a path
   -- the multiples are based on "educated" guesses as to
   -- how many we'll need of each object... and they should also be prime,
   -- which will hopefully help keep things unique
      (
       Ada.Containers.Hash_Type(
          Route.Id * 61 + Route.Time * 53 + Route.Orebots * 47
          + Route.Claybots * 43 + Route.Obsidibots * 41
          + Route.Geodebots * 31 + Route.Ore * 29 + Route.Clay * 23
          + Route.Obsidian * 17 + Route.Geode * 7
         )
      );

   function Maximize_Bfs(Id: Positive; Time_Limit: Positive) return Natural is
   -- uses a breadth-first search to maximize the number of geodes mined
   -- in the given time limit

      use all type Ada.Containers.Count_Type;

      package Path_Queue_Interfaces
      is new Ada.Containers.Synchronized_Queue_Interfaces
         ( Element_Type => Path );

      package Path_Queues is new Ada.Containers.Unbounded_Synchronized_Queues
         ( Queue_Interfaces => Path_Queue_Interfaces );

      Paths: Path_Queues.Queue;

      -- SUBSECTION
      -- optimization
      --
      -- the following few lines proved to be the most effective optimization,
      -- taking performance down from minutes or even hours to mere seconds
      --
      -- the idea is this: for any given combination of bots,
      -- keep track of how many minerals they have mined;
      -- no instance where you get the same number of bots, but fewer minerals,
      -- is worth pursuing

      type Robot_Count is record
         Orebots, Claybots, Obsidibots, Geodebots: Natural;
      end record;

      type Mineral_Count is record
         Ore, Clay, Obsidian, Geode: Natural;
      end record;

      function Robot_Hash(State: Robot_Count) return Ada.Containers.Hash_Type is
      -- reasonably sure we don't need more than 20 of any bot
      -- (might be wrong about that, considering the example)
         ( Ada.Containers.Hash_Type
              ( State.Orebots * 80_000 + State.Claybots * 400
               + State.Obsidibots * 20 + State.Geodebots)
          );

      package Mineral_Vectors is new Ada.Containers.Vectors
         (
          Index_Type => Positive,
          Element_Type => Mineral_Count
         );

      function "="(Left, Right: Mineral_Vectors.Vector) return Boolean is
      -- really wish this were defined already
         (
          Left.Length = Right.Length
          and then
             ( for all I in 1 .. Left.Length
              => Left(Positive(I)) = Right(Positive(I))
             )
         );

      package Robot_Cache_Sets is new Ada.Containers.Hashed_Maps
         ( Key_Type => Robot_Count,
           Element_Type => Mineral_Vectors.Vector,
           Hash => Robot_Hash,
           Equivalent_Keys => "="
          );

      Robot_Cache: Robot_Cache_Sets.Map;
      -- our cache!

      procedure Place_If_No_Better_Choice(Route: Path) is
      -- takes care of both the caching and the queuing:
      -- queues a route only if it would give us an option with more minerals
      -- than previous rotes with the same number of bots

         RC: Robot_Count := ( Orebots => Route.Orebots,
                              Claybots => Route.Claybots,
                              Obsidibots => Route.Obsidibots,
                              Geodebots => Route.Geodebots );
         MC: Mineral_Count := ( Ore => Route.Ore,
                                Clay => Route.Clay,
                                Obsidian => Route.Obsidian,
                                Geode    => Route.Geode );
         V: Mineral_Vectors.Vector;

      begin

         if Robot_Cache.Contains(Rc) then -- update existing cache

            V := Robot_Cache(Rc);
            if ( for some M of V
                  => Mc.Ore <= M.Ore
                  and then Mc.Clay <= M.Clay
                  and then Mc.Obsidian <= M.Obsidian
                  and then Mc.Geode <= M.Geode )
            then
               null;
            else
               V.Append(Mc);
               Robot_Cache.Include(Rc, V);
               Paths.Enqueue( Route );
            end if;

         else -- insert cache element

            V.Append(Mc);
            Robot_Cache.Include(Rc, V);
            Paths.Enqueue( Route );

         end if;

      end Place_If_No_Better_Choice;

      -- SUBSECTION
      -- global variables

      Result: Natural := 0;

      Last_Time: Positive := 1;

   begin

      Text_IO.Put_Line("blueprint" & Id'Image
                       & " with time limit" & Time_Limit'Image);

      -- THIS WAS A VERY BAD IDEA I AM LEAVING IT TO SHAME MYSELF
      -- kinda made sense at the time, though
      --
      --  if Blueprints(Id).Obsidibot.Clay_Cost
      --     + Blueprints(Id).Geodebot.Obsidian_Cost
      --     --  + Blueprints(Id).Claybot.Ore_Cost
      --     > 24
      --  then
      --     return 0;
      --  end if;

      -- prime the queue
      Paths.Enqueue( ( Default_Path with delta Id => Id ) );

      -- do the breadth-first search!
      while Paths.Current_Use > 0 loop

         declare Current, Next, To_Insert: Path;
            Current_Bp: Blueprint;
         begin

            Paths.Dequeue(Current);

            if Current.Time > Last_Time then
               Text_IO.Put_Line("time in minutes:" & Current.Time'Image
                                & " paths queued:" & Paths.Current_Use'Image);
               Last_Time := Current.Time;
               Robot_Cache.Clear;
            end if;

            Current_Bp := Blueprints(Current.Id);

            if Current.Time > Time_Limit then

               Result := Natural'Max(Result, Current.Geode);

            else

               -- thank you Jeremy Grosser for point out that blog post to me
               -- `with delta` is AWESOME

               Next := ( Current with delta
                            Time     => Current.Time + 1,
                         Ore      => Current.Ore + Current.Orebots,
                         Clay     => Current.Clay + Current.Claybots,
                         Obsidian => Current.Obsidian + Current.Obsidibots,
                         Geode    => Current.Geode + Current.Geodebots
                        );

               if Current.Ore >= Current_Bp.Geodebot.Ore_Cost
                  and then Current.Obsidian >= Current_Bp.Geodebot.Obsidian_Cost
               then
                  -- if you have a choice for a geodebot, ALWAYS take it

                  To_Insert := ( Next with delta
                                    Ore       => Next.Ore
                                 - Current_Bp.Geodebot.Ore_Cost,
                                 Obsidian  => Next.Obsidian
                                 - Current_Bp.Geodebot.Obsidian_Cost,
                                 Geodebots => Next.Geodebots + 1 );

                  Place_If_No_Better_Choice(To_Insert);

               else

                  -- another bad idea commented out
                  if Current.Ore >= Current_Bp.Obsidibot.Ore_Cost
                     and then Current.Clay >= Current_Bp.Obsidibot.Clay_Cost
                  --  and then Current_Bp.Geodebot.Obsidian_Cost <
                  --     Next.Obsidian + (Current.Obsidibots + 1) * (25 - Next.Time)
                  then
                     To_Insert := ( Next with delta
                                       Ore          => Next.Ore
                                    - Current_Bp.Obsidibot.Ore_Cost,
                                    Clay         => Next.Clay
                                    - Current_Bp.Obsidibot.Clay_Cost,
                                    Obsidibots => Next.Obsidibots + 1 );
                     Place_If_No_Better_Choice(To_Insert);
                  end if;

                  -- again
                  if Current.Ore >= Current_Bp.Claybot.Ore_Cost
                     and then Current.Claybots < Current_Bp.Obsidibot.Clay_Cost
                     --  and then
                     --     Current_Bp.Obsidibot.Clay_Cost
                     --        < Next.Clay
                     --  + (Current.Claybots + 1) * (25 - Next.Time)
                     --  and then Next.Clay <= Max_Clay
                  then
                     To_Insert := ( Next with delta
                                       Ore      => Next.Ore
                                    - Current_Bp.Claybot.Ore_Cost,
                                    Claybots => Next.Claybots + 1 );
                     Place_If_No_Better_Choice(To_Insert);
                  end if;

                  if Current.Ore >= Current_Bp.Orebot.Ore_Cost
                     and then
                        Current.Orebots
                           < Natural'Max(Current_Bp.Claybot.Ore_Cost,
                                         Natural'Max(
                                            Current_Bp.Obsidibot.Ore_Cost,
                                            Current_Bp.Geodebot.Ore_Cost)
                                        )
                  then
                     To_Insert := ( Next with delta
                                       Ore     => Next.Ore
                                    - Current_Bp.Orebot.Ore_Cost,
                                    Orebots => Next.Orebots + 1 );
                     Place_If_No_Better_Choice(To_Insert);
                  end if;

                  Place_If_No_Better_Choice(Next);

               end if;
            end if;
         end;
      end loop;

      return Result;

   end Maximize_Bfs;

   Total_Quality: Natural := 0;

begin

   Read_Input;

   for I in 1 .. Num_Blueprints loop
      declare
         Current_Quality: Natural := Quality_Level( I, Maximize_Bfs(I, 24) );
      begin
         Text_IO.Put_Line("quality is" & Current_Quality'Image);
         Total_Quality := Total_Quality + Current_Quality;
         Text_IO.Put_Line("total quality is" & Total_Quality'Image);
      end;
   end loop;

   Total_Quality := 1; -- 1 to multiply

   for I in 1 .. 3 loop
      declare
         Current_Quality: Natural := Maximize_Bfs(I, 32);
      begin
         Text_IO.Put_Line("quality is" & Current_Quality'Image);
         Total_Quality := Total_Quality * Current_Quality;
         Text_IO.Put_Line("total quality is" & Total_Quality'Image);
      end;
   end loop;


end Main;
