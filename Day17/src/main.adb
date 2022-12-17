-- Advent of Code 2022, Day 16
--
-- John Perry
--
-- Pyroclastic Flow
--
-- part 1: count the tower height after 1_000_000 tetris-like blocks fall
--
-- part 2: do the same, but after 1 trillion
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

   -- I had 10061 jets
   -- curiously, that just so happens to be prime
   type Jet is ( Left, Right );
   package Jet_Vectors is new Ada.Containers.Vectors
      ( Index_Type => Natural,
        Element_Type => Jet
       );
   All_Jets: Jet_Vectors.Vector;

   -- curiously, there is a prime number of figures
   type Figure is ( Minus, Plus, Ell, Beam, Square );
   function Figure_After(F: Figure) return Figure is
      ( if F /= Figure'Last then Figure'Succ(F) else Figure'First );
   Figure_Size: array( Figure ) of Positive := ( 1, 3, 3, 4, 2 );

   -- there is a prime number of fillings, but that's not curious
   type Filling is ( Rock, Void );
   -- there is a prime number of columns, and that's possibly curious
   type Cavern_Row is array( 1 .. 7 ) of Filling;

   package Cavern_Vectors is new Ada.Containers.Vectors
      (
       Index_Type => Positive,
       Element_Type => Cavern_Row
      );

   Cavern: Cavern_Vectors.Vector;

   -- SECTION
   -- I/O

   Filename: constant String
      := ( if Doing_Example then "example.txt" else "input.txt" );

   Input_File: Text_IO.File_Type;

   procedure Read_Jets is
   -- read the sensor / beacon data

   begin

      Text_IO.Open(Input_File, Text_IO.In_File, Filename);

      while not Text_IO.End_Of_File(Input_File) loop

         declare
            Input_String  : String := Text_IO.Get_Line(Input_File);
         begin
            for C of Input_String loop
               All_Jets.Append
                  ( ( if C = '<' then Left else Right ) );
            end loop;
         end;

      end loop;

      Text_IO.Close(Input_File);

   end Read_Jets;

   procedure Draw
      (F                   : Figure;
       Fig_Left, Fig_Bottom: Natural;
       Shape               : Filling := Rock)
   is
   -- draws the shape into the cavern
   -- we need to do this to draw a pretty picture...
   -- but also to place the figure into the cavern for future reference
   begin

      -- nothing sophisticated here; it's pretty tedious
      case F is

         when Minus =>
            for I in Fig_Left - 3 .. Fig_Left loop
               Cavern(Fig_Bottom)(I) := Shape;
            end loop;

         when Plus =>
            for I in Fig_Left - 2 .. Fig_Left loop
               Cavern(Fig_Bottom + 1)(I) := Shape;
            end loop;
            Cavern(Fig_Bottom)(Fig_Left - 1) := Shape;
            Cavern(Fig_Bottom + 2)(Fig_Left - 1) := Shape;

         when Ell =>
            for I in Fig_Left - 2 .. Fig_Left loop
               Cavern(Fig_Bottom)(I) := Shape;
            end loop;
            Cavern(Fig_Bottom + 1)(Fig_Left - 2) := Shape;
            Cavern(Fig_Bottom + 2)(Fig_Left - 2) := Shape;

         when Beam =>
            for I in Fig_Bottom .. Fig_Bottom + 3 loop
               Cavern(I)(Fig_Left) := Shape;
            end loop;

         when Square =>
            for I in Fig_Bottom .. Fig_Bottom + 1 loop
               for J in Fig_Left - 1.. Fig_Left loop
                  Cavern(I)(J) := Shape;
               end loop;
            end loop;

      end case;

   end Draw;

   procedure Erase(F       : Figure;
                   Fig_Left  : Natural;
                   Fig_Bottom: Natural)
   is
   -- does the opposite of `Draw`. literally
   begin
      Draw(F, Fig_Left, Fig_Bottom, Void);
   end Erase;

   procedure Push(F       : Figure;
                  Fig_Left  : in out Natural;
                  Fig_Bottom: Natural;
                  Jet_Number: Natural)
   is
   -- pushes `F`, whose position is at `Fig_Left` and `Fig_Bottom`,
   -- in the direction given by the indicated jet number

   begin

      -- nothing sophisticated here; it was pretty tedious
      case All_Jets(Jet_Number) is

         when Left =>

            case F is

               when Minus =>
                  if Fig_Left < 7
                     and then Cavern(Fig_Bottom)(Fig_Left + 1) /= Rock then
                     Fig_Left := Fig_Left + 1;
                  end if;

               when Plus =>
                  if Fig_Left < 7
                     and then Cavern(Fig_Bottom)(Fig_Left) /= Rock
                     and then Cavern(Fig_Bottom + 1)(Fig_Left + 1) /= Rock
                     and then Cavern(Fig_Bottom+ 2)(Fig_Left) /= Rock
                  then
                     Fig_Left := Fig_Left + 1;
                  end if;

               when Ell =>
                  if Fig_Left < 7
                     and then Cavern(Fig_Bottom)(Fig_Left + 1) /= Rock
                     and then Cavern(Fig_Bottom + 1)(Fig_Left - 1) /= Rock
                     and then Cavern(Fig_Bottom + 2)(Fig_Left - 1) /= Rock
                  then
                     Fig_Left := Fig_Left + 1;
                  end if;

               when Beam =>
                  if Fig_Left < 7
                     and then ( for all I in Fig_Bottom .. Fig_Bottom + 3
                        => Cavern(I)(Fig_Left + 1) /= Rock )
                  then
                     Fig_Left := Fig_Left + 1;
                  end if;

               when Square =>
                  if Fig_Left < 7
                     and then Cavern(Fig_Bottom)(Fig_Left + 1) /= Rock
                     and then Cavern(Fig_Bottom + 1)(Fig_Left + 1) /= Rock
                  then
                     Fig_Left := Fig_Left + 1;
                  end if;

            end case;

         when Right =>

            case F is

               when Minus =>
                  if Fig_Left > 4
                     and then Cavern(Fig_Bottom)(Fig_Left - 4) /= Rock
                  then
                     Fig_Left := Fig_Left - 1;
                  end if;

               when Plus =>
                  if Fig_Left > 3
                     and then Cavern(Fig_Bottom)(Fig_Left - 2) /= Rock
                     and then Cavern(Fig_Bottom + 1)(Fig_Left - 3) /= Rock
                     and then Cavern(Fig_Bottom + 2)(Fig_Left - 2) /= Rock
                  then
                     Fig_Left := Fig_Left - 1;
                  end if;

               when Ell =>
                  if Fig_Left > 3
                     and then Cavern(Fig_Bottom)(Fig_Left - 3) /= Rock
                     and then Cavern(Fig_Bottom + 1)(Fig_Left - 3) /= Rock
                     and then Cavern(Fig_Bottom + 2)(Fig_Left - 3) /= Rock
                  then
                     Fig_Left := Fig_Left - 1;
                  end if;

               when Beam =>
                  if Fig_Left > 1
                     and then ( for all I in Fig_Bottom .. Fig_Bottom + 3
                                 => Cavern(I)(Fig_Left - 1) /= Rock )
                  then
                     Fig_Left := Fig_Left - 1;
                  end if;

               when Square =>
                  if Fig_Left > 2
                     and then Cavern(Fig_Bottom)(Fig_Left - 2) /= Rock
                     and then Cavern(Fig_Bottom + 1)(Fig_Left - 2) /= Rock
                  then
                     Fig_Left := Fig_Left - 1;
                  end if;

            end case;

      end case;

   end Push;

   procedure Drop
      (F: Figure; Fig_Left: Natural; Fig_Bottom: in out Natural)
   is
   -- moves `F`, whose position is `Fig_Left` and `Fig_Bottom`,
   -- down one unit... IF possible

   begin

      if Fig_Bottom = 1 then return; end if;

      case F is

         when Minus =>
            if ( for all I in Fig_Left - 3 .. Fig_Left
                     => Cavern(Fig_Bottom - 1)(I) /= Rock )
            then
               Fig_Bottom := Fig_Bottom - 1;
            end if;

         when Plus =>
            if Cavern(Fig_Bottom)(Fig_Left) /= Rock
               and then Cavern(Fig_Bottom - 1)(Fig_Left - 1) /= Rock
               and then Cavern(Fig_Bottom)(Fig_Left - 2) /= Rock
            then
               Fig_Bottom := Fig_Bottom - 1;
            end if;

         when Ell =>
            if ( for all I in Fig_Left - 2 .. Fig_Left
                     => Cavern(Fig_Bottom - 1)(I) /= Rock)
            then
               Fig_Bottom := Fig_Bottom - 1;
            end if;

         when Beam =>
            if Cavern(Fig_Bottom - 1)(Fig_Left) /= Rock
            then
               Fig_Bottom := Fig_Bottom - 1;
            end if;

         when Square =>
            if Cavern(Fig_Bottom - 1)(Fig_Left) /= Rock
               and then Cavern(Fig_Bottom - 1)(Fig_Left - 1) /= Rock
            then
               Fig_Bottom := Fig_Bottom - 1;
            end if;

      end case;

   end Drop;

   procedure Print_Cavern is
   -- pretty cool, and helpful sometimes... not just for debugging!
   -- also for solving Part 2, if you're into a good time...
   -- (see below)

   begin

      for I in reverse 1 .. Cavern.Length loop
         Text_IO.Put('|');
         for J in Cavern_Row'Range loop
            Text_IO.Put( ( if Cavern(Positive(I))(8 - J) = Rock then '#' else '.' ) );
         end loop;
         Text_IO.Put('|');
         Text_IO.Put_Line(I'Image);
      end loop;

      -- bottom
      Text_IO.Put('+');
      for J in Cavern_Row'Range loop
         Text_IO.Put('-');
      end loop;
      Text_IO.Put('+');

      Text_IO.New_Line;

   end Print_Cavern;

   procedure Part1 is
   -- does what it promises

      Fig: Figure := Minus;
      Cavern_Top: Natural := 0;
      Fallen_Rocks: Natural := 0;
      Falling      : Boolean; -- whether the figure is falling
      Fig_Left,
         Fig_Bottom,
         Old_Fig_Bottom: Natural; -- location, location, location
      Jet_Number: Natural := 0;
      Empty_Row   : Cavern_Row := ( others => Void );

   begin

      while Fallen_Rocks < 2022 loop

         -- add space if needed
         while Natural(Cavern.Length) < Cavern_Top + 8 loop
            Cavern.Append(Empty_Row);
         end loop;

         -- setup
         Fallen_Rocks := Fallen_Rocks + 1;
         Falling := True;
         Fig_Left := 5;
         Fig_Bottom := Cavern_Top + 4;

         -- put it into the figure (not really necessary here)
         Draw(Fig, Fig_Left, Fig_Bottom);

         while Falling loop

            Old_Fig_Bottom := Fig_Bottom; -- remember our location
            Erase(Fig, Fig_Left, Fig_Bottom); -- not really necessary here
                                              -- unless you draw above
                                              -- and/or during the loop below

            -- the winds blow...
            Push(Fig, Fig_Left, Fig_Bottom, Jet_Number);
            Jet_Number := ( Jet_Number + 1 ) rem Natural( All_Jets.Length );

            -- the leav-- er, the rocks fall...
            Drop(Fig, Fig_Left, Fig_Bottom);

            Draw(Fig, Fig_Left, Fig_Bottom); -- this is necessary
                                             -- on the last step at least

            if Fig_Bottom = Old_Fig_Bottom then
               -- we've stopped dropping!
               Falling := False;
               Cavern_Top :=
                  ( if Fig_Bottom + Figure_Size(Fig) - 1 < Cavern_Top
                    then Cavern_Top
                    else Fig_Bottom + Figure_Size(Fig) - 1
                   );
               Fig := Figure_After(Fig);
            end if;

         end loop;

      end loop;

      Print_Cavern;
      Text_IO.Put_Line("the tower is" & Cavern_Top'Image & " units tall");

   end Part1;

   type Col_Cutoff_Array is array(Cavern_Row'Range) of Natural;
   -- stores where a column is cut off by a figure,
   -- so that we can determine where to clip the cavern view

   procedure Show_Cutoff(Cutoffs: Col_Cutoff_Array) is
   -- useful for debugging
   begin
      for I in Col_Cutoff_Array'Range loop
         Text_IO.Put(Positive'Image(Cutoffs(I)));
      end loop;
   end Show_Cutoff;

   procedure Prune(Fig       : Figure;
                   Fig_Left  : Positive;
                   Fig_Bottom: Natural;
                   Col_Cutoff: in out Col_Cutoff_Array;
                   Rows_Pruned: out Natural
                  )
   is
   -- prunes the cavern view, so that it doesn't become too long
   -- this is safe to do as long as all the columns are blocked
   -- it's probably not necessary in part 2, and by itself it won't do the job,
   -- but it's pretty cool and helped confirm my suspicions on how to solve it

      Min_Row_Needed: Natural := Natural'Last;
      Empty_Row     : Cavern_Row := ( others => Void );

   begin

      case Fig is

         when Minus =>
            for I in Fig_Left - 3 .. Fig_Left loop
               Col_Cutoff(I) := Natural'Max(Col_Cutoff(I), Fig_Bottom);
            end loop;

         when Plus =>
            Col_Cutoff(Fig_Left)
               := Natural'Max(Col_Cutoff(Fig_Left), Fig_Bottom + 1);
            Col_Cutoff(Fig_Left - 1)
               := Natural'Max(Col_Cutoff(Fig_Left - 1), Fig_Bottom + 2);
            Col_Cutoff(Fig_Left - 2)
               := Natural'Max(Col_Cutoff(Fig_Left - 2), Fig_Bottom + 1);

         when Ell =>
            Col_Cutoff(Fig_Left)
               := Natural'Max(Col_Cutoff(Fig_Left), Fig_Bottom);
            Col_Cutoff(Fig_Left - 1)
               := Natural'Max(Col_Cutoff(Fig_Left - 1), Fig_Bottom);
            Col_Cutoff(Fig_Left - 2)
               := Natural'Max(Col_Cutoff(Fig_Left - 2), Fig_Bottom + 2);

         when Beam =>
            Col_Cutoff(Fig_Left)
               := Natural'Max(Col_Cutoff(Fig_Left), Fig_Bottom + 3);

         when Square =>
            Col_Cutoff(Fig_Left)
               := Natural'Max(Col_Cutoff(Fig_Left), Fig_Bottom + 1);
            Col_Cutoff(Fig_Left - 1)
               := Natural'Max(Col_Cutoff(Fig_Left - 1), Fig_Bottom + 1);

      end case;

      -- what's the lowest row we need to keep "visible"?
      for I in Col_Cutoff_Array'Range loop
         Min_Row_Needed := Natural'Min(Min_Row_Needed, Col_Cutoff(I));
      end loop;

      if Min_Row_Needed > 10 then
         -- snip the ones below it!

         for I in Min_Row_Needed .. Positive(Cavern.Length) loop
            Cavern(I - Min_Row_Needed + 1) := Cavern(I);
         end loop;

         for I in Positive(Cavern.Length) - Min_Row_Needed + 1 .. Positive(Cavern.Length) loop
            Cavern(I) := Empty_Row;
         end loop;

         Rows_Pruned := Min_Row_Needed - 1;
         for I in Col_Cutoff_Array'Range loop
            Col_Cutoff(I) := Col_Cutoff(I) - Rows_Pruned;
         end loop;

      else
         Rows_Pruned := 0;

      end if;

   end Prune;

   procedure Part2 is
   -- does what it says, and pretty quickly, too!
   -- variables identical to Part1 are left undocumented for now

      Fig           : Figure := Minus;
      Cavern_Top    : Long_Long_Integer := 0;
      Fallen_Rocks  : Long_Long_Integer := 0;
      Empty_Row     : Cavern_Row := ( others => Void );
      Falling       : Boolean;
      Fig_Left      : Natural;
      Fig_Bottom,
         Old_Fig_Bottom: Long_Long_Integer;
      Jet_Number    : Natural := 0;

      Col_Cutoff    : Col_Cutoff_Array := ( others => 0 ); -- see type doc
      Rows_Removed  : Long_Long_Integer := 0; -- number of rows pruned
                                              -- we need this
                                              -- to keep Fig_Bottom honest

      Last_Fallen,
         Last_Top,
         Prev_Fallen,
         Prev_Top      : Long_Long_Integer := 0; -- originally these were only
                                                 -- for debugging and curiosity,
                                                 -- but I realized they could
                                                 -- be used to solve the problem
      Val1, Val2,
         Val3, Val4,
         Val5          : Long_Long_Integer := 0; -- used to solve the problem

   begin

      while Fallen_Rocks < 100_000 loop -- I'm not doing 1 trillion, you sadist

         while Long_Long_Integer(Cavern.Length) + Rows_Removed < Cavern_Top + 8
         loop
            Cavern.Append(Empty_Row);
         end loop;

         Fallen_Rocks := Fallen_Rocks + 1;
         Falling := True;
         Fig_Left := 5;
         Fig_Bottom := Cavern_Top + 4;

         Draw(Fig, Fig_Left, Natural(Fig_Bottom - Rows_Removed));

         while Falling loop

            Old_Fig_Bottom := Fig_Bottom;

            -- we prune the cavern view to prevent memory
            -- from getting out of hand
            -- this probably isn't necessary to solve the problem,
            -- but it sure was convenient for a few things (e.g., debugging)
            -- that means we have to translate the true bottom to something
            -- that corresponds to the view
            -- since we use a natural number, it's a `Natural_Bottom`
            declare
               Natural_Bottom: Natural := Natural(Fig_Bottom - Rows_Removed);
            begin

               Erase(Fig, Fig_Left, Natural_Bottom);
               Push(Fig, Fig_Left, Natural_Bottom, Jet_Number);

               Jet_Number := ( Jet_Number + 1 ) rem Natural( All_Jets.Length );
               if Jet_Number = 0 then
                  -- these values were the key to solving it
                  Text_IO.Put_Line("jet completed; have" & Fallen_Rocks'Image
                                   & " fallen rocks and" & Cavern_Top'Image
                                   & " as the tower height");
                  Text_IO.Put_Line("...BUT WAIT! THAT'S NOT ALL!");
                  Text_IO.Put_Line("these are"
                                   & Long_Long_Integer'Image
                                      (Fallen_Rocks - Prev_Fallen)
                                   & " and"
                                   & Long_Long_Integer'Image
                                      (Cavern_Top - Prev_Top)
                                   & " larger than before! HMMMMMMMM... #1"
                                  );
                  if Val1 = 0 then
                     Val1 := Fallen_Rocks - Prev_Fallen;
                     Val2 := Cavern_Top - Prev_Top;
                     Text_IO.Put_Line("values a and b are"
                                      & Val1'Image & Val2'Image);
                  else
                     Val3 := Fallen_Rocks - Prev_Fallen;
                     Val4 := Cavern_Top - Prev_Top;
                     Text_IO.Put_Line("values c and d are"
                                      & Val3'Image & Val4'Image);
                  end if;

                  Text_IO.New_Line;
                  Prev_Fallen := Fallen_Rocks;
                  Prev_Top := Cavern_Top;

                  -- for a good time uncomment the next line
                  -- and be amazed...
                  --  Print_Cavern;

               end if;

               Drop(Fig, Fig_Left, Natural_Bottom);
               Draw(Fig, Fig_Left, Natural_Bottom);

               -- correct Fig_Bottom after a drop with Natural_Bottom
               Fig_Bottom := Long_Long_Integer(Natural_Bottom) + Rows_Removed;

            end;

            -- back to true coordinates
            if Fig_Bottom = Old_Fig_Bottom then
               Falling := False;
               Cavern_Top :=
                  ( if Fig_Bottom + Long_Long_Integer(Figure_Size(Fig)) - 1
                    < Cavern_Top
                    then Cavern_Top
                    else Fig_Bottom + Long_Long_Integer(Figure_Size(Fig)) - 1
                   );
            end if;

         end loop;

         -- time to consider pruning
         declare
            Rows_Pruned: Natural;
            Natural_Bottom: Natural := Natural(Fig_Bottom - Rows_Removed);
         begin

            Prune(Fig, Fig_Left, Natural_Bottom, Col_Cutoff, Rows_Pruned);
            Rows_Removed := Rows_Removed + Long_Long_Integer(Rows_Pruned);
            Fig_Bottom := Long_Long_Integer(Natural_Bottom) + Rows_Removed;

         end;

         Fig := Figure_After(Fig);

         -- set up to solve the problem
         -- it comes with a convenient explanation
         -- whether you understand it is not guaranteed
         if Val3 /= 0
            and then
               ( Fallen_Rocks - Val1) rem Val3
            = (1_000_000_000_000 - Val1) rem Val3
         then
            Text_IO.Put_Line("after" & Fallen_Rocks'Image & " fallen rocks "
                             & " our top is at" & Cavern_Top'Image
                             & "; these values are"
                             & Long_Long_Integer'Image(Fallen_Rocks - Prev_Fallen)
                             & " and"
                             & Long_Long_Integer'Image(Cavern_Top - Prev_Top)
                             & " larger than previously! HMMMMMMMM... #2"
                            );
            Text_IO.Put_Line("value e is"
                             & Long_Long_Integer'Image(Cavern_Top - Prev_Top)
                            );
            Text_IO.New_Line;
            Val5 := Cavern_Top - Prev_Top;
            Last_Fallen := Fallen_Rocks;
            Last_Top := Cavern_Top;
         end if;

      end loop; -- "i quit!" says your computer

      Text_IO.Put_Line("I can't be bothered to count that hight.");
      Text_IO.Put_Line("I will use some math: a" & Val1'Image
                       & " b" & Val3'Image
                       & " c" & Val2'Image
                       & " d" & Val4'Image
                       & " e" & Val5'Image
                      );
      Text_IO.Put_Line("(you'll see them noted above, btw)");

      Text_IO.Put_Line("the magic formula is "
                       & "( ( one trillion - a ) / b ) * d + c + e"
                      );
      Text_IO.Put_Line("(when you divide, drop the decimal -- DON'T round up)");

      Text_IO.Put_Line("the tower height after a ridiculous amount of rocks "
                       & "will be"
                       & Long_Long_Integer'Image
                          ( ( (1_000_000_000_000 - Val1) / Val3 ) * Val4 + Val2
                           + Val5
                          )
                      );
   end Part2;

begin

   Read_Jets;
   Part1;
   Cavern.Clear;
   Part2;

end Main;
