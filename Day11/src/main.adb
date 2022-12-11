-- Advent of Code 2022, Day 11
--
-- John Perry
--
-- Monkey in the Middle
--
-- part 1: determine which two monkeys are the most active after 20 rounds
--
-- part 2: same, but after 100_000 rounds
--

pragma Ada_2022;

with Ada.Text_IO;

procedure Main is

   package Text_IO renames Ada.Text_IO;

   Doing_Example: constant Boolean := False;

   -- SECTION
   -- Monkey time

   -- SUBSECTION
   -- ranges of monkeys

   type Monkey_Range is new Natural range 0 .. 7;
   -- total number of monkeys possible
   subtype Work_Range
      is Monkey_Range range 0 .. ( if Doing_Example then 3 else 7 );
   -- number of monkeys actually working
   -- (gnat will let me define the array of monkeys
   -- using 3 for one value of Doing_Example and 7 for the other,
   -- but it will issue a warning, which I didn't want to see)

   -- SUBSECTION
   -- items

   subtype Item_Type is Long_Long_Integer; -- type of the items

   type Item_Array
      is array ( 1 .. ( if Doing_Example then 10 else 35 ) ) of Item_Type;

   type Worry_Change is ( Add, Multiply, Square );
   -- how an Worry_Level's value changes when you worry about it

   -- SUBSECTION
   -- at long last, the monkeys

   type Monkey is record

      Items                : Item_Array := ( others => 0 ); -- items held
      Next_Free            : Natural := 1; -- the next free position in `Items`

      Operation            : Worry_Change := Add; -- interest change
      Operand              : Item_Type := 0;      -- when I worry

      Test                 : Item_Type := 0; -- how monkey decides
                                             -- where to send Worry_Level:
                                             -- if divisible by `Test`, then
                                             -- it is sent to `Dest_True`;
                                             -- otherwise, to `Dest_False`
      Dest_True, Dest_False: Monkey_Range := 0;

      Items_Inspected      : Item_Type := 0;

   end record;

   type Monkey_Array is array ( Monkey_Range ) of Monkey;

   Monkeys: Monkey_Array
   -- transcribing this might not be the best idea I ever had,
   -- but it saved me the trouble of writing a parser
      := ( if Doing_Example then (
           0 => (
                 Items           => ( 79, 98, others => 0 ),
                 Next_Free       => 3,
                 Operation       => Multiply,
                 Operand         => 19,
                 Test            => 23,
                 Dest_True       => 2,
                 Dest_False      => 3,
                 Items_Inspected => 0
                ),
           1 => (
                 Items           => ( 54, 65, 75, 74, others => 0 ),
                 Next_Free       => 5,
                 Operation       => Add,
                 Operand         => 6,
                 Test            => 19,
                 Dest_True       => 2,
                 Dest_False      => 0,
                 Items_Inspected => 0
                ),
           2 => (
                 Items           => ( 79, 60, 97, others => 0 ),
                 Next_Free       => 4,
                 Operation       => Square,
                 Operand         => 0,
                 Test            => 13,
                 Dest_True       => 1,
                 Dest_False      => 3,
                 Items_Inspected => 0
                ),
           3 => (
                 Items           => ( 74, others => 0 ),
                 Next_Free       => 2,
                 Operation       => Add,
                 Operand         => 3,
                 Test            => 17,
                 Dest_True       => 0,
                 Dest_False      => 1,
                 Items_Inspected => 0
                ),
           others => <>
          )
           else (
              0 => (
                    Items           => ( 76, 88, 96, 97, 58, 61, 67, others => 0 ),
                    Next_Free       => 8,
                    Operation       => Multiply,
                    Operand         => 19,
                    Test            => 3,
                    Dest_True       => 2,
                    Dest_False      => 3,
                    Items_Inspected => 0
                   ),
              1 => (
                    Items           => ( 93, 71, 79, 83, 69, 70, 94, 98, others => 0 ),
                    Next_Free       => 9,
                    Operation       => Add,
                    Operand         => 8,
                    Test            => 11,
                    Dest_True       => 5,
                    Dest_False      => 6,
                    Items_Inspected => 0
                   ),
              2 => (
                    Items           => ( 50, 74, 67, 92, 61, 76, others => 0 ),
                    Next_Free       => 7,
                    Operation       => Multiply,
                    Operand         => 13,
                    Test            => 19,
                    Dest_True       => 3,
                    Dest_False      => 1,
                    Items_Inspected => 0
                   ),
              3 => (
                    Items           => ( 76, 92, others => 0 ),
                    Next_Free       => 3,
                    Operation       => Add,
                    Operand         => 6,
                    Test            => 5,
                    Dest_True       => 1,
                    Dest_False      => 6,
                    Items_Inspected => 0
                   ),
              4 => (
                    Items           => ( 74, 94, 55, 87, 62, others => 0 ),
                    Next_Free       => 6,
                    Operation       => Add,
                    Operand         => 5,
                    Test            => 2,
                    Dest_True       => 2,
                    Dest_False      => 0,
                    Items_Inspected => 0
                   ),
              5 => (
                    Items           => ( 59, 62, 53, 62, others => 0 ),
                    Next_Free       => 5,
                    Operation       => Square,
                    Operand         => 0,
                    Test            => 7,
                    Dest_True       => 4,
                    Dest_False      => 7,
                    Items_Inspected => 0
                   ),
              6 => (
                    Items           => ( 1 => 62, others => 0 ),
                    Next_Free       => 2,
                    Operation       => Add,
                    Operand         => 2,
                    Test            => 17,
                    Dest_True       => 5,
                    Dest_False      => 7,
                    Items_Inspected => 0
                   ),
              7 => (
                    Items           => ( 85, 54, 53, others => 0 ),
                    Next_Free       => 4,
                    Operation       => Add,
                    Operand         => 3,
                    Test            => 13,
                    Dest_True       => 4,
                    Dest_False      => 0,
                    Items_Inspected => 0
                   )
             )
          );

   -- the only way to make part 2 feasible is to divide each Worry_Level's value
   -- by the product of all the monkeys' moduli,
   -- because the interest value is useful only to determine how
   -- the monkeys pass them off, and that is determined by divisibility;
   -- that is, by the modulus
   --
   -- not even a `Big_Integer` would work here
   Part_2_Modulus: Item_Type
      := ( if Doing_Example then 23 * 19 * 13 * 17
           else 3 * 11 * 19 * 5 * 2 * 7 * 17 * 13
          );

   procedure Do_Your_Business(My: in out Monkey; Part: Positive := 1) is
   -- the monkey does his business

      Worry_Level: Item_Type;
      True_Monkey renames Monkeys(My.Dest_True);
      False_Monkey renames Monkeys(My.Dest_False);

   begin

      -- inspect and change worry level
      for I in 1 .. My.Next_Free - 1 loop

         Worry_Level := My.Items(I);
         My.Items_Inspected := My.Items_Inspected + 1;

         -- change in my worry level
         case My.Operation is
            when Add => Worry_Level := Worry_Level + My.Operand;
            when Multiply => Worry_Level := Worry_Level * My.Operand;
            when Square => Worry_Level := Worry_Level * Worry_Level;
         end case;
         Worry_Level := ( case Part is
                             when 1 => Worry_Level / 3,
                             when others => Worry_Level rem Part_2_Modulus
                         );

         -- monkey decided what to do by testing worry level
         if Worry_Level rem My.Test = 0 then
            True_Monkey.Items(True_Monkey.Next_Free) := Worry_Level;
            True_Monkey.Next_Free := @ + 1;
         else
            False_Monkey.Items(False_Monkey.Next_Free) := Worry_Level;
            False_Monkey.Next_Free := @ + 1;
         end if;
      end loop;

      -- I've considered all my items, so I have none left!
      My.Next_Free := 1;

   end Do_Your_Business;

   function Level_Of_Monkey_Business return Item_Type is
   -- product of the largest two numbers of items considered

      Largest, Next_Largest: Item_Type := 0;

   begin

      for Monkey of Monkeys loop
         if Monkey.Items_Inspected > Largest then
            Next_Largest := Largest;
            Largest := Monkey.Items_Inspected;
         else
            Next_Largest
               := Long_Long_Integer'Max(Next_Largest, Monkey.Items_Inspected);
         end if;
      end loop;

      return Largest * Next_Largest;

   end Level_Of_Monkey_Business;

   Saved_Monkeys: Monkey_Array := Monkeys;

begin

   for I in 1 .. 20 loop
      for J in Work_Range loop
         Do_Your_Business(Monkeys(J));
      end loop;
   end loop;

   --  for I in Work_Range loop
   --     Text_IO.Put_Line("Monkey" & I'Image
   --                      & " inspected" & Monkeys(I).Items_Inspected'Image
   --                      & " items"
   --                     );
   --  end loop;
   Text_IO.Put_Line("the level of monkey business in part 1 is"
                    & Level_Of_Monkey_Business'Image
                   );

   Monkeys := Saved_Monkeys;

   for I in 1 .. 10_000 loop
      for J in Work_Range loop
         Do_Your_Business(Monkeys(J), Part => 2);
      end loop;
   end loop;

   --  for I in Work_Range loop
   --     Text_IO.Put_Line("Monkey" & I'Image
   --                      & " inspected" & Monkeys(I).Items_Inspected'Image
   --                      & " items"
   --                     );
   --  end loop;
   Text_IO.Put_Line("the level of monkey business in part 2 is"
                    & Level_Of_Monkey_Business'Image
                   );

end Main;
