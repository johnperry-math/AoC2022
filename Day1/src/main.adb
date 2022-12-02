-- Advent of Code 2022, Day 1
--
-- John Perry
--
-- Calorie Counting
--
-- part 1: determine which elf is carrying the most calories,
--         and report how many
--
-- part 2: determine which three elves are carrying the most calories,
--         and report how many

with Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Main is

   -- SECTION
   -- input-related

   Filename: constant String := "input.txt";
   package ATIO renames Ada.Text_IO;
   Input_File : ATIO.File_Type;

   -- SECTION
   -- custom packages and types

   package Elf_Calories_Package is new Ada.Containers.Vectors
      (
       Index_Type => Positive,
       Element_Type => Positive
      );

   type Elf is record
   -- each elf caries several items;
   -- we know only the individual items' calories,
   -- but we can cache the sum
      Total_Calories: Natural := 0;
      Items         : Elf_Calories_Package.Vector;
   end record;

   package Expedition_Package is new Ada.Containers.Vectors
      (
       Index_Type => Positive,
       Element_Type => Elf
      );

   -- SECTION
   -- useful "global" data

   Expedition: Expedition_Package.Vector;
   -- our elf expedition

   -- SECTION
   -- useful procedures

   function Max_Cals(Expedition: Expedition_Package.Vector) return Positive
      -- solution to part 1
   is
      Sum: Natural := 0;
   begin
      for Elf of Expedition loop
         Sum := (if Sum > Elf.Total_Calories then Sum else Elf.Total_Calories);
      end loop;
      return Positive(Sum);
   end Max_Cals;

   function Decreasing_Calories(Left, Right: Elf) return Boolean
      -- True iff Left.Total_Calories is larger than Right.Total_Calories
   is
      ( Left.Total_Calories > Right.Total_Calories );

   function Max_Three_Cals(Expedition: in out Expedition_Package.Vector)
                           return Positive
      -- solution to part 2
   is
      package Expedition_Sort is new Expedition_Package.Generic_Sorting
         (
          "<" => Decreasing_Calories
         );
   begin
      Expedition_Sort.Sort(Expedition);
      return Expedition(1).Total_Calories + Expedition(2).Total_Calories
         + Expedition(3).Total_Calories;
   end Max_Three_Cals;

   procedure Read_Data is
   begin
      ATIO.Open(Input_File, ATIO.In_File, "input.txt");

      while not ATIO.End_Of_File(Input_File) loop
         declare
            New_Elf: Elf;
         begin
            Read_Elf: loop
               declare
                  Input_String: String := ATIO.Get_Line(Input_File);
               begin
                  --  ATIO.Put_Line("Read " & Input_String);
                  if Input_String'Length > 0 then
                     New_Elf.Total_Calories := New_Elf.Total_Calories
                        + Positive'Value(Input_String);
                     New_Elf.Items.Append(Positive'Value(Input_String));
                  else
                     exit Read_Elf;
                  end if;
               end;
               if ATIO.End_Of_File(Input_File) then exit Read_Elf; end if;
            end loop Read_Elf;
            Expedition.Append(New_Elf);
         end;

         --  ATIO.Put_Line("Number of elves: " & Expedition.Length'Image);

      end loop;

      ATIO.Close(Input_File);

   end Read_Data;

begin

   Read_Data;

   ATIO.Put_Line("The elf with the most calories has" & Max_Cals(Expedition)'Image);

   ATIO.Put_Line("The three elves with the most calories have"
                    & Max_Three_Cals(Expedition)'Image);

end Main;
