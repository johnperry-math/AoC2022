-- Advent of Code 2022, Day 8
--
-- John Perry
--
-- Treetop Tree House
--
-- part 1: count the number of trees visible from outside the grid
--
-- part 2: find the tree with the highest scene score,
-- which is product of the numbers of trees visible in each cardinal direction
--

with Ada.Text_IO;

procedure Main is

   -- SECTION
   -- input-related

   Testing: constant Boolean := False;
   Filename: constant String
      := ( if Testing then "example.txt" else "input.txt" );
   package ATIO renames Ada.Text_IO;
   package ATIIO is new Ada.Text_IO.Integer_IO
      (
       Num => Natural
      );
   Input_File    : ATIO.File_Type;

   -- SECTION
   -- types and global variables

   -- SUBSECTION
   -- the tree patch

   Dimension: constant Positive := ( if Testing then 5 else 99 );

   subtype Tree_Height is Natural range 0 .. 9;
   type Tree_Patch_Array is
      array( 1 .. Dimension , 1 .. Dimension ) of Tree_Height;

   Tree_Patch: Tree_Patch_Array;

   -- SUBSECTION
   -- convenient iteration through the tree

   type Direction is (North, South, East, West);
   type Delta_Array is array(Direction) of Integer;
   Delta_X: Delta_Array := ( 0, 0, 1, -1 );
   Delta_Y: Delta_Array := ( -1, 1, 0, 0 );


   -- SECTION I/O

   procedure Read_Input is
      Tree_Row: String(1 .. Dimension);
   begin
      ATIO.Open(Input_File, ATIO.In_File, Filename);
      for I in Tree_Patch'Range(1) loop
         ATIO.Get(Input_File, Tree_Row);
         for J in Tree_Patch'Range(2) loop
            Tree_Patch(I, J) := Character'Pos(Tree_Row(J)) - Character'Pos('0');
         end loop;
      end loop;
      ATIO.Close(Input_File);
   end Read_Input;

   -- SECTION
   -- part 1

   function Visible_From(Where: Direction; Row, Col: Integer) return Boolean is
   -- returns True if the tree in the given row and column
   -- is visible from the given direction

      X: Integer := Col;
      Y: Integer := Row;
      Dx: Integer := Delta_X(Where);
      Dy: Integer := Delta_Y(Where);

   begin

      loop
         -- iterate in given direction until either we fall off the grid
         -- or a tree blocks us

         X := X + Dx;
         Y := Y + Dy;
         if not ( X in Tree_Patch'Range(2) and Y in Tree_Patch'Range(1) ) then
            exit;
         end if;

         -- if another tree is at least as tall as this one,
         -- then this one is not visible
         if Tree_Patch(Y, X) >= Tree_Patch(Row, Col) then
            return False;
         end if;

      end loop;

      -- if we make it here, nothing has blocked the tree, so it is visible
      return True;

   end Visible_From;

   function Number_Of_Visible_Trees return Natural is
   -- return the number of trees visible from outside the grid
   -- when looking into it from a cardinal direction

      Result: Natural := 0;

   begin

      for Row in Tree_Patch'Range(1) loop
         for Col in Tree_Patch'Range(2) loop

            if ( for some Dir in Direction => Visible_From(Dir, Row, Col) ) then
               Result := Result + 1;
            end if;

         end loop;
      end loop;

      return Result;

   end Number_Of_Visible_Trees;

   -- part 2

   function Tree_Score(Row, Col: Integer) return Natural is
   -- returns the scene score for the tree in the given row and column

      Result: Natural := 1;

   begin

      for Dir in Direction loop

         declare
            Dx: Integer := Delta_X(Dir);
            Dy: Integer := Delta_Y(Dir);
            X: Integer := Col;
            Y: Integer := Row;
            Trees_Visible: Natural := 0;
         begin

            loop
               -- iterate in current direction until either we fall off the grid
               -- or a tree blocks us

               X := X + Dx;
               Y := Y + Dy;

               if X in Tree_Patch'Range(2) and Y in Tree_Patch'Range(1) then
                  Trees_Visible := Trees_Visible + 1;
                  if Tree_Patch(Y, X) >= Tree_Patch(Row, Col) then
                     -- this tree blocks us
                     exit;
                  end if;
               else
                  -- fell off the grid
                  exit;
               end if;

            end loop;

            Result := Result * Trees_Visible;

         end;

      end loop;

      return Result;

   end Tree_Score;

   function Max_Scene_Score return Natural is
   -- returns the maximum scenic score

      Result: Natural := 0;

   begin

      for Row in Tree_Patch_Array'Range(1) loop
         for Col in Tree_Patch_Array'Range(2) loop
            Result := Natural'Max(Result, Tree_Score(Row, Col));
         end loop;
      end loop;

      return Result;

   end Max_Scene_Score;

begin

   Read_Input;

   -- part 1
   ATIO.Put_Line(Number_Of_Visible_Trees'Image & " are visible");

   -- part 2
   ATIO.Put_Line("max scene score is" & Max_Scene_Score'Image);

end Main;
