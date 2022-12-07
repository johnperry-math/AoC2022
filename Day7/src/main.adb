-- Advent of Code 2022, Day 7
--
-- John Perry
--
-- No Space Left On Device
--
-- part 1: from the listed input and output, determine the number of directories
-- whose size is at most 100_000
--
-- part 2: determine the smallest directory you need to delete
-- in order to free up enough space on the device to allow an update
--
-- I may have overengineered this one

with Ada.Text_IO;
with Ada.Containers.Multiway_Trees;

procedure Main with SPARK_Mode => On is

   -- SECTION
   -- input-related, part 1

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
   -- file system objects

   type FS_Object is ( File, Folder );
   subtype FS_Name is String(1..30);
   type FS_Record is record
      Name: FS_Name;
      Kind: FS_Object;
      Size: Natural := 0;
   end record;

   package FS_Trees is new Ada.Containers.Multiway_Trees
      (
       Element_Type => FS_Record
      );
   FS_Tree: FS_Trees.Tree;

   procedure Print_FS_Name(Name: FS_Name) is
   -- the name is filled with spaces we don't want to see;
   -- this prints out everything except those spaces

      Stop_At: Positive := FS_Name'First;

   begin

      while Name(Stop_At) /= ' ' loop
         Stop_At := Stop_At + 1;
      end loop;

      ATIO.Put(Name(FS_Name'First .. Stop_At - 1));

   end Print_FS_Name;

   type Tree_Action is access procedure(What: FS_Record; Data: Natural);
   -- this saves me from writing two different recurse procedures:
   -- one to print (needed during debugging), the other to solve part 2
   -- presumably part 1 could also have been solved using this,
   -- but I did it in `Read_Transcript`

   type Update_Data is access function(Data: Natural) return Natural;
   -- similar to `Tree_Action`, except this updates `Data` on each descent
   -- into the recursion

   procedure Print_Node(Thing: FS_Record; Level: Natural) is
   -- prints the node's name, "(dir)" if it's a directory, and then the size
   begin
      for I in 1 .. Level loop ATIO.Put(' '); end loop;
      ATIO.Put("- ");
      Print_FS_Name(Thing.Name);
      ATIO.Put_Line(( if Thing.Kind = Folder then " (dir)" else "" )
                    & Thing.Size'Image
                   );
   end Print_Node;

   function Print_Update_Level(Level: Natural) return Natural is ( Level + 1 );

   procedure Recurse_Tree
      (
       Start_Pos: FS_Trees.Cursor;
       Action   : Tree_Action;
       Update   : Update_Data;
       Start_Lev: Natural := 0
      )
   is
   -- goes through `Start_Pos` and its siblings,
   -- performing `Action` on each element, and
   -- if they are directories, recursing into their contents
      use all type FS_Trees.Cursor;
      Pos: FS_Trees.Cursor := Start_Pos;
      Lev: Natural := Start_Lev;
   begin
      while Pos /= FS_Trees.No_Element loop
         Action(FS_Trees.Element(Pos), Lev);
         Lev := Update(Lev);
         if FS_Trees.Element(Pos).Kind = Folder then
            Recurse_Tree(FS_Trees.First_Child(Pos), Action, Update, Lev);
         end if;
         Pos := FS_Trees.Next_Sibling(Pos);
      end loop;
   end Recurse_Tree;

   procedure Write_Out_Tree is
   -- writes out the tree using `Recurse_Tree`,
   -- with `Print_Node` as the action
   -- and `Print_Update_Level` as the update function
      Pos: FS_Trees.Cursor := FS_Trees.First_Child(FS_Tree.Root);
   begin
      Recurse_Tree(Pos, Print_Node'Access, Print_Update_Level'Access);
   end Write_Out_Tree;

   Root: FS_Record
      := ( Name => ( 1 => '/', others => ' ' ),
           Kind => Folder,
           Size => 0
          );

   -- SECTION
   -- data for part 1

   Number_Small: Natural := 0; -- number of folders smaller than 100_000
   -- (they didn't actually want this, dadburnit)
   Sum_Small: Natural := 0; -- sum of the sizes of folders smaller than 100_000

   -- SECTION
   -- Read the command history
   -- we'll also solve part 1 while doing this

   -- SUBSECTION
   -- error handling

   Cannot_Descend_Into_File: exception;
   Cannot_List_File: exception;
   Cannot_Rise_From_Folder: exception;
   Invalid_Cd_Path: exception;
   Unknown_Command: exception;
   Unknown_File_Type: exception;
   Unknown_Path: exception;

   Line_Number: Positive := 1;

   -- SUBSECTION
   -- reading the input

   procedure Adjust_Size(Pos: FS_Trees.Cursor) is
   -- sums the sizes of the children of Pos and stores it there
   -- also updates `Number_Small` and `Sum_Small`

      use all type FS_Trees.Cursor;

      Size : Natural := 0;
      Child: FS_Trees.Cursor := FS_Trees.First_Child(Pos);

   begin

      -- sum the children's sizes
      while Child /= FS_Trees.No_Element loop
         Size := Size + FS_Trees.Element(Child).Size;
         Child := FS_Trees.Next_Sibling(Child);
      end loop;

      -- update
      FS_Tree.Reference(Pos).Size := Size;

      -- if this is a "small" size, update that data, too
      if Size <= 100_000 then
         Number_Small := Number_Small + 1;
         Sum_Small := Sum_Small + Size;
      end if;

   end Adjust_Size;

   procedure Parse_Command(Pos: in out FS_Trees.Cursor) is
   -- parses a command from the history

      use all type FS_Trees.Cursor;

      Command: String := Atio.Get_Line(Input_File);
      Read_Name: FS_Name := ( others => ' ' );

   begin

      pragma Assert(Command(1) = '$'); -- turned out to be unwarranted
                                       -- but better safe than sorry

      if Command(3..4) = "cd" then
         -- change directory; need to determine the target

         if Command(6) = '/' then
            -- root
            Pos := FS_Trees.First_Child(FS_Tree.Root);

         elsif Command(6) = '.' and then Command(7) = '.' then
            -- up one level; we adjust the indirect size here, as well
            if FS_Trees.Element(Pos).Kind = Folder then
               -- the following guard was probably unwarranted
               if FS_Trees.Element(Pos).Size = 0 then
                  Adjust_Size(Pos);
               end if;
               Pos := FS_Trees.Parent(Pos);
            else
               raise Cannot_Rise_From_Folder;
            end if;

         elsif Command(6) in 'a' .. 'z' then
            -- down into named folder
            Read_Name(1 .. Command'Length - 6 + 1)
               := Command(6 .. Command'Length);

            -- find child; raise exception if not found
            declare Child: FS_Trees.Cursor := FS_Trees.First_Child(Pos);
            begin

               while Child /= FS_Trees.No_Element loop
                  if FS_Trees.Element(Child).Name = Read_Name then
                     if FS_Trees.Element(Child).Kind = Folder then
                        Pos := Child;
                        exit;
                     else
                        raise Cannot_Descend_Into_File with "at" & Line_Number'Image;
                     end if;
                  else
                     Child := FS_Trees.Next_Sibling(Child);
                  end if;
               end loop;

               if Child = FS_Trees.No_Element then
                  raise Unknown_Path with "at" & Line_Number'Image;
               end if;

            end;

         else
            raise Invalid_Cd_Path with Command(6..Command'Length);

         end if;

      elsif Command(3..4) = "ls" then
         -- list directory entries; need to create children

         -- the following guard was unwarranted, but better safe than sorry
         if FS_Trees.Element(Pos).Kind /= Folder then
            raise Cannot_List_File;
         end if;

         declare
            Next_Character: Character; -- for lookahead
            Eol           : Boolean; -- for lookahead; not read, only written
         begin

            while not ATIO.End_Of_File(Input_File) loop

               -- first determine if we are done: look for a command prompt
               ATIO.Look_Ahead(Input_File, Next_Character, Eol);

               if Next_Character = '$' then exit;

               else
                  Line_Number := Line_Number + 1; -- useful during development
                  declare Filename: String := Atio.Get_Line(Input_File);
                  begin

                     if Filename(1..3) = "dir" then -- directories begin w/"dir"
                        declare
                           New_Record: FS_Record;
                           New_Name  : FS_Name := (others => ' ');
                        begin
                           New_Name(1 .. Filename'Length - 5 + 1)
                              := Filename(5 .. Filename'Length);
                           New_Record.Name := New_Name;
                           New_Record.Kind := Folder;
                           New_Record.Size := 0;
                           FS_Tree.Append_Child(Pos, New_Record);
                        end;

                     elsif Filename(1) in '0'..'9' then -- files begin with size
                        declare
                           New_Record: FS_Record;
                           New_Name  : FS_Name := ( others => ' ' );
                           New_Size  : Natural;
                           Position  : Positive := 1;
                        begin
                           ATIIO.Get(Filename, New_Size, Position);
                           New_Name(1 .. Filename'Length - Position - 2 + 1)
                              := Filename(Position + 2 .. Filename'Length);
                           New_Record.Name := New_Name;
                           New_Record.Kind := File;
                           New_Record.Size := New_Size;
                           FS_Tree.Append_Child(Pos, New_Record);
                        end;

                     else raise Unknown_File_Type; -- better safe than sorry

                     end if;

                  end;

               end if;

            end loop;

            -- this messed me up on Part 1
            -- I was only updating directory sizes when I saw a "cd .."
            -- but the last element(s) in the command history are files,
            -- so their parent folders won't be updated
            -- good example of "premature optimization is the root of all evil"
            if ATIO.End_Of_File(Input_File) then
               while Pos /= FS_Tree.Root and then FS_Trees.Element(Pos).Size = 0
               loop
                  Adjust_Size(Pos);
                  Pos := FS_Trees.Parent(Pos);
               end loop;
            end if;

         end;

      else raise Unknown_Command with "at" & Line_Number'Image;
         -- better safe than sorry!
      end if;

   end Parse_Command;

   procedure Read_Transcript is
      -- basically calls `Parse_Command` after opening and closing the input
      Pos: FS_Trees.Cursor := FS_Tree.Root;
   begin
      ATIO.Open(Input_File, ATIO.In_File, Filename);
      while not ATIO.End_Of_File(Input_File) loop
         Parse_Command(Pos);
         Line_Number := Line_Number + 1;
      end loop;
      ATIO.Close(Input_File);
   end Read_Transcript;

   -- SECTION
   -- part 2

   Disk_Size: constant := 70_000_000; -- given in puzzle
   Space_Needed: constant := 30_000_000; -- given in puzzle
   Smallest_Useful_Folder: FS_Record
      := ( Name => ( others => ' ' ), Size => Disk_Size, Kind => Folder );

   procedure Update_Smallest_Big_Dir(Thing: FS_Record; Needed_Size: Natural) is
   -- compares `Thing`'s `Size` to `Smallest_Useful_Folder`'s `Size`,
   -- and if it's smaller, _and_ if it's larger than `Needed_Size`,
   -- replaces `Smallest_Useful_Folder`
   begin
      if Thing.Kind = Folder and then Thing.Size < Smallest_Useful_Folder.Size
         and then Thing.Size > Needed_Size
      then
         Smallest_Useful_Folder := Thing;
      end if;
   end Update_Smallest_Big_Dir;

   function Update_Level(Size: Natural) return Natural is ( Size );
   -- absolutely should not update the needed size while recursive
   -- (I really lucked out on this one, as the abstract recursion function
   -- was updating its data by adding one each time,
   -- so I ran the risk of getting a somewhat larger value...)

   procedure Find_Smallest_Big_Dir is
   -- finds the smallest directory big enough to delete
   -- and so free enough space for the update,
   -- via use of `Recurse_Tree`
      Pos: FS_Trees.Cursor := FS_Trees.First_Child(FS_Tree.Root);
      Size_To_Delete: Natural := 30_000_000
                        - (Disk_Size - FS_Trees.Element(Pos).Size);
   begin
      ATIO.Put_Line("need to delete" & Size_To_Delete'Image);
      Recurse_Tree(Pos,
                   Update_Smallest_Big_Dir'Access,
                   Update_Level'Access,
                   Size_To_Delete
                  );
   end Find_Smallest_Big_Dir;

begin

   -- part 1

   FS_Tree.Append_Child(FS_Tree.Root, Root);
   Read_Transcript;
   ATIO.Put_Line("there are" & Number_Small'Image & " small-ish files"
                & " of total size" & Sum_Small'Image);

   -- used during debugging
   -- Write_Out_Tree;

   -- part 2

   Find_Smallest_Big_Dir;
   ATIO.Put("can delete the file ");
   Print_FS_Name(Smallest_Useful_Folder.Name);
   ATIO.Put_Line(" which has size" & Smallest_Useful_Folder.Size'Image);

end Main;
