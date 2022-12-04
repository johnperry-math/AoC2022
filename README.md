# AoC2022

[Advent of Code 2022](https://adventofcode.com/2022). **In Ada, no less!**
Because, after 2019 and 2020, I _still_ haven't learned my lesson.

The elves are on an expedition for reindeer food.

## Day 1: Calorie Counting

Determine which elf/elves have the most calories, and report how many calories.
* Part 1 asks for the most calories carried by any one elf.
* Part 2 asks for the calories carried by the three elves with the most calories.

## Day 2: Rock Paper Scissors

An elf gives you a strategy guide for playing rock-paper-scissors.
This strikes you as suspicious, so you check it out.
* Part 1 has you score the strategy when you play it one way.
* Part 2 has you score the strategy when you play it another way.

(Seems odd that the scores were always natural;
given that you suspect the elf, a loss should have a negative score IMHO.)

## Day 3: Rucksack Reorganization

Elves have common items in rucksacks.
* Part 1 asks for the sum of the priorities of items
  common to each rucksack's two compartments.
  Fortunately, each rucksack has only one common item.
* Part 2 asks for the sum of the priorities of items
  common each each group of three rucksacks.
  Fortunately, the groups are already grouped,
  and each group has only one common item.

  I ended up solving this three different ways:
  * In the first, I just searched through the strings.
  * In the second, I used hashed sets of characters to simplify the searching.
    It might be a little quicker, too, but the input is small,
    so I haven't bothered measuring it.
  * In the third, I used an array of booleans indexed by a range of characters
    that includes our desired character set.
    This should be much quicker than the first two approaches,
    though it took a while to remember how to implement this properly.
