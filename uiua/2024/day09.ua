# AoC 2024.09
"2333133121414131402"
P₁ ← (
  # parse and build indexes
  × ↯:1_¯1⊸△ \+↯:0_1⊸△≡⋕
  # .### [2 3 3 3 1 3 3 1 2 1 4 1 4 1 3 1 4 0 2]          
  # .### [0 ¯1 1 ¯2 2 ¯3 3 ¯4 4 ¯5 5 ¯6 6 ¯7 7 ¯8 8 ¯9 9] 
  ≡⨬(∘|⋅¯1)⊸<₀
  # .## [0 ¯1 1 ¯1 2 ¯1 3 ¯1 4 ¯1 5 ¯1 6 ¯1 7 ¯1 8 ¯1 9] 
  # expand, with negatives for empty space
  /◇⊂≡(□↯):
  # .## [0 0 ¯1 ¯1 ¯1 1 1 1 ¯1 ¯1 ¯1 2 ¯1 ¯1 ¯1 3 3 3 ¯1 4 4 ¯1 5 5 5 5 ¯1 6 6 6 6 ¯1 7 7 7 ¯1 8 8 8 8 9 9] 
  # calculate how many empty block there are
  /+<₀..
  # .## 14 
  # since they'll all end up at the end, we can take that many off the back
  ⊃(▽¬⊸<₀⇌↘)(↙)-⊙⧻
  # .#### [0 0 ¯1 ¯1 ¯1 1 1 1 ¯1 ¯1 ¯1 2 ¯1 ¯1 ¯1 3 3 3 ¯1 4 4 ¯1 5 5 5 5 ¯1 6] 
  # .#### [9 9 8 8 8 8 7 7 7 6 6 6]
  # find indices of empty blocks                                             
  ⊙(⊚<₀.)
  # .### [2 3 4 8 9 10 12 13 14 18 21 26] 
  # .### [9 9 8 8 8 8 7 7 7 6 6 6]   
  # build array with new values in empty indices     
  ⬚0⌝⊡ ∵¤:
  # .### [0 0 ¯1 ¯1 ¯1 1 1 1 ¯1 ¯1 ¯1 2 ¯1 ¯1 ¯1 3 3 3 ¯1 4 4 ¯1 5 5 5 5 ¯1 6] 
  # .### [0 0 9 9 8 0 0 0 8 8 8 0 7 7 7 0 0 0 6 0 0 6 0 0 0 0 6]               
  # sub
  ⬚0+⊙(×>₀.)
  # .## [0 0 9 9 8 1 1 1 8 8 8 2 7 7 7 3 3 3 6 4 4 6 5 5 5 5 6 6] 
  # calculate checksum
  /+×°△△.
)
⍤⤙= 1928 P₁

AOC ~ "../aoc.ua"
AOC~Input 2024 9
P₁↘₋₁
