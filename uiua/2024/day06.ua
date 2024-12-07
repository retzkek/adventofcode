# AoC 2024.06
$ ....#.....
$ .........#
$ ..........
$ ..#.......
$ .......#..
$ ..........
$ .#..^.....
$ ........#.
$ #.........
$ ......#...
Test  ←
Parse ← ⊜∘≠@\n.
Parse Test

Idx ← ♭₂⊞⊟⇡⊙⇡°⊟△
Obs ← ♭=@#
Gd  ← ♭=@^
⊃(▽Gd)(▽Obs)⟜Idx
### ╭─      
### ╷ 0 4   
###   1 9   
###   3 2   
###   4 7   
###   6 1   
###   7 8   
###   8 0   
###   9 6   
###       ╯ 
### ╭─      
### ╷ 6 4   
###       ╯ 
# Move coord in dir: 0=up, 1=right, 2=down, 3=left
Move ← ⨬(⍜⊢-₁|⍜⊣+₁|⍜⊢+₁|⍜⊣-₁)
┌─╴test
  ⍤⤙≍ 5_4 Move 0 6_4
  ⍤⤙≍ 6_5 Move 1 6_4
  ⍤⤙≍ 7_4 Move 2 6_4
  ⍤⤙≍ 6_3 Move 3 6_4
└─╴
InBound ← /×♭×⊃≥₀<₁₀
┌─╴test
  ⍤⤙= 1 InBound 0_0
  ⍤⤙= 0 InBound ¯1_¯1
  ⍤⤙= 0 InBound 10_10
└─╴
# ⍢(⊂⍜⊢(Move 0)?⟜↘₁|×⊃(¬/+♭⌕)(InBound))⊂.
# that works, but we need to get the direction off the stack and update it...
⍢(⟜(⊂Move):⊢,?|⊙(×⊃(¬/+♭⌕⊢)(InBound)))0
# ugh stack juggling is hard :(
