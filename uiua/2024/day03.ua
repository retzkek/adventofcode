# AoC 2024.03
Test  ← $ xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
Parse ← ≡(≡⋕ ↘1) regex $ mul\((\d+),(\d+)\)
Parse Test
## ╭─       
## ╷  2 4   
##    5 5   
##   11 8   
##    8 5   
##        ╯ 
One ← /+/×⍉
⍤ ⤙= 161 One

Inp ← Parse &fras $"_/.config/aocd/2024/3/input.txt" &var "HOME"
&p One Inp
