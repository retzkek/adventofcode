Test  ← $ 7 6 4 2 1
        $ 1 2 7 8 9
        $ 9 7 6 2 1
        $ 1 3 2 4 5
        $ 8 6 4 4 1
        $ 1 3 6 7 9
        $ 1 2 3 4 5 6
Parse ← ⊜(□⊜⋕≠@ .)≠@\n.
Parse Test
## {[7 6 4 2 1] [1 2 7 8 9] [9 7 6 2 1] [1 3 2 4 5] [8 6 4 4 1] [1 3 6 7 9] [1 2 3 4 5 6]} 
Increasing  ← ≍°△△.⍏
Decreasing  ← ≍°△△.⍖
Changes     ← /-⊟⊃(↙ -1 ⧻.)(↘ 1)
SafeChanges ← = /+ ×⊃(≤3)(>0) ⟜⧻ ⌵Changes
Safe        ← =2 ++⊃⊃Increasing Decreasing SafeChanges
NSafe       ← /+ ≡(Safe °□)
⍤. ≍ [1 0 0 0 0 1 1] ≡(Safe °□)
⍤. =3 NSafe Parse Test
Inp ← &fras $"_/.config/aocd/2024/2/input.txt" &var "HOME"
&p NSafe Parse Inp
