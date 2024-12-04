# AoC 2024.04
$ MMMSXXMASM
$ MSAMXMSMSA
$ AMXSXMAAMM
$ MSAMASMSMX
$ XMASAMXAMM
$ XXAMMXXAMA
$ SMSMSASXSS
$ SAXAMASAAA
$ MAMMMXMMMM
$ MXMXAXMASX
Test  ←
Parse ← ⊜∘≠@\n.
Parse Test
Forward  ← /+≡(/+⌕"XMAS")
Backward ← /+≡(/+⌕"SAMX")
UpDown   ← +⊃Forward Backward ⍉
Diag     ← UpDown ⬚@ ≡(⊂{⍥@ })°⊏
Diag₂    ← UpDown ⬚@ ≡(⊂{⍥@ })⇌°⊏
P₁       ← ++++⊃⊃⊃⊃Forward Backward UpDown Diag Diag₂
⍤⤙= 18 P₁

AOC ~ "../aoc.ua"
AOC~Input 2024 4
&p P₁ Parse
