# AoC 2024.04
# https://uiua.org/pad?src=0_14_0-dev_6__IyBBb0MgMjAyNC4wNAokIE1NTVNYWE1BU00KJCBNU0FNWE1TTVNBCiQgQU1YU1hNQUFNTQokIE1TQU1BU01TTVgKJCBYTUFTQU1YQU1NCiQgWFhBTU1YWEFNQQokIFNNU01TQVNYU1MKJCBTQVhBTUFTQUFBCiQgTUFNTU1YTU1NTQokIE1YTVhBWE1BU1gKVGVzdCAg4oaQClBhcnNlIOKGkCDiipziiJjiiaBAXG4uClBhcnNlIFRlc3QKTFIgICDihpAgLysoK-KIqSgvK-KMlSnih4wsLCJYTUFTIikKVUQgICDihpAgTFIg4o2JCkRpYWcg4oaQICviiKkoVUQg4qyaQCDiiaEo4oqCe-KNpUAgfSkp4oeMLCzCsOKKjwpQ4oKBICAg4oaQIC8rW-KKgyhMUnxVRHxEaWFnKV0K4o2k4qSZPSAxOCBQ4oKBCgpQYXJzZSBUZXN0CkRpYWdzIOKGkCDiioMo4oqh4o2J4oqfLsKw4oqPKSjiiqHijYniip_ih4wuwrDiio8pClDigoIgICAg4oaQIC8rw5fiiaHiiKkoK-KIqSjiiY0iTUFTIinih4wuKSDiiaFEaWFncyDima3igoPip4jiiJhbMyAzXQrijaQg4qSZPSA5IFDigoIK
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
LR   ← /+(+∩(/+⌕)⇌,,"XMAS")
UD   ← LR ⍉
Diag ← +∩(UD ⬚@ ≡(⊂{⍥@ }))⇌,,°⊏
P₁   ← /+[⊃(LR|UD|Diag)]
⍤⤙= 18 P₁

Parse Test
Diags ← ⊟⊃(⊡⍉⊟.°⊏)(⊡⍉⊟⇌.°⊏)
P₂    ← /+≡(/×≡(+∩(≍"MAS")⇌.))♭₃⧈Diags[3 3]
⍤ ⤙= 9 P₂

AOC ~ "../aoc.ua"
AOC~Input 2024 4
⊃(&p P₂ Parse)(&p P₁ Parse)
