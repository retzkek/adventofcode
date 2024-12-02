# AoC 2024.02
# https://uiua.org/pad?src=0_14_0-dev_5__IyBBb0MgMjAyNC4wMgpUZXN0ICDihpAgJCA3IDYgNCAyIDEKICAgICAgICAkIDEgMiA3IDggOQogICAgICAgICQgOSA3IDYgMiAxCiAgICAgICAgJCAxIDMgMiA0IDUKICAgICAgICAkIDggNiA0IDQgMQogICAgICAgICQgMSAzIDYgNyA5CiAgICAgICAgJCAxIDIgMyA0IDUgNgogICAgICAgICQgMSAxIDEgMQpQYXJzZSDihpAg4oqcKOKWoeKKnOKLleKJoEAgLiniiaBAXG4uClBhcnNlIFRlc3QKIyMKSW5jcmVhc2luZyAg4oaQIOKJjcKw4paz4pazLuKNjwpEZWNyZWFzaW5nICDihpAg4omNwrDilrPilrMu4o2WCkNoYW5nZXMgICAgIOKGkCAvLeKKn-KKgyjihpkgLTEg4qe7Liko4oaYIDEpICMgY291bGQgc2ltcGxpZnkgd2l0aCB3aW4KU2FmZUNoYW5nZXMg4oaQID0gLysgw5fiioMo4omkMykoPjApIOKfnOKnuyDijLVDaGFuZ2VzClNhZmUgICAgICAgIOKGkCA9MSDDlyviioPiioNJbmNyZWFzaW5nIERlY3JlYXNpbmcgU2FmZUNoYW5nZXMKTlNhZmUgICAgICAg4oaQIC8rIOKJoShTYWZlIMKw4pahKQrijaTipJniiY0gWzEgMCAwIDAgMCAxIDEgMF0g4omhKFNhZmUgwrDilqEpCuKNpOKkmT0gMyBOU2FmZSBQYXJzZSBUZXN0CgpJZGVudCAgICAg4oaQIC_iioIg4oqe4oa7ICjCr-KHoSkg4p-cKMKkID0wIOKHoSkKQWx0cyAgICAgIOKGkCDiiaHilr0gOiDCpOKfnCjCrElkZW50IOKnuykKRGFtcFNhZmUgIOKGkCA-MCAvKyDiiaFTYWZlIEFsdHMKTkRhbXBTYWZlIOKGkCAvKyDiiaEo4qisKERhbXBTYWZlKSgx4peMKSBTYWZlLiDCsOKWoSkK4o2k4qSZ4omNIFsxIDAgMCAxIDEgMSAxIDBdIOKJoShEYW1wU2FmZSDCsOKWoSkgUGFyc2UgVGVzdArijaTipJk9IDUgTkRhbXBTYWZlIFBhcnNlIFRlc3QK
Test  ← $ 7 6 4 2 1
        $ 1 2 7 8 9
        $ 9 7 6 2 1
        $ 1 3 2 4 5
        $ 8 6 4 4 1
        $ 1 3 6 7 9
        $ 1 2 3 4 5 6
        $ 1 1 1 1
        $ 1 1 1 10 1
Parse ← ⊜(□⊜⋕≠@ .)≠@\n.
Parse Test
## {[7 6 4 2 1] [1 2 7 8 9] [9 7 6 2 1] [1 3 2 4 5] [8 6 4 4 1] [1 3 6 7 9] [1 2 3 4 5 6] [1 1 1 1] [1 1 1 10 1]}
Increasing  ← ≍°△△.⍏
Decreasing  ← ≍°△△.⍖
Changes     ← /-⊟⊃(↙ -1 ⧻.)(↘ 1) # could simplify with win
SafeChanges ← = /+ ×⊃(≤3)(>0) ⟜⧻ ⌵Changes
Safe        ← =1 ×+⊃⊃Increasing Decreasing SafeChanges
NSafe       ← /+ ≡(Safe °□)
┌─╴test
  ⍤. ≍ [1 0 0 0 0 1 1 0 0] ≡(Safe °□)
  ⍤. =3 NSafe Parse Test
└─╴

Inp ← Parse &fras $"_/.config/aocd/2024/2/input.txt" &var "HOME"
&p NSafe Inp

Ident     ← /⊂ ⊞↻ (¯⇡) ⟜(¤ =0 ⇡) # could simplify with just table
Alts      ← ≡▽ : ¤⟜(¬Ident ⧻)
DampSafe  ← >0 /+ ≡Safe Alts
NDampSafe ← /+ ≡(⨬(DampSafe)(1◌) Safe. °□)
┌─╴test
  ⍤⤙≍ [1 0 0 1 1 1 1 0 0] ≡(DampSafe °□) Parse Test
  ⍤⤙= 5 NDampSafe Parse Test
└─╴

&p NDampSafe Inp
