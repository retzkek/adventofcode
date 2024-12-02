# AoC 2024.02
# https://uiua.org/pad?src=0_14_0-dev_5__IyBBb0MgMjAyNC4wMgpUZXN0ICDihpAgJCA3IDYgNCAyIDEKICAgICAgICAkIDEgMiA3IDggOQogICAgICAgICQgOSA3IDYgMiAxCiAgICAgICAgJCAxIDMgMiA0IDUKICAgICAgICAkIDggNiA0IDQgMQogICAgICAgICQgMSAzIDYgNyA5CiAgICAgICAgJCAxIDIgMyA0IDUgNgogICAgICAgICQgMSAxIDEgMQpQYXJzZSDihpAg4oqcKOKWoeKKnOKLleKJoEAgLiniiaBAXG4uClBhcnNlIFRlc3QKIyMKSW5jcmVhc2luZyAg4oaQIOKJjcKw4paz4pazLuKNjwpEZWNyZWFzaW5nICDihpAg4omNwrDilrPilrMu4o2WCkNoYW5nZXMgICAgIOKGkCAvLSDijYkg4perMgpTYWZlQ2hhbmdlcyDihpAgPSAvKyDDl-KKgyjiiaQzKSg-MCkg4p-c4qe7IOKMtUNoYW5nZXMKU2FmZSAgICAgICAg4oaQID0xIMOXK-KKg-KKg0luY3JlYXNpbmcgRGVjcmVhc2luZyBTYWZlQ2hhbmdlcwpOU2FmZSAgICAgICDihpAgLysg4omhKFNhZmUgwrDilqEpCuKUjOKUgOKVtHRlc3QKICDijaTipJniiY0gWzEgMCAwIDAgMCAxIDEgMF0g4omhKFNhZmUgwrDilqEpCiAg4o2k4qSZPSAzIE5TYWZlIFBhcnNlIFRlc3QK4pSU4pSA4pW0CgpJZGVudCAgICAg4oaQIOKKnj0gLuKHoQpBbHRzICAgICAg4oaQIOKJoeKWvSA6IMKk4p-cKMKsSWRlbnQg4qe7KQpEYW1wU2FmZSAg4oaQID4wIC8rIOKJoVNhZmUgQWx0cwpORGFtcFNhZmUg4oaQIC8rIOKJoSjiqKwoRGFtcFNhZmUpKDHil4wpIFNhZmUuIMKw4pahKQrilIzilIDilbR0ZXN0CiAg4o2k4qSZ4omNIFsxIDAgMCAxIDEgMSAxIDBdIOKJoShEYW1wU2FmZSDCsOKWoSkgUGFyc2UgVGVzdAogIOKNpOKkmT0gNSBORGFtcFNhZmUgUGFyc2UgVGVzdArilJTilIDilbQK
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
Changes     ← /- ⍉ ◫2
SafeChanges ← = /+ ×⊃(≤3)(>0) ⟜⧻ ⌵Changes
Safe        ← =1 ×+⊃⊃Increasing Decreasing SafeChanges
NSafe       ← /+ ≡(Safe °□)
┌─╴test
  ⍤. ≍ [1 0 0 0 0 1 1 0 0] ≡(Safe °□)
  ⍤. =3 NSafe Parse Test
└─╴

Inp ← Parse &fras $"_/.config/aocd/2024/2/input.txt" &var "HOME"
&p NSafe Inp

Ident     ← ⊞= .⇡
Alts      ← ≡▽ : ¤⟜(¬Ident ⧻)
DampSafe  ← >0 /+ ≡Safe Alts
NDampSafe ← /+ ≡(⨬(DampSafe)(1◌) Safe. °□)
┌─╴test
  ⍤⤙≍ [1 0 0 1 1 1 1 0 0] ≡(DampSafe °□) Parse Test
  ⍤⤙= 5 NDampSafe Parse Test
└─╴

&p NDampSafe Inp
