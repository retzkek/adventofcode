# AoC 2024.02
# https://uiua.org/pad?src=0_14_0-dev_6__IyBBb0MgMjAyNC4wMgpUZXN0ICDihpAgJCA3IDYgNCAyIDEKICAgICAgICAkIDEgMiA3IDggOQogICAgICAgICQgOSA3IDYgMiAxCiAgICAgICAgJCAxIDMgMiA0IDUKICAgICAgICAkIDggNiA0IDQgMQogICAgICAgICQgMSAzIDYgNyA5CiAgICAgICAgJCAxIDIgMyA0IDUgNgogICAgICAgICQgMSAxIDEgMQpQYXJzZSDihpAg4oqcKOKWoeKKnOKLleKJoEAgLiniiaBAXG4uClBhcnNlIFRlc3QKU2FmZSDihpAg4oal4oipL8OX4oqDKMOX4oqDPuKCgOKJpOKCgykow5fiioPiiaXigovigoM84oKAKeKniC0KUOKCgSAgIOKGkCAvKyDiiaHil4dTYWZlCuKUjOKUgOKVtHRlc3QKICDijaTipJniiY0gezEgMCAwIDAgMCAxIDEgMH0g4o2aU2FmZQogIOKNpOKkmT0gMyBQ4oKBIFBhcnNlIFRlc3QK4pSU4pSA4pW0CgrihYggICAgIOKGkCDiip49IC7ih6EK4qW6ICAgICDihpAg4omh4pa9IDogwqTin5wowqzihYgg4qe7KQpTYWZl4oKCIOKGkCA-4oKAIC8rIOKJoVNhZmUg4qW6ClDigoIgICAg4oaQIC8rIOKJoeKXhyjiqKwoU2FmZeKCgikoMeKXjCkgU2FmZS4pCuKUjOKUgOKVtHRlc3QKICDijaTipJniiY0gWzEgMCAwIDEgMSAxIDEgMF0g4omh4peHU2FmZeKCgiBQYXJzZSBUZXN0CiAg4o2k4qSZPSA1IFDigoIgUGFyc2UgVGVzdArilJTilIDilbQK
Test  ← $ 7 6 4 2 1
        $ 1 2 7 8 9
        $ 9 7 6 2 1
        $ 1 3 2 4 5
        $ 8 6 4 4 1
        $ 1 3 6 7 9
        $ 1 2 3 4 5 6
        $ 1 1 1 1
Parse ← ⊜(□⊜⋕≠@ .)≠@\n.
Parse Test
Safe ← ↥∩/×⊃(×⊃>₀≤₃)(×⊃≥₋₃<₀)⧈-
P₁   ← /+ ≡◇Safe
┌─╴test
  ⍤⤙≍ {1 0 0 0 0 1 1 0} ⍚Safe
  ⍤⤙= 3 P₁ Parse Test
└─╴

ⅈ     ← ⊞= .⇡
⥺     ← ≡▽ : ¤⟜(¬ⅈ ⧻)
Safe₂ ← >₀ /+ ≡Safe ⥺
P₂    ← /+ ≡◇(⨬(Safe₂)(1◌) Safe.)
┌─╴test
  ⍤⤙≍ [1 0 0 1 1 1 1 0] ≡◇Safe₂ Parse Test
  ⍤⤙= 5 P₂ Parse Test
└─╴

AOC ~ "../aoc.ua"
AOC~Input 2024 2
⊃(&p P₂ Parse)(&p P₁ Parse)
