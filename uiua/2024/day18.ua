# AoC 2024.18
# https://uiua.org/pad?src=0_14_0-rc_3__IyBBb0MgMjAyNC4xOAoKSW5wIOKGkCAmZnJhcyAiaW5wLnR4dCIKUCAgIOKGkCDiipwo4oqc4ouV4oq44omgQCwp4oq44omgQFxuClDigoEg4oaQICgKICDiiprCrMKw4oqaCiAg4oqiIHBhdGgo4pa94oq4KOKIiik6K0HigoLCpHziiY3iipnii4XiiJgpICRTdGFydDBfMAogIC3igoHip7sKKQrijaTipJk9MjIgUOKCgSDihpnigoHigoIgUCBJbnAgJEdvYWw2XzYKCkRyYXcg4oaQIOKWveKfnOKJoeKWvTIwIOKNicKw4oqaCgpQ4oKCIOKGkCAoCiAg4qSZ4oiYCiAg4o2iKOKGmMKvMXzijaMoMOKXjFDigoF8MSkpCiAg4oqi4oaY4qe74oqZ4peMCikK4o2k4qSZ4omNIDZfMSBQ4oKCICRHb2FsNl82IFAgSW5wCg==
$ 5,4
$ 4,2
$ 4,5
$ 3,0
$ 2,1
$ 6,3
$ 2,4
$ 1,5
$ 0,6
$ 3,3
$ 2,6
$ 5,1
$ 1,2
$ 5,5
$ 2,5
$ 6,5
$ 1,4
$ 0,4
$ 6,4
$ 1,1
$ 6,1
$ 1,0
$ 0,5
$ 1,6
$ 2,0
Inp ←
P   ← ⊜(⊜⋕⊸≠@,)⊸≠@\n
P₁ ← (
  ⊚¬°⊚
  ⊢ path(▽⊸(∊):+A₂¤|≍⊙⋅∘) $Start0_0
  -₁⧻
)
⍤⤙=22 P₁ $↙₁₂ P Inp $Goal6_6

Draw ← ▽⟜≡▽20 ⍉°⊚

AOC ~ "../aoc.ua"
AOC~Input 2024 18
P₁ ↙₁₀₂₄ P ⊙70_70

P₂ ← (
  ⤙∘
  ⍢(↘¯1|⍣(0◌P₁|1))
  ⊢↘⧻⊙◌
)
⍤⤙≍ 6_1 P₂ $Goal6_6 P Inp

AOC~Input 2024 18
P₂ 70_70 P
