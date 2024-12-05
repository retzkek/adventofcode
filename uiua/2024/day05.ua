# AoC 2024.05
$ 47|53
$ 97|13
$ 97|61
$ 97|47
$ 75|29
$ 61|13
$ 75|53
$ 29|13
$ 97|29
$ 53|29
$ 61|53
$ 97|53
$ 61|29
$ 47|13
$ 75|47
$ 97|75
$ 47|61
$ 75|61
$ 47|29
$ 75|13
$ 53|13
$ 
$ 75,47,61,53,29
$ 97,61,53,29,13
$ 75,29,13
$ 75,97,47,61,53
$ 61,13,29
$ 97,13,75,29,47
Test  ←
Parse ← (
  °⊟⊜□¬⌕"\n\n".     # split rules from updates
  ◇⊜(□⊜⋕⊸≠@|)⊸≠@\n  # parse rules
  ◇⊜(□⊜⋕⊸≠@,)⊸≠@\n: # parse updates
)
Parse Test
# number of violations of page ordering rules
👎 ← (
  ◇▽⊙⍚♭₂♭(□⊞>.⇡⧻°□).⍚⊞⊟. # build list of all page orderings
  ◇≡⇌:¤                  # turn rules into anti-rules
  /+≡(/+♭◇⌕)             # count anti-rules appearing in orderings
)
P₁ ← (
  ▽:⟜(=₀≡👎⊙¤)      # filter incorrect orderings
  ≡(⊡⊙°□)⊸(⌊÷₂≡◇⧻) # get the middle pages
  /+
)
⍤⤙= 143 P₁

AOC ~ "../aoc.ua"
P₁ Parse AOC~Input 2024 5
