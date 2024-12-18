# AoC 2024.17
$ Register A: 729
$ Register B: 0
$ Register C: 0
$ 
$ Program: 0,1,5,4,3,0
Inp ←

P ← (
  °$$ Register A: _
   $$ Register B: _
   $$ Register C: _
   $$ 
   $$ Program: _
  # parse
  ⊓(∩⋕|⋕⊙(⊜⋕≠@,.))
  # move program to top
  ⤙⊙⊙⊙◌
)

#               A  B   C
Cmb ← ⨬(0|1|2|3|∘|⋅∘∘|⋅⋅∘)

# The adv instruction (opcode 0) performs division. The numerator is the value in the A register. The denominator is found by raising 2 to the power of the instruction's combo operand. (So, an operand of 2 would divide A by 4 (2^2); an operand of 5 would divide A by 2^B.) The result of the division operation is truncated to an integer and then written to the A register.
Adv ← ÷⊃(ⁿ:2Cmb|⋅⊙⊙∘)
⍤⤙≍[5 0 0] ⊟₃ Adv 2 20 0 0
⍤⤙≍[5 3 1] ⊟₃ Adv 5 40 3 1

# The bxl instruction (opcode 1) calculates the bitwise XOR of register B and the instruction's literal operand, then stores the result in register B.
Bxl ← ⊃(⋅∘|°⋯=₁ ⬚0+ ∩⋯ ⊙⋅∘|⋅⋅⋅∘)
⍤⤙≍[1 2 3] ⊟₃ Bxl 8 1 10 3

# The bst instruction (opcode 2) calculates the value of its combo operand modulo 8 (thereby keeping only its lowest 3 bits), then writes that value to the B register.
Bst ← ⊃(⋅∘|◿₈Cmb|⋅⋅⋅∘)
⍤⤙≍[0 1 9] ⊟₃ Bst 6 0 0 9

# P Inp
# ⨬(Adv|Bxl|Bst)°⊟↙2
