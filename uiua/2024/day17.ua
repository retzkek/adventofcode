# AoC 2024.17
$ Register A: 729
$ Register B: 0
$ Register C: 0
$ 
$ Program: 0,1,5,4,3,0
Inp ←

# Parse Input
# Ret: pointer, program, A, B, C
P ← |1.6 (
  °$$ Register A: _
   $$ Register B: _
   $$ Register C: _
   $$ 
   $$ Program: _
  # parse
  ⊓(∩⋕|⋕⊙(⊜⋕≠@,.))
  # move program to top
  ⤙⊙⊙⊙◌
  # instruction pointer
  0
  # output
  []
)

#                A    B     C
Cmb ← ⨬(0|1|2|3|⋅⋅∘|⋅⋅⋅∘∘|⋅⋅⋅⋅∘)

# All instructions |6.5, Except Out, which is |7.6
# Args: [output], operand, pointer, program, A, B, C
# Ret: [output], pointer, program, A, B, C

# The adv instruction (opcode 0) performs division. The numerator is the value in the A register. The denominator is found by raising 2 to the power of the instruction's combo operand. (So, an operand of 2 would divide A by 4 (2^2); an operand of 5 would divide A by 2^B.) The result of the division operation is truncated to an integer and then written to the A register.
Adv ← +₂⊙⊙(⌊÷)⊃(⋅⊙∘|ⁿ:2Cmb|⋅⋅⋅⊙⊙∘)
⍤⤙≍[2 ¯1 5 0 0] ⊟₅ Adv 2 0 ¯1 20 0 0
⍤⤙≍[2 ¯1 5 3 1] ⊟₅ Adv 5 0 ¯1 40 3 1

Xor ← °⋯=₁ ⬚0+ ∩⋯
⍤⤙=2 Xor 8 10
# The bxl instruction (opcode 1) calculates the bitwise XOR of register B and the instruction's literal operand, then stores the result in register B.
Bxl ← +₂⊃(⋅⊙⊙∘|Xor⊙⋅⋅⋅∘|⋅⋅⋅⋅⋅∘)
⍤⤙≍[2 ¯1 1 2 3] ⊟₅ Bxl 8 0 ¯1 1 10 3

# The bst instruction (opcode 2) calculates the value of its combo operand modulo 8 (thereby keeping only its lowest 3 bits), then writes that value to the B register.
Bst ← +₂⊃(⋅⊙⊙∘|◿₈Cmb|⋅⋅⋅⋅⋅∘)
⍤⤙≍[2 ¯1 0 1 9] ⊟₅ Bst 6 0 ¯1 0 0 9

# The jnz instruction (opcode 3) does nothing if the A register is 0. However, if the A register is not zero, it jumps by setting the instruction pointer to the value of its literal operand; if this instruction jumps, the instruction pointer is not increased by 2 after this instruction.
Jnz ← ⨬(+₂⋅∘|∘)◡⋅⋅⋅>₀
⍤⤙≍[3 ¯1 0 1 2] ⊟₅ Jnz 1 1 ¯1 0 1 2
⍤⤙≍[4 ¯1 1 1 2] ⊟₅ Jnz 4 1 ¯1 1 1 2

# The bxc instruction (opcode 4) calculates the bitwise XOR of register B and register C, then stores the result in register B. (For legacy reasons, this instruction reads an operand but ignores it.)
Bxc ← +₂⊃(⋅⊙⊙∘|⋅⋅⋅⋅⊸Xor)
⍤⤙≍[2 ¯1 0 2 10] ⊟₅ Bxc 1 0 ¯1 0 8 10

# The out instruction (opcode 5) calculates the value of its combo operand modulo 8, then outputs that value. (If a program outputs multiple values, they are separated by commas.)
Out ← ⊃(⊂⊙(◿₈Cmb)|+₂⋅⋅⊙⊙⊙⊙∘)
⍤⤙≍{[1 2 3] 2 ¯1 0 0 11} □₆ Out [1 2] 6 0 ¯1 0 0 11

P Inp
Run ← ⨬(⊙Adv|⊙Bxl|⊙Bst|⊙Jnz|⊙Bxc|Out) :⊙◡⊃(⊡|⊡+₁)
⍢(Run|<⧻:◌)
