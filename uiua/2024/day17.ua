# AoC 2024.17
$ Register A: 729
$ Register B: 0
$ Register C: 0
$ 
$ Program: 0,1,5,4,3,0
$ 
Inp ←

# Parse Input
# Pointer Program a b c Output ? Input
P ← |1.6 (
  ⊙($Output [])
  # parse program and registers
  °$$ Register A: _
   $$ Register B: _
   $$ Register C: _
   $$ 
   $$ Program: _
   $$
  ⊓(⊃($a⋕|$b⋕◌)|$c⋕⊙($Program⊜⋕≠@,.))
  # move program to top
  ⤙⊙⊙⊙◌
  ⊙($Inst0)
)

# All instructions |5.4, Except Out, which is |6.5
# Args: operand, pointer, A, B, C, [output]
# Ret:           pointer, A, B, C, [output]

# Combination operand
# Res ? Operand Pointer a b c
Cmb ← ⨬(0|1|2|3|⋅∘|⋅⋅∘|⋅⋅⋅∘)

# The adv instruction (opcode 0) performs division. The numerator is the value in the A register. The denominator is found by raising 2 to the power of the instruction's combo operand. (So, an operand of 2 would divide A by 4 (2^2); an operand of 5 would divide A by 2^B.) The result of the division operation is truncated to an integer and then written to the A register.
# Pointer a b c ? Operand Pointer a b c
Adv ← +₂⊙(⌊÷)⊃(⋅∘|ⁿ:2Cmb|⋅⋅⊙⊙∘)
⍤⤙≍[2 5 0 0] ⊟₄ Adv 2 0 20 0 0
⍤⤙≍[2 5 3 1] ⊟₄ Adv 5 0 40 3 1

# The bdv instruction (opcode 6) works exactly like the adv instruction except that the result is stored in the B register. (The numerator is still read from the A register.)
# Pointer a b c ? Operand Pointer a b c
Bdv ← +₂⊙⊙(⌊÷)⊃(⋅⊙∘|ⁿ:2Cmb|⋅⋅⊙∘)
⍤⤙≍[2 20 5 0] ⊟₄ Bdv 2 0 20 0 0

# The cdv instruction (opcode 7) works exactly like the adv instruction except that the result is stored in the C register. (The numerator is still read from the A register.)
# Pointer a b c ? Operand Pointer a b c
Cdv ← +₂⊙⊙⊙(⌊÷)⊃(⋅⊙⊙∘|ⁿ:2Cmb|⋅⋅∘)
⍤⤙≍[2 20 0 5] ⊟₄ Cdv 2 0 20 0 0

Xor ← °⋯=₁ ⬚0+ ∩⋯
⍤⤙=2 Xor 8 10
# The bxl instruction (opcode 1) calculates the bitwise XOR of register B and the instruction's literal operand, then stores the result in register B.
# Pointer a b c ? Operand Pointer a b c
Bxl ← +₂⊃(⋅⊙∘|Xor⊙⋅⋅∘|⋅⋅⋅⋅∘)
⍤⤙≍[2 1 2 3] ⊟₄ Bxl 8 0 1 10 3

# The bst instruction (opcode 2) calculates the value of its combo operand modulo 8 (thereby keeping only its lowest 3 bits), then writes that value to the B register.
# Pointer a b c ? Operand Pointer a b c
Bst ← +₂⊃(⋅⊙∘|◿₈Cmb|⋅⋅⋅⋅∘)
⍤⤙≍[2 0 1 9] ⊟₄ Bst 6 0 0 0 9

# The jnz instruction (opcode 3) does nothing if the A register is 0. However, if the A register is not zero, it jumps by setting the instruction pointer to the value of its literal operand; if this instruction jumps, the instruction pointer is not increased by 2 after this instruction.
# Pointer a ? Operand Pointer a
Jnz ← ⨬(+₂⋅∘|∘)◡⋅⋅>₀
⍤⤙≍[3 0 1 2] ⊟₄ Jnz 1 1 0 1 2
⍤⤙≍[4 1 1 2] ⊟₄ Jnz 4 1 1 1 2

# The bxc instruction (opcode 4) calculates the bitwise XOR of register B and register C, then stores the result in register B. (For legacy reasons, this instruction reads an operand but ignores it.)
# Pointer a b c ? Operand Pointer a b c
Bxc ← +₂⊃(⋅⊙∘|⋅⋅⋅⊸Xor)
⍤⤙≍[2 0 2 10] ⊟₄ Bxc 1 0 0 8 10

# The out instruction (opcode 5) calculates the value of its combo operand modulo 8, then outputs that value. (If a program outputs multiple values, they are separated by commas.)
# Pointer a b c Output ? Operand Pointer a b c Output
Out ← ⊃(+₂⋅⊙⊙⊙∘|⊂:◿₈Cmb)
⍤⤙≍{2 0 0 11 [1 2 3]} □₅ Out 6 0 0 0 11 [1 2]

P Inp
Run ← ⊙⨬(Adv|Bxl|Bst|Jnz|Bxc|Out|Bdv|Cdv) ⟜⊸(⊃(⊡|⊡+₁):)
⍢(Run|<⧻)
Disp ← /◇(⊂⊂:@,)°⋕◌◌◌◌◌
Disp

AOC ~ "../aoc.ua"
AOC~Input 2024 17
⍢(Run|<⧻) P
Disp
