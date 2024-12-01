# Experimental!
Inp ← &fras $"_/.config/aocd/2024/1/input.txt" &var "HOME"
Test ← $ 3   4
       $ 4   3
       $ 2   5
       $ 1   3
       $ 3   9
       $ 3   3
Parse ← (
  ≠@\n. # find newlines
  ⊜□    # split lines
  ≠@ .  # find spaces
  ⍚⊜□   # split numbers
  ∵⋕°□  # parse numbers into array 
)
Dist ← (
  ⍉≡⍆⍉   # sort rows
  ≡/(⌵-) # distance
  /+     # sum
)
⍤$"_ should be 11": =11. Dist Parse Test
&p Dist Parse Inp
