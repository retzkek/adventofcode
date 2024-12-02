# https://uiua.org/pad?src=0_14_0-dev_5__VGVzdCAg4oaQICQgMTAwMAogICAgICAgICQgMjAwMAogICAgICAgICQgMzAwMAogICAgICAgICQgCiAgICAgICAgJCA0MDAwCiAgICAgICAgJCAKICAgICAgICAkIDUwMDAKICAgICAgICAkIDYwMDAKICAgICAgICAkIAogICAgICAgICQgNzAwMAogICAgICAgICQgODAwMAogICAgICAgICQgOTAwMAogICAgICAgICQgCiAgICAgICAgJCAxMDAwMApQYXJzZSDihpAgKAogIOKKnOKWocKs4oyVIlxuXG4iLiAgICAjIHBhcnRpdGlvbiBlbHZlcwogIOKItSjilqHii5XiipzilqHiiaBAXG4uwrDilqEpICMgc3BsaXQgYW5kIHBhcnNlIGl0ZW1zCikKUGFyc2UgVGVzdAojIwpNYXhDYWwg4oaQIC_ihqXiiLUoLyvCsOKWoSkK4o2kLiA9MjQwMDAgTWF4Q2FsClRvcFRocmVlIOKGkCAoCiAg4oi1KC8rwrDilqEpICMgdG90YWwgY2Fsb3JpZXMgcGVyIGVsZgogIOKKj-KGmTPijZYuICAgIyBnZXQgdG9wIHRocmVlCiAgLysgICAgICAjIHN1bQopCuKNpC4gPTQ1MDAwIFRvcFRocmVlIFBhcnNlIFRlc3QK
Test  ← $ 1000
        $ 2000
        $ 3000
        $ 
        $ 4000
        $ 
        $ 5000
        $ 6000
        $ 
        $ 7000
        $ 8000
        $ 9000
        $ 
        $ 10000
Parse ← (
  ⊜□¬⌕"\n\n".    # partition elves
  ∵(□⋕⊜□≠@\n.°□) # split and parse items
)
Parse Test
## {[1000 2000 3000] [4000] [5000 6000] [7000 8000 9000] [10000]}
MaxCal ← /↥∵(/+°□)
⍤. =24000 MaxCal

Inp ← Parse &fras $"_/.config/aocd/2022/1/input.txt" &var "HOME"
&p MaxCal Inp

TopThree ← (
  ∵(/+°□) # total calories per elf
  ⊏↙3⍖.   # get top three
  /+      # sum
)
⍤. =45000 TopThree Parse Test

&p TopThree Inp
