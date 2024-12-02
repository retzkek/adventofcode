# Experimental!
# https://www.uiua.org/pad?src=0_14_0-dev_5__VGVzdCAg4oaQICQgQSBZCiAgICAgICAgJCBCIFgKICAgICAgICAkIEMgWgpQYXJzZSDihpAg4oqcKOKKnOKImOKJoEAgLiniiaBAXG4uClBhcnNlIFRlc3QKIyMKU2NvcmVzICAgICDihpAgKyBbWzEgMiAzXV0g4oavIFszIDNdIFszIDYgMCAwIDMgNiA2IDAgM10KTGVmdCAgICAgICDihpAgIkFCQyIKUmlnaHQgICAgICDihpAgIlhZWiIKU2NvcmUgICAgICDihpAg4oqhIDogU2NvcmVzIOKKn-KKgyjiipc6TGVmdCDiiqLiiqIpKOKKlzpSaWdodCDiiqLiiqMpClRvdGFsU2NvcmUg4oaQIC8r4omhU2NvcmUK4o2kLiA9MTUgVG90YWxTY29yZQo%3D
Test  ← $ A Y
        $ B X
        $ C Z
Parse ← ⊜(⊜∘≠@ .)≠@\n.
Parse Test
## ╭─      
## ╷ "A"   
## ╷ "Y"   
##         
##   "B"   
##   "X"   
##         
##   "C"   
##   "Z"   
##       ╯ 
Scores     ← + [[1 2 3]] ↯ [3 3] [3 6 0 0 3 6 6 0 3]
Left       ← "ABC"
Right      ← "XYZ"
Score      ← ⊡ : Scores ⊟⊃(⊗:Left ⊢⊢)(⊗:Right ⊢⊣)
TotalScore ← /+≡Score
⍤. =15 TotalScore

Inp ← Parse &fras $"_/.config/aocd/2022/2/input.txt" &var "HOME"
&p TotalScore Inp
