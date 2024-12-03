# AoC 2024.03
# https://uiua.org/pad?src=0_14_0-dev_5__IyBBb0MgMjAyNC4wMwpUZXN0T25lICDihpAgJCB4bXVsKDIsNCklJm11bFszLDddIUBeZG9fbm90X211bCg1LDUpK211bCgzMiw2NF10aGVuKG11bCgxMSw4KW11bCg4LDUpKQpQYXJzZU9uZSDihpAgcmVnZXggJCBtdWxcKChcZCspLChcZCspXCkKUGFyc2VPbmUgVGVzdE9uZQojIwpPbmUg4oaQIC8rL8OX4o2JIOKJoSjiiaHii5Ug4oaYMSkK4o2k4qSZPSAxNjEgT25lCgojIFBhcnQgMgpUZXN0VHdvICDihpAgJCB4bXVsKDIsNCkmbXVsWzMsN10hXmRvbid0KClfbXVsKDUsNSkrbXVsKDMyLDY0XShtdWwoMTEsOCl1bmRvKCk_bXVsKDgsNSkpClBhcnNlVHdvIOKGkCDirJoiInJlZ2V4ICQgbXVsXCgoXGQrKSwoXGQrKVwpfGRvXChcKXxkb24ndFwoXCkKUGFyc2VUd28gVGVzdFR3bwojIwpUd28g4oaQICgKICDiiaEoKyviioPiioMoLyvijJUibXVsIikow5cyIC8r4oyVImRvbiIpKMOXMyAvK-KMlSJkbygiKcKw4pah4oqiKS4gIyBjbGFzc2lmeQogICMjCiAg4qyaMVwo4qisKOKImHziiJh8wq8xfDEpIDopICMgcHJvcG9nYXRlIGRvKCkgYW5kIGRvbid0KCkKICAjIwogIOKWvT4wICMgZHJvcCBkaXNhYmxlZCBtdWwoKQogICMjCiAg4pa9OuKfnOKJoSgvK-KMlSJtdWwiwrDilqHiiqIpICMgZHJvcCBkbygpIGFuZCBkb24ndCgpCiAgIyMKICBPbmUKKQrijaTipJk9IDQ4IFR3bwojIGV4dHJhIHRlc3QgY2FzZSBzaW5jZSBmaXJzdCBzb2x1dGlvbiB3YXMgdGhyb3duIG9mZiBieSBkbygpIGNvbWluZyBiZWZvcmUgZG9uJ3QoKQpUZXN0VGhyZWUg4oaQICQgeG11bCgyLDQpZG8oKSZtdWxbMyw3XSFeZG9uJ3QoKV9tdWwoNSw1KWRvbid0KCkrbXVsKDMyLDY0XShtdWwoMTEsOCl1bmRvKCk_bXVsKDgsNSkpCuKNpOKkmT0gNDggVHdvIFBhcnNlVHdvIFRlc3RUaHJlZQo=
# Part 1
TestOne  ← $ xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
ParseOne ← regex $ mul\((\d+),(\d+)\)
ParseOne TestOne
## ╭─
## ╷ ⌜mul(2,4)⌟  ⌜2⌟  ⌜4⌟
##   ⌜mul(5,5)⌟  ⌜5⌟  ⌜5⌟
##   ⌜mul(11,8)⌟ ⌜11⌟ ⌜8⌟
##   ⌜mul(8,5)⌟  ⌜8⌟  ⌜5⌟
##                        ╯
One ← /+/×⍉ ≡(≡⋕ ↘1)
⍤⤙= 161 One

# Part 2
TestTwo  ← $ xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
ParseTwo ← ⬚""regex $ mul\((\d+),(\d+)\)|do\(\)|don't\(\)
ParseTwo TestTwo
## ╭─
## ╷ ⌜mul(2,4)⌟  ⌜2⌟  ⌜4⌟
##   ⌜don't()⌟   ⌜⌟   ⌜⌟
##   ⌜mul(5,5)⌟  ⌜5⌟  ⌜5⌟
##   ⌜mul(11,8)⌟ ⌜11⌟ ⌜8⌟
##   ⌜do()⌟      ⌜⌟   ⌜⌟
##   ⌜mul(8,5)⌟  ⌜8⌟  ⌜5⌟
##                        ╯
Two ← (
  ≡(++⊃⊃(/+⌕"mul")(×2 /+⌕"don")(×3 /+⌕"do(")°□⊢). # classify
  ↘1 ⬚1\(⨬(∘|∘|¯1|1) :)                           # propogate do() and don't()
  # ≡(&p $"_ _"),,
  ▽>0               # drop disabled mul()                                           ╯
  ▽:⟜≡(/+⌕"mul"°□⊢) # drop do() and don't()                     ╯                      ╯
  One
)
⍤⤙= 48 Two
# extra test case since first solution was thrown off by do() coming before don't()
TestThree ← $ xmul(2,4)do()&mul[3,7]!^don't()_mul(5,5)+don't()mul(32,64](mul(11,8)undo()?mul(8,5))
⍤⤙= 48 Two ParseTwo TestThree

InpOne ← ParseOne&fras $"_/.config/aocd/2024/3/input.txt" &var "HOME"
&p One InpOne

InpTwo ← ParseTwo&fras $"_/.config/aocd/2024/3/input.txt" &var "HOME"
&p Two InpTwo
