# AoC 2024.03
# https://uiua.org/pad?src=0_14_0-dev_5__IyBBb0MgMjAyNC4wMwpUZXN0ICAg4oaQICQgeG11bCgyLDQpZG8oKSZtdWxbMyw3XSFeZG9uJ3QoKV9tdWwoNSw1KWRvbid0KCkrbXVsKDMyLDY0XShtdWwoMTEsOCl1bmRvKCk_bXVsKDgsNSkpClBhcnNl4oKBIOKGkCByZWdleCAkIG11bFwoKFxkKyksKFxkKylcKQpQYXJzZeKCgSBUZXN0CiMjClDigoEg4oaQIOKsmjAvKCviipkoL8OX4ouV4oaY4oKBKSkK4o2k4qSZPSAxNjEgUOKCgQoKUGFyc2XigoIg4oaQIOKsmiIicmVnZXggJCBtdWxcKChcZCspLChcZCspXCl8ZG9cKFwpfGRvbid0XChcKQpQYXJzZeKCgiBUZXN0CiMjClDigoIg4oaQICgKICDiiaEoK8OX4oKC4oipKC8r4oyVKSJkbygiLCJkb24iIMKw4pah4oqiKS4gIyBjbGFzc2lmeQogICMjCiAg4oaY4oKB4qyaMVwo4qisKOKImHzCrzF8MSk6KSAjIHByb3BvZ2F0ZSBkbygpIGFuZCBkb24ndCgpCiAgIyMKICDilr0-4oKAICMgZHJvcCBkaXNhYmxlZCBtdWwoKSBhbmQgZG9uJ3QoKSAgICAgICAgICAgICAgICAgIOKVrwogICMjCiAg4pa94omhKC8r4oyVIm11bCLCsOKWoeKKoikuICMgZHJvcCBkbygpCiAgIyMKICBQ4oKBCikK4o2k4qSZPSA0OCBQ4oKCCg==
Test   ← $ xmul(2,4)do()&mul[3,7]!^don't()_mul(5,5)don't()+mul(32,64](mul(11,8)undo()?mul(8,5))
Parse₁ ← regex $ mul\((\d+),(\d+)\)
Parse₁ Test
P₁ ← ⬚0/(+⊙(/×⋕↘₁))
⍤⤙= 161 P₁

Parse₂ ← ⬚""regex $ mul\((\d+),(\d+)\)|do\(\)|don't\(\)
Parse₂ Test
P₂ ← (
  ≡(+×₂∩(/+⌕)"do(","don" °□⊢). # classify
  ↘₁⬚1\(⨬(∘|¯1|1):)            # propogate do() and don't()
  ▽>₀                          # drop disabled mul() and don't()                  ╯
  ▽≡(/+⌕"mul"°□⊢).             # drop do()
  P₁
)
⍤⤙= 48 P₂

AOC ~ "../aoc.ua"
AOC~Input 2024 3
&p⊃(P₁ Parse₁) (P₂ Parse₂)
