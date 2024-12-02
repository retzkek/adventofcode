# Experimental!
# https://uiua.org/pad?src=0_14_0-dev_5__SW5wICAg4oaQICQgMyAgIDQKICAgICAgICAkIDQgICAzCiAgICAgICAgJCAyICAgNQogICAgICAgICQgMSAgIDMKICAgICAgICAkIDMgICA5CiAgICAgICAgJCAzICAgMwpQYXJzZSDihpAgKAogIOKJoEBcbi4gIyBmaW5kIG5ld2xpbmVzCiAg4oqc4pahICAgICMgc3BsaXQgbGluZXMKICDiiaBAIC4gICMgZmluZCBzcGFjZXMKICDijZriipzilqEgICAjIHNwbGl0IG51bWJlcnMKICDiiLXii5XCsOKWoSAgIyBwYXJzZSBudW1iZXJzIGludG8gYXJyYXkKKQrilIzilIDilbR0ZXN0CiAgUGFyc2UgSW5wCiAgW1szIDRdCiAgIFs0IDNdCiAgIFsyIDVdCiAgIFsxIDNdCiAgIFszIDldCiAgIFszIDNdXQogIOKNpC4g4omNCuKUlOKUgOKVtApEaXN0IOKGkCAoCiAg4o2J4omh4o2G4o2JICAgIyBzb3J0IHJvd3MKICDiiaEvKOKMtS0pICMgZGlzdGFuY2UKICAvKyAgICAgIyBzdW0KKQrijaQkIkRpc3Q9Xywgd2FudCAxMSI6ID0xMS4gRGlzdCBQYXJzZSBJbnAKClNpbWlsIOKGkCAoCiAg4oqi4p-c4oqj4o2JICAgIyBzcGxpdCByb3dzIG9udG8gc3RhY2sKICDin5ziip49ICAgICMgZmluZCBzYW1lIHZhbHVlcwogIC8rLyvijYnDlyAjIG11bHRpcGx5IGJ5IGxlZnQgdGhlbiBzdW0KKQrijaQkIlNpbWlsPV8sIHdhbnQgMzEiOiA9MzEuIFNpbWlsIFBhcnNlIElucAo=
Inp   ← &fras $"_/.config/aocd/2024/1/input.txt" &var "HOME"
Test  ← $ 3   4
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
┌─╴test
  Parse Test
  [[3 4]
   [4 3]
   [2 5]
   [1 3]
   [3 9]
   [3 3]]
  ⍤. ≍
└─╴
Dist ← (
  ⍉≡⍆⍉   # sort rows
  ≡/(⌵-) # distance
  /+     # sum
)
⍤$"Dist=_, want 11": =11. Dist Parse Test
&p Dist Parse Inp

Simil ← (
  ⊢⟜⊣⍉   # split rows onto stack
  ⟜⊞=    # find same values
  /+/+⍉× # multiply by left then sum
)
⍤$"Simil=_, want 31": =31. Simil Parse Test
&p Simil Parse Inp
