# AoC 2024.18 Visualization
# Video: https://img.kmr.me/adventofcode/2024_day18_2.webm
# Pad: https://uiua.org/pad?src=0_14_0-rc_3__IyBBb0MgMjAyNC4xOCBBbmltYXRpb24KSW5wIOKGkCAiNSw0XG40LDJcbjQsNVxuMywwXG4yLDFcbjYsM1xuMiw0XG4xLDVcbjAsNlxuMywzXG4yLDZcbjUsMVxuMSwyXG41LDVcbjIsNVxuNiw1XG4xLDRcbjAsNFxuNiw0XG4xLDFcbjYsMVxuMSwwXG4wLDVcbjEsNlxuMiwwIgojIFBhcnNlIGlucHV0LCBzcGxpdHRpbmcgb24gbmV3bGluZXMgYW5kIHRoZW4gY29tbWFzClAg4oaQIOKKnCjiipzii5XiirjiiaBALCniirjiiaBAXG4KCiMgRmluZCBmaXJzdCBwYXRoIGZyb20gMF8wIHRvIEVuZCwgZ29pbmcgYXJvdW5kIEJsb2NrcwojIFBhdGggPyBCbG9ja3MgRW5kClDigoEg4oaQICgKICDCsOKKmiAgICAgICAgIyBjb252ZXJ0IGJsb2NrZWQgbG9jYXRpb25zIHRvIG1hc2sKICDiipko4oavK-KCgTowLikgIyBjcmVhdGUgZnVsbC1zaXplIHplcm8gbWFzawogIOKsmjArICAgICAgICMgc3VtIHRoZSBtYXNrcyB0byBtYWtlIGZ1bGwgc2l6ZQogIOKKmsKsICAgICAgICAjIG5lZ2F0ZSBhbmQgY29udmVydCB0byBsaXN0IG9mIG9wZW4gbG9jYXRpb25zCiAgIyBlYXN5IG1vZGUgcGF0aCBmaW5kZXIsIGdpdmVuIG5laWdoYm9ycywgd2VpZ2h0cywgYW5kIGVuZCBjb25kaXRpb24KICDiiqIgcGF0aCgKICAgICtB4oKCwqQgIyBjcmVhdGUgbGlzdCBvZiBhbGwgbmVpZ2hib3JzCiAgICDilr3iirjiiIo6ICMgZmlsdGVyIHRvIG9ubHkgb3BlbiBsb2NhdGlvbnMKICB8IOKJjeKKmeKLheKImCAjIGVuZCBhdCBFbmQKICApICRTdGFydDBfMAopCgojIENyZWF0ZSBpbWFnZSBvZiBsb2NhdGlvbnMgYXQgZ2l2ZW4gSW50ZW5zaXRpZXMKIyBMb2NhdGlvbnMgPyBJbWFnZSBJbnRlbnNpdHkKRHJhdyDihpAg4pa94p-c4omh4pa9OCDijYnirJow4oyd4oqhCgpTY2FsZSDihpAgKyAwLjIzNDM3NSDigb_igoIgw5cgMC44NzUKCiMgQ3JlYXRlIEdJRiBvZiBwYXRoIGNoYW5naW5nIGFzIGJsb2NrcyBhcmUgYWRkZWQgb25lIGF0IGEgdGltZQojIEdpZiA_IEJsb2NrcyBFbmQKQW5pbWF0ZSDihpAgKAogIOKKgyjih6Hip7sgICMgZ2VuZXJhdGUgbGlzdCBvZiBob3cgbWFueSBibG9ja3MgdG8gdGFrZSBwZXIgZnJhbWUKICB8IMKk4oqZwqQgIyBmaXggQmxvY2tzIGFuZCBFbmQgZm9yIGl0ZXJhdGlvbiBvdmVyIGZyYW1lcwogICkKICDirJow4omhKAogICAg4oaZICMgdGFrZSBibG9ja3MgZm9yIGZyYW1lCiAgICDin5zijaMoCiAgICAgIFDigoEgICAgICMgZmluZCBwYXRoLCBrZWVwaW5nIGNvcHkgb2YgYmxvY2tzIGZvciBHIGNoYW5uZWwKICAgIHwg4ouFWzBfMF0gIyBvciBpZiBubyBwYXRoIChlcnJvcikgc2hvdyBzdGFydGluZyBwb2ludAogICAgKQogICAg4oqDKERyYXfin5woU2NhbGXDty3igoHin5zih6Hip7spICMgZHJhdyBibG9ja3Mgd2l0aCBkZWNheWluZyBpbnRlbnNpdHkKICAgIHwgRHJhdzriiaEvw5c9Li7il4wgICAgICAgIyBkcmF3IHBhdGggd2l0aCAxIGludGVuc2l0eQogICAgKQogICAg4o2J4qyaMOKKn-KCgywgIyBwdXQgcGF0aCBpbiBSQiBjaGFubmVscyBhbmQgY29tYmluZSBpbnRvIGltYWdlCiAgKQopCgojIERyYXcgJGJsb2NrcyBQIElucCAkY29sb3LDtzI14oehMjUgJGVuZDZfNgpBbmltYXRlICRibG9ja3MgUCBJbnAgJGVuZDZfNgo=
Inp ← "5,4\n4,2\n4,5\n3,0\n2,1\n6,3\n2,4\n1,5\n0,6\n3,3\n2,6\n5,1\n1,2\n5,5\n2,5\n6,5\n1,4\n0,4\n6,4\n1,1\n6,1\n1,0\n0,5\n1,6\n2,0"

# Parse input, splitting on newlines and then commas
P ← ⊜(⊜⋕⊸≠@,)⊸≠@\n

# Find first path from 0_0 to End, going around Blocks
# Path ? Blocks End
P₁ ← (
  °⊚        # convert blocked locations to mask
  ⊙(↯+₁:0.) # create full-size zero mask
  ⬚0+       # sum the masks to make full size
  ⊚¬        # negate and convert to list of open locations
  # easy mode path finder, given neighbors, weights, and end condition
  ⊢ path(
    +A₂¤ # create list of all neighbors
    ▽⊸∊: # filter to only open locations
  | ≍⊙⋅∘ # end at End
  ) $Start0_0
)

# Create image of locations, at given Intensities
# Locations ? Image Intensity
Draw ← ▽⟜≡▽2 ⍉⬚0⌝⊡

Scale ← + 0.234375 ⁿ₂ × 0.875

# Create GIF of path changing as blocks are added one at a time
# Gif ? Blocks End
Animate ← (
  ⊃(⇡⧻  # generate list of how many blocks to take per frame
  | ¤⊙¤ # fix Blocks and End for iteration over frames
  )
  ⬚0wait≡pool(
    ↙ # take blocks for frame
    ⟜⍣(
      P₁     # find path, keeping copy of blocks for G channel
    | ⋅[0_0] # or if no path (error) show starting point
    )
    ⊃(Draw⟜(Scale÷-₁⟜⇡⧻) # draw blocks with decaying intensity
    | Draw:≡/×=..◌       # draw path with 1 intensity
    )
    ⍉⬚0⊟₃, # put path in RB channels and combine into image
  )
)

Animate P Inp 6_6
&fwa "day18_test.gif" gif 10

AOC ~ "../aoc.ua"
Inp₁ ← AOC~Input 2024 18
&fwa "day18.gif" gif 50 Animate P Inp₁ 70_70
# gif was upscaled for publishing with ffmpeg:
# ffmpeg -i day18.gif-vf "scale=720:-1" day18.webm
