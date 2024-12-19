# AoC 2024.18 Visualization
# Video: https://img.kmr.me/adventofcode/2024_day18.webm
# Pad: https://uiua.org/pad?src=0_14_0-rc_3__IyBBb0MgMjAyNC4xOCBBbmltYXRpb24KSW5wIOKGkCAiNSw0XG40LDJcbjQsNVxuMywwXG4yLDFcbjYsM1xuMiw0XG4xLDVcbjAsNlxuMywzXG4yLDZcbjUsMVxuMSwyXG41LDVcbjIsNVxuNiw1XG4xLDRcbjAsNFxuNiw0XG4xLDFcbjYsMVxuMSwwXG4wLDVcbjEsNlxuMiwwIgojIFBhcnNlIGlucHV0LCBzcGxpdHRpbmcgb24gbmV3bGluZXMgYW5kIHRoZW4gY29tbWFzClAg4oaQIOKKnCjiipzii5XiirjiiaBALCniirjiiaBAXG4KCiMgRmluZCBmaXJzdCBwYXRoIGZyb20gMF8wIHRvIEVuZCwgZ29pbmcgYXJvdW5kIEJsb2NrcwojIFBhdGggPyBCbG9ja3MgRW5kClDigoEg4oaQICgKICDCsOKKmiAgICAgICAgIyBjb252ZXJ0IGJsb2NrZWQgbG9jYXRpb25zIHRvIG1hc2sKICDiipko4oavK-KCgTowLikgIyBjcmVhdGUgZnVsbC1zaXplIHplcm8gbWFzawogIOKsmjArICAgICAgICMgc3VtIHRoZSBtYXNrcyB0byBtYWtlIGZ1bGwgc2l6ZQogIOKKmsKsICAgICAgICAjIG5lZ2F0ZSBhbmQgY29udmVydCB0byBsaXN0IG9mIG9wZW4gbG9jYXRpb25zCiAgIyBlYXN5IG1vZGUgcGF0aCBmaW5kZXIsIGdpdmVuIG5laWdoYm9ycywgd2VpZ2h0cywgYW5kIGVuZCBjb25kaXRpb24KICDiiqIgcGF0aCgKICAgICtB4oKCwqQgIyBjcmVhdGUgbGlzdCBvZiBhbGwgbmVpZ2hib3JzCiAgICDilr3iirjiiIo6ICMgZmlsdGVyIHRvIG9ubHkgb3BlbiBsb2NhdGlvbnMKICB8IOKJjeKKmeKLheKImCAjIGVuZCBhdCBFbmQKICApICRTdGFydDBfMAopCgojIENyZWF0ZSBpbWFnZSBvZiBsb2NhdGlvbnMsIHNjYWxlZCA4eAojIExvY2F0aW9ucyA_IEltYWdlCkRyYXcg4oaQIOKWveKfnOKJoeKWvTgg4o2JwrDiipoKCiMgQ3JlYXRlIEdJRiBvZiBwYXRoIGNoYW5naW5nIGFzIGJsb2NrcyBhcmUgYWRkZWQgb25lIGF0IGEgdGltZQojIEdpZiA_IEJsb2NrcyBFbmQKQW5pbWF0ZSDihpAgKAogIOKKgyjih6Hip7sgICMgZ2VuZXJhdGUgbGlzdCBvZiBob3cgbWFueSBibG9ja3MgdG8gdGFrZSBwZXIgZnJhbWUKICB8IMKk4oqZwqQgIyBmaXggQmxvY2tzIGFuZCBFbmQgZm9yIGl0ZXJhdGlvbiBvdmVyIGZyYW1lcwogICkKICDirJow4omhKAogICAg4oaZICMgdGFrZSBibG9ja3MgZm9yIGZyYW1lCiAgICDin5zijaMoCiAgICAgIFDigoEgICAgICMgZmluZCBwYXRoLCBrZWVwaW5nIGNvcHkgb2YgYmxvY2tzIGZvciBHIGNoYW5uZWwKICAgIHwg4ouFWzBfMF0gIyBvciBpZiBubyBwYXRoIChlcnJvcikgc2hvdyBzdGFydGluZyBwb2ludAogICAgKQogICAg4oip4oKDRHJhdywgIyBkdXBsaWNhdGUgcGF0aCBmb3IgUkIgY2hhbm5lbHMgYW5kIGRyYXcKICAgIOKNieKsmjDiip_igoMgICAjIGNvbWJpbmUgaW50byBpbWFnZQogICkKKQoKQW5pbWF0ZSBQIElucCA2XzYK
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

# Create image of locations, scaled 8x
# Locations ? Image
Draw ← ▽⟜≡▽8 ⍉°⊚

# Create GIF of path changing as blocks are added one at a time
# Gif ? Blocks End
Animate ← (
  ⊃(⇡⧻  # generate list of how many blocks to take per frame
  | ¤⊙¤ # fix Blocks and End for iteration over frames
  )
  ⬚0≡(
    ↙ # take blocks for frame
    ⟜⍣(
      P₁     # find path, keeping copy of blocks for G channel
    | ⋅[0_0] # or if no path (error) show starting point
    )
    ∩₃Draw, # duplicate path for RB channels and draw
    ⍉⬚0⊟₃   # combine into image
  )
)

Animate P Inp 6_6
&fwa "day18_test.gif" gif 10

AOC ~ "../aoc.ua"
Inp₁ ← AOC~Input 2024 18
Animate P Inp₁ 70_70
&fwa "day18.gif" gif 10
# gif was upscaled and sped up for publishing with ffmpeg:
# ffmpeg -i day18.gif -r 50 -vf "setpts=0.2*PTS, scale=720:-1" day18_50fps.webm
