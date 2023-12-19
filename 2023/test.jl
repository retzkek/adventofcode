using FFMPEG, Colors
using Colors: N0f8
process = @ffmpeg_env open(`$(FFMPEG.ffmpeg) -framerate 30 -f rawvideo -pixel_format rgb24 -r 30 -s:v 500x500 -i pipe:0 -vf vflip -y test.mkv`, "w")
write(process, rand(RGB{N0f8}, 500, 500))
close(process)
wait(process)
