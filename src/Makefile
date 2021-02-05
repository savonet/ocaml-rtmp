SOURCES = AMF.ml IO.ml FLV.ml RTMP.ml
RESULT = RTMP
PACKS = unix

all: dncl server client readflv

c: client test.flv
	./client

s: server
	./server

r: readflv
	./readflv

run: all
	./server & $(MAKE) test

self: all
	./server & ./client

# Build example files
%: %.ml dncl
	ocamlopt unix.cmxa RTMP.cmxa $< -o $@

test:
	# ffmpeg -f lavfi -i testsrc=duration=10:size=1280x720:rate=30 -f flv rtmp:///localhost/live2/secret-key
	ffmpeg -f lavfi -i testsrc=duration=10:size=1280x720:rate=30 -f lavfi -i sine=frequency=440:duration=10 -c:a pcm_s16le -f flv rtmp:///localhost/live2/secret-key
	# ffmpeg -f lavfi -i testsrc=duration=10:size=1280x720:rate=30 -f lavfi -i sine=frequency=440:duration=10 -c:a pcm_s16le -f flv /tmp/x.flv
	# ffmpeg -f lavfi -i "sine=frequency=1000:duration=5" -f ogg rtmp:///localhost/myStream.sdp
	# gst-launch-1.0 -v videotestsrc ! avenc_flv ! flvmux ! rtmpsink location='rtmp://localhost/path/to/stream live=1'
	# gst-launch-1.0 -v audiotestsrc ! avenc_flv ! flvmux ! rtmpsink location='rtmp://localhost/path/to/stream live=1'
	# gst-launch-1.0 -v rtmpsrc location=rtmp://localhost/someurl ! fakesink

test-server:
	ffmpeg -f flv -listen 1 -i rtmp://localhost:1935/live2/secret-key -c copy server.flv

youtube:
	ffmpeg -f lavfi -i testsrc=size=1280x720:rate=30 -f lavfi -i sine=frequency=440 -c:a pcm_s16le -f flv rtmp://a.rtmp.youtube.com/live2/$(shell cat stream-key)

test.flv:
	ffmpeg -f lavfi -i testsrc=duration=60:size=1280x720:rate=30 -f lavfi -i sine=frequency=440:duration=60 -c:a pcm_s16le -f flv $@

include OCamlMakefile
