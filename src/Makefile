SOURCES = AMF.ml IO.ml FLV.ml RTMP.ml
RESULT = RTMP
PACKS = unix

all: dncl server client

run: all
	./server & $(MAKE) test

# Build example files
%: %.ml dncl
	ocamlopt unix.cmxa RTMP.cmxa $< -o $@

test:
	ffmpeg -f lavfi -i testsrc=duration=10:size=1280x720:rate=30 -f flv rtmp:///localhost/myStream.sdp
	# ffmpeg -f lavfi -i "sine=frequency=1000:duration=5" -f ogg rtmp:///localhost/myStream.sdp
	# gst-launch-1.0 -v videotestsrc ! avenc_flv ! flvmux ! rtmpsink location='rtmp://localhost/path/to/stream live=1'
	# gst-launch-1.0 -v audiotestsrc ! avenc_flv ! flvmux ! rtmpsink location='rtmp://localhost/path/to/stream live=1'
	# gst-launch-1.0 -v rtmpsrc location=rtmp://localhost/someurl ! fakesink

test.flv:
	ffmpeg -f lavfi -i testsrc=duration=60:size=1280x720:rate=30 -f flv $@

include OCamlMakefile
