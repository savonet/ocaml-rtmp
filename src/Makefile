SOURCES = AMF.ml IO.ml RTMP.ml
RESULT = RTMP

all: dncl

test:
	# ffmpeg -f lavfi -i testsrc=duration=10:size=1280x720:rate=30 -f flv rtmp:///localhost/myStream.sdp
	gst-launch-1.0 -v videotestsrc ! avenc_flv ! flvmux ! rtmpsink location='rtmp://localhost/path/to/stream live=1'
	# gst-launch-1.0 -v rtmpsrc location=rtmp://localhost/someurl ! fakesink

include OCamlMakefile
