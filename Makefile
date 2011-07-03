
SRC_DIR := src
BIN_DIR := bin
SOURCES := ${wildcard ${SRC_DIR}/*.cpp} ${wildcard ${SRC_DIR}/*.cc}
HEADERS := ${wildcard ${SRC_DIR}/*.h}
EXE := ${BIN_DIR}/protoc-gen-erlang

default: ${EXE}

${EXE}: ${SOURCES} ${HEADERS}
	mkdir -p ${BIN_DIR}
	g++ -Wall -g -lprotobuf -lprotoc -lpthread -o $@ ${SOURCES}
	
	
run: ${EXE}
	-rm -rf output
	mkdir output
	PATH=${PATH}:${BIN_DIR} protoc --erlang_out=erl_protobuf_test erl_protobuf_test/proto_src/*.proto
	cd erl_protobuf_test ; ./rebar eunit skip_deps=true

clean:
	-rm -rf ${EXE}

.PHONY: default run clean
