
SRC_DIR := src
BIN_DIR := bin
SOURCES := ${wildcard ${SRC_DIR}/*.cpp} ${wildcard ${SRC_DIR}/*.cc}
HEADERS := ${wildcard ${SRC_DIR}/*.h}
EXE := ${BIN_DIR}/protoc-gen-erlang
INSTALL_DIR := /usr/local/bin

default: ${EXE}

${EXE}: ${SOURCES} ${HEADERS}
	mkdir -p ${BIN_DIR}
	g++ -Wall -g -lprotobuf -lprotoc -lpthread -o $@ ${SOURCES}
	
	
test: ${EXE}
	-rm -rf output
	mkdir output
	PATH=${BIN_DIR}:${PATH} protoc --erlang_out=triq_tests:erl_protobuf_test erl_protobuf_test/proto_src/*.proto
	cd erl_protobuf_test ; ./rebar compile ; ./rebar eunit skip_deps=true

clean:
	-rm -rf ${EXE}
	-cd erl_protobuf_test ; ./rebar clean

install: test
	cp ${EXE} ${INSTALL_DIR}

.PHONY: default run clean install
