/*
 * main.cpp
 *
 *  Created on: May 2, 2011
 *      Author: jason
 */

#include <google/protobuf/compiler/plugin.h>
#include "ErlangGenerator.h"

int main(int argc, char* argv[]) {
  google::protobuf::compiler::erlang::ErlangGenerator generator;
  return google::protobuf::compiler::PluginMain(argc, argv, &generator);
}
