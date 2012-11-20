// Erlang pluging to Protocol Buffers
// Copyright 2011 Tensor Wrench LLC.
// https://github.com/TensorWrench/protoc-gen-erlang

// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     * Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above
// copyright notice, this list of conditions and the following disclaimer
// in the documentation and/or other materials provided with the
// distribution.
//     * Neither the name of Google Inc. nor the names of its
// contributors may be used to endorse or promote products derived from
// this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#include "ErlangGenerator.h"

namespace google {
namespace protobuf {
namespace compiler {
namespace erlang {

const char * const ErlangGenerator::kTypeToName[19] = {
  "ERROR",     // 0 is reserved for errors
  "double",    // TYPE_DOUBLE
  "float",     // TYPE_FLOAT
  "int64",     // TYPE_INT64
  "uint64",    // TYPE_UINT64
  "int32",     // TYPE_INT32
  "fixed64",   // TYPE_FIXED64
  "fixed32",   // TYPE_FIXED32
  "bool",      // TYPE_BOOL
  "string",    // TYPE_STRING
  "group",     // TYPE_GROUP
  "message",   // TYPE_MESSAGE
  "bytes",     // TYPE_BYTES
  "uint32",    // TYPE_UINT32
  "enum",      // TYPE_ENUM
  "sfixed32",  // TYPE_SFIXED32
  "sfixed64",  // TYPE_SFIXED64
  "sint32",    // TYPE_SINT32
  "sint64",    // TYPE_SINT64
};

ErlangGenerator::ErlangGenerator()   : is_strict_naming(false) {
}

ErlangGenerator::~ErlangGenerator() {
}

bool ErlangGenerator::Generate(const FileDescriptor * file, const string & parameter, GeneratorContext* context, string * error) const
{
  typedef std::vector<std::pair<std::string,std::string> > config_vec;
  config_vec config;
  bool tests=false;
  ParseGeneratorParameter(parameter,&config);
  for(config_vec::iterator i=config.begin();i != config.end();++i)
  {
    if("triq_tests" == i->first)
      tests=true;
    if("stict_naming" == i->first)
      is_strict_naming=true;
  }
  std::string name=module_name(file);
  std::stringstream out;
  out << "";
  {
    scoped_ptr<ZeroCopyOutputStream> header(context->Open("include/"+name+".hrl"));
    Printer printer(header.get(),'$');
    generate_header(printer,file, context);
  }

  {
    scoped_ptr<ZeroCopyOutputStream> source(context->Open("src/"+name+".erl"));
    Printer printer(source.get(),'$');
    generate_source(printer,file,context);
  }

  if(tests)
  {
    scoped_ptr<ZeroCopyOutputStream> source(context->Open("test/"+name+"_tests.erl"));
    Printer printer(source.get(),'$');
    generate_test(printer,file);
  }

  *error = out.str();
  return true;
}


}}}} // namespace google::protobuf::compiler::erlang
