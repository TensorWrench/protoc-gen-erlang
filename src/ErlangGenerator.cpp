/*
 * ErlangGenerator.cpp
 *
 *  Created on: May 2, 2011
 *      Author: jason
 */
#include <iostream>
#include <sstream>
#include <google/protobuf/descriptor.h>
#include <google/protobuf/io/zero_copy_stream.h>
#include <google/protobuf/io/printer.h>

#include "ErlangGenerator.h"
#include "ErlangUtils.h"

namespace google {
namespace protobuf {
namespace compiler {
namespace erlang {
using google::protobuf::FileDescriptor;
using google::protobuf::FieldDescriptor;
using google::protobuf::Descriptor;
using google::protobuf::io::ZeroCopyOutputStream;
using google::protobuf::io::Printer;

void generate_source(Printer& out, const FileDescriptor* file);
void generate_header(Printer& out, const FileDescriptor* file);
void generate_test(Printer& out, const FileDescriptor* file);

ErlangGenerator::ErlangGenerator() {
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
  }
  std::string name=module_name(file);
  std::stringstream out;
  out << "";
  {
    scoped_ptr<ZeroCopyOutputStream> header(context->Open("include/"+name+".hrl"));
    Printer printer(header.get(),'$');
    generate_header(printer,file);
  }

  {
    scoped_ptr<ZeroCopyOutputStream> source(context->Open("src/"+name+".erl"));
    Printer printer(source.get(),'$');
    generate_source(printer,file);
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
