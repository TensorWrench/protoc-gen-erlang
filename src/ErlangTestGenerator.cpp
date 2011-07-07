// Erlang pluging to Protocol Buffers
// Copyright 2011 Tensor Wrench LLC.  All rights reserved.
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

/*
 * Export for message domains.
 */
void ErlangGenerator::message_export(Printer& out, const Descriptor* d) const
{
  for(int i=0;i < d->enum_type_count();++i)
  {
    out.Print("$enum$/0,","enum",enum_domain_name(d->enum_type(i)));
  }
  for(int i=0; i< d->nested_type_count();++i)
  {
    message_export(out,d->nested_type(i));
    out.PrintRaw(",\n");
  }
  out.Print("$rec$/0","rec",record_domain_name(d));
}

/*
 * The triq domain generation for an enum
 */
void ErlangGenerator::enum_domain(Printer& out, const EnumDescriptor* d) const
{
  out.Print("$enum$() -> oneof([","enum",enum_domain_name(d));
  for(int i=0;i<d->value_count();++i)
  {
    out.PrintRaw(enum_value(d->value(i)));
    if(i < d->value_count()-1)
      out.PrintRaw(",");
  }
  out.PrintRaw("]).\n\n");
}

/*
 * Implementation of message domains.
 */
void ErlangGenerator::message_domain(Printer& out,const Descriptor* d) const
{
  for(int i=0;i < d->enum_type_count();++i)
  {
    enum_domain(out,d->enum_type(i));
  }
  out.PrintRaw("\n");
  for(int i=0;i < d->nested_type_count();++i)
  {
    message_domain(out,d->nested_type(i));
  }
  out.Print("$domain$() ->\n  #$rec${\n","domain",record_domain_name(d),"rec",record_name(d));
  for(int i=0; i< d->field_count();++i)
  {
    const FieldDescriptor* f=d->field(i);
    string domain=kTypeToName[f->type()];
    switch(f->type()) {
    case FieldDescriptor::TYPE_MESSAGE:
      domain=record_domain_name(f->message_type());
      break;
    case FieldDescriptor::TYPE_ENUM:
      domain=enum_domain_name(f->enum_type());
      break;
    default:
      break;
    }
    if(f->is_repeated())
    {
      out.Print("    $field$ = list($domain$())","field",field_name(f),"domain",domain);
    } else if(f->is_optional() && !f->has_default_value()) {
      out.Print("    $field$ = frequency([{5,undefined},{45,$domain$()}])","field",field_name(f),"domain",domain);
    } else {
      out.Print("    $field$ = $domain$()","field",field_name(f),"domain",domain);
    }
    if(i < d->field_count() -1)
      out.PrintRaw(",\n");
  }
  out.PrintRaw("}.\n\n");
}

/*
 * The actual test case that encodes a random message, then decodes it.
 */
void ErlangGenerator::message_roundtrip(Printer& out, const Descriptor* d) const
{
  for(int i=0;i < d->nested_type_count();++i)
  {
    message_roundtrip(out,d->nested_type(i));
  }
  std::map<string,string> vars;
  vars["test"]=message_test_name(d);
  vars["domain"]=record_domain_name(d);
  vars["module"]=module_name(d->file());
  vars["encode"]=encode_name(d);
  vars["decode"]=decode_name(d);
  out.Print(vars,"$test$() -> \n"
      "    {timeout, 60,\n"
      "     ?_assert(\n"
      "      triq:check(\n"
      "        ?FORALL(R,$domain$(),\n"
      "           begin\n"
      "             Bin = iolist_to_binary($module$:$encode$(R)),\n"
      "             Decoded = $module$:$decode$(Bin),"
      "             case R=:= Decoded of\n"
      "                false -> ?debugFmt(\"Failed roundtrip on:~nRaw    =~p~nDecoded=~p~n\",[R,Decoded]), false;"
      "                true -> true"
      "             end"
      "           end\n"
      "        ) % forall\n"
      "      ) % check\n"
      "    )}. % assert\n\n"
      );
}

void ErlangGenerator::generate_test(Printer& out, const FileDescriptor* file) const
{
  out.Print("-module($module$_tests).\n"
            "-include_lib(\"libprotobuf/include/protocol_buffers_triq.hrl\").\n"
            "-include_lib(\"eunit/include/eunit.hrl\").\n"
            "-include(\"$module$.hrl\").\n\n"
            "-export([","module",module_name(file));
  for(int i=0;i < file->enum_type_count();++i)
  {
    out.Print("$enum$/0","enum",enum_domain_name(file->enum_type(i)));
    out.PrintRaw(",");
  }
  out.PrintRaw("\n");
  for(int i=0;i < file->message_type_count();++i)
  {
    message_export(out,file->message_type(i));
    if(i < file->message_type_count()-1)
      out.PrintRaw(",\n");
  }
  out.PrintRaw("]).\n\n");

  // domain functions
  for(int i=0;i < file->enum_type_count();++i)
  {
    enum_domain(out,file->enum_type(i));
  }
  out.PrintRaw("\n");
  for(int i=0;i < file->message_type_count();++i)
  {
    message_domain(out,file->message_type(i));
  }
  out.PrintRaw("\n");
  // roundtrip test functions
  for(int i=0;i < file->message_type_count();++i)
  {
    message_roundtrip(out,file->message_type(i));
  }
}

}}}} // namespace google:protobuf:compiler:erlang
