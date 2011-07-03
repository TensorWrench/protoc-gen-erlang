
#include <google/protobuf/descriptor.h>
#include <google/protobuf/io/zero_copy_stream.h>
#include <google/protobuf/io/printer.h>

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

//-module(addressbook_pb_tests).

//-include_lib("libprotobuf/include/protocol_buffers_triq.hrl").
//-include_lib("eunit/include/eunit.hrl").
//-include("addressbook_pb.hrl").
//
//-export([dom_Person__PhoneType/0,dom_Person__PhoneNumber/0]).
//
//dom_Person__PhoneType() ->
//  oneof(['MOBILE','HOME','WORK']).
//
//dom_Person__PhoneNumber() ->
//  #'Person__PhoneNumber'{
//                        number=string(),
//                        type=dom_Person__PhoneType()
//                        }.
//
//roundtrip_Person__PhoneNumber_test_() ->
//    {timeout, 60,
//     ?_assert(
//      triq:check(
//        ?FORALL(R,dom_Person__PhoneNumber(),
//           begin
//             Bin = iolist_to_binary(addressbook_pb:encode_Person__PhoneNumber(R)),
//             %?debugFmt(" vals are R= ~p and decode(Bin)=~p~n",[R,address])
//             R=:=addressbook_pb:decode_Person__PhoneNumber(Bin)
//           end
//        ) % forall
//      ) % check
//    )}. % assert


void message_export(Printer& out, const Descriptor* d)
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

void enum_domain(Printer& out, const EnumDescriptor* d)
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

void message_domain(Printer& out,const Descriptor* d)
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

void message_roundtrip(Printer& out, const Descriptor* d)
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
  //roundtrip_Person__PhoneNumber_test_() ->
}

void generate_test(Printer& out, const FileDescriptor* file)
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
