
#include <google/protobuf/descriptor.h>
#include <google/protobuf/io/zero_copy_stream.h>
#include <google/protobuf/io/printer.h>

#include "ErlangUtils.h"
/**
 * notable behaviors:
 * repeated string or bytes that are empty-- [<<>>,<<>>]  encode as a list of zero byte strings/bins
 * packable types encode as packed regardless of the attribute
 * TODO: check that a basic field defined multiple times correctly creates an array, since roundtrip tests only exercise the packed array branch
 */


namespace google {
namespace protobuf {
namespace compiler {
namespace erlang {

using google::protobuf::FileDescriptor;
using google::protobuf::FieldDescriptor;
using google::protobuf::Descriptor;
using google::protobuf::io::ZeroCopyOutputStream;
using google::protobuf::io::Printer;
namespace {


void export_for_enum(Printer& out, const EnumDescriptor* d)
{
  out.Print("  $to$/1,$from$/1","to",to_enum_name(d),"from",from_enum_name(d));
}

void export_for_message(Printer& out, const Descriptor* d)
{
  for(int i=0; i< d->nested_type_count();++i)
  {
    export_for_message(out,d->nested_type(i));
    out.PrintRaw(",\n");
  }
  for(int i=0; i < d->enum_type_count();++i)
  {
    export_for_enum(out,d->enum_type(i));
    out.PrintRaw(",\n");
  }
  out.Print("  $encode$/1,$decode$/1","encode",encode_name(d),"decode",decode_name(d));
}

/* Produces:
 * (3,Val,Rec) -> Rec#'Person'{email=protocol_buffers:cast(string,Val);
 * (4,{length_encoded,Bin},Rec) -> Rec#'Person'{phone = [Rec#'Person'.phone | decode_Person__PhoneNumber(Bin)]}
 */
void field_to_decode_function(Printer &out, const FieldDescriptor* field)
{
  std::stringstream ss;
  ss << field->number();

  std::map<string,string> vars;
  vars["id"]=ss.str();
  vars["rec"]=to_atom(normalized_scope(field->containing_type()));
  vars["field"] = to_atom(field->name());
  vars["type"]=string(kTypeToName[field->type()]);

  // handle packed repeated fields

  switch (field->type()) {
    case FieldDescriptor::TYPE_STRING:
    case FieldDescriptor::TYPE_BYTES:
      if(field->is_repeated())
        out.Print(vars,"($id$,Val,#$rec${$field$=F}=Rec) when is_list(F) -> Rec#$rec${$field$ = Rec#$rec$.$field$ ++ [protocol_buffers:cast($type$,Val)]}\n");
      else
        out.Print(vars,"($id$,Val,Rec) -> Rec#$rec${$field$ = protocol_buffers:cast($type$,Val)}");
      break;
    case FieldDescriptor::TYPE_MESSAGE:
      vars["decode"]=decode_name(field->message_type());
      if(field->is_repeated())
        out.Print(vars,"($id$,{length_encoded,Bin},#$rec${$field$=F}=Rec) when is_list(F) -> Rec#$rec${$field$ = Rec#$rec$.$field$ ++ [$decode$(Bin)]}\n");
      else
        out.Print(vars,"($id$,{length_encoded,Bin},Rec) -> Rec#$rec${$field$ = $decode$(Bin)}");
      break;
    case FieldDescriptor::TYPE_ENUM:
      vars["to_enum"]=to_enum_name(field->enum_type());
      if(field->is_repeated())
        out.Print(vars,"($id$,{varint,Enum},#$rec${$field$=F}=Rec) when is_list(F) -> Rec#$rec${$field$=Rec#$rec$.$field$ ++ [$to_enum$(Enum)]}\n");
      else
        out.Print(vars,"($id$,{varint,Enum},Rec) -> Rec#$rec${$field$=$to_enum$(Enum)}");
      break;
    case FieldDescriptor::TYPE_GROUP:
      // not supported
      break;

    default:
      if(field->is_repeated())
      {
        // packed repeated returns array
        out.Print(vars,"        ($id$,{length_encoded,_}=Val,#$rec${$field$=F}=Rec) when is_list(F) -> Rec#$rec${$field$ = Rec#$rec$.$field$ ++ protocol_buffers:cast($type$,Val)};\n");
        // repeated that's not packed does not return an array
        out.Print(vars,"        ($id$,Val,#$rec${$field$=F}=Rec) when is_list(F) -> Rec#$rec${$field$ = Rec#$rec$.$field$ ++ [protocol_buffers:cast($type$,Val)]}\n");
      } else {
        out.Print(vars,"($id$,Val,Rec) -> Rec#$rec${$field$ = protocol_buffers:cast($type$,Val)}");
      }
  }
}

void encode_decode_for_enum(Printer& out, const EnumDescriptor* d)
{
  // to_enum
  for(int enumI=0;enumI < d->value_count();enumI++)
  {
    std::stringstream ss;
    ss << d->value(enumI)->number();
    out.Print("$to_enum$($id$) -> $enum$;\n",
        "to_enum",to_enum_name(d),
        "id",ss.str(),
        "enum",to_atom(d->value(enumI)->name()));
  }
  out.Print("$to_enum$(undefined) -> undefined.\n\n","to_enum",to_enum_name(d));

  // from_enum
  for(int enumI=0;enumI < d->value_count();enumI++)
  {
    std::stringstream ss;
    ss << d->value(enumI)->number();
    out.Print("$from_enum$($enum$) -> $id$;\n",
        "from_enum",from_enum_name(d),
        "id",ss.str(),
        "enum",to_atom(d->value(enumI)->name()));
  }
  out.Print("$from_enum$(undefined) -> undefined.\n\n","from_enum",from_enum_name(d));

}


void encode_decode_for_message(Printer& out, const Descriptor* d)
{
  for(int i=0; i < d->enum_type_count();++i)
    encode_decode_for_enum(out,d->enum_type(i));

  for(int i=0; i< d->nested_type_count();++i)
    encode_decode_for_message(out,d->nested_type(i));

  // decode functions
  out.Print("$function$(Binary) ->\n"
            "  protocol_buffers:decode(Binary,#$msg${},\n"
            "     fun",
              "function",decode_name(d),
              "msg",to_atom(normalized_scope(d)));

  for(int i=0; i< d->field_count();++i)
  {
    field_to_decode_function(out,d->field(i));
    if(i < d->field_count()-1)
    {
      out.PrintRaw(";\n        ");
    }
  }
  out.PrintRaw("\n      end).\n\n");

  // encode functions
  out.Print("$function$(R) when is_record(R,$rec$) ->\n"
            "  [\n",
              "function",encode_name(d),
              "rec",to_atom(normalized_scope(d)));
  for(int i=0; i< d->field_count();++i)
  {
    const FieldDescriptor* field=d->field(i);
    std::stringstream ss;
    ss << field->number();

    std::map<string,string> vars;
    vars["id"]=ss.str();
    vars["rec"]=to_atom(normalized_scope(field->containing_type()));
    vars["field"] = to_atom(field->name());
    vars["type"]=string(kTypeToName[field->type()]);
    vars["decode"]=decode_name(field->containing_type());
    switch(field->type()) {
    case FieldDescriptor::TYPE_ENUM:
      vars["from_enum"]=from_enum_name(field->enum_type());
      if(field->is_repeated())
      {
        out.Print(vars,"    [protocol_buffers:encode($id$,int32,$from_enum$(X)) || X <- R#$rec$.$field$]");
      } else {
        out.Print(vars,"    protocol_buffers:encode($id$,int32,$from_enum$(R#$rec$.$field$))");
      }
      break;
    case FieldDescriptor::TYPE_MESSAGE:
      vars["encode"]=encode_name(field->message_type());
      if(field->is_repeated())
        out.Print(vars,"    [ protocol_buffers:encode($id$,length_encoded,$encode$(X)) || X <- R#$rec$.$field$]");
      else
        out.Print(vars,"    protocol_buffers:encode($id$,length_encoded,$encode$(R#$rec$.$field$))");
      break;
    case FieldDescriptor::TYPE_BYTES:
    case FieldDescriptor::TYPE_STRING:
      if(field->is_repeated())
        out.Print(vars,"    [ protocol_buffers:encode($id$,length_encoded,X) || X <- R#$rec$.$field$]");
      else
        out.Print(vars,"    protocol_buffers:encode($id$,length_encoded,R#$rec$.$field$)");
      break;

    default:
      out.Print(vars,"    protocol_buffers:encode($id$,$type$,R#$rec$.$field$)");
    }
    if(i<d->field_count()-1)
      out.PrintRaw(",\n");
  }
  out.PrintRaw("\n  ].\n\n");
}
} // anon namespace


void generate_source(Printer& out, const FileDescriptor* file)
{
  out.Print("-module($module$).\n"
            "-include(\"$module$.hrl\").\n\n"
            "-export([\n"
            ,"module",module_name(file));
  for(int i=0; i < file->enum_type_count();++i)
  {
    export_for_enum(out,file->enum_type(i));
    out.PrintRaw(",\n");
  }
  for(int i=0; i < file->message_type_count();++i) {
    export_for_message(out,file->message_type(i));
    if(i < file->message_type_count()-1)
      out.PrintRaw(",\n");
  }

  out.PrintRaw("]).\n\n");

  for(int i=0; i < file->enum_type_count();++i)
  {
    encode_decode_for_enum(out,file->enum_type(i));
  }

  for(int i=0; i < file->message_type_count();++i) {
    encode_decode_for_message(out,file->message_type(i));
  }


}
}}}} // namespace google::protobuf::compiler::erlang
