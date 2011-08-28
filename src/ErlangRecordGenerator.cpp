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

/*
 * The eeps-8 type for this field.
 */
const std::string ErlangGenerator::to_erlang_typespec(const FieldDescriptor* fd) const
{
  string type(""), end("");
  if(fd->is_optional())
    type="'undefined' | ";
  if(fd->is_repeated())
  {
    type="[";
    end="]";
  }

  switch(fd->type()) {
  case FieldDescriptor::TYPE_DOUBLE:
  case FieldDescriptor::TYPE_FLOAT:
    return type + "float()" + end;

  case FieldDescriptor::TYPE_UINT32:
  case FieldDescriptor::TYPE_UINT64:
  case FieldDescriptor::TYPE_FIXED64:
  case FieldDescriptor::TYPE_FIXED32:
    return type + "non_neg_integer()" + end;

  case FieldDescriptor::TYPE_INT64:
  case FieldDescriptor::TYPE_INT32:
  case FieldDescriptor::TYPE_SFIXED32:
  case FieldDescriptor::TYPE_SFIXED64:
  case FieldDescriptor::TYPE_SINT32:
  case FieldDescriptor::TYPE_SINT64:
    return type + "integer()" + end;

  case FieldDescriptor::TYPE_BOOL:
    return type + "boolean()" + end;

  case FieldDescriptor::TYPE_STRING:
  case FieldDescriptor::TYPE_BYTES:
    return type + "binary()" + end;

  case FieldDescriptor::TYPE_GROUP:
    return type + "any()" + end; // not supporting groups

  case FieldDescriptor::TYPE_MESSAGE:
    return type + "#" + record_name(fd->message_type()) + "{}" + end;

  case FieldDescriptor::TYPE_ENUM:
    return type + enum_name(fd->enum_type()) + "()" + end;
  }
  return "";
}

/**
 * The default value, used by the record.
 */
const string ErlangGenerator::default_value_for_field(const FieldDescriptor* field) const
{
  stringstream ss;
  if (field->is_repeated()) ss << "[";
  switch (field->cpp_type()) {
    case FieldDescriptor::CPPTYPE_DOUBLE:
      ss << field->default_value_double();
      break;
    case FieldDescriptor::CPPTYPE_FLOAT:
      ss << field->default_value_float();
      break;
    case FieldDescriptor::CPPTYPE_INT32:
      ss << field->default_value_int32();
      break;
    case FieldDescriptor::CPPTYPE_UINT32:
      ss << field->default_value_uint32();
      break;
    case FieldDescriptor::CPPTYPE_INT64:
      ss << field->default_value_int64();
      break;
    case FieldDescriptor::CPPTYPE_UINT64:
      ss << field->default_value_uint64();
      break;
    case FieldDescriptor::CPPTYPE_BOOL:
      ss << (field->default_value_bool() ? "true" : "false");
      break;
    case FieldDescriptor::CPPTYPE_STRING:
      ss << "<<\"" << field->default_value_string() << "\">>";
      break;
    case FieldDescriptor::CPPTYPE_ENUM:
      ss << enum_value(field->default_value_enum());
      break;
    case FieldDescriptor::CPPTYPE_MESSAGE:
      ss << "!DEFAULT_VALUE_FOR_MESSAGE_FIELD_IS_NOT_SUPPORTED!";
      break;
  }
  if (field->is_repeated()) ss << "]";
  return ss.str();
}

/*
 * Creates an old style edoc and new -type specifier for the record
 */
void ErlangGenerator::enum_to_typespec(Printer& out, const EnumDescriptor* enum_type) const
{
  // edoc specification
  out.Print("%% @type $enum$() = ","enum", enum_name(enum_type));
  for(int enumI=0;enumI < enum_type->value_count();enumI++)
  {
    out.Print("$s$","s",to_atom(enum_type->value(enumI)->name()));
    if(enumI < enum_type->value_count()-1)
      out.Print("$s$","s"," | ");
  }
  out.Print(".\n");
  // -type specification
  out.Print("-type $enum$() :: ","enum", enum_name(enum_type));
  for(int enumI=0;enumI < enum_type->value_count();enumI++)
  {
    out.Print("$s$","s",to_atom(enum_type->value(enumI)->name()));
    if(enumI < enum_type->value_count()-1)
      out.Print("$s$","s"," | ");
  }
  out.Print(".\n\n");
}

/*
 * Makes the record.
 */
void ErlangGenerator::message_to_record(Printer& out,const Descriptor* msg) const
{
  for (int i = 0; i < msg->enum_type_count(); i++) {
    enum_to_typespec(out,msg->enum_type(i));
  }

  for(int i=0; i< msg->nested_type_count(); ++i)
    message_to_record(out,msg->nested_type(i));

  // print the edoc information
  std::string scoped=normalized_scope(msg);

  out.Print("%% @type $name$() = #$name${\n",
      "name",to_atom(scoped + "_record"));
  for(int j=0; j < msg->field_count();++j)
  {
    const FieldDescriptor* fd=msg->field(j);
    out.Print("%%   $field$() = $type$","field",to_atom(fd->name()),"type",to_erlang_typespec(fd));
    if(j < msg->field_count()-1)
      out.Print(",\n");
    else
      out.Print("\n%% }.\n");
  }

  // print the record definition
  out.Print("-record($name$,{","name", to_atom(scoped));

  string fn;

  int count = msg->field_count();
  for (int i = 0; i < count; i++) {
    const FieldDescriptor* field = msg->field(i);
    fn = field_name(field);

    out.Print("\n");

    if (field->has_default_value()) {
      out.Print("  $field_name$ = $default$ :: $type$", "field_name", fn, "default", default_value_for_field(field),"type",to_erlang_typespec(field));
    } else if(field-> is_repeated()){
      out.Print("  $field_name$ = [] :: $type$", "field_name", fn,"type",to_erlang_typespec(field));
    } else {
      out.Print("  $field_name$ :: $type$" , "field_name",fn,"type", to_erlang_typespec(field));
    }

    if (i < count-1)
      out.PrintRaw(",");
  }

  out.Print("}).\n\n");
}

void ErlangGenerator::generate_header(Printer& out, const FileDescriptor* file) const
{
  for (int i = 0; i < file->enum_type_count(); i++) {
    enum_to_typespec(out,file->enum_type(i));
  }

  for(int i=0; i < file->message_type_count();++i) {
    message_to_record(out,file->message_type(i));
  }
}
}}}} // namespace google::protobuf::compiler::erlang
