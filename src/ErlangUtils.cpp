#include <sstream>
#include "ErlangUtils.h"

namespace google {
namespace protobuf {
namespace compiler {
namespace erlang {
const char * const kTypeToName[19] = {
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

std::string to_erlang_typespec(const FieldDescriptor* fd)
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
}}}} // namespace google::protobuf::compiler::erlang
