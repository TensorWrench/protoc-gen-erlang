/*
 * ErlangUtils.h
 *
 *  Created on: Jun 6, 2011
 *      Author: jason
 */

#ifndef ERLANGUTILS_H_
#define ERLANGUTILS_H_
#include <iostream>
#include <sstream>
#include <google/protobuf/descriptor.h>
#include <google/protobuf/wire_format.h>

#define TRACE std::cerr << __FILE__ << "@" << __LINE__ << " in " << __FUNCTION__ << std::endl

namespace google {
namespace protobuf {
namespace compiler {
namespace erlang {
extern const char * const kTypeToName[19];
std::string to_erlang_typespec(const FieldDescriptor* fd);}

inline const string to_atom(const string& s) {
  string::const_iterator i = s.begin();
  // first character can't be a number or capital
  bool quote_free = islower(*i) || *i=='_';
  for(++i;i != s.end() && quote_free; ++i)
    quote_free = isalnum(*i) || *i=='_';
  if(quote_free)
    return s;
  else
    return string("'") + s + "'";
}



inline const string int_to_string(uint32 s)
{
  std::stringstream out;
  out << s;
  return out.str();
}


inline const string file_basename(const string& filename) {
  return filename.substr(0,filename.find(".proto")).substr(filename.find_last_of('/')+1);
}

inline const string module_name(const FileDescriptor* file) {
  return to_atom(file_basename(file->name())+"_pb");
}

inline const std::string normalized_scope(const string& full_name, const string& package="")
{
  std::stringstream out;
  string name;
  if(!package.empty())
    name=full_name.substr(package.size()+1);
  else
    name=full_name;
  for(string::const_iterator sI=name.begin();sI != name.end(); ++sI)
  {
    if(*sI == '.')
      out << "__";
    else
      out << *sI;
  }
  return out.str();
}

inline const std::string normalized_scope(const Descriptor* d)
{
  return normalized_scope(d->full_name(),d->file()->package());
}


inline const string record_name(const Descriptor* message, const string& prefix="", const string& postfix="") {
  return to_atom(prefix
      + normalized_scope(message->full_name(),message->file()->package())
      +postfix);
}

inline const string enum_name(const EnumDescriptor* message, const string& prefix="", const string& postfix="") {
  return to_atom(prefix
      + normalized_scope(message->full_name(),message->file()->package())
      +postfix);
}


inline const string field_name(const FieldDescriptor* field) {
  return to_atom(field->name());
}

inline const string field_tag(const FieldDescriptor* field) {
  return int_to_string(internal::WireFormat::MakeTag(field));
}

inline const string field_getter(const FieldDescriptor* field) {
  return record_name(field->containing_type()) + "." + to_atom(field->name());
}

inline const string enum_value(const EnumValueDescriptor* value) {
  return to_atom(value->name());
}

inline const string enum_domain_name(const EnumDescriptor* d)
{
  return enum_name(d,"dom_");
}
inline const string record_domain_name(const Descriptor* d)
{
  return record_name(d,"dom_");
}

inline const std::string decode_name(const Descriptor* d)
{
  return to_atom(string("decode_") + normalized_scope(d));
}

inline const std::string encode_name(const Descriptor* d)
{
  return to_atom(string("encode_") + normalized_scope(d));
}

inline const std::string to_enum_name(const EnumDescriptor* d)
{
  return string("to_") + normalized_scope(d->full_name(),d->file()->package());
}

inline const std::string from_enum_name(const EnumDescriptor* d)
{
  return string("from_") + normalized_scope(d->full_name(),d->file()->package());
}

inline const std::string message_test_name(const Descriptor* d)
{
  return record_name(d,"dom_","_test_");
}

}}} // namespace google::protobuf::compiler

#endif /* ERLANGUTILS_H_ */
