/*
 * ErlangGenerator.h
 *
 *  Created on: May 2, 2011
 *      Author: jason
 */

#ifndef ERLANGGENERATOR_H_
#define ERLANGGENERATOR_H_
#include <iostream>
#include <sstream>

#include <google/protobuf/compiler/code_generator.h>
#include <google/protobuf/descriptor.h>
#include <google/protobuf/io/zero_copy_stream.h>
#include <google/protobuf/io/printer.h>
#include <google/protobuf/wire_format.h>

namespace google {
namespace protobuf {
namespace compiler {
namespace erlang {
using google::protobuf::FileDescriptor;
using google::protobuf::FieldDescriptor;
using google::protobuf::Descriptor;
using google::protobuf::io::ZeroCopyOutputStream;
using google::protobuf::io::Printer;


class ErlangGenerator : public CodeGenerator {
public:
	ErlangGenerator();
	virtual ~ErlangGenerator();

	virtual bool Generate(const FileDescriptor * file, const string & parameter, GeneratorContext* context, string * error) const;
private:
	static const char * const kTypeToName[19];
	mutable bool is_strict_naming;

	// record header generation
	const std::string to_erlang_typespec(const FieldDescriptor* fd) const;
	const string default_value_for_field(const FieldDescriptor* field) const;
	void enum_to_typespec(Printer& out, const EnumDescriptor* enum_type) const;
	void message_to_record(Printer& out,const Descriptor* msg) const;
	void generate_header(Printer& out, const FileDescriptor* file) const;

	// source generation
	void export_for_enum(Printer& out, const EnumDescriptor* d) const;
	void export_for_message(Printer& out, const Descriptor* d) const;
	void field_to_decode_function(Printer &out, const FieldDescriptor* field) const;
	void encode_decode_for_enum(Printer& out, const EnumDescriptor* d) const;
	void encode_decode_for_message(Printer& out, const Descriptor* d) const;
	void generate_source(Printer& out, const FileDescriptor* file) const;

	// test generation
	void message_export(Printer& out, const Descriptor* d) const;
	void enum_domain(Printer& out, const EnumDescriptor* d) const;
	void message_domain(Printer& out,const Descriptor* d) const;
	void message_roundtrip(Printer& out, const Descriptor* d) const;
	void generate_test(Printer& out, const FileDescriptor* file) const;

	inline const string to_atom(const string& orig) const
	{
	  std::string s;

	  // convert from CamelCase to lower_case if not strict
	  if(is_strict_naming)
	  {
	    s=orig;
	  } else {
	    std::stringstream ss;

	    string::const_iterator i=orig.begin();
	    if(isupper(*i))
	      ss << (char) tolower(*i);
	    else
	      ss << *i;
	    bool skip_next_underscore=true;
	    // replace any capital with _ + tolower() after the first
	    for(++i; i != orig.end(); ++i)
	    {
	      if(isupper(*i))
	      {
	        if(!skip_next_underscore)
	          ss <<  "_";
	        ss << (char) tolower(*i);
	        skip_next_underscore=true;
	      }
	      else
	      {
	        ss << *i;
	        skip_next_underscore=(*i=='_');
	      }

	    }
	    s=ss.str();
	  }
	  string::const_iterator i = s.begin();
	  // first character must be a lower or '_' if strict, any alpha or '_' otherwise
	  bool quote_free = (is_strict_naming?islower(*i):isalpha(*i)) || *i=='_';

	  // look for anything that would force a quote
	  for(++i;i != s.end() && quote_free; ++i)
	    quote_free = quote_free || isalnum(*i) || *i=='_';

	  if(quote_free)
	    return s;
	  else
	    return string("'") + s + "'";
	}

	inline const string int_to_string(uint32 s) const
	{
	  std::stringstream out;
	  out << s;
	  return out.str();
	}

	inline const string file_basename(const string& filename)  const {
	  return filename.substr(0,filename.find(".proto")).substr(filename.find_last_of('/')+1);
	}

	inline const string module_name(const FileDescriptor* file)  const {
	  return to_atom(file_basename(file->name())+"_pb");
	}

	inline const std::string normalized_scope(const string& full_name, const string& package="") const
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

	inline const std::string normalized_scope(const Descriptor* d) const
	{
	  if(is_strict_naming)
	    return normalized_scope(d->full_name(),d->file()->package());
	  else
      return normalized_scope(d->full_name(),d->file()->package());
	}


	inline const string record_name(const Descriptor* message, const string& prefix="", const string& postfix="")  const {
	  return to_atom(prefix
	      + normalized_scope(message->full_name(),message->file()->package())
	      +postfix);
	}

	inline const string enum_name(const EnumDescriptor* message, const string& prefix="", const string& postfix="")  const {
	  return to_atom(prefix
	      + normalized_scope(message->full_name(),message->file()->package())
	      +postfix);
	}

	inline const string field_name(const FieldDescriptor* field)  const {
	  return to_atom(field->name());
	}

	inline const string field_tag(const FieldDescriptor* field)  const {
	  return int_to_string(internal::WireFormat::MakeTag(field));
	}

	inline const string field_getter(const FieldDescriptor* field)  const {
	  return record_name(field->containing_type()) + "." + to_atom(field->name());
	}

	inline const string enum_value(const EnumValueDescriptor* value)  const {
	  return to_atom(value->name());
	}

	inline const string enum_domain_name(const EnumDescriptor* d) const
	{
	  return enum_name(d,"dom_");
	}
	inline const string record_domain_name(const Descriptor* d) const
	{
	  return record_name(d,"dom_");
	}

	inline const std::string decode_name(const Descriptor* d) const
	{
	  return to_atom(string("decode_") + normalized_scope(d));
	}

	inline const std::string encode_name(const Descriptor* d) const
	{
	  return to_atom(string("encode_") + normalized_scope(d));
	}

	inline const std::string to_enum_name(const EnumDescriptor* d) const
	{
	  return to_atom(string("to_") + normalized_scope(d->full_name(),d->file()->package()));
	}

	inline const std::string from_enum_name(const EnumDescriptor* d) const
	{
	  return to_atom(string("from_") + normalized_scope(d->full_name(),d->file()->package()));
	}

	inline const std::string message_test_name(const Descriptor* d) const
	{
	  return record_name(d,"dom_","_test_");
	}

};

}}}} // namespace google::protobuf::compiler::erlang
#endif /* ERLANGGENERATOR_H_ */
