/*
 * ErlangGenerator.h
 *
 *  Created on: May 2, 2011
 *      Author: jason
 */

#ifndef ERLANGGENERATOR_H_
#define ERLANGGENERATOR_H_
#include <google/protobuf/compiler/code_generator.h>

namespace google {
namespace protobuf {
namespace compiler {
namespace erlang {

class ErlangGenerator : public CodeGenerator {
public:
	ErlangGenerator();
	virtual ~ErlangGenerator();

	virtual bool Generate(const FileDescriptor * file, const string & parameter, GeneratorContext* context, string * error) const;
};

}}}} // namespace google::protobuf::compiler::erlang
#endif /* ERLANGGENERATOR_H_ */
