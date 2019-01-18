//===--- PrinssharedmemoryusageCheck.cpp - clang-tidy----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "PrinssharedmemoryusageCheck.h"
#include "clang/AST/ASTContext.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "llvm/ADT/APSInt.h"

#include <numeric>

using namespace clang::ast_matchers;

namespace clang {
namespace tidy {
namespace btc {

void PrinssharedmemoryusageCheck::registerMatchers(MatchFinder *Finder) {
  Finder->addMatcher(
      callExpr(allOf(
          callee(functionDecl(anyOf(hasName("GetSatz"), hasName("GetvSatz"),
                                    hasName("GetPSatz"), hasName("PutSatz"),
                                    hasName("PutvSatz")))
                     .bind("callee")),
          hasArgument(0, expr().bind("filenr")),
          hasArgument(2, expr().bind("ptr")))),
      this);
}

void PrinssharedmemoryusageCheck::check(
    const MatchFinder::MatchResult &Result) {
  const auto *MatchedCallee = Result.Nodes.getNodeAs<FunctionDecl>("callee");
  const auto *MatchedFileNrArg = Result.Nodes.getNodeAs<Expr>("filenr");
  const auto *MatchedPtrArg = Result.Nodes.getNodeAs<Expr>("ptr");

  llvm::APSInt EvalResult;
  std::string FunctionName = MatchedCallee->getName();

  if (MatchedFileNrArg->EvaluateAsInt(EvalResult, *Result.Context)) {
    auto PsmIt = PsmMap.find(static_cast<int>(EvalResult.getExtValue()));
    if (PsmIt != PsmMap.end()) {
      if (!PsmIt->second.StructTypes.empty()) {
        auto PSMType = MatchedPtrArg->getType().getTypePtrOrNull();
        if (PSMType && (PSMType->isPointerType() || PSMType->isArrayType())) {
          clang::QualType BaseType;
          if (FunctionName == "GetPSatz") {
            BaseType = PSMType->getAs<PointerType>()
                           ->getPointeeType()
                           ->getPointeeType();
          }
          if (BaseType.isNull()) {
            if (PSMType->isPointerType()) {
              BaseType = PSMType->getAs<PointerType>()->getPointeeType();
            } else {
              if (PSMType->isArrayType()) {
                BaseType = PSMType->getAsArrayTypeUnsafe()->getElementType();
              }
            }
          }
          if (!BaseType.isNull()) {
            auto MaybeTemplateParam =
                BaseType.getTypePtr()
                    ->getAs<clang::SubstTemplateTypeParmType>();
            if (MaybeTemplateParam) {
              BaseType = MaybeTemplateParam->getReplacementType();
            }
            const clang::Type *UnderlyingType = BaseType.getTypePtr();
            if (UnderlyingType) {
              if (UnderlyingType->isVoidType()) {
                diag(MatchedPtrArg->getExprLoc(),
                     "base type for shared memory is 'void'!",
                     clang::DiagnosticIDs::Warning);
                return;
              } else {
                if (UnderlyingType->isUnionType()) {
                  diag(MatchedPtrArg->getExprLoc(),
                       "base type for shared memory is a union. Use the "
                       "concrete member!",
                       clang::DiagnosticIDs::Warning);
                  return;
                }
			  }
            }
            const clang::IdentifierInfo *BaseTypeId =
                BaseType.getBaseTypeIdentifier();
            if (!BaseTypeId) {
              auto MaybeTypedef = BaseType.getTypePtr()->getAs<TypedefType>();
              if (MaybeTypedef) {
                BaseTypeId = MaybeTypedef->getDecl()->getIdentifier();
              }
            }
            if (BaseTypeId) {
              auto UsedType = BaseTypeId->getName();
              auto AllowedTypes = PsmIt->second.StructTypes;
              if (std::find(AllowedTypes.begin(), AllowedTypes.end(),
                            UsedType) == AllowedTypes.end()) {
                if (std::find(AllowedTypes.begin(), AllowedTypes.end(),
                              "<NOTYPE>") == AllowedTypes.end()) {
                  diag(MatchedPtrArg->getExprLoc(),
                       "wrong type '%0' for psm %1 found in call to %3. "
                       "Type should be one of '%2'!")
                      << UsedType << PsmIt->second.PSMName
                      << std::accumulate(
                             AllowedTypes.begin(), AllowedTypes.end(),
                             std::string(""),
                             [](std::string partialresult, std::string item) {
                               return partialresult.empty()
                                          ? item
                                          : partialresult + ", " + item;
                             })
                      << FunctionName;
                }
              }
            } else {
              auto MaybeUnion = BaseType.getTypePtr();
              if (MaybeUnion->isUnionType()) {
                diag(MatchedPtrArg->getExprLoc(),
                     "base type for shared memory is a union. Use the concrete member!",
                     clang::DiagnosticIDs::Warning);
              } else {
                diag(MatchedPtrArg->getExprLoc(),
                     "base type for shared memory could not be determined!",
                     clang::DiagnosticIDs::Warning);
              }
            }
          } else {
            diag(MatchedPtrArg->getExprLoc(),
                 "base type for shared memory could not be determined!",
                 clang::DiagnosticIDs::Warning);
          }
        } else {
          diag(MatchedPtrArg->getExprLoc(),
               "third parameter must be a pointer!",
               clang::DiagnosticIDs::Error);
        }
      } else {
        diag(MatchedPtrArg->getExprLoc(),
             "type for shared memory with number %0 not known. Please update "
             "this clang-tidy check!",
             clang::DiagnosticIDs::Error)
            << static_cast<int>(EvalResult.getExtValue());
      }
    } else {
      diag(MatchedPtrArg->getExprLoc(),
           "unknown shared memory with number %0 found. Please update this "
           "clang-tidy check!",
           clang::DiagnosticIDs::Error)
          << static_cast<int>(EvalResult.getExtValue());
    }
  }
} // namespace btc
} // namespace btc
} // namespace tidy
} // namespace clang
