//===--- ModernizeUseAutoCheck.h - clang-tidy--------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_BTC_MODERNIZEUSEAUTOCHECK_H
#define LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_BTC_MODERNIZEUSEAUTOCHECK_H

#include "../ClangTidy.h"

namespace clang {
namespace tidy {
namespace btc {

/// FIXME: Write a short description.
///
/// For the user-facing documentation see:
/// http://clang.llvm.org/extra/clang-tidy/checks/btc-modernize-use-auto.html
class ModernizeUseAutoCheck : public ClangTidyCheck {
public:
  ModernizeUseAutoCheck(StringRef Name, ClangTidyContext *Context);
  void storeOptions(ClangTidyOptions::OptionMap &Opts) override;
  void registerMatchers(ast_matchers::MatchFinder *Finder) override;
  void check(const ast_matchers::MatchFinder::MatchResult &Result) override;

private:
  void replaceIterators(const DeclStmt *D, ASTContext *Context);
  void replaceExpr(const DeclStmt *D, ASTContext *Context,
                   llvm::function_ref<QualType(const Expr *)> GetType,
                   StringRef Message);
  void replaceDecl(const DeclStmt *D, ASTContext *Context);

  struct ReplaceDeclData {
    std::string newInitializerExpression;
    std::string conditionMessageFragment;

    explicit operator bool() const { return !newInitializerExpression.empty(); }
  };

  ReplaceDeclData handleConstructExpr(const CXXConstructExpr *Construct,
                                      ASTContext *Context,
                                      const QualType &FirstDeclType);

  ReplaceDeclData handleExpr(const Expr *ExprNode, ASTContext *Context,
                             const QualType &FirstDeclType);

  static ReplaceDeclData
  makeDefaultInitializerExpression(ASTContext *Context,
                                   const QualType &FirstDeclType);

  static std::string makeTypeString(ASTContext *Context,
                                    const QualType &FirstDeclType);

  const unsigned int MinTypeNameLength;
  const bool RemoveStars;
};

} // namespace btc
} // namespace tidy
} // namespace clang

#endif // LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_BTC_MODERNIZEUSEAUTOCHECK_H
