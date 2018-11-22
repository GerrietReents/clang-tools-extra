// RUN: %check_clang_tidy %s btc-modernize-use-auto %t -- -config="{CheckOptions: [{key: btc-modernize-use-auto.MinTypeNameLength, value: '0'}]}" -- -std=c++11 -frtti

class MyType {
public:
  MyType() = default;
  explicit MyType(int) {}
};

class MyTypeWithDefaultArguments {
public:
  MyTypeWithDefaultArguments(int x = 0);
};

template<typename T>
struct ConversionSource {  
};

template<typename T>
struct ConversionTarget {
  ConversionTarget(ConversionSource<T> src);
};

template<typename T>
ConversionSource<T> MakeConversionSource(T value);

template <typename T>
class MyTemplate {
  void member_function() {
    MyType ac1;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: use auto when declaring a default-initialized variable
    // CHECK-FIXES: auto ac1 = MyType{};

    T t;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: use auto when declaring a default-initialized variable
    // CHECK-FIXES: auto t = T{};

    // FIXME support this case
    ConversionTarget<T> implicitlyConverted = MakeConversionSource(T{});
    // noCHECK-MESSAGES: :[[@LINE-1]]:5: warning: use auto ...
    // noCHECK-FIXES: auto implicitlyConverted = ConversionTarget<T>{MakeConversionSource(T{})};
  }
};

template class MyTemplate<int>;

using MyTypeAlias = MyType;
using MyTypePtrAlias = MyType *;
using MyTypeConstAlias = const MyType;

class MyDerivedType : public MyType {};

void auto_default_initialized() {
  const MyType ac1;
  // CHECK-MESSAGES: :[[@LINE-1]]:9: warning: use auto when declaring a default-initialized variable
  // CHECK-FIXES: const auto ac1 = MyType{};

  const MyType ac2 = MyType{};
  // CHECK-MESSAGES: :[[@LINE-1]]:9: warning: use auto when declaring a default-initialized variable
  // CHECK-FIXES: const auto ac2 = MyType{};

  MyType ax = MyType{};
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto when declaring a default-initialized variable
  // CHECK-FIXES: auto ax = MyType{};

  MyType ay = MyType();
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto when declaring a default-initialized variable
  // CHECK-FIXES: auto ay = MyType{};

  MyType a;
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto when declaring a default-initialized variable
  // CHECK-FIXES: auto a = MyType{};

  MyType az = {};
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto when declaring a default-initialized variable
  // CHECK-FIXES: auto az = MyType{};

  MyTypeAlias aAlias;
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto when declaring a default-initialized variable
  // CHECK-FIXES: auto aAlias = MyTypeAlias{};

  // no initialization, cannot use auto to achieve this, auto will always 0-initialize
  MyTypePtrAlias aPtrAlias;

  MyTypeConstAlias aConstAlias;
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto when declaring a default-initialized variable
  // CHECK-FIXES: auto aConstAlias = MyTypeConstAlias{};

  const MyTypeConstAlias aConstAliasExplicitlyConst;
  // CHECK-MESSAGES: :[[@LINE-1]]:9: warning: use auto when declaring a default-initialized variable
  // CHECK-FIXES: const auto aConstAliasExplicitlyConst = MyTypeConstAlias{};

  // FIXME make a warning but no autofix in this case?
  // do not provide an autofix if the types do not match literally
  MyTypeAlias aAliasMismatch = MyType{};

  // FIXME can this somehow use auto?
  MyType *aPtr;

  // FIXME also apply to multiple variables declared in one statement?
  MyType a1, a2;
  // noCHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto when declaring a default-initialized variable
  // noCHECK-FIXES: auto a1 = MyType{}; auto a2 = MyType{};

  MyTypeWithDefaultArguments xDefault;
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto when declaring a default-initialized variable
  // CHECK-FIXES: auto xDefault = MyTypeWithDefaultArguments{};

  // no initialization, cannot use auto to achieve this, auto will always 0-initialize
  int b;

  int b2 = int{};
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto when initializing with a cast to avoid duplicating the type name
  // CHECK-FIXES: auto b2 = int{};

  int b3 = {};
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto when initializing from an expression result
  // CHECK-FIXES: auto b3 = int{};

  // FIXME implement this case? is this possible?
  int bArray[1];
  // noCHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto when declaring a default-initialized variable
  // noCHECK-FIXES: auto bArray = int[1]{};

  // auto uShort = unsigned short{42}; would not be valid because unsigned short is not a "single-word type name"
  // FIXME create a warning but no autofix?
  unsigned short uShort = 42;

  // Don't warn when 'auto' is already being used.
  auto aut = MyType{};
}

void auto_non_default_initialized() {
  // FIXME implement this case
  MyType a(42);
  // noCHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto
  // noCHECK-FIXES: auto a = MyType{42};

  // FIXME implement this case
  MyType b{42};
  // noCHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto
  // noCHECK-FIXES: auto b = MyType{42};

  // FIXME implement this case
  MyType c = MyType{42};
  // noCHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto
  // noCHECK-FIXES: auto c = MyType{42};
}

MyType MakeMyType();
MyType MakeMyType(int);
MyTypeAlias MakeMyTypeAlias();

void auto_initialized_from_single_function_call() {
  MyType a = MakeMyType();
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto
  // CHECK-FIXES: auto a = MakeMyType();

  MyType a1(MakeMyType());
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto
  // CHECK-FIXES: auto a1 = MakeMyType();

  MyType a2 = MakeMyType(42);
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto
  // CHECK-FIXES: auto a2 = MakeMyType(42);

  MyTypeAlias a3 = MakeMyTypeAlias();
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto
  // CHECK-FIXES: auto a3 = MakeMyTypeAlias();

  // FIXME create a warning but no autofix in this case? or create an autofix for auto b = MyTypeAlias{MakeMyType()};
  // do not fix if the types do not match
  MyType a4 = MakeMyTypeAlias();

  // FIXME create a warning but no autofix in this case? or create an autofix for auto b = MyTypeAlias{MakeMyType()};
  // do not fix if the types do not match
  MyTypeAlias b = MakeMyType();
}

void auto_initialized_from_other_expression() {
  int a = 4 + 5;
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto
  // CHECK-FIXES: auto a = 4 + 5;

  // FIXME implement this case
  long b = 4 + 5;
  // noCHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto
  // noCHECK-FIXES: auto b = long{4 + 5};
}

// Don't warn for parameters, both for declarations and definitions.
void func(MyType myVar);
void func(MyType myVar) {}

// Don't warn for static/non-static data members.
class OtherType {
  static MyType staticDataMember;
  MyType nonStaticDataMember;
};

//   * Use {{}} for regular expressions.
// ex.: {{^}}void awesome_f();{{$}}
