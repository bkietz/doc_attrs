// https://github.com/llvm/llvm-project/issues/46197

extern "C" {
void printf(const char *, ...);
}

namespace basic {
namespace greetings {

__attribute__((description(R"(
  classic greeting, usually used with `world`
  aka `greetings::world`
  aka `basic::greetings::world`
  aka `::basic::greetings::world`
  `hello("foo")`
)"))) void
hello(const char *target) {
  printf("hello %s\n", target);
}

// note: we can look up world even though it's declared after hello
const char *world = "world";

__attribute__((usage([] { hello(world); }))) void hello(const char *target);

} // namespace greetings
} // namespace basic
