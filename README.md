# documentation attributes

This plugin arose from thinking about what it'd look like for documentation to be more
of a first class construct in the code.

The gist is that instead of writing special comments as in doxygen and other apidoc
systems, you add [attributes](https://en.cppreference.com/w/cpp/language/attributes)
to your declarations like so:

```c++
// test.cc
__attribute__((description("classic greeting, usually used with `world`")))
void hello(const char *target) {
  printf("hello %s\n", target);
}

const char *world = "world";

__attribute__((usage([] { hello(world); })))
void hello(const char *target);
```

Then run clang with the DocAttr plugin:

```
$ clang++ -cc1 -std=c++17 test.cc \
    -load build/libDocAttr.so \
    -plugin extract_doc_attrs \
    -plugin-arg-extract_doc_attrs doc.json
#                                 ^~~~~~~~
```

After which a JSON document containing the extracted attributes will be stored
in `doc.json`. The generated JSON contains things which are useful for later
API doc generation, like:
- The location of the documented declaration
- The documented declaration's fully qualified name
- Disambiguated references to other declarations which appear in the description text

Since all this information is provided by the compiler itself, it's as correct
as it can be; references are looked up by consulting `clang::Sema` so there's no
need to worry about flaky best-effort parser/indexers which were really designed for Java.

Usage examples are provided with lambdas, which means they are checked by the compiler
for errors. For just one example, typos like `std::srting` will be caught immediately.
Note that this is only true for compiler instances which have loaded the plugin; other
compilers will skip documentation attributes aggressively so their presence will not slow
cells of your CI matrix which don't care to parse them.


### Future work

- Usages could be run, too- making each of them a cute, cheap, inline unit test.
- Reference lookup in description text is currently quite limited; it wouldn't be
  able to resolve `std::vector<int>::iterator` for example. (obviously) this is a
  limitation of the toyishness of this project, not of clang.

### Getting started

```
# clone the repo
git clone https://github.com/bkietz/doc_attrs
cd doc_attrs

# get dependencies with conda/mamba
mamba create -n doc_attrs -c conda-forge --file conda_env.txt
mamba activate doc_attrs

# setup & build
mkdir build; cd build
cmake -GNinja -DCMAKE_BUILD_TYPE=Debug ..
ninja

# run clang with the plugin:
# (show help):
clang++ -cc1 \
  -load build/libDocAttr.so \
  -plugin extract_doc_attrs

# (regenerate doc.json):
clang++ -cc1 -std=c++17 test.cc \
  -load build/libDocAttr.so \
  -plugin extract_doc_attrs \
  -plugin-arg-extract_doc_attrs doc.json
```

