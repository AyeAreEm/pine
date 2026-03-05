#ifndef COMPILER_H
#define COMPILER_H

#include "cli.h"
#include "stmnts.h"
#include "stb_ds.h"
#include "module.h"
#include "strb.h"

typedef enum Optimize {
    OptNone,
    OptDebug,
    OptRelease,
    OptSmall,
    OptFast,
} Optimize;

typedef struct CompilerOptions {
    const char *output;
    Optimize optimize;
} CompilerOptions;

typedef struct Compiler {
    Arr(Module) modules;
    Cli cli;
    CompilerOptions options;
    strb path;
} Compiler;

Compiler compiler_init(Cli cli);
void compiler_compile(Compiler *compiler);

// lex + parse
void compiler_import(Compiler *compiler, const char *path);
Module *modules_find(Arr(Module) modules, const char *path);

#endif // COMPILER_H
