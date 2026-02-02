#ifndef MODULE_H
#define MODULE_H

#include "dgraph.h"
#include "stb_ds.h"
#include "stmnts.h"
#include "symtab.h"

// string key, i64 value hashmap
typedef Sh(int64_t) hmsi64;

typedef struct Module {
    const char *name;
    Arr(Stmnt) ast;
    SymTab symtab;
    Dgraph dgraph;
    hmsi64 *typedef_sizes;
} Module;

Module module_init(const char *name, Arr(Stmnt) ast);

#endif // MODULE_H
