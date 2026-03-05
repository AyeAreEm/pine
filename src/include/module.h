#ifndef MODULE_H
#define MODULE_H

#include "dgraph.h"
#include "stb_ds.h"
#include "stmnts.h"
#include "symtab.h"

// string key, i64 value hashmap
typedef Sh(int64_t) hmsi64;

typedef struct Module {
    strb path;
    Arr(Stmnt) ast;
    SymTab symtab;
    Dgraph dgraph;
    hmsi64 *typedef_sizes;
    size_t index;
    bool analysed;
} Module;

Module module_init(strb path, Arr(Stmnt) ast, size_t index);

#endif // MODULE_H
