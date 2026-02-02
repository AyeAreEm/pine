#include "include/module.h"
#include "include/dgraph.h"
#include "include/symtab.h"

Module module_init(const char *name, Arr(Stmnt) ast) {
    hmsi64 *typedef_sizes = NULL;
    shdefault(typedef_sizes, -1);

    return (Module){
        .name = name,
        .ast = ast,
        .typedef_sizes = typedef_sizes,
        .dgraph = dgraph_init(),
        .symtab = symtab_init(),
    };
}
