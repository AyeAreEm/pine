#include "include/module.h"
#include "include/dgraph.h"
#include "include/symtab.h"

Module module_init(strb path, Arr(Stmnt) ast, size_t index) {
    hmsi64 *typedef_sizes = NULL;
    shdefault(typedef_sizes, -1);

    return (Module){
        .path = path,
        .ast = ast,
        .typedef_sizes = typedef_sizes,
        .dgraph = dgraph_init(),
        .symtab = symtab_init(),
        .analysed = false,
        .index = index,
    };
}
