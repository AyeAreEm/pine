#include "include/dgraph.h"
#include "include/utils.h"

Dgraph dgraph_init(void) {
    return (Dgraph){
        .names = NULL,
        .children = NULL,
    };
}

void dgraph_push(Dgraph *graph, Dnode node) {
    bool found = false;
    for (size_t i = 0; i < arrlenu(graph->names); i++) {
        if (streq(graph->names[i], node.name)) {
            found = true;
            break;
        }
    }

    if (!found) {
        arrpush(graph->names, node.name);
        arrpush(graph->children, node);
    }
}
