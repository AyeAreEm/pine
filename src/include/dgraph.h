#ifndef DGRAPH_H
#define DGRAPH_H

#include "stmnts.h"

typedef struct Dnode {
    const char *name;
    Stmnt us;
    Arr(const char*) children;
} Dnode;

typedef struct Dgraph {
    Arr(const char*) names;
    Arr(Dnode) children;
} Dgraph;

Dgraph dgraph_init(void);
void dgraph_push(Dgraph *graph, Dnode node);

#endif // DGRAPH_H
