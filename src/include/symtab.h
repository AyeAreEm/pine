#ifndef SYMTAB_H
#define SYMTAB_H

#include "stb_ds.h"
#include "lexer.h"

typedef struct Sema Sema;
typedef struct Stmnt Stmnt;

typedef struct SymTab {
    Arr(Arr(Stmnt)) stmnts;
    Arr(Arr(const char*)) keys;
    size_t cur_scope;
} SymTab;

SymTab symtab_init(void);
Stmnt symtab_find(Sema *sema, const char *key, Cursor cursor);
void symtab_push(Sema *sema, const char *key, Stmnt value);
void symtab_new_scope(Sema *sema);
void symtab_pop_scope(Sema *sema);

#endif // SYMTAB_H
