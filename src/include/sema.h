#ifndef SEMA_H
#define SEMA_H

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>
#include "lexer.h"
#include "stb_ds.h"
#include "exprs.h"
#include "stmnts.h"
#include "module.h"

#define ERRORS_MAX 5
void elog(Sema *sema, Cursor cursor, const char *msg, ...);

typedef struct Sema {
    Arr(Module) modules;
    size_t module_idx;

    struct {
        Stmnt fn; // can be SkNone
        bool forl;
        bool casef;
        bool fall;
    } envinfo;

    const char *filename;
    int error_count;
} Sema;

#define SEMA_CURRENT_MODULE sema->modules[sema->module_idx]

Sema sema_init(Arr(Module) modules);
Type *resolve_expr_type(Sema *sema, Expr *expr);
void sema_analyse(Sema *sema);
void sema_extern(Sema *sema, Stmnt *stmnt);
void sema_defer(Sema *sema, Stmnt *stmnt);
void sema_fn_decl(Sema *sema, Stmnt *stmnt);
void sema_block(Sema *sema, Arr(Stmnt) body);
void sema_directive(Sema *sema, Stmnt *stmnt);
void sema_expr(Sema *sema, Expr *expr);

// returns SkNone if not found
Stmnt ast_find_decl(Arr(Stmnt) ast, const char *key);

#endif // SEMA_H
