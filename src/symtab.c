#include <stdio.h>
#include "include/exprs.h"
#include "include/stmnts.h"
#include "include/types.h"
#include "include/utils.h"
#include "include/sema.h"

SymTab symtab_init(void) {
    SymTab symtab = {
        .stmnts = NULL,
        .keys = NULL,
        .cur_scope = 0,
    };
    arrpush(symtab.keys, NULL);
    arrpush(symtab.stmnts, NULL);

    return symtab;
}

Stmnt symtab_find(Sema *sema, Expr key, Cursor cursor) {
    size_t module_idx = sema->module_idx;

    // NOTE: might be a little hacky but it works... i think
    if (key.kind == EkFieldAccess && key.fieldacc.accessing->type.kind == TkModule) {
        module_idx = key.fieldacc.accessing->type.module->index;
        key = *key.fieldacc.field;
    }

    for (size_t i = 0; i < arrlenu(sema->modules[module_idx].symtab.keys[sema->modules[module_idx].symtab.cur_scope]); i++) {
        if (streq(key.ident, sema->modules[module_idx].symtab.keys[sema->modules[module_idx].symtab.cur_scope][i])) {
            return sema->modules[module_idx].symtab.stmnts[sema->modules[module_idx].symtab.cur_scope][i];
        }
    }

    // if not in symtab, see if it's defined at least
    Stmnt stmnt = ast_find_decl(sema->modules[module_idx].ast, key.ident);
    if (stmnt.kind != SkNone) return stmnt;

    elog(sema, cursor, "use of undefined \"%s\"", key);
    return stmnt_none();
}

void symtab_push(Sema *sema, const char *key, Stmnt value) {
    for (size_t i = 0; i < arrlenu(SEMA_CURRENT_MODULE.symtab.keys[SEMA_CURRENT_MODULE.symtab.cur_scope]); i++) {
        if (streq(key, SEMA_CURRENT_MODULE.symtab.keys[SEMA_CURRENT_MODULE.symtab.cur_scope][i])) {
            Cursor cursor = SEMA_CURRENT_MODULE.symtab.stmnts[SEMA_CURRENT_MODULE.symtab.cur_scope][i].cursor;
            elog(sema, value.cursor, "redeclaration of \"%s\" from %s:%d:%d", key, sema->filename, cursor.row, cursor.col);
            return;
        }
    }

    arrpush(SEMA_CURRENT_MODULE.symtab.keys[SEMA_CURRENT_MODULE.symtab.cur_scope], key);
    arrpush(SEMA_CURRENT_MODULE.symtab.stmnts[SEMA_CURRENT_MODULE.symtab.cur_scope], value);
}

void symtab_new_scope(Sema *sema) {
    Arr(const char*) keys = NULL;
    Arr(Stmnt) stmnts = NULL;

    for (size_t i = 0; i < arrlenu(SEMA_CURRENT_MODULE.symtab.keys[SEMA_CURRENT_MODULE.symtab.cur_scope]); i++) {
        arrpush(keys, SEMA_CURRENT_MODULE.symtab.keys[SEMA_CURRENT_MODULE.symtab.cur_scope][i]);
        arrpush(stmnts, SEMA_CURRENT_MODULE.symtab.stmnts[SEMA_CURRENT_MODULE.symtab.cur_scope][i]);
    }

    arrpush(SEMA_CURRENT_MODULE.symtab.keys, keys);
    arrpush(SEMA_CURRENT_MODULE.symtab.stmnts, stmnts);
    SEMA_CURRENT_MODULE.symtab.cur_scope++;
}

void symtab_pop_scope(Sema *sema) {
    // (void) to silence warnings
    (void)arrpop(SEMA_CURRENT_MODULE.symtab.keys);
    (void)arrpop(SEMA_CURRENT_MODULE.symtab.stmnts);
    SEMA_CURRENT_MODULE.symtab.cur_scope--;
}
