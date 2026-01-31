#include "include/exprs.h"

Expr expr_none(void) {
    return (Expr){.kind = EkNone};
}

Expr expr_true(Cursor cursor) {
    return (Expr){
        .kind = EkTrue,
        .cursor = cursor,
        .type = type_bool(TYPEVAR, cursor),
    };
}

Expr expr_false(Cursor cursor) {
    return (Expr){
        .kind = EkFalse,
        .cursor = cursor,
        .type = type_bool(TYPEVAR, cursor),
    };
}

Expr expr_null(Type t, Cursor cursor) {
    return (Expr){
        .kind = EkNull,
        .cursor = cursor,
        .type = t,
    };
}

Expr expr_type(Type v, Cursor cursor) {
    return (Expr){
        .kind = EkType,
        .cursor = cursor,
        .type = (Type){
            .kind = TkTypeId,
        },
        .type_expr = v,
    };
}

Expr expr_intlit(const char *s, Type t, Cursor cursor) {
    return (Expr){
        .kind = EkIntLit,
        .cursor = cursor,
        .type = t,
        .lit = s,
    };
}

Expr expr_floatlit(const char *s, Type t, Cursor cursor) {
    return (Expr){
        .kind = EkFloatLit,
        .cursor = cursor,
        .type = t,
        .lit = s,
    };
}

Expr expr_charlit(const char *s, Cursor cursor) {
    return (Expr){
        .kind = EkCharLit,
        .cursor = cursor,
        .type = type_char(TYPEVAR, cursor),
        .lit = s,
    };
}

Expr expr_strlit(const char *s, Cursor cursor) {
    return (Expr){
        .kind = EkStrLit,
        .cursor = cursor,
        .type = type_string(TkUntypedString, TYPEVAR, cursor),
        .lit = s,
    };
}

Expr expr_ident(const char *v, Type t, Cursor cursor) {
    return (Expr){
        .kind = EkIdent,
        .cursor = cursor,
        .type = t,
        .ident = v,
    };
}

Expr expr_literal(Literal v, Type t, Cursor cursor) {
    return (Expr){
        .kind = EkLiteral,
        .cursor = cursor,
        .type = t,
        .literal = v,
    };
}

Expr expr_fncall(FnCall v, Type t, Cursor cursor) {
    return (Expr){
        .kind = EkFnCall,
        .cursor = cursor,
        .type = t,
        .fncall = v,
    };
}

Expr expr_binop(Binop v, Type t, Cursor cursor) {
    return (Expr){
        .kind = EkBinop,
        .cursor = cursor,
        .type = t,
        .binop = v,
    };
}

Expr expr_unop(Unop v, Type t, Cursor cursor) {
    return (Expr){
        .kind = EkUnop,
        .cursor = cursor,
        .type = t,
        .unop = v,
    };
}

Expr expr_group(Arr(Expr) v, Type t, Cursor cursor) {
    return (Expr){
        .kind = EkGrouping,
        .cursor = cursor,
        .type = t,
        .group = v,
    };
}

Expr expr_range(RangeLit v, Type t, Cursor cursor) {
    return (Expr){
        .kind = EkRangeLit,
        .cursor = cursor,
        .type = t,
        .rangelit = v,
    };
}

Expr expr_fieldaccess(FieldAccess v, Type t, Cursor cursor) {
    return (Expr){
        .kind = EkFieldAccess,
        .cursor = cursor,
        .type = t,
        .fieldacc = v,
    };
}

Expr expr_arrayindex(ArrayIndex v, Type t, Cursor cursor) {
    return (Expr){
        .kind = EkArrayIndex,
        .cursor = cursor,
        .type = t,
        .arrayidx = v,
    };
}

Expr expr_arrayslice(ArraySlice v, Type t, Cursor cursor) {
    return (Expr){
        .kind = EkArraySlice,
        .cursor = cursor,
        .type = t,
        .arrayslice = v,
    };
}
