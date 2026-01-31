#ifndef EXPRS_H
#define EXPRS_H

#include <stddef.h>
#include <stdint.h>
#include "lexer.h"
#include "types.h"

typedef struct Stmnt Stmnt;

typedef enum ExprKind {
    EkNone,

    // types are also exprs
    EkType,

    EkIntLit,
    EkFloatLit,
    EkCharLit,
    EkStrLit,
    EkRangeLit,
    EkLiteral,

    EkIdent,
    EkFnCall,

    EkBinop,
    EkUnop,

    EkTrue,
    EkFalse,
    EkGrouping,

    EkFieldAccess,
    EkArrayIndex,
    EkArraySlice,

    EkNull,
} ExprKind;

typedef enum LitKind {
    LitkNone,
    LitkExprs,
    LitkVars,
} LitKind;

typedef struct Literal {
    LitKind kind;

    union {
        Arr(Expr) exprs;
        Arr(Stmnt) vars; // VarReassign
    };
} Literal;

typedef struct RangeLit {
    Expr *start;
    Expr *end;
    bool inclusive;
} RangeLit;

typedef struct FieldAccess {
    Expr *accessing;
    Expr *field;
    bool deref;
} FieldAccess;

typedef struct ArrayIndex {
    Expr *accessing;
    Expr *index;
} ArrayIndex;

typedef struct ArraySlice {
    Expr *accessing;
    Expr *slice;
} ArraySlice;

typedef struct FnCall {
    Expr *name;
    LitKind arg_kind;

    union {
        Arr(Expr) exprs;
        Arr(Stmnt) vars; // VarReassign
    } args;
} FnCall;

typedef enum BinopKind {
    BkPlus,
    BkMinus,
    BkDivide,
    BkMultiply,
    BkMod,

    BkLess,
    BkLessEqual,
    BkGreater,
    BkGreaterEqual,
    BkEquals,
    BkInequals,

    BkBitOr,
    BkBitAnd,
    BkBitXor,
    BkLeftShift,
    BkRightShift,

    BkAnd,
    BkOr,
} BinopKind;

// Binary Operation
typedef struct Binop {
    BinopKind kind;

    Expr *left;
    Expr *right;
} Binop;

typedef enum UnopKind {
    UkBitNot,
    UkNot,
    UkNegate,
    UkAddress,
    UkCast,
    UkSizeof,
} UnopKind;

// Unary Operation
typedef struct Unop {
    UnopKind kind;
    Expr *val;
} Unop;

typedef struct Expr {
    ExprKind kind;
    Cursor cursor;
    Type type;

    union {
        Type type_expr;

        const char *lit;
        const char *ident;

        Literal literal;
        FnCall fncall;

        Binop binop;
        Unop unop;

        Expr *group;
        RangeLit rangelit;
        FieldAccess fieldacc;
        ArrayIndex arrayidx;
        ArraySlice arrayslice;
    };
} Expr;

Expr expr_none(void);
Expr expr_true(Cursor cursor);
Expr expr_false(Cursor cursor);
Expr expr_null(Type t, Cursor cursor);
Expr expr_type(Type v, Cursor cursor);

Expr expr_intlit(const char *s, Type t, Cursor cursor);
Expr expr_floatlit(const char *s, Type t, Cursor cursor);
Expr expr_charlit(const char *s, Cursor cursor);
Expr expr_strlit(const char *s, Cursor cursor);
Expr expr_ident(const char *s, Type t, Cursor cursor);

Expr expr_literal(Literal v, Type t, Cursor cursor);
Expr expr_fncall(FnCall v, Type t, Cursor cursor);
Expr expr_binop(Binop v, Type t, Cursor cursor);
Expr expr_unop(Unop v, Type t, Cursor cursor);
Expr expr_group(Arr(Expr) v, Type t, Cursor cursor);
Expr expr_range(RangeLit v, Type t, Cursor cursor);
Expr expr_fieldaccess(FieldAccess v, Type t, Cursor cursor);
Expr expr_arrayindex(ArrayIndex v, Type t, Cursor cursor);
Expr expr_arrayslice(ArraySlice v, Type t, Cursor cursor);

#endif // EXPRS_H
