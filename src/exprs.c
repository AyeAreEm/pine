#include "include/exprs.h"
#include "include/strb.h"
#include "include/types.h"
#include "include/stmnts.h"
#include "include/module.h"
#include "include/utils.h"
#include <stdio.h>

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

Expr expr_import(Import v, Cursor cursor) {
    return (Expr){
        .kind = EkImport,
        .cursor = cursor,
        .type = type_module(v.module),
        .import = v,
    };
}

strb expr_stringify(Expr expr) {
    strb ret = NULL;

    switch (expr.kind) {
        case EkNone:
            break;
        case EkArraySlice: {
            strb access = expr_stringify(*expr.arrayslice.accessing);
            strb range = expr_stringify(*expr.arrayslice.slice);
            strbprintf(&ret, "%s[%s]", access, range);
            strbfree(access);
            strbfree(range);
            break;
        }
        case EkArrayIndex: {
            strb access = expr_stringify(*expr.arrayidx.accessing);
            strb index = expr_stringify(*expr.arrayidx.index);
            strbprintf(&ret, "%s[%s]", access, index);
            strbfree(access);
            strbfree(index);
            break;
        }
        case EkNull:
            strbprintf(&ret, "Null");
            break;
        case EkFieldAccess: {
            strb access = expr_stringify(*expr.fieldacc.accessing);
            if (expr.fieldacc.deref) {
                strbprintf(&ret, "%s.&", access);
            } else {
                strb field = expr_stringify(*expr.fieldacc.field);
                strbprintf(&ret, "%s.%s", access, field);
                strbfree(field);
            }
            strbfree(access);
            break;
        }
        case EkGrouping: {
            strb e = expr_stringify(*expr.group);
            strbprintf(&ret, "(%s)", e);
            strbfree(e);
            break;
        }
        case EkTrue:
            strbprintf(&ret, "True");
            break;
        case EkFalse:
            strbprintf(&ret, "False");
            break;
        case EkIntLit:
        case EkFloatLit:
            strbprintf(&ret, "%s", expr.lit);
            break;
        case EkStrLit:
            strbprintf(&ret, "\"%s\"", expr.lit);
            break;
        case EkCharLit:
            strbprintf(&ret, "'%s'", expr.lit);
            break;
        case EkIdent:
            strbprintf(&ret, "%s", expr.ident);
            break;
        case EkType: {
            strb t = string_from_type(expr.type_expr);
            strbprintf(&ret, "%s", t);
            strbfree(t);
            break;
        }
        case EkRangeLit: {
            strb start = expr_stringify(*expr.rangelit.start);
            strb end = expr_stringify(*expr.rangelit.end);
            strbprintf(&ret, "%s..%c%s", start, expr.rangelit.inclusive ? '=' : '<', end);
            strbfree(start);
            strbfree(end);
            break;
        }
        case EkLiteral:
            strbprintf(&ret, "{");
            if (expr.literal.kind == LitkExprs) {
                for (size_t i = 0; i < arrlenu(expr.literal.exprs); i++) {
                    strb e = expr_stringify(expr.literal.exprs[i]);
                    strbprintf(&ret, "%s, ", e);
                    strbfree(e);
                }
            } else {
                for (size_t i = 0; i < arrlenu(expr.literal.vars); i++) {
                    strb e = stmnt_stringify(expr.literal.vars[i]);
                    strbprintf(&ret, "%s, ", e);
                    strbfree(e);
                }
            }
            strbprintf(&ret, "}");
            break;
        case EkFnCall: {
            strb name = expr_stringify(*expr.fncall.name);
            if (expr.literal.kind == LitkExprs) {
                for (size_t i = 0; i < arrlenu(expr.literal.exprs); i++) {
                    strb e = expr_stringify(expr.literal.exprs[i]);
                    strbprintf(&ret, "%s, ", e);
                    strbfree(e);
                }
            } else {
                for (size_t i = 0; i < arrlenu(expr.literal.vars); i++) {
                    strb e = stmnt_stringify(expr.literal.vars[i]);
                    strbprintf(&ret, "%s, ", e);
                    strbfree(e);
                }
            }
            strbprintf(&ret, ")");
            strbfree(name);
            break;
        }
        case EkImport: {
            strbprintf(&ret, "Import \"%s\"", expr.import.module->path);
            break;
        }
        case EkUnop: {
            strb val = expr_stringify(*expr.unop.val);
            switch (expr.unop.kind) {
                case UkAddress:
                    strbprintf(&ret, "&%s", val);
                    break;
                case UkNegate:
                    strbprintf(&ret, "-%s", val);
                    break;
                case UkNot:
                    strbprintf(&ret, "!%s", val);
                    break;
                case UkBitNot:
                    strbprintf(&ret, "~%s", val);
                    break;
                case UkCast: {
                    strb type = string_from_type(expr.type);
                    strbprintf(&ret, "(%s)%s", type, val);
                    strbfree(type);
                    break;
                case UkSizeof:
                    strbprintf(&ret, "%s", val);
                    break;
                }
            }
            strbfree(val);
            break;
        }
        case EkBinop: {
            strb lhs = expr_stringify(*expr.binop.left);
            strb rhs = expr_stringify(*expr.binop.right);
            const char *binopstr = "";
            switch (expr.binop.kind) {
                case BkPlus:
                    binopstr = "+";
                    break;
                case BkMinus:
                    binopstr = "-";
                    break;
                case BkMultiply:
                    binopstr = "*";
                    break;
                case BkDivide:
                    binopstr = "/";
                    break;
                case BkMod:
                    binopstr = "%";
                    break;
                case BkLess:
                    binopstr = "<";
                    break;
                case BkLessEqual:
                    binopstr = "<=";
                    break;
                case BkGreater:
                    binopstr = ">";
                    break;
                case BkGreaterEqual:
                    binopstr = ">=";
                    break;
                case BkEquals:
                    binopstr = "==";
                    break;
                case BkInequals:
                    binopstr = "!=";
                    break;
                case BkLeftShift:
                    binopstr = "<<";
                    break;
                case BkRightShift:
                    binopstr = ">>";
                    break;
                case BkBitAnd:
                    binopstr = "&";
                    break;
                case BkBitOr:
                    binopstr = "|";
                    break;
                case BkBitXor:
                    binopstr = "^";
                    break;
                case BkAnd:
                    binopstr = "and";
                    break;
                case BkOr:
                    binopstr = "or";
                    break;
            }
            strbprintf(&ret, "%s %s %s", lhs, binopstr, rhs);
            strbfree(lhs);
            strbfree(rhs);
            break;
        }
    }

    return ret;
}
