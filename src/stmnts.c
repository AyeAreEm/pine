#include "include/stmnts.h"
#include "include/exprs.h"
#include "include/strb.h"
#include "include/types.h"
#include "include/utils.h"

Stmnt stmnt_none(void) {
    return (Stmnt){
        .kind = SkNone,
    };
}

Stmnt stmnt_fndecl(FnDecl v, Cursor cursor) {
    return (Stmnt){
        .kind = SkFnDecl,
        .cursor = cursor,
        .fndecl = v,
    };
}

Stmnt stmnt_structdecl(StructDecl v, Cursor cursor) {
    return (Stmnt){
        .kind = SkStructDecl,
        .cursor = cursor,
        .structdecl = v,
    };
}

Stmnt stmnt_enumdecl(EnumDecl v, Cursor cursor) {
    return (Stmnt){
        .kind = SkEnumDecl,
        .cursor = cursor,
        .enumdecl = v,
    };
}

Stmnt stmnt_vardecl(VarDecl v, Cursor cursor) {
    return (Stmnt){
        .kind = SkVarDecl,
        .cursor = cursor,
        .vardecl = v,
    };
}

Stmnt stmnt_varreassign(VarReassign v, Cursor cursor) {
    return (Stmnt){
        .kind = SkVarReassign,
        .cursor = cursor,
        .varreassign = v,
    };
}

Stmnt stmnt_constdecl(ConstDecl v, Cursor cursor) {
    return (Stmnt){
        .kind = SkConstDecl,
        .cursor = cursor,
        .constdecl = v,
    };
}

Stmnt stmnt_return(Return v, Cursor cursor) {
    return (Stmnt){
        .kind = SkReturn,
        .cursor = cursor,
        .returnf = v,
    };
}

Stmnt stmnt_defer(Stmnt *v, Cursor cursor) {
    return (Stmnt){
        .kind = SkDefer,
        .defer = v,
        .cursor = cursor,
    };
}

Stmnt stmnt_continue(Cursor cursor) {
    return (Stmnt){
        .kind = SkContinue,
        .cursor = cursor,
    };
}

Stmnt stmnt_break(Cursor cursor) {
    return (Stmnt){
        .kind = SkBreak,
        .cursor = cursor,
    };
}

Stmnt stmnt_fall(Cursor cursor) {
    return (Stmnt){
        .kind = SkFall,
        .cursor = cursor,
    };
}

Stmnt stmnt_fncall(FnCall v, Cursor cursor) {
    return (Stmnt){
        .kind = SkFnCall,
        .cursor = cursor,
        .fncall = v,
    };
}

Stmnt stmnt_if(If v, Cursor cursor) {
    return (Stmnt){
        .kind = SkIf,
        .cursor = cursor,
        .iff = v,
    };
}

Stmnt stmnt_switch(Switch v, Cursor cursor) {
    return (Stmnt) {
        .kind = SkSwitch,
        .cursor = cursor,
        .switchf = v,
    };
}

Stmnt stmnt_case(Case v, Cursor cursor) {
    return (Stmnt){
        .kind = SkCase,
        .cursor = cursor,
        .casef = v,
    };
}

Stmnt stmnt_for(For v, Cursor cursor) {
    return (Stmnt){
        .kind = SkFor,
        .cursor = cursor,
        .forf = v,
    };
}

Stmnt stmnt_foreach(ForEach v, Cursor cursor) {
    return (Stmnt){
        .kind = SkForEach,
        .cursor = cursor,
        .foreach = v,
    };
}

Stmnt stmnt_block(Arr(Stmnt) v, Cursor cursor) {
    return (Stmnt){
        .kind = SkBlock,
        .cursor = cursor,
        .block = v,
    };
}

Stmnt stmnt_extern(Stmnt *v, Cursor cursor) {
    return (Stmnt){
        .kind = SkExtern,
        .cursor = cursor,
        .externf = v,
    };
}

Stmnt stmnt_directive(Directive v, Cursor cursor) {
    return (Stmnt){
        .kind = SkDirective,
        .cursor = cursor,
        .directive = v,
    };
}

Stmnt stmnt_metadata(Metadata v) {
    return (Stmnt){
        .kind = SkMetadata,
        .metadata = v,
        .cursor = {0, 0},
    };
}

strb stmnt_stringify(Stmnt stmnt) {
    strb ret = NULL;

    switch(stmnt.kind) {
        case SkNone:
        case SkDirective:
            strbprintf(&ret, "Directive");
            break;
        case SkMetadata:
            strbprintf(&ret, "Metadata");
            break;
        case SkDefer: {
            strb line = stmnt_stringify(*stmnt.defer);
            strbprintf(&ret, "Defer %s", line);
            strbfree(line);
            break;
        }
        case SkExtern: {
            strb line = stmnt_stringify(*stmnt.externf);
            strbprintf(&ret, "Extern %s", line);
            strbfree(line);
            break;
        }
        case SkBlock:
            for (size_t i = 0; i < arrlenu(stmnt.block); i++) {
                strb line = stmnt_stringify(stmnt.block[i]);
                strbprintfln(&ret, "  %s", line);
                strbfree(line);
            }
            break;
        case SkFnCall: {
            strb name = expr_stringify(*stmnt.fncall.name);
            strbprintf(&ret, "%s(", name);

            if (stmnt.fncall.arg_kind == LitkExprs) {
                for (size_t i = 0; i < arrlenu(stmnt.fncall.args.exprs); i++) {
                    strb e = expr_stringify(stmnt.fncall.args.exprs[i]);
                    strbprintf(&ret, "%s, ", e);
                    strbfree(e);
                }
            }
            strbprintf(&ret, ")");

            strbfree(name);
            break;
        }
        case SkForEach: {
            strb iter = expr_stringify(stmnt.foreach.iterator);
            strbprintf(&ret, "For (%s) [", iter);

            for (size_t i = 0; i < 2; i++) {
                if (stmnt.foreach.captures[i].kind == CkNone) {
                    break;
                }

                if (stmnt.foreach.captures[i].kind == CkIdent) {
                    strb cap = expr_stringify(stmnt.foreach.captures[i].ident);
                    strbprintf(&ret, " %s");
                    strbfree(cap);
                } else {
                    strb cap = stmnt_stringify(*stmnt.foreach.captures[i].decl);
                    strbprintf(&ret, " %s");
                    strbfree(cap);
                }
            }
            strbprintf(&ret, "]");

            for (size_t i = 0; i < arrlenu(stmnt.foreach.body); i++) {
                strb line = stmnt_stringify(stmnt.foreach.body[i]);
                strbprintf(&ret, "  %s", line);
                strbfree(line);
            }

            strbfree(iter);
            break;
        }
        case SkFor: {
            strb decl = stmnt_stringify(*stmnt.forf.decl);
            strb cond = expr_stringify(stmnt.forf.condition);
            strb update = stmnt_stringify(*stmnt.forf.update);

            strbprintf(&ret, "For (%s; %s; %s;)", decl, cond, update);

            for (size_t i = 0; i < arrlenu(stmnt.forf.body); i++) {
                strb line = stmnt_stringify(stmnt.forf.body[i]);
                strbprintf(&ret, "  %s", line);
                strbfree(line);
            }

            strbfree(decl);
            strbfree(cond);
            strbfree(update);
            break;
        }
        case SkSwitch: {
            strb cond = expr_stringify(stmnt.switchf.value);
            strbprintf(&ret, "Switch (%s)", cond);

            for (size_t i = 0; i < arrlenu(stmnt.switchf.cases); i++) {
                strb casef = stmnt_stringify(stmnt.switchf.cases[i]);
                strbprintf(&ret, "%s");
                strbfree(casef);
            }

            strbfree(cond);
            break;
        }
        case SkCase: {
            strb cond = expr_stringify(stmnt.casef.value);
            strbprintf(&ret, "Case %s");

            for (size_t i = 0; i < arrlenu(stmnt.casef.body); i++) {
                strb line = stmnt_stringify(stmnt.casef.body[i]);
                strbprintf(&ret, "  %s");
                strbfree(line);
            }

            strbfree(cond);
            break;
        }
        case SkVarReassign: {
            strb name = expr_stringify(stmnt.varreassign.name);
            strb value = expr_stringify(stmnt.varreassign.value);
            strbprintf(&ret, "%s = %s", name, value);
            strbfree(name);
            strbfree(value);
            break;
        }
        case SkReturn: {
            strb val = expr_stringify(stmnt.returnf.value);
            strbprintf(&ret, "Return %s", val);
            strbfree(val);
            break;
        }
        case SkEnumDecl: {
            strb name = expr_stringify(stmnt.enumdecl.name);
            strbprintfln(&ret, "%s :: Enum");

            for (size_t i = 0; i < arrlenu(stmnt.enumdecl.fields); i++) {
                strb line = stmnt_stringify(stmnt.enumdecl.fields[i]);
                strbprintf(&ret, "  %s", line);
                strbfree(line);
            }

            strbfree(name);
            break;
        }
        case SkStructDecl: {
            strb name = expr_stringify(stmnt.structdecl.name);
            strbprintfln(&ret, "%s :: Struct");

            for (size_t i = 0; i < arrlenu(stmnt.structdecl.fields); i++) {
                strb line = stmnt_stringify(stmnt.structdecl.fields[i]);
                strbprintf(&ret, "  %s", line);
                strbfree(line);
            }

            strbfree(name);
            break;
        }
        case SkFnDecl: {
            strb name = expr_stringify(stmnt.fndecl.name);
            strbprintf(&ret, "%s :: Fn(", name);

            for (size_t i = 0; i < arrlenu(stmnt.fndecl.args); i++) {
                strb arg = stmnt_stringify(stmnt.fndecl.args[i]);
                strbprintf(&ret, "%s, ", arg);
                strbfree(arg);
            }

            strb type = string_from_type(stmnt.fndecl.type);
            strbprintfln(&ret, ") %s", type);

            if (stmnt.fndecl.has_body) {
                for (size_t i = 0; i < arrlenu(stmnt.fndecl.body); i++) {
                    strb line = stmnt_stringify(stmnt.fndecl.body[i]);
                    strbprintfln(&ret, "  %s", line);
                    strbfree(line);
                }
            }

            strbfree(name);
            strbfree(type);
            break;
        }
        case SkVarDecl: {
            strb name = expr_stringify(stmnt.vardecl.name);
            strb type = string_from_type(stmnt.vardecl.type);
            strb value = expr_stringify(stmnt.vardecl.value);
            strbprintf(&ret, "%s: %s = %s", name, type, value);
            strbfree(name);
            strbfree(type);
            strbfree(value);
            break;
        }
        case SkConstDecl: {
            strb name = expr_stringify(stmnt.constdecl.name);
            strb type = string_from_type(stmnt.constdecl.type);
            strb value = expr_stringify(stmnt.constdecl.value);
            strbprintf(&ret, "%s: %s: %s", name, type, value);
            strbfree(name);
            strbfree(type);
            strbfree(value);
            break;
        }
        case SkBreak:
            strbprintf(&ret, "Break");
            break;
        case SkContinue:
            strbprintf(&ret, "Continue");
            break;
        case SkFall:
            strbprintf(&ret, "Fall");
            break;
        case SkIf: {
            strb cond = expr_stringify(stmnt.iff.condition);
            strbprintf(&ret, "If (%s) ", cond);

            if (stmnt.iff.capture.kind == CkIdent) {
                strb capture = expr_stringify(stmnt.iff.capture.ident);
                strbprintf(&ret, "[%s]", capture);
                strbfree(capture);
            } else {
                strbprintfln(&ret, "");
            }

            for (size_t i = 0; i < arrlenu(stmnt.iff.body); i++) {
                strb line = stmnt_stringify(stmnt.iff.body[i]);
                strbprintfln(&ret, "  %s", line);
                strbfree(line);
            }
            strbprintf(&ret, "Else ");

            for (size_t i = 0; i < arrlenu(stmnt.iff.els); i++) {
                strb line = stmnt_stringify(stmnt.iff.body[i]);
                strbprintfln(&ret, "  %s", line);
                strbfree(line);
            }
            strbfree(cond);
            break;
        }
    }

    return ret;
}

void print_stmnts(Arr(Stmnt) stmnts) {
    for (size_t i = 0; i < arrlenu(stmnts); i++) {
        strb stmnt = stmnt_stringify(stmnts[i]);
        printfln("%s", stmnt);
        strbfree(stmnt);
    }
}
