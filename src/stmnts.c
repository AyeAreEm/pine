#include "include/stmnts.h"

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
