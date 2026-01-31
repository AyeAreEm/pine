#ifndef STMNTS_H
#define STMNTS_H

#include <stddef.h>
#include "exprs.h"
#include "lexer.h"

typedef struct Stmnt Stmnt;

typedef enum StmntKind {
    SkNone,
    SkFnDecl,
    SkStructDecl,
    SkEnumDecl,
    SkVarDecl,
    SkVarReassign,
    SkReturn,
    SkContinue,
    SkBreak,
    SkFall,
    SkFnCall,
    SkConstDecl,
    SkIf,
    SkSwitch,
    SkCase,
    SkFor,
    SkForEach,
    SkBlock,
    SkExtern,
    SkDirective,
    SkDefer,
} StmntKind;

typedef struct FnDecl {
    Expr name;
    Type type;
    Arr(Stmnt) args;
    Arr(Stmnt) body;
    bool has_body;
} FnDecl;

typedef struct StructDecl {
    Expr name;
    Arr(Stmnt) fields;
} StructDecl;

typedef StructDecl EnumDecl;

typedef struct VarDecl {
    Expr name;
    Type type;
    Expr value;
} VarDecl;

typedef VarDecl VarReassign;
typedef VarDecl ConstDecl;

typedef struct Return {
    Type type;
    Expr value;
} Return;

typedef enum CaptureKind {
    CkNone,
    CkIdent,
    CkConstDecl,
} CaptureKind;

typedef struct Capture {
    CaptureKind kind;
    union {
        Expr ident;
        Stmnt *decl;
    };
} Capture;

typedef struct If {
    Expr condition;
    Capture capture;

    Arr(Stmnt) body;
    Arr(Stmnt) els;
} If;

typedef struct Switch {
    Expr value;
    Capture capture;

    Arr(Stmnt) cases;
} Switch;

typedef struct Case {
    Expr value;
    Arr(Stmnt) body;
    bool fall;
} Case;

typedef struct For {
    Stmnt *decl;
    Expr condition;
    Stmnt *update;
    Arr(Stmnt) body;
} For;

typedef struct ForEach {
    Expr iterator;
    Capture captures[2];

    Arr(Stmnt) body;
} ForEach;

typedef enum DirectiveKind {
    DkNone,
    DkImport,
} DirectiveKind;

typedef struct Directive {
    DirectiveKind kind;

    const char *str; // import 
} Directive;

typedef struct Stmnt {
    StmntKind kind;
    Cursor cursor;

    union {
        FnDecl fndecl;
        FnCall fncall;
        StructDecl structdecl;
        EnumDecl enumdecl;
        VarDecl vardecl;
        VarReassign varreassign;
        ConstDecl constdecl;

        Return returnf;
        Stmnt *defer;

        If iff;
        Switch switchf;
        Case casef;
        For forf;
        ForEach foreach;
        Stmnt *externf;

        Arr(Stmnt) block;
        Directive directive;
    };
} Stmnt;

Stmnt stmnt_none(void);
Stmnt stmnt_extern(Stmnt *v, Cursor cursor);
Stmnt stmnt_fndecl(FnDecl v, Cursor cursor);
Stmnt stmnt_structdecl(StructDecl v, Cursor cursor);
Stmnt stmnt_enumdecl(EnumDecl v, Cursor cursor);
Stmnt stmnt_vardecl(VarDecl v, Cursor cursor);
Stmnt stmnt_varreassign(VarReassign v, Cursor cursor);
Stmnt stmnt_constdecl(ConstDecl v, Cursor cursor);

Stmnt stmnt_return(Return v, Cursor cursor);
Stmnt stmnt_continue(Cursor cursor);
Stmnt stmnt_break(Cursor cursor);
Stmnt stmnt_fall(Cursor cursor);
Stmnt stmnt_defer(Stmnt *v, Cursor cursor);

Stmnt stmnt_if(If v, Cursor cursor);
Stmnt stmnt_switch(Switch v, Cursor cursor);
Stmnt stmnt_case(Case v, Cursor cursor);
Stmnt stmnt_for(For v, Cursor cursor);
Stmnt stmnt_foreach(ForEach v, Cursor cursor);
Stmnt stmnt_block(Arr(Stmnt) v, Cursor cursor);

Stmnt stmnt_directive(Directive v, Cursor cursor);
Stmnt stmnt_fncall(FnCall v, Cursor cursor);

#endif // STMNTS_H
