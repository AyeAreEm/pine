#ifndef LEXER_H
#define LEXER_H

#include <stdint.h>
#include <stdbool.h>
#include "stb_ds.h"
#include "strb.h"

typedef struct Cursor {
    uint32_t row;
    uint32_t col;
} Cursor;

typedef enum TokenKind {
    TokIdent,
    TokIntLit,
    TokFloatLit,
    TokCharLit,
    TokStrLit,
    TokDirective,    // #...

    TokColon,        // :
    TokSemiColon,    // ;

    TokEqual,        // =
    TokLeftAngle,    // <
    TokRightAngle,   // >

    TokLeftBracket,  // (
    TokRightBracket, // )

    TokLeftCurl,     // {
    TokRightCurl,    // }

    TokLeftSquare,   // [
    TokRightSquare,  // ]

    TokComma,        // ,
    TokDot,          // .
    TokCaret,        // ^

    TokPlus,         // +
    TokMinus,        // -
    TokStar,         // *
    TokSlash,        // /
    TokPercent,      // %
    TokBackSlash,    /* \ */

    TokBar,          // |
    TokAmpersand,    // &
    TokTilde,        // ~
    TokExclaim,      // !

    TokUnderscore,   // _

    TokQuestion,     // ?

    TokNone,
} TokenKind;
const char *tokenkind_stringify(TokenKind kind);

typedef struct Token {
    TokenKind kind;
    Cursor cursor;
    const char *string;
} Token;

Token token_none(void);
void print_tokens(Token *tokens);

// returns strb, needs to be freed
strb token_stringify(Token tok);

#define BUF_CAP 255

typedef struct Lexer {
    Arr(Token) tokens;

    char ch;
    char buf[BUF_CAP];
    size_t buf_len;
    Cursor cursor;

    int ignore_index;
    bool in_single_line_comment;
    bool in_block_comment;
    bool escaped;
    bool in_quotes;
    bool in_double_quotes;
    bool is_directive;
} Lexer;

Lexer lexer(const char *source);
#endif // LEXER_H
