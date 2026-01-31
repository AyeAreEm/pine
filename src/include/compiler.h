#ifndef COMPILER_H
#define COMPILER_H

#include "stmnts.h"

// handles importing a new file
// lex + parse + sema
// returns typechecked ast
Arr(Stmnt) import(const char *filename);

#endif // COMPILER_H
