#include <time.h>
#include <stdlib.h>
#include "include/cli.h"
#include "include/compiler.h"

#define STB_DS_IMPLEMENTATION
#include "include/stb_ds.h"

int main(int argc, char **argv) {
    srand(time(NULL));
    stbds_rand_seed(time(NULL));
    Cli cli = cli_parse(argv, argc);

    Compiler compiler = compiler_init(cli);
    compiler_compile(&compiler);

    return 0;
}
