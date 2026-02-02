#include <stdio.h>
#include <string.h>
#include "include/module.h"
#include "include/stmnts.h"
#include "include/compiler.h"
#include "include/cli.h"
#include "include/utils.h"
#include "include/lexer.h"
#include "include/parser.h"
#include "include/sema.h"
#include "include/gen.h"

Compiler compiler_init(Cli cli) {
    return (Compiler){
        .modules = NULL,
        .cli = cli,
        .options = (CompilerOptions){
            .output = "",
            .optimize = OptDebug,
        }
    };
}

static void compiler_invoke_cc(Compiler *compiler) {
    const char *cc = get_c_compiler();
    strb com = NULL;
    strbprintf(&com, "%s -o %s output.c ", cc, compiler->options.output);

    char *op = "";
    switch (compiler->options.optimize) {
        case OptNone:
            op = "-O0";
            break;
        case OptDebug:
            op = "-Og -g";
            break;
        case OptRelease:
            op = "-O2";
            break;
        case OptFast:
            op = "-O3";
            break;
        case OptSmall:
            op = "-Os";
            break;
    }
    strbprintf(&com, "%s", op);

    // for (size_t i = 0; i < arrlenu(flags.links); i++) {
    //     strbprintf(&com, " %s", flags.links[i]);
    // }

    FILE *fd = popen(com, "r");
    if (fd == NULL) {
        comp_elog("failed to compile");
    }

    if (pclose(fd) != 0) {
        comp_elog("failed to compile");
    }

    if (!compiler->cli.emitc) {
        remove("output.c");
        remove("output.h");
    }

    strbfree(com);
}

void compiler_import(Compiler *compiler, const char *path) {
    Arr(char*) files = files_in_folder(path, ".pine");
    Arr(Stmnt) ast = NULL;

    if (files == NULL) {
        comp_elog("could not import \"%s\"... ensure it is a folder", path);
    }

    for (size_t i = 0; i < arrlenu(files); i++) {
        char *content = {0};
        bool content_ok = read_entire_file(files[i], &content);
        if (!content_ok) {
            comp_elog("failed to read %s", files[i]);
        }

        Lexer lex = lexer(content);
        free(content); // NOTE: all strings are heap allocated from content during lexing, so it is fine to free here

        Parser parser = parser_init(lex.tokens, files[i]);
        parser_import_pass(compiler, parser);
        arrpush(ast, stmnt_metadata((Metadata){.kind = MkFilename, .data = files[i]}));
        for (Stmnt stmnt = parser_parse(&parser); stmnt.kind != SkNone; stmnt = parser_parse(&parser)) {
            arrpush(ast, stmnt);
        }
        print_stmnts(ast);

        if (parser.error_count > 0) {
            exit(1);
        }
    }

    Module module = module_init(streq(path, ".") ? "root" : path, ast);
    arrpush(compiler->modules, module);
}

void compiler_build(Compiler *compiler) {
    compiler_import(compiler, compiler->cli.rootfolder);

    Sema sema = sema_init(compiler->modules);
    sema_analyse(&sema);

    if (sema.error_count > 0) {
        exit(1);
    }

    exit(1);

    Gen gen = gen_init(sema.modules[0].ast, sema.modules[0].dgraph, compiler->cli.rootfolder);
    gen_generate(&gen);

    write_entire_file("output.h", gen.defs);
    write_entire_file("output.c", gen.code);

    if (strlen(compiler->options.output) == 0) {
        compiler->options.output = filename_from_path(compiler->cli.rootfolder);
    }
    compiler_invoke_cc(compiler);
}

void compiler_run(Compiler *compiler) {
    strb com = NULL;
#if defined(__linux__) || defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__) || defined(__sun) || defined(__CYGWIN__)
    strbprintf(&com, "./%s", compiler->options.output);
#elif defined(_WIN32) || defined(__MINGW32__)
    strbprintf(&com, "./%s.exe", exe);
#endif

    for (int i = 0; i < compiler->cli.argc; i++) {
        strbprintf(&com, " %s", compiler->cli.argv[i]);
    }

    FILE *fd = popen(com, "r");
    if (fd == NULL) {
        comp_elog("failed to run `%s`", com);
    }

    char buf[1024];
    while (fgets(buf, sizeof(buf), fd) != NULL) {
        printf("%s", buf);
    }

    pclose(fd);
    strbfree(com);
}

void compiler_compile(Compiler *compiler) {
    cli_usage(compiler->cli, false);

    switch (compiler->cli.command) {
        case CommandBuild: {
            compiler_build(compiler);
        } break;
        case CommandRun: {
            compiler_build(compiler);
            compiler_run(compiler);
        } break;
        default:
            printfln(TERM_RED "error" TERM_END ": no command given");
            cli_usage(compiler->cli, true);
            break;
    }
}
