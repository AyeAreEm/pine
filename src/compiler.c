#include <stdio.h>
#include <string.h>
#include "include/module.h"
#include "include/stmnts.h"
#include "include/compiler.h"
#include "include/cli.h"
#include "include/strb.h"
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
        },
        .path = NULL,
    };
}

static void compiler_invoke_cc(Compiler *compiler) {
    const char *cc = get_c_compiler();
    strb com = NULL;
    strbprintf(&com, "%s -o %s.exe output.c ", cc, compiler->options.output);

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

    // debug("%s", com);
    FILE *fd = popen(com, "r");
    if (fd == NULL) {
        panic("failed to compile");
    }

    if (pclose(fd) != 0) {
        panic("failed to compile");
    }

    if (!compiler->cli.emitc) {
        remove("output.c");
        remove("output.h");
    }

    strbfree(com);
}

Module *modules_find(Arr(Module) modules, const char *path) {
    if (modules == NULL) {
        return NULL;
    }

    for (size_t i = 0; i < arrlenu(modules); i++) {
        if (streq(path, modules[i].path)) {
            return &modules[i];
        }
    }

    return NULL;
}

void compiler_import(Compiler *compiler, const char *path) {
    if (modules_find(compiler->modules, path) != NULL) {
        return;
    }

    strb previous_path = compiler->path;
#if defined(_WIN32) || defined(__MINGW32__)
    strb new_path = NULL; strbprintf(&new_path, "%s\\%s\\", previous_path, path);
#else
    strb new_path = NULL; strbprintf(&new_path, "%s/%s", previous_path, path);
#endif
    compiler->path = new_path;

    Arr(char*) files = files_in_folder(compiler->path, ".pine");
    Arr(Stmnt) ast = NULL;

    if (files == NULL) {
        panic("could not import \"%s\"... ensure it is a folder", compiler->path);
    }

    for (size_t i = 0; i < arrlenu(files); i++) {
        char *content = {0};
        bool content_ok = read_entire_file(files[i], &content);
        if (!content_ok) {
            panic("failed to read %s", files[i]);
        }

        Lexer lex = lexer(content);
        free(content); // NOTE: all strings are heap allocated from content during lexing, so it is fine to free here
        if (arrlenu(lex.tokens) == 0) {
            continue;
        }

        Parser parser = parser_init(lex.tokens, files[i]);
        parser_import_pass(compiler, parser);
        parser.modules = compiler->modules;

        arrpush(ast, stmnt_metadata((Metadata){.kind = MkFilename, .data = files[i]}));
        for (Stmnt stmnt = parser_parse(&parser); stmnt.kind != SkNone; stmnt = parser_parse(&parser)) {
            arrpush(ast, stmnt);
        }

        if (parser.error_count > 0) {
            exit(1);
        }
    }

    strb p = NULL; strbprintf(&p, "%s", compiler->path);
    Module module = module_init(p, ast, arrlenu(compiler->modules));
    arrpush(compiler->modules, module);
    strbfree(new_path);
    compiler->path = previous_path;
}

void compiler_build(Compiler *compiler) {
    strbprintf(&compiler->path, ".");
    compiler_import(compiler, compiler->cli.rootfolder);

    // for (size_t i = 0; i < arrlenu(compiler->modules); i++) {
    //     print_stmnts(compiler->modules[i].ast);
    // }

    Sema sema = sema_init(compiler->modules);
    sema_analyse(&sema);

    if (sema.error_count > 0) {
        exit(1);
    }

    Gen gen = gen_init(sema.modules);
    gen_generate(&gen);

    write_entire_file("output.h", gen.defs);
    write_entire_file("output.c", gen.code);

    if (strlen(compiler->options.output) == 0) {
        if (streq(compiler->cli.rootfolder, ".")) {
            char *cwd = get_cwd();
            char *cwd_name = strip_path(cwd);
            compiler->options.output = strdup(cwd_name);
            // debug("%s", compiler->options.output);
            free(cwd);
        } else {
            compiler->options.output = compiler->cli.rootfolder;
        }
    }
    compiler_invoke_cc(compiler);
}

void compiler_run(Compiler *compiler) {
    strb com = NULL;
    strbprintf(&com, "./%s.exe", compiler->options.output);

    for (int i = 0; i < compiler->cli.argc; i++) {
        strbprintf(&com, " %s", compiler->cli.argv[i]);
    }

    FILE *fd = popen(com, "r");
    if (fd == NULL) {
        panic("failed to run `%s`", com);
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
