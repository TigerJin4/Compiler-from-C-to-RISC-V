cmake_minimum_required(VERSION 3.13)
project(project_2_TigerJin4 C)

set(CMAKE_C_STANDARD 11)

include_directories(.)
include_directories(cgen-lib)

add_executable(project_2_TigerJin4
        cgen-lib/print.h
        ast-print.h
        ast.h
        cgen-helpers.c
        cgen-helpers.h
        cgen.c
        cgen.h
        dast-print.h
        dast.h
        decorate-errors.h
        decorate.h
        instructions.c
        instructions.h
        main.c
        parser-errors.h
        parser-helpers.h
        parser.h
        preprocessor.h
        string-helpers.h
        tokenizer-errors.h
        tokenizer.h
        tokens.h
        utils.h)
