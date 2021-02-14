cmake_minimum_required(VERSION 3.8 FATAL_ERROR)
project(treesitter)

set(CMAKE_BUILD_TYPE Release)

include_directories(../tree-sitter/lib/src ../tree-sitter/lib/include ../.)

add_library(treesitter SHARED ../grammar/src/parser.c ../tree-sitter/lib/src/lib.c)
