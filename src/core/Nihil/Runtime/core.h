#pragma once
#ifndef NIHIL_RUNTIME_CORE_H
#define NIHIL_RUNTIME_CORE_H

#include <stddef.h>

struct VPattern_s;
struct VExpr_s;
struct VBranch_s;
struct VDecl_s;

struct VPattern_s
{
    enum VPattern_Cons
    {
        CrPInteger,
        CrPDouble,
        CrPCharacter,
        CrPId,
        CrPWildcard,
        CrPTuple,
        CrPConstructor
    } ctor;

    union
    {
        struct { long p_i; } pInteger;
        struct { double p_d; } pDouble;
        struct { char p_c; } pCharacter;
        struct { const char *p_name; } pId;
        struct { unsigned long n; const const struct VPattern_s **p_patterns; } pTuple;
        struct { const char *p_name; unsigned long n; const const struct VPattern_s **p_args; } pConstructor;
    } value;
} VPattern;

struct VExpr_s
{
    enum VExpr_Cons
    {
        CrEId,
        CrEInteger,
        CrEDouble,
        CrECharacter,
        CrELambda,
        CrEApplication,
        CrETuple,
        CrETypeHole,
        CrEMatch,
        CrELet
    } ctor;

    union
    {
        struct { const char *v_name; } eId;
        struct { long v_i; } eInteger;
        struct { double v_d; } eDouble;
        struct { char v_c; } eCharacter;
        struct { const struct VPattern_s *v_arg; const struct VExpr_s *v_ex; } eLambda;
        struct { const struct VExpr_s *v_fun; const struct VExpr_s *v_x; } eApplication;
        struct { unsigned long n; const const struct VExpr_s **v_vals; } eTuple;
        struct { const struct VExpr_s *v_expr; unsigned long n; const const struct VBranch_s **v_branches; } eMatch;
        struct { unsigned long n; const const struct VDecl_s **v_decls; const struct VExpr_s *v_expr; } eLet;
    } value;
};

struct VBranch_s
{
    const struct VPattern_s *b_pattern;
    const struct VExpr_s *b_expr;
};

struct VDecl_s
{
    const char *d_name;
    const struct VExpr_s *d_val;
};

#endif