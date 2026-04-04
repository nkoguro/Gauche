#include <inttypes.h>
#include "f.h"

char f_c(void)
{
    return 9;
}

int f_i(void)
{
    return 42;
}

float f_f(void)
{
    return 1.25f;
}

double f_d(void)
{
    return 3.14;
}

void f_v(void)
{
}

int f_i_i(int n)
{
    return n+1;
}

float f_f_f(float x)
{
    return x/2;
}

double f_d_d(double d)
{
    return d*2;
}

struct foo *f_pstruct_c_pstruct(struct foo *st, char c)
{
    st->c = c;
    return st;
}

struct foo *f_pstruct_s_pstruct(struct foo *st, short s)
{
    st->s = s;
    return st;
}

struct foo *f_pstruct_i_pstruct(struct foo *st, int i)
{
    st->i = i;
    return st;
}

struct foo *f_pstruct_l_pstruct(struct foo *st, long l)
{
    st->l = l;
    return st;
}

struct foo *f_pstruct_f_pstruct(struct foo *st, float f)
{
    st->f = f;
    return st;
}

struct foo *f_pstruct_d_pstruct(struct foo *st, double d)
{
    st->d = d;
    return st;
}
