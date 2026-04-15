#include <inttypes.h>
#include <stdarg.h>
#include "f.h"


char F_c(void)
{
    return 9;
}

int F_i(void)
{
    return 42;
}

float F_f(void)
{
    return 1.25f;
}

double F_d(void)
{
    return 3.14;
}

void F_v(void)
{
}

int Fi_i(int n)
{
    return n+1;
}

float Ff_f(float x)
{
    return x/2;
}

float Ffff_f(float x, float y, float z)
{
    return x+y+z;
}

double Fd_d(double d)
{
    return d*2;
}

double Gd_d(double d)
{
    return d*4;
}

double Fddd_d(double x, double y, double z)
{
    return x+y+z;
}

double Fifd_d(int x, float y, double z)
{
    return x*y + x*z;
}

float Fifd_f(int x, float y, double z)
{
    return -(x*y + x*z);
}

double Fidf_d(int x, double y, float z)
{
    return x*y - x*z;
}

float Fidf_f(int x, double y, float z)
{
    return -(x*y - x*z);
}

struct foo *F_pstruct_c_pstruct(struct foo *st, char c)
{
    st->c = c;
    return st;
}

struct foo *F_pstruct_s_pstruct(struct foo *st, short s)
{
    st->s = s;
    return st;
}

struct foo *F_pstruct_i_pstruct(struct foo *st, int i)
{
    st->i = i;
    return st;
}

struct foo *F_pstruct_l_pstruct(struct foo *st, long l)
{
    st->l = l;
    return st;
}

struct foo *F_pstruct_f_pstruct(struct foo *st, float f)
{
    st->f = f;
    return st;
}

struct foo *F_pstruct_d_pstruct(struct foo *st, double d)
{
    st->d = d;
    return st;
}

int Fivar(int cnt, ...)
{
    int r = 0;
    va_list ap;
    va_start(ap, cnt);

    while (cnt-- > 0) {
        int v = va_arg(ap, int);
        r += v;
    }
    va_end(ap);
    return r;
}

double Fdvar(int cnt, ...)
{
    double r = 0;
    va_list ap;
    va_start(ap, cnt);

    while (cnt-- > 0) {
        double v = va_arg(ap, double);
        r += v;
    }
    va_end(ap);
    return r;
}
