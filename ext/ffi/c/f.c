#include <inttypes.h>
#include <stdarg.h>
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

float f_f_fff(float x, float y, float z)
{
    return x+y+z;
}

double f_d_d(double d)
{
    return d*2;
}

double f_d_ddd(double x, double y, double z)
{
    return x+y+z;
}

double f_d_ifd(int x, float y, double z)
{
    return x*y + x*z;
}

double f_d_idf(int x, double y, float z)
{
    return x*y - x*z;
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

int f_ivar(int cnt, ...)
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

double f_dvar(int cnt, ...)
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
