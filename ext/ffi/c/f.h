#include <stdio.h>
#include <gauche.h>

struct foo {
    char c;
    int i;
    short s;
    long l;
    float f;
    double d;
};

extern char f_c(void);
extern int f_i(void);
extern float f_f(void);
extern double f_d(void);
extern void f_v(void);

extern int fi_i(int);
extern float ff_f(float);
extern double f_d_d(double);

extern struct foo *f_pstrct_c_pstruct(struct foo*, char);
extern struct foo *f_pstrct_s_pstruct(struct foo*, short);
extern struct foo *f_pstrct_i_pstruct(struct foo*, int);
extern struct foo *f_pstrct_l_pstruct(struct foo*, long);
extern struct foo *f_pstrct_f_pstruct(struct foo*, float);
extern struct foo *f_pstrct_d_pstruct(struct foo*, double);

extern int f_ivar(int cnt, ...);
extern double f_dvar(int cnt, ...);
