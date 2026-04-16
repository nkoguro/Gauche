#include <gauche.h>

ScmObj F_o(void)
{
    return SCM_INTERN("foo");
}

ScmObj Foo_o(ScmObj x, ScmObj y)
{
    return Scm_Cons(x, y);
}

ScmObj Foooooooooo_o(ScmObj a, ScmObj b, ScmObj c, ScmObj d, ScmObj e,
                     ScmObj f, ScmObj g, ScmObj h, ScmObj i, ScmObj j)
{
    return Scm_List(j, i, h, g, f, e, d, c, b, a, NULL);
}

ScmObj Fcb(ScmObj proc)
{
    return Scm_ApplyRec1(proc, SCM_MAKE_INT(1));
}
