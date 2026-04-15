#include <gauche.h>

ScmObj F_o(void)
{
    return SCM_INTERN("foo");
}

ScmObj Foo_o(ScmObj x, ScmObj y)
{
    return Scm_Cons(x, y);
}
