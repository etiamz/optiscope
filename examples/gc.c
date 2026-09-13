// Test that GC prioritizes annihilation of duplicators & delimiters, thereby
// keeping total graph rewrites linear with respect to `N`. Originally suggested
// by `@LeeeeT` (GitHub handle).

// Usage: `$ ./command/test_gc.py`.

#include <optiscope.h>

#ifndef N
#define N 10
#endif

static struct lambda_term *
church_body(
    const uint64_t n,
    struct lambda_term *const restrict f,
    struct lambda_term *const restrict x) {
    if (0 == n) {
        return var(x);
    } else {
        return apply(var(f), church_body(n - 1, f, x));
    }
}

static struct lambda_term *
church(const uint64_t n) {
    struct lambda_term *f, *x;

    return lambda(f, lambda(x, church_body(n, f, x)));
}

static struct lambda_term *
term(void) {
    struct lambda_term *b, *t, *f;

    return apply(
        apply(church(N), church(2)),
        lambda(
            b,
            lambda(
                t,
                lambda(
                    f, apply(apply(var(b), apply(var(f), var(f))), var(f))))));
}

int
main(void) {
    optiscope_open_pools();
    optiscope_algorithm(stdout, term());
    puts("");
    optiscope_close_pools();
}
