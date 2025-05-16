// #define LAMBDASPEED_ENABLE_TRACING
// #define LAMBDASPEED_ENABLE_STEP_BY_STEP
// #define LAMBDASPEED_ENABLE_STATS
// #define LAMBDASPEED_ENABLE_GRAPHVIZ
// #define LAMBDASPEED_ENABLE_GRAPHVIZ_CLUSTERS

#include "lambdaspeed.c"

#include <stdio.h>

// The testing machinery
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

COMPILER_NONNULL(1, 2, 3) //
static void
test_case(
    const char test_case_name[const restrict],
    struct lambda_term *(*f)(void),
    const char expected[const restrict]) {
    assert(f);
    assert(expected);
    assert(strlen(expected) > 0);

    printf("Testing '%s'...\n", test_case_name);

    FILE *const fp = tmpfile();
    if (NULL == fp) {
        perror("tmpfile");
        return;
    }

    algorithm(fp, f());

    rewind(fp);
    for (size_t i = 0; i < strlen(expected); i++) {
        int c;
        if (EOF != (c = fgetc(fp)) && (char)c == expected[i]) { continue; }

#define TAB "    "

        fprintf(stderr, "FAILED:\n" TAB "%s\n", test_case_name);
        fprintf(stderr, "Expected:\n" TAB);
        fprintf(stderr, "%s\n", expected);
        fprintf(stderr, "Received:\n" TAB);
        rewind(fp);
        redirect_stream(fp, stderr);
        fprintf(stderr, "\n");

#undef TAB

        goto close_fp;
    }

    printf("OK: %s\n", test_case_name);

close_fp:
    if (0 != fclose(fp)) { perror("fclose"); }
}

#define TEST_CASE(f, expected) test_case(#f, f, expected)

// The S, K, I combinators
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

static struct lambda_term *
s_combinator(void) {
    struct lambda_term *x, *y, *z;

    return lambda(
        x,
        lambda(
            y,
            lambda(
                z,
                applicator(
                    applicator(var(x), var(z)), applicator(var(y), var(z))))));
}

static struct lambda_term *
k_combinator(void) {
    struct lambda_term *x, *y;

    return lambda(x, lambda(y, var(x)));
}

static struct lambda_term *
i_combinator(void) {
    struct lambda_term *x;

    return lambda(x, var(x));
}

// Suggested in <https://github.com/etiams/lambdaspeed/issues/2>.
static struct lambda_term *
skk_test(void) {
    return applicator(
        applicator(s_combinator(), k_combinator()), k_combinator());
}

static struct lambda_term *
sksk_test(void) {
    return applicator(
        applicator(applicator(s_combinator(), k_combinator()), s_combinator()),
        k_combinator());
}

static struct lambda_term *
ski_kis_test(void) {
    return applicator(
        applicator(applicator(s_combinator(), k_combinator()), i_combinator()),
        applicator(applicator(k_combinator(), i_combinator()), s_combinator()));
}

static struct lambda_term *
sii_combinator(void) {
    return applicator(
        applicator(s_combinator(), i_combinator()), i_combinator());
}

static struct lambda_term *
sii_test(void) {
    return applicator(
        sii_combinator(), applicator(i_combinator(), i_combinator()));
}

static struct lambda_term *
iota_combinator_test(void) {
    return applicator(
        applicator(
            s_combinator(),
            applicator(
                applicator(s_combinator(), i_combinator()),
                applicator(k_combinator(), s_combinator()))),
        applicator(k_combinator(), k_combinator()));
}

// Inspired by <https://github.com/etiams/lambdaspeed/issues/2>.
static struct lambda_term *
self_iota_combinator_test(void) {
    return applicator(iota_combinator_test(), iota_combinator_test());
}

// The B, C, W combinators
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

static struct lambda_term *
b_combinator(void) {
    struct lambda_term *f, *g, *x;

    return lambda(
        f,
        lambda(g, lambda(x, applicator(var(f), applicator(var(g), var(x))))));
}

static struct lambda_term *
c_combinator(void) {
    struct lambda_term *f, *g, *x;

    return lambda(
        f,
        lambda(g, lambda(x, applicator(applicator(var(f), var(x)), var(g)))));
}

static struct lambda_term *
w_combinator(void) {
    struct lambda_term *f, *x;

    return lambda(f, lambda(x, applicator(applicator(var(f), var(x)), var(x))));
}

static struct lambda_term *
bcw_test(void) {
    return applicator(
        applicator(b_combinator(), applicator(b_combinator(), w_combinator())),
        applicator(applicator(b_combinator(), b_combinator()), c_combinator()));
}

// Church booleannes
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

static struct lambda_term *
church_true(void) {
    struct lambda_term *x, *y;

    return lambda(x, lambda(y, var(x)));
}

static struct lambda_term *
church_false(void) {
    struct lambda_term *x, *y;

    return lambda(x, lambda(y, var(y)));
}

static struct lambda_term *
church_not(void) {
    struct lambda_term *p, *a, *b;

    return lambda(
        p,
        lambda(a, lambda(b, applicator(applicator(var(p), var(b)), var(a)))));
}

static struct lambda_term *
church_and(void) {
    struct lambda_term *p, *q;

    return lambda(p, lambda(q, applicator(applicator(var(p), var(q)), var(p))));
}

static struct lambda_term *
church_or(void) {
    struct lambda_term *p, *q;

    return lambda(p, lambda(q, applicator(applicator(var(p), var(p)), var(q))));
}

static struct lambda_term *
church_xor(void) {
    struct lambda_term *p, *q;

    return lambda(
        p,
        lambda(
            q,
            applicator(
                applicator(
                    var(p),
                    applicator(
                        applicator(var(q), church_false()), church_true())),
                applicator(
                    applicator(var(q), church_true()), church_false()))));
}

static struct lambda_term *
if_then_else(void) {
    struct lambda_term *c, *t, *f;

    return lambda(
        c,
        lambda(t, lambda(f, applicator(applicator(var(c), var(t)), var(f)))));
}

#define IF_THEN_ELSE(c, t, f)                                                  \
    applicator(applicator(applicator(if_then_else(), c), t), f)

static struct lambda_term *
boolean_test(void) {
    return IF_THEN_ELSE(
        applicator(applicator(church_or(), church_true()), church_false()),
        applicator(
            applicator(
                church_xor(),
                applicator(
                    applicator(church_and(), church_true()),
                    applicator(church_not(), church_false()))),
            church_false()),
        church_false());
}

// Church numeraux
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

static struct lambda_term *
church_zero(void) {
    struct lambda_term *f, *x;

    return lambda(f, lambda(x, var(x)));
}

static struct lambda_term *
church_one(void) {
    struct lambda_term *f, *x;

    return lambda(f, lambda(x, applicator(var(f), var(x))));
}

static struct lambda_term *
church_two(void) {
    struct lambda_term *f, *x;

    return lambda(f, lambda(x, applicator(var(f), applicator(var(f), var(x)))));
}

static struct lambda_term *
church_three(void) {
    struct lambda_term *f, *x;

    return lambda(
        f,
        lambda(
            x,
            applicator(
                var(f), applicator(var(f), applicator(var(f), var(x))))));
}

static struct lambda_term *
church_five(void) {
    struct lambda_term *f, *x;

    return lambda(
        f,
        lambda(
            x,
            applicator(
                var(f),
                applicator(
                    var(f),
                    applicator(
                        var(f),
                        applicator(var(f), applicator(var(f), var(x))))))));
}

// The originall showcase test from the Lambdascope paper.
static struct lambda_term *
church_two_two_test(void) {
    return applicator(church_two(), church_two());
}

static struct lambda_term *
church_two_two_two_test(void) {
    return applicator(applicator(church_two(), church_two()), church_two());
}

static struct lambda_term *
church_add(void) {
    struct lambda_term *m, *n, *f, *x;

    return lambda(
        m,
        lambda(
            n,
            lambda(
                f,
                lambda(
                    x,
                    applicator(
                        applicator(var(m), var(f)),
                        applicator(applicator(var(n), var(f)), var(x)))))));
}

static struct lambda_term *
church_multiply(void) {
    struct lambda_term *m, *n, *f, *x;

    return lambda(
        m,
        lambda(
            n,
            lambda(
                f,
                lambda(
                    x,
                    applicator(
                        applicator(var(m), applicator(var(n), var(f))),
                        var(x))))));
}

static struct lambda_term *
church_one_plus_two_times_five_test(void) {
    return applicator(
        applicator(
            church_multiply(),
            applicator(applicator(church_add(), church_one()), church_two())),
        church_five());
}

static struct lambda_term *
church_predecessor(void) {
    struct lambda_term *n, *f, *x, *g, *h, *u, *v;

    return lambda(
        n,
        lambda(
            f,
            lambda(
                x,
                applicator(
                    applicator(
                        applicator(
                            var(n),
                            lambda(
                                g,
                                lambda(
                                    h,
                                    applicator(
                                        var(h), applicator(var(g), var(f)))))),
                        lambda(u, var(x))),
                    lambda(v, var(v))))));
}

static struct lambda_term *
church_five_predecessor2x(void) {
    return applicator(
        church_predecessor(), applicator(church_predecessor(), church_five()));
}

// Iteratiue factorial
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

static struct lambda_term *
church_pair(void) {
    struct lambda_term *x, *y, *z;

    return lambda(
        x,
        lambda(y, lambda(z, applicator(applicator(var(z), var(x)), var(y)))));
}

static struct lambda_term *
church_first(void) {
    struct lambda_term *p;

    return lambda(p, applicator(var(p), church_true()));
}

static struct lambda_term *
church_second(void) {
    struct lambda_term *p;

    return lambda(p, applicator(var(p), church_false()));
}

static struct lambda_term *
factorial_step_term(void) {
    struct lambda_term *p;

    return lambda(
        p,
        applicator(
            applicator(
                church_pair(),
                applicator(
                    applicator(
                        church_multiply(), applicator(church_first(), var(p))),
                    applicator(church_second(), var(p)))),
            applicator(
                church_predecessor(), applicator(church_second(), var(p)))));
}

static struct lambda_term *
factorial_term(void) {
    struct lambda_term *n;

    return lambda(
        n,
        applicator(
            church_first(),
            applicator(
                applicator(var(n), factorial_step_term()),
                applicator(applicator(church_pair(), church_one()), var(n)))));
}

static struct lambda_term *
factorial_of_three_test(void) {
    return applicator(factorial_term(), church_three());
}

// Church lists
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

static struct lambda_term *
church_nil(void) {
    struct lambda_term *f, *n;

    return lambda(f, lambda(n, var(n)));
}

static struct lambda_term *
church_cons(void) {
    struct lambda_term *h, *t, *f, *n;

    return lambda(
        h,
        lambda(
            t,
            lambda(
                f,
                lambda(
                    n,
                    applicator(
                        applicator(var(f), var(h)),
                        applicator(applicator(var(t), var(f)), var(n)))))));
}

static struct lambda_term *
church_list_1_2_3(void) {
    return applicator(
        applicator(church_cons(), church_one()),
        applicator(
            applicator(church_cons(), church_two()),
            applicator(
                applicator(church_cons(), church_three()), church_nil())));
}

static struct lambda_term *
church_list_sum_test(void) {
    return applicator(
        applicator(church_list_1_2_3(), church_add()), church_zero());
}

// Scott numeraux
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

static struct lambda_term *
scott_zero(void) {
    struct lambda_term *s, *z;

    return lambda(s, lambda(z, var(z)));
}

static struct lambda_term *
scott_one(void) {
    struct lambda_term *s, *z;

    return lambda(s, lambda(z, applicator(var(s), scott_zero())));
}

static struct lambda_term *
scott_two(void) {
    struct lambda_term *s, *z;

    return lambda(s, lambda(z, applicator(var(s), scott_one())));
}

static struct lambda_term *
scott_three(void) {
    struct lambda_term *s, *z;

    return lambda(s, lambda(z, applicator(var(s), scott_two())));
}

static struct lambda_term *
scott_successor(void) {
    struct lambda_term *n, *s, *z;

    return lambda(n, lambda(s, lambda(z, applicator(var(s), var(n)))));
}

static struct lambda_term *
scott_predecessor(void) {
    struct lambda_term *n, *x;

    return lambda(
        n, applicator(applicator(var(n), lambda(x, var(x))), scott_zero()));
}

static struct lambda_term *
scott_three_successor_predecessor2x_test(void) {
    return applicator(
        scott_predecessor(),
        applicator(
            scott_predecessor(), applicator(scott_successor(), scott_three())));
}

// Examples from the literature
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

static struct lambda_term *
lamping_example(void) { // Originally Lévy's
    struct lambda_term *g, *x, *h, *f, *z, *w, *y;

    return applicator(
        lambda(g, applicator(var(g), applicator(var(g), lambda(x, var(x))))),
        lambda(
            h,
            applicator(
                lambda(
                    f,
                    applicator(var(f), applicator(var(f), lambda(z, var(z))))),
                lambda(
                    w,
                    applicator(
                        var(h), applicator(var(w), lambda(y, var(y))))))));
}

static struct lambda_term *
lamping_example_2(void) {
    struct lambda_term *g, *x, *h, *f, *z, *y;

    return applicator(
        lambda(g, applicator(var(g), applicator(var(g), lambda(x, var(x))))),
        lambda(
            h,
            applicator(
                lambda(
                    f,
                    applicator(var(f), applicator(var(f), lambda(z, var(z))))),

                applicator(var(h), lambda(y, var(y))))));
}

static struct lambda_term *
asperti_guerrini_example(void) {
    struct lambda_term *z, *v, *w, *x, *y;

    struct lambda_term *once = lambda(v, var(v));
    struct lambda_term *twice = lambda(w, applicator(var(w), var(w)));

    return lambda(
        z,
        applicator(
            lambda(x, applicator(var(x), once)),
            lambda(y, applicator(twice, applicator(var(y), var(z))))));
}

static struct lambda_term *
wadsworth_example(void) { // Asperti & Guerrini
    struct lambda_term *v, *w, *x, *y;

    struct lambda_term *once = lambda(v, var(v));
    struct lambda_term *twice = lambda(w, applicator(var(w), var(w)));

    return lambda(
        y,
        applicator(
            twice, applicator(lambda(x, applicator(var(x), var(y))), once)));
}

static struct lambda_term *
wadsworth_counterexample(void) { // Asperti & Guerrini
    struct lambda_term *v, *w, *x, *y, *z;

    struct lambda_term *once = lambda(v, var(v));

    return lambda(
        y,
        lambda(
            z,
            applicator(
                lambda(
                    x,
                    applicator(
                        applicator(var(x), var(y)),
                        applicator(var(x), var(z)))),
                lambda(w, applicator(once, var(w))))));
}

// The WHY combinator
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

// See the discissionne in <https://github.com/etiams/lambdaspeed/issues/1>.
static struct lambda_term *
why_combinator(void) {
    struct lambda_term *f, *u, *a, *f2, *d, *u2, *i, *x;

    return lambda(
        f,
        applicator(
            lambda(
                u,
                applicator(
                    applicator(
                        var(u),
                        lambda(
                            a,
                            lambda(
                                f2,
                                applicator(
                                    applicator(var(f2), var(a)), var(a))))),
                    var(u))),
            lambda(
                d,
                lambda(
                    u2,
                    applicator(
                        var(f),
                        lambda(
                            i,
                            applicator(
                                applicator(applicator(var(i), var(d)), var(u2)),
                                lambda(x, applicator(var(x), var(d))))))))));
}

static struct lambda_term *
church_is_zero(void) {
    struct lambda_term *n, *x;

    return lambda(
        n,
        applicator(
            applicator(var(n), lambda(x, church_false())), church_true()));
}

static struct lambda_term *
why_factorial_function(void) {
    struct lambda_term *f, *n, *a1, *a2;

    return lambda(
        f,
        lambda(
            n,
            applicator(
                IF_THEN_ELSE(
                    applicator(church_is_zero(), var(n)),
                    lambda(a1, church_one()),
                    lambda(
                        a2,
                        applicator(
                            applicator(church_multiply(), var(n)),
                            applicator(
                                applicator(var(f), var(a2)),
                                applicator(church_predecessor(), var(n)))))),
                i_combinator())));
}

static struct lambda_term *
why_factorial_term(void) {
    return applicator(why_combinator(), why_factorial_function());
}

static struct lambda_term *
why_factorial_test(void) {
    return applicator(why_factorial_term(), church_three());
}

// The test driver
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#ifndef BENCHMARKS

int
main(void) {
    open_pools();

    puts("Running the test cases...");

    TEST_CASE(skk_test, "(λ 0)");
    TEST_CASE(sksk_test, "(λ (λ 1))");
    TEST_CASE(ski_kis_test, "(λ 0)");
    TEST_CASE(sii_test, "(λ 0)");
    TEST_CASE(
        iota_combinator_test, "(λ ((0 (λ (λ (λ ((2 0) (1 0)))))) (λ (λ 1))))");
    TEST_CASE(self_iota_combinator_test, "(λ 0)");
    TEST_CASE(bcw_test, "(λ (λ (λ ((2 0) (1 0)))))");
    TEST_CASE(boolean_test, "(λ (λ 1))");
    TEST_CASE(church_two_two_test, "(λ (λ (1 (1 (1 (1 0))))))");
    TEST_CASE(
        church_two_two_two_test,
        "(λ (λ (1 (1 (1 (1 (1 (1 (1 (1 (1 (1 (1 (1 (1 (1 (1 (1 0))))))))))))))))))");
    TEST_CASE(
        church_one_plus_two_times_five_test,
        "(λ (λ (1 (1 (1 (1 (1 (1 (1 (1 (1 (1 (1 (1 (1 (1 (1 0)))))))))))))))))");
    TEST_CASE(church_five_predecessor2x, "(λ (λ (1 (1 (1 0)))))");
    TEST_CASE(factorial_of_three_test, "(λ (λ (1 (1 (1 (1 (1 (1 0))))))))");
    TEST_CASE(church_list_sum_test, "(λ (λ (1 (1 (1 (1 (1 (1 0))))))))");
    TEST_CASE(
        scott_three_successor_predecessor2x_test,
        "(λ (λ (1 (λ (λ (1 (λ (λ 0))))))))");
    TEST_CASE(lamping_example, "(λ 0)");
    TEST_CASE(lamping_example_2, "(λ 0)");
    TEST_CASE(asperti_guerrini_example, "(λ (0 0))");
    TEST_CASE(wadsworth_example, "(λ (0 0))");
    TEST_CASE(wadsworth_counterexample, "(λ (λ (1 0)))");
    TEST_CASE(why_factorial_test, "(λ (λ (1 (1 (1 (1 (1 (1 0))))))))");

    close_pools();
}

#endif // BENCHMARKS
