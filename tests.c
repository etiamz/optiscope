#include "optiscope.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Testing Machinery
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

static int exit_code = EXIT_SUCCESS;

#define TEST_CASE(f, expected) test_case(#f, f, expected)

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

    optiscope_open_pools();
    optiscope_algorithm(fp, f());
    optiscope_close_pools();

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
        optiscope_redirect_stream(fp, stderr);
        fprintf(stderr, "\n");

#undef TAB

        exit_code = EXIT_FAILURE;

        goto close_fp;
    }

    printf("Good: %s\n", test_case_name);

close_fp:
    if (0 != fclose(fp)) { perror("fclose"); }
}

// S, K, I Combinators
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

static struct lambda_term *
s_combinator(void) {
    struct lambda_term *x, *y, *z;

    return lambda(
        x,
        lambda(
            y, lambda(z, apply(apply(var(x), var(z)), apply(var(y), var(z))))));
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

static struct lambda_term *
skk_test(void) {
    return apply(apply(s_combinator(), k_combinator()), k_combinator());
}

static struct lambda_term *
sksk_test(void) {
    return apply(
        apply(apply(s_combinator(), k_combinator()), s_combinator()),
        k_combinator());
}

static struct lambda_term *
ski_kis_test(void) {
    return apply(
        apply(apply(s_combinator(), k_combinator()), i_combinator()),
        apply(apply(k_combinator(), i_combinator()), s_combinator()));
}

static struct lambda_term *
sii_combinator(void) {
    return apply(apply(s_combinator(), i_combinator()), i_combinator());
}

static struct lambda_term *
sii_test(void) {
    return apply(sii_combinator(), apply(i_combinator(), i_combinator()));
}

static struct lambda_term *
iota_combinator_test(void) {
    return apply(
        apply(
            s_combinator(),
            apply(
                apply(s_combinator(), i_combinator()),
                apply(k_combinator(), s_combinator()))),
        apply(k_combinator(), k_combinator()));
}

static struct lambda_term *
self_iota_combinator_test(void) {
    return apply(iota_combinator_test(), iota_combinator_test());
}

// B, C, W Combinators
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

static struct lambda_term *
b_combinator(void) {
    struct lambda_term *f, *g, *x;

    return lambda(
        f, lambda(g, lambda(x, apply(var(f), apply(var(g), var(x))))));
}

static struct lambda_term *
c_combinator(void) {
    struct lambda_term *f, *g, *x;

    return lambda(
        f, lambda(g, lambda(x, apply(apply(var(f), var(x)), var(g)))));
}

static struct lambda_term *
w_combinator(void) {
    struct lambda_term *f, *x;

    return lambda(f, lambda(x, apply(apply(var(f), var(x)), var(x))));
}

static struct lambda_term *
bcw_test(void) {
    return apply(
        apply(b_combinator(), apply(b_combinator(), w_combinator())),
        apply(apply(b_combinator(), b_combinator()), c_combinator()));
}

// Unary Arithmetic
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

// clang-format off
static uint64_t square(const uint64_t x) { return x * x; }

static uint64_t cube(const uint64_t x) { return x * x * x; }

static uint64_t halve(const uint64_t x) { return x / 2; }

static uint64_t plus_one(const uint64_t x) { return x + 1; }

static uint64_t minus_one(const uint64_t x) { return x - 1; }
// clang-format on

static struct lambda_term *
unary_arithmetic(void) {
    struct lambda_term *f, *x;

    return unary_call(
        halve,
        apply(
            lambda(f, apply(var(f), cell(4))),
            lambda(x, unary_call(cube, unary_call(square, var(x))))));
}

// Binary Arithmetic
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

// clang-format off
static uint64_t add(const uint64_t x, const uint64_t y)
    { return x + y; }

static uint64_t multiply(const uint64_t x, const uint64_t y)
    { return x * y; }

static uint64_t subtract(const uint64_t x, const uint64_t y)
    { return x - y; }

static uint64_t divide(const uint64_t x, const uint64_t y)
    { return x / y; }
// clang-format on

static struct lambda_term *
binary_arithmetic(void) {
    struct lambda_term *f, *x;

    return apply(
        lambda(
            f,
            binary_call(
                divide,
                binary_call(subtract, apply(var(f), cell(10)), cell(8)),
                cell(2))),
        lambda(
            x,
            binary_call(multiply, binary_call(add, var(x), cell(5)), cell(2))));
}

// Conditional Logic With Recursion
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

// clang-format off
static uint64_t equals(const uint64_t x, const uint64_t y)
    { return x == y; }
// clang-format on

static struct lambda_term *
conditionals(void) {
    struct lambda_term *x;

    return if_then_else(
        apply(
            lambda(
                x,
                if_then_else(
                    binary_call(equals, var(x), cell(100)), cell(0), cell(1))),
            cell(100)),
        cell(5),
        cell(10));
}

// clang-format off
static uint64_t is_zero(const uint64_t x) { return 0 == x; }

static uint64_t is_one(const uint64_t x) { return 1 == x; }
// clang-format on

static struct lambda_term *
fibonacci_function(
    struct lambda_term *const restrict rec,
    struct lambda_term *const restrict recx) {
    struct lambda_term *n;

    return lambda(
        n,
        if_then_else(
            unary_call(is_zero, var(n)),
            cell(0),
            if_then_else(
                unary_call(is_one, var(n)),
                cell(1),
                binary_call(
                    add,
                    apply(rec, binary_call(subtract, var(n), cell(1))),
                    apply(recx, binary_call(subtract, var(n), cell(2)))))));
}

static struct lambda_term *
fix_fibonacci_term(void) {
    struct lambda_term *rec;

    return fix(lambda(rec, fibonacci_function(var(rec), var(rec))));
}

static struct lambda_term *
fibonacci_term(void) {
    return fibonacci_function(expand(fibonacci_term), expand(fibonacci_term));
}

static struct lambda_term *
fix_fibonacci_test(void) {
    return apply(fix_fibonacci_term(), cell(10));
}

static struct lambda_term *
fibonacci_test(void) {
    return apply(fibonacci_term(), cell(10));
}

// Church Booleans
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
        p, lambda(a, lambda(b, apply(apply(var(p), var(b)), var(a)))));
}

static struct lambda_term *
church_and(void) {
    struct lambda_term *p, *q;

    return lambda(p, lambda(q, apply(apply(var(p), var(q)), var(p))));
}

static struct lambda_term *
church_or(void) {
    struct lambda_term *p, *q;

    return lambda(p, lambda(q, apply(apply(var(p), var(p)), var(q))));
}

static struct lambda_term *
church_xor(void) {
    struct lambda_term *p, *q;

    return lambda(
        p,
        lambda(
            q,
            apply(
                apply(
                    var(p),
                    apply(apply(var(q), church_false()), church_true())),
                apply(apply(var(q), church_true()), church_false()))));
}

static struct lambda_term *
church_if_then_else(void) {
    struct lambda_term *c, *t, *f;

    return lambda(
        c, lambda(t, lambda(f, apply(apply(var(c), var(t)), var(f)))));
}

#define CHURCH_IF_THEN_ELSE(c, t, f)                                           \
    apply(apply(apply(church_if_then_else(), c), t), f)

static struct lambda_term *
boolean_test(void) {
    return CHURCH_IF_THEN_ELSE(
        apply(apply(church_or(), church_true()), church_false()),
        apply(
            apply(
                church_xor(),
                apply(
                    apply(church_and(), church_true()),
                    apply(church_not(), church_false()))),
            church_false()),
        church_false());
}

// Church Numerals
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

static struct lambda_term *
church_zero(void) {
    struct lambda_term *f, *x;

    return lambda(f, lambda(x, var(x)));
}

static struct lambda_term *
church_one(void) {
    struct lambda_term *f, *x;

    return lambda(f, lambda(x, apply(var(f), var(x))));
}

static struct lambda_term *
church_two(void) {
    struct lambda_term *f, *x;

    return lambda(f, lambda(x, apply(var(f), apply(var(f), var(x)))));
}

static struct lambda_term *
church_three(void) {
    struct lambda_term *f, *x;

    return lambda(
        f, lambda(x, apply(var(f), apply(var(f), apply(var(f), var(x))))));
}

static struct lambda_term *
church_five(void) {
    struct lambda_term *f, *x;

    return lambda(
        f,
        lambda(
            x,
            apply(
                var(f),
                apply(
                    var(f),
                    apply(var(f), apply(var(f), apply(var(f), var(x))))))));
}

// The originall showcase test from the Lambdascope paper.
static struct lambda_term *
church_two_two_test(void) {
    return apply(church_two(), church_two());
}

static struct lambda_term *
church_two_two_two_test(void) {
    return apply(apply(church_two(), church_two()), church_two());
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
                    apply(
                        apply(var(m), var(f)),
                        apply(apply(var(n), var(f)), var(x)))))));
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
                    x, apply(apply(var(m), apply(var(n), var(f))), var(x))))));
}

static struct lambda_term *
church_one_plus_two_times_five_test(void) {
    return apply(
        apply(
            church_multiply(),
            apply(apply(church_add(), church_one()), church_two())),
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
                apply(
                    apply(
                        apply(
                            var(n),
                            lambda(
                                g,
                                lambda(
                                    h, apply(var(h), apply(var(g), var(f)))))),
                        lambda(u, var(x))),
                    lambda(v, var(v))))));
}

static struct lambda_term *
church_predecessor2x(void) {
    struct lambda_term *n;

    return lambda(
        n, apply(church_predecessor(), apply(church_predecessor(), var(n))));
}

static struct lambda_term *
church_five_predecessor2x(void) {
    return apply(church_predecessor2x(), church_five());
}

// Iterative Factorial
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

static struct lambda_term *
church_pair(void) {
    struct lambda_term *x, *y, *z;

    return lambda(
        x, lambda(y, lambda(z, apply(apply(var(z), var(x)), var(y)))));
}

static struct lambda_term *
church_first(void) {
    struct lambda_term *p;

    return lambda(p, apply(var(p), church_true()));
}

static struct lambda_term *
church_second(void) {
    struct lambda_term *p;

    return lambda(p, apply(var(p), church_false()));
}

static struct lambda_term *
factorial_step_term(void) {
    struct lambda_term *p;

    return lambda(
        p,
        apply(
            apply(
                church_pair(),
                apply(
                    apply(church_multiply(), apply(church_first(), var(p))),
                    apply(church_second(), var(p)))),
            apply(church_predecessor(), apply(church_second(), var(p)))));
}

static struct lambda_term *
factorial_term(void) {
    struct lambda_term *n;

    return lambda(
        n,
        apply(
            church_first(),
            apply(
                apply(var(n), factorial_step_term()),
                apply(apply(church_pair(), church_one()), var(n)))));
}

static struct lambda_term *
factorial_of_three_test(void) {
    return apply(factorial_term(), church_three());
}

// Y Combinator
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

static struct lambda_term *
y_combinator(void) {
    struct lambda_term *f, *x, *y;

    return lambda(
        f,
        apply(
            lambda(x, apply(var(f), apply(var(x), var(x)))),
            lambda(y, apply(var(f), apply(var(y), var(y))))));
}

static struct lambda_term *
church_is_zero(void) {
    struct lambda_term *n, *x;

    return lambda(
        n, apply(apply(var(n), lambda(x, church_false())), church_true()));
}

static struct lambda_term *
church_is_one(void) {
    struct lambda_term *n;

    // Assuming that `n` is positive.
    return lambda(
        n, apply(church_is_zero(), apply(church_predecessor(), var(n))));
}

static struct lambda_term *
church_y_factorial_function(void) {
    struct lambda_term *f, *n;

    return lambda(
        f,
        lambda(
            n,
            CHURCH_IF_THEN_ELSE(
                apply(church_is_zero(), var(n)),
                church_one(),
                apply(
                    apply(church_multiply(), var(n)),
                    apply(var(f), apply(church_predecessor(), var(n)))))));
}

static struct lambda_term *
church_y_factorial_term(void) {
    return apply(y_combinator(), church_y_factorial_function());
}

static struct lambda_term *
church_fix_factorial_term(void) {
    return fix(church_y_factorial_function());
}

static struct lambda_term *
church_y_factorial_test(void) {
    return apply(church_y_factorial_term(), church_three());
}

static struct lambda_term *
church_fix_factorial_test(void) {
    return apply(church_fix_factorial_term(), church_three());
}

static struct lambda_term *
church_y_fibonacci_function(void) {
    struct lambda_term *rec, *n;

    return lambda(
        rec,
        lambda(
            n,
            CHURCH_IF_THEN_ELSE(
                apply(church_is_zero(), var(n)),
                church_zero(),
                CHURCH_IF_THEN_ELSE(
                    apply(church_is_one(), var(n)),
                    church_one(),
                    apply(
                        apply(
                            church_add(),
                            apply(
                                var(rec), apply(church_predecessor(), var(n)))),
                        apply(
                            var(rec),
                            apply(church_predecessor2x(), var(n))))))));
}

static struct lambda_term *
church_y_fibonacci_term(void) {
    return apply(y_combinator(), church_y_fibonacci_function());
}

static struct lambda_term *
church_fix_fibonacci_term(void) {
    return fix(church_y_fibonacci_function());
}

static struct lambda_term *
church_y_fibonacci_test(void) {
    return apply(church_y_fibonacci_term(), church_three());
}

static struct lambda_term *
church_fix_fibonacci_test(void) {
    return apply(church_fix_fibonacci_term(), church_three());
}

// WHY Combinator
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

// Suggested by Marvin Borner <git@marvinborner.de>.
static struct lambda_term *
why_combinator(void) {
    struct lambda_term *f, *u, *a, *f2, *d, *u2, *i, *x;

    return lambda(
        f,
        apply(
            lambda(
                u,
                apply(
                    apply(
                        var(u),
                        lambda(
                            a,
                            lambda(f2, apply(apply(var(f2), var(a)), var(a))))),
                    var(u))),
            lambda(
                d,
                lambda(
                    u2,
                    apply(
                        var(f),
                        lambda(
                            i,
                            apply(
                                apply(apply(var(i), var(d)), var(u2)),
                                lambda(x, apply(var(x), var(d))))))))));
}

static struct lambda_term *
church_why_factorial_function(void) {
    struct lambda_term *rec, *n;

    return lambda(
        rec,
        lambda(
            n,
            CHURCH_IF_THEN_ELSE(
                apply(church_is_zero(), var(n)),
                church_one(),
                apply(
                    apply(church_multiply(), var(n)),
                    apply(
                        apply(var(rec), i_combinator()),
                        apply(church_predecessor(), var(n)))))));
}

static struct lambda_term *
church_why_factorial_term(void) {
    return apply(why_combinator(), church_why_factorial_function());
}

static struct lambda_term *
church_why_factorial_test(void) {
    return apply(church_why_factorial_term(), church_three());
}

// Church Lists
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
                    apply(
                        apply(var(f), var(h)),
                        apply(apply(var(t), var(f)), var(n)))))));
}

static struct lambda_term *
church_list_1_2_3(void) {
    return apply(
        apply(church_cons(), cell(1)),
        apply(
            apply(church_cons(), cell(2)),
            apply(apply(church_cons(), cell(3)), church_nil())));
}

static struct lambda_term *
church_sum_list(void) {
    struct lambda_term *list, *x, *y;

    return lambda(
        list,
        apply(
            apply(
                var(list),
                lambda(x, lambda(y, binary_call(add, var(x), var(y))))),
            cell(0)));
}

static struct lambda_term *
church_sum_list_test(void) {
    return apply(church_sum_list(), church_list_1_2_3());
}

static struct lambda_term *
church_reverse(void) {
    struct lambda_term *xs, *x, *k, *f, *n;

    return lambda(
        xs,
        apply(
            apply(
                var(xs),
                lambda(
                    x,
                    lambda(
                        k,
                        lambda(
                            f,
                            lambda(
                                n,
                                apply(
                                    apply(var(k), var(f)),
                                    apply(apply(var(f), var(x)), var(n)))))))),
            church_nil()));
}

static struct lambda_term *
church_reverse_test(void) {
    return apply(church_reverse(), church_list_1_2_3());
}

static struct lambda_term *
church_append(void) {
    struct lambda_term *xs, *ys, *f, *n;

    return lambda(
        xs,
        lambda(
            ys,
            lambda(
                f,
                lambda(
                    n,
                    apply(
                        apply(var(xs), var(f)),
                        apply(apply(var(ys), var(f)), var(n)))))));
}

static struct lambda_term *
church_list_4_5_6(void) {
    return apply(
        apply(church_cons(), cell(4)),
        apply(
            apply(church_cons(), cell(5)),
            apply(apply(church_cons(), cell(6)), church_nil())));
}

static struct lambda_term *
church_append_test(void) {
    return apply(
        apply(church_append(), church_list_1_2_3()), church_list_4_5_6());
}

// Scott Numerals
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

static struct lambda_term *
scott_zero(void) {
    struct lambda_term *s, *z;

    return lambda(s, lambda(z, var(z)));
}

static struct lambda_term *
scott_one(void) {
    struct lambda_term *s, *z;

    return lambda(s, lambda(z, apply(var(s), scott_zero())));
}

static struct lambda_term *
scott_two(void) {
    struct lambda_term *s, *z;

    return lambda(s, lambda(z, apply(var(s), scott_one())));
}

static struct lambda_term *
scott_three(void) {
    struct lambda_term *s, *z;

    return lambda(s, lambda(z, apply(var(s), scott_two())));
}

static struct lambda_term *
scott_successor(void) {
    struct lambda_term *n, *s, *z;

    return lambda(n, lambda(s, lambda(z, apply(var(s), var(n)))));
}

static struct lambda_term *
scott_predecessor(void) {
    struct lambda_term *n, *x;

    return lambda(n, apply(apply(var(n), lambda(x, var(x))), scott_zero()));
}

static struct lambda_term *
scott_three_successor_predecessor2x_test(void) {
    return apply(
        scott_predecessor(),
        apply(scott_predecessor(), apply(scott_successor(), scott_three())));
}

// Scott Lists
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

static struct lambda_term *
scott_nil(void) {
    struct lambda_term *n, *c;

    return lambda(n, lambda(c, var(n)));
}

static struct lambda_term *
scott_cons(void) {
    struct lambda_term *h, *t, *n, *c;

    return lambda(
        h,
        lambda(t, lambda(n, lambda(c, apply(apply(var(c), var(h)), var(t))))));
}

static struct lambda_term *
scott_singleton(void) {
    struct lambda_term *x;

    return lambda(x, apply(apply(scott_cons(), var(x)), scott_nil()));
}

static struct lambda_term *
scott_sum_list(void) {
    struct lambda_term *list, *x, *xs;

    return lambda(
        list,
        apply(
            apply(var(list), cell(0)),
            lambda(
                x,
                lambda(
                    xs,
                    binary_call(
                        add,
                        var(x),
                        apply(expand(scott_sum_list), var(xs)))))));
}

static struct lambda_term *
scott_list_1_2_3_4_5(void) {
    return apply(
        apply(scott_cons(), cell(1)),
        apply(
            apply(scott_cons(), cell(2)),
            apply(
                apply(scott_cons(), cell(3)),
                apply(
                    apply(scott_cons(), cell(4)),
                    apply(apply(scott_cons(), cell(5)), scott_nil())))));
}

static struct lambda_term *
scott_sum_list_test(void) {
    return apply(scott_sum_list(), scott_list_1_2_3_4_5());
}

// clang-format off
static uint64_t less_than_or_equal(const uint64_t x, const uint64_t y)
    { return x <= y; }
// clang-format on

static struct lambda_term *
scott_insert(void) {
    struct lambda_term *y, *list, *z, *zs;

    return lambda(
        y,
        lambda(
            list,
            apply(
                apply(var(list), apply(expand(scott_singleton), var(y))),
                lambda(
                    z,
                    lambda(
                        zs,
                        if_then_else(
                            binary_call(less_than_or_equal, var(y), var(z)),
                            apply(
                                apply(expand(scott_cons), var(y)),
                                apply(
                                    apply(expand(scott_cons), var(z)),
                                    var(zs))),
                            apply(
                                apply(expand(scott_cons), var(z)),
                                apply(
                                    apply(expand(scott_insert), var(y)),
                                    var(zs)))))))));
}

static struct lambda_term *
scott_insertion_sort(void) {
    struct lambda_term *list, *x, *xs;

    return lambda(
        list,
        apply(
            apply(var(list), expand(scott_nil)),
            lambda(
                x,
                lambda(
                    xs,
                    apply(
                        apply(expand(scott_insert), var(x)),
                        apply(expand(scott_insertion_sort), var(xs)))))));
}

// clang-format off
static uint64_t concatenate_ints(uint64_t x, const uint64_t y) {
    uint64_t z = y;
    do { x *= 10; } while (z /= 10);
    return x + y;
}
// clang-format on

static struct lambda_term *
scott_concatenate_list(void) {
    struct lambda_term *list, *x, *xs;

    return lambda(
        list,
        apply(
            apply(var(list), cell(0)),
            lambda(
                x,
                lambda(
                    xs,
                    binary_call(
                        concatenate_ints,
                        var(x),
                        apply(expand(scott_concatenate_list), var(xs)))))));
}

static struct lambda_term *
scott_list_9_2_7_3_8_1_4(void) {
    return apply(
        apply(scott_cons(), cell(9)),
        apply(
            apply(scott_cons(), cell(2)),
            apply(
                apply(scott_cons(), cell(7)),
                apply(
                    apply(scott_cons(), cell(3)),
                    apply(
                        apply(scott_cons(), cell(8)),
                        apply(
                            apply(scott_cons(), cell(1)),
                            apply(
                                apply(scott_cons(), cell(4)),
                                scott_nil())))))));
}

static struct lambda_term *
scott_insertion_sort_test(void) {
    return apply(
        scott_concatenate_list(),
        apply(scott_insertion_sort(), scott_list_9_2_7_3_8_1_4()));
}

// Scott Quicksort
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

// clang-format off
static uint64_t less_than(const uint64_t x, const uint64_t y)
    { return x < y; }

static uint64_t greater_than_or_equal(const uint64_t x, const uint64_t y)
    { return x >= y; }
// clang-format on

static struct lambda_term *
scott_filter(void) {
    struct lambda_term *f, *list, *x, *xs;

    return lambda(
        f,
        lambda(
            list,
            apply(
                apply(var(list), expand(scott_nil)),
                lambda(
                    x,
                    lambda(
                        xs,
                        if_then_else(
                            apply(var(f), var(x)),
                            apply(
                                apply(expand(scott_cons), var(x)),
                                apply(
                                    apply(expand(scott_filter), var(f)),
                                    var(xs))),
                            apply(
                                apply(expand(scott_filter), var(f)),
                                var(xs))))))));
}

static struct lambda_term *
scott_append(void) {
    struct lambda_term *xs, *ys, *x, *xss;

    return lambda(
        xs,
        lambda(
            ys,
            apply(
                apply(var(xs), var(ys)),
                lambda(
                    x,
                    lambda(
                        xss,
                        apply(
                            apply(expand(scott_cons), var(x)),
                            apply(
                                apply(expand(scott_append), var(xss)),
                                var(ys))))))));
}

static struct lambda_term *
scott_quicksort(void) {
    struct lambda_term *list, *x, *xs, *y, *z;

    // clang-format off
    return lambda(list,
        apply(apply(var(list), expand(scott_nil)),
            lambda(x, lambda(xs, apply(apply(expand(scott_append),
                apply(expand(scott_quicksort),
                    apply(
                        apply(expand(scott_filter),
                            lambda(y, binary_call(less_than, var(y), var(x)))),
                        var(xs)))),
                apply(apply(expand(scott_cons), var(x)),
                    apply(expand(scott_quicksort),
                        apply(
                            apply(expand(scott_filter),
                                lambda(z, binary_call(greater_than_or_equal,
                                    var(z), var(x)))),
                            var(xs)))))))));
    // clang-format on
}

static struct lambda_term *
scott_quicksort_test(void) {
    return apply(
        scott_concatenate_list(),
        apply(scott_quicksort(), scott_list_9_2_7_3_8_1_4()));
}

// Scott Merge Sort
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

static struct lambda_term *
scott_split(void) {
    struct lambda_term *list, *k, *x, *xs, *y, *ys, *left, *right;

    // clang-format off
    return lambda(list, lambda(k, apply(
        apply(
            var(list),
            apply(apply(var(k), expand(scott_nil)), expand(scott_nil))),
        lambda(x, lambda(xs, apply(
            apply(
                var(xs),
                apply(
                    apply(var(k), apply(expand(scott_singleton), var(x))),
                    expand(scott_nil))),
            lambda(y, lambda(ys, apply(
                apply(expand(scott_split), var(ys)),
                lambda(left, lambda(right, apply(
                    apply(
                        var(k),
                        apply(apply(expand(scott_cons), var(x)), var(left))),
                    apply(
                        apply(expand(scott_cons), var(y)),
                        var(right))))))))))))));
    // clang-format on
}

static struct lambda_term *
scott_merge(void) {
    struct lambda_term *xs, *ys, *x, *xss, *y, *yss;

    // clang-format off
    return lambda(xs, lambda(ys, apply(
        apply(var(ys), var(xs)),
        lambda(y, lambda(yss, apply(
            apply(var(xs), var(ys)),
            lambda(x, lambda(xss, if_then_else(
                binary_call(less_than, var(x), var(y)),
                apply(
                    apply(expand(scott_cons), var(x)),
                    apply(apply(expand(scott_merge), var(xss)), var(ys))),
                apply(
                    apply(expand(scott_cons), var(y)),
                    apply(
                        apply(expand(scott_merge), var(xs)),
                        var(yss))))))))))));
    // clang-format on
}

static struct lambda_term *
scott_merge_sort(void) {
    struct lambda_term *list, *x, *xs, *left, *right, *dummy, *dummyx;

    // clang-format off
    return lambda(list, apply(
        apply(var(list), expand(scott_nil)),
        lambda(x, lambda(xs, apply(
            apply(var(xs), apply(expand(scott_singleton), var(x))),
            lambda(dummy, lambda(dummyx, apply(
                apply(expand(scott_split), var(list)),
                lambda(left, lambda(right, apply(
                    apply(
                        expand(scott_merge),
                        apply(expand(scott_merge_sort), var(left))),
                    apply(
                        expand(scott_merge_sort),
                        var(right)))))))))))));
    // clang-format on
}

static struct lambda_term *
scott_merge_sort_test(void) {
    return apply(
        scott_concatenate_list(),
        apply(scott_merge_sort(), scott_list_9_2_7_3_8_1_4()));
}

// Scott Bubble Sort
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

static struct lambda_term *
scott_bubble_swap(void) {
    struct lambda_term *list, *n, *x, *xs, *y, *ys;

    // clang-format off
    return lambda(list, lambda(n, if_then_else(
        unary_call(is_zero, var(n)),
        var(list),
        apply(apply(var(list), var(list)), lambda(x, lambda(xs,
            apply(apply(var(xs), var(list)), lambda(y, lambda(ys, if_then_else(
                binary_call(less_than, var(x), var(y)),
                apply(
                    apply(expand(scott_cons), var(x)),
                    apply(
                        apply(expand(scott_bubble_swap), var(xs)),
                        unary_call(minus_one, var(n)))),
                apply(
                    apply(expand(scott_cons), var(y)),
                    apply(
                        apply(
                            expand(scott_bubble_swap),
                            apply(apply(expand(scott_cons), var(x)),
                                var(ys))),
                    unary_call(minus_one, var(n))))))))))))));
    // clang-format on
}

static struct lambda_term *
scott_bubble_go(void) {
    struct lambda_term *list, *n;

    return lambda(
        list,
        lambda(
            n,
            if_then_else(
                unary_call(is_zero, var(n)),
                var(list),
                apply(
                    apply(
                        expand(scott_bubble_go),
                        apply(
                            apply(expand(scott_bubble_swap), var(list)),
                            var(n))),
                    unary_call(minus_one, var(n))))));
}

static struct lambda_term *
scott_list_length(void) {
    struct lambda_term *list, *x, *xs;

    return lambda(
        list,
        apply(
            apply(var(list), cell(0)),
            lambda(
                x,
                lambda(
                    xs,
                    unary_call(
                        plus_one,
                        apply(expand(scott_list_length), var(xs)))))));
}

static struct lambda_term *
scott_bubble_sort(void) {
    struct lambda_term *list, *x, *xs;

    return lambda(
        list,
        apply(
            apply(var(list), expand(scott_nil)),
            lambda(
                x,
                lambda(
                    xs,
                    apply(
                        apply(expand(scott_bubble_go), var(list)),
                        unary_call(
                            minus_one,
                            apply(expand(scott_list_length), var(list))))))));
}

static struct lambda_term *
scott_bubble_sort_test(void) {
    return apply(
        scott_concatenate_list(),
        apply(scott_bubble_sort(), scott_list_9_2_7_3_8_1_4()));
}

// N-Queens
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

static struct lambda_term *
scott_member(void) {
    struct lambda_term *x, *list, *y, *ys;

    return lambda(
        x,
        lambda(
            list,
            apply(
                apply(var(list), cell(0)),
                lambda(
                    y,
                    lambda(
                        ys,
                        if_then_else(
                            binary_call(equals, var(x), var(y)),
                            cell(1),
                            apply(
                                apply(expand(scott_member), var(x)),
                                var(ys))))))));
}

static struct lambda_term *
scott_threat(void) {
    struct lambda_term *k, *m, *list, *x, *xs;

    return lambda(
        k,
        lambda(
            m,
            lambda(
                list,
                apply(
                    apply(var(list), cell(0)),
                    lambda(
                        x,
                        lambda(
                            xs,
                            if_then_else(
                                binary_call(
                                    equals,
                                    var(k),
                                    binary_call(subtract, var(x), var(m))),
                                cell(1),
                                if_then_else(
                                    binary_call(
                                        equals,
                                        var(k),
                                        binary_call(subtract, var(m), var(x))),
                                    cell(1),
                                    apply(
                                        apply(
                                            apply(
                                                expand(scott_threat),
                                                unary_call(plus_one, var(k))),
                                            var(m)),
                                        var(xs))))))))));
}

static struct lambda_term *
scott_queen_aux(void) {
    struct lambda_term *m, *b, *n;

#define QUEEN_AUX_CALL                                                         \
    apply(                                                                     \
        apply(                                                                 \
            apply(expand(scott_queen_aux), unary_call(minus_one, var(m))),     \
            var(b)),                                                           \
        var(n))

    // clang-format off
    return lambda(m, lambda(b, lambda(n, if_then_else(
            binary_call(equals, var(m), cell(0)),
            expand(scott_nil),
            if_then_else(
                apply(apply(expand(scott_member), var(m)), var(b)),
                QUEEN_AUX_CALL,
                if_then_else(
                    apply(apply(apply(expand(scott_threat), cell(1)), var(m)), var(b)),
                    QUEEN_AUX_CALL,
                    if_then_else(
                        binary_call(
                            equals,
                            apply(expand(scott_list_length), var(b)),
                            unary_call(minus_one, var(n))),
                        apply(
                            apply(
                                expand(scott_append),
                                apply(
                                    apply(
                                        expand(scott_cons),
                                        apply(apply(expand(scott_cons), var(m)), var(b))),
                                    expand(scott_nil))),
                            QUEEN_AUX_CALL),
                        apply(
                            apply(
                                expand(scott_append),
                                apply(
                                    apply(
                                        apply(expand(scott_queen_aux), var(n)),
                                        apply(
                                            apply(expand(scott_cons), var(m)),
                                            var(b))),
                                    var(n))),
                            QUEEN_AUX_CALL))))))));
    // clang-format on

#undef QUEEN_AUX_CALL
}

static struct lambda_term *
scott_queen(void) {
    struct lambda_term *n;

    return lambda(
        n,
        apply(
            apply(apply(expand(scott_queen_aux), var(n)), expand(scott_nil)),
            var(n)));
}

static struct lambda_term *
scott_nqueens_test(void) {
    return apply(scott_list_length(), apply(scott_queen(), cell(6)));
}

// Scott Binary Trees
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

static struct lambda_term *
scott_leaf(void) {
    struct lambda_term *v, *leaf, *node;

    return lambda(v, lambda(leaf, lambda(node, apply(var(leaf), var(v)))));
}

static struct lambda_term *
scott_node(void) {
    struct lambda_term *lhs, *rhs, *leaf, *node;

    return lambda(
        lhs,
        lambda(
            rhs,
            lambda(
                leaf,
                lambda(node, apply(apply(var(node), var(lhs)), var(rhs))))));
}

static struct lambda_term *
scott_tree_sum(void) {
    struct lambda_term *tree, *v, *lhs, *rhs;

    return lambda(
        tree,
        apply(
            apply(var(tree), lambda(v, var(v))),
            lambda(
                lhs,
                lambda(
                    rhs,
                    binary_call(
                        add,
                        apply(expand(scott_tree_sum), var(lhs)),
                        apply(expand(scott_tree_sum), var(rhs)))))));
}

static struct lambda_term *
scott_tree_map(void) {
    struct lambda_term *f, *tree, *v, *lhs, *rhs;

    // clang-format off
    return lambda(f, lambda(tree,
        apply(
            apply(
                var(tree),
                lambda(v, apply(scott_leaf(), apply(var(f), var(v))))),
            lambda(lhs, lambda(rhs,
                apply(
                    apply(
                        scott_node(),
                        apply(apply(expand(scott_tree_map), var(f)), var(lhs))),
                    apply(
                        apply(expand(scott_tree_map), var(f)), var(rhs))))))));
    // clang-format on
}

static struct lambda_term *
scott_example_tree(void) {
    return apply(
        apply(
            scott_node(),
            apply(
                apply(scott_node(), apply(scott_leaf(), cell(1))),
                apply(scott_leaf(), cell(2)))),
        apply(
            apply(scott_node(), apply(scott_leaf(), cell(3))),
            apply(scott_leaf(), cell(4))));
}

static struct lambda_term *
scott_tree_sum_test(void) {
    return apply(scott_tree_sum(), scott_example_tree());
}

static struct lambda_term *
scott_tree_map_and_sum_test(void) {
    struct lambda_term *x;

    return apply(
        scott_tree_sum(),
        apply(
            apply(
                scott_tree_map(),
                lambda(x, binary_call(multiply, var(x), cell(2)))),
            scott_example_tree()));
}

// Ackermann Function
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

static struct lambda_term *
ackermann_function(
    struct lambda_term *const restrict rec,
    struct lambda_term *const restrict recx,
    struct lambda_term *const restrict recxx) {
    struct lambda_term *m, *n;

    return lambda(
        m,
        lambda(
            n,
            if_then_else(
                unary_call(is_zero, var(m)),
                unary_call(plus_one, var(n)),
                if_then_else(
                    unary_call(is_zero, var(n)),
                    apply(apply(rec, unary_call(minus_one, var(m))), cell(1)),
                    apply(
                        apply(recx, unary_call(minus_one, var(m))),
                        apply(
                            apply(recxx, var(m)),
                            unary_call(minus_one, var(n))))))));
}

static struct lambda_term *
fix_ackermann_term(void) {
    struct lambda_term *rec;

    return fix(lambda(rec, ackermann_function(var(rec), var(rec), var(rec))));
}

static struct lambda_term *
ackermann_term(void) {
    return ackermann_function(
        expand(ackermann_term), expand(ackermann_term), expand(ackermann_term));
}

static struct lambda_term *
fix_ackermann_test(void) {
    return apply(apply(fix_ackermann_term(), cell(3)), cell(3));
}

static struct lambda_term *
ackermann_test(void) {
    return apply(apply(ackermann_term(), cell(3)), cell(3));
}

// Examples From Literature
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

static struct lambda_term *
lamping_example(void) { // Originally Lévy's
    struct lambda_term *g, *x, *h, *f, *z, *w, *y;

    return apply(
        lambda(g, apply(var(g), apply(var(g), lambda(x, var(x))))),
        lambda(
            h,
            apply(
                lambda(f, apply(var(f), apply(var(f), lambda(z, var(z))))),
                lambda(w, apply(var(h), apply(var(w), lambda(y, var(y))))))));
}

static struct lambda_term *
lamping_example_2(void) {
    struct lambda_term *g, *x, *h, *f, *z, *y;

    return apply(
        lambda(g, apply(var(g), apply(var(g), lambda(x, var(x))))),
        lambda(
            h,
            apply(
                lambda(f, apply(var(f), apply(var(f), lambda(z, var(z))))),

                apply(var(h), lambda(y, var(y))))));
}

static struct lambda_term *
asperti_guerrini_example(void) {
    struct lambda_term *z, *v, *w, *x, *y;

    struct lambda_term *once = lambda(v, var(v));
    struct lambda_term *twice = lambda(w, apply(var(w), var(w)));

    return lambda(
        z,
        apply(
            lambda(x, apply(var(x), once)),
            lambda(y, apply(twice, apply(var(y), var(z))))));
}

static struct lambda_term *
wadsworth_example(void) { // Asperti & Guerrini
    struct lambda_term *v, *w, *x, *y;

    struct lambda_term *once = lambda(v, var(v));
    struct lambda_term *twice = lambda(w, apply(var(w), var(w)));

    return lambda(
        y, apply(twice, apply(lambda(x, apply(var(x), var(y))), once)));
}

static struct lambda_term *
wadsworth_counterexample(void) { // Asperti & Guerrini
    struct lambda_term *v, *w, *x, *y, *z;

    struct lambda_term *once = lambda(v, var(v));

    return lambda(
        y,
        lambda(
            z,
            apply(
                lambda(x, apply(apply(var(x), var(y)), apply(var(x), var(z)))),
                lambda(w, apply(once, var(w))))));
}

// Test Driver
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#ifndef OPTISCOPE_TESTS_NO_MAIN

int
main(void) {
    puts("Running the test cases...");

    TEST_CASE(skk_test, "(λ 0)");
    TEST_CASE(sksk_test, "(λ (λ 1))");
    TEST_CASE(ski_kis_test, "(λ 0)");
    TEST_CASE(sii_test, "(λ 0)");
    TEST_CASE(
        iota_combinator_test, "(λ ((0 (λ (λ (λ ((2 0) (1 0)))))) (λ (λ 1))))");
    TEST_CASE(self_iota_combinator_test, "(λ 0)");
    TEST_CASE(bcw_test, "(λ (λ (λ ((2 0) (1 0)))))");
    TEST_CASE(unary_arithmetic, "cell[2048]");
    TEST_CASE(binary_arithmetic, "cell[11]");
    TEST_CASE(conditionals, "cell[10]");
    TEST_CASE(fix_fibonacci_test, "cell[55]");
    TEST_CASE(fibonacci_test, "cell[55]");
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
    TEST_CASE(church_y_factorial_test, "(λ (λ (1 (1 (1 (1 (1 (1 0))))))))");
    TEST_CASE(church_fix_factorial_test, "(λ (λ (1 (1 (1 (1 (1 (1 0))))))))");
    TEST_CASE(church_y_fibonacci_test, "(λ (λ (1 (1 0))))");
    TEST_CASE(church_fix_fibonacci_test, "(λ (λ (1 (1 0))))");
    TEST_CASE(church_why_factorial_test, "(λ (λ (1 (1 (1 (1 (1 (1 0))))))))");
    TEST_CASE(church_sum_list_test, "cell[6]");
    TEST_CASE(
        church_reverse_test,
        "(λ (λ ((1 cell[3]) ((1 cell[2]) ((1 cell[1]) 0)))))");
    TEST_CASE(
        church_append_test,
        "(λ (λ ((1 cell[1]) ((1 cell[2]) ((1 cell[3]) ((1 cell[4]) ((1 cell[5]) ((1 cell[6]) 0))))))))");
    TEST_CASE(
        scott_three_successor_predecessor2x_test,
        "(λ (λ (1 (λ (λ (1 (λ (λ 0))))))))");
    TEST_CASE(scott_sum_list_test, "cell[15]");
    TEST_CASE(scott_insertion_sort_test, "cell[12347890]");
    TEST_CASE(scott_quicksort_test, "cell[12347890]");
    TEST_CASE(scott_merge_sort_test, "cell[12347890]");
    TEST_CASE(scott_bubble_sort_test, "cell[12347890]");
    TEST_CASE(scott_nqueens_test, "cell[4]");
    TEST_CASE(scott_tree_sum_test, "cell[10]");
    TEST_CASE(scott_tree_map_and_sum_test, "cell[20]");
    TEST_CASE(fix_ackermann_test, "cell[61]");
    TEST_CASE(ackermann_test, "cell[61]");
    TEST_CASE(lamping_example, "(λ 0)");
    TEST_CASE(lamping_example_2, "(λ 0)");
    TEST_CASE(asperti_guerrini_example, "(λ (0 0))");
    TEST_CASE(wadsworth_example, "(λ (0 0))");
    TEST_CASE(wadsworth_counterexample, "(λ (λ (1 0)))");

    return exit_code;
}

#endif // OPTISCOPE_TESTS_NO_MAIN
