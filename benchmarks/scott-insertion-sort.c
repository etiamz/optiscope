#define OPTISCOPE_TESTS_NO_MAIN
#include "../tests.c"

static struct lambda_term *
generate_list_go(void) {
    struct lambda_term *n, *i, *acc;

    // clang-format off
    return lambda(n, lambda(i, lambda(acc, if_then_else(
        binary_call(less_than, var(i), var(n)),
        apply(apply(apply(expand(generate_list_go),
            var(n)),
            unary_call(plus_one, var(i))),
            apply(apply(scott_cons(), var(i)), var(acc))),
        var(acc)))));
    // clang-format on
}

static struct lambda_term *
generate_list(void) {
    struct lambda_term *n;

    // clang-format off
    return lambda(n, apply(apply(apply(expand(generate_list_go),
        var(n)),
        cell(0)),
        scott_nil()));
    // clang-format on
}

#define BENCHMARK_TERM                                                         \
    apply(                                                                     \
        expand(scott_sum_list),                                                \
        apply(                                                                 \
            expand(scott_insertion_sort),                                      \
            apply(expand(generate_list), cell(1000))))

int
main(void) {
    optiscope_open_pools();
    optiscope_algorithm(NULL, BENCHMARK_TERM);
    optiscope_close_pools();
}
