#include "../optiscope.h"

static struct lambda_term *
church_two(void) {
    struct lambda_term *f, *x;

    return lambda(f, lambda(x, apply(var(f), apply(var(f), var(x)))));
}

static struct lambda_term *
church_two_two(void) { // from the Lambdascope paper
    return apply(church_two(), church_two());
}

int
main(void) {
    optiscope_open_pools();
    optiscope_algorithm(stdout, church_two_two());
    puts("");
    optiscope_close_pools();
}
