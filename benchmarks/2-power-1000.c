#define BENCHMARKS

#include "../tests.c"

static struct lambda_term *
church_ten(void) {
    return applicator(
        applicator(church_multiply(), church_five()), church_two());
}

static struct lambda_term *
church_hundred(void) {
    return applicator(
        applicator(church_multiply(), church_ten()), church_ten());
}

static struct lambda_term *
church_thousand(void) {
    return applicator(
        applicator(church_multiply(), church_hundred()), church_ten());
}

int
main(void) {
    open_pools();
    algorithm(NULL, applicator(church_thousand(), church_two()));
    close_pools();
}
