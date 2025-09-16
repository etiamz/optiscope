#define OPTISCOPE_TESTS_NO_MAIN
#include "../tests.c"

#define BENCHMARK_TERM                                                         \
    apply(apply(apply(expand(tak_term), cell(24)), cell(9)), cell(3))

int
main(void) {
    optiscope_open_pools();
    optiscope_algorithm(NULL, BENCHMARK_TERM);
    optiscope_close_pools();
}
