#define OPTISCOPE_TESTS_NO_MAIN
#include "../tests.c"

#define BENCHMARK_TERM apply(apply(expand(ackermann_term), cell(3)), cell(8))

int
main(void) {
    optiscope_open_pools();
    optiscope_algorithm(NULL, BENCHMARK_TERM);
    optiscope_close_pools();
}
