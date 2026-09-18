#define OPTISCOPE_TESTS_NO_MAIN
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function"
#include "../tests.c"
#pragma GCC diagnostic pop

#define BENCHMARK_TERM apply(apply(expand(ackermann_term), cell(3)), cell(8))

int
main(void) {
    optiscope_open_pools();
    optiscope_algorithm(NULL, BENCHMARK_TERM);
    optiscope_close_pools();
}
