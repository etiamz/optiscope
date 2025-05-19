#define OPTISCOPE_TESTS_NO_MAIN
#include "../tests.c"

#define BENCHMARK_TERM applicator(fix(fast_y_fibonacci_function()), cell(30))

int
main(void) {
    optiscope_open_pools();
    optiscope_algorithm(NULL, BENCHMARK_TERM);
    optiscope_close_pools();
}
