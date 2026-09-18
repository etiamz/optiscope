#define OPTISCOPE_TESTS_NO_MAIN
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function"
#include "../tests.c"
#pragma GCC diagnostic pop

#define BENCHMARK_TERM                                                         \
    apply(expand(scott_list_length), apply(expand(scott_queen), cell(10)))

int
main(void) {
    optiscope_open_pools();
    optiscope_algorithm(NULL, BENCHMARK_TERM);
    optiscope_close_pools();
}
