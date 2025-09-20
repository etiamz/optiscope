#include "../optiscope.h"

#include <stdlib.h>
#include <string.h>

static uint64_t
my_puts(const uint64_t s, const uint64_t token) {
    (void)token;

    return (uint64_t)puts((const char *)s);
}

static uint64_t
my_gets(const uint64_t token) {
    (void)token;

    char *const s = malloc(256);
    if (NULL == s) { perror("malloc"), abort(); }
    scanf("%255s", s);
    return (uint64_t)s;
}

static uint64_t
my_free(const uint64_t s, const uint64_t token) {
    (void)token;

    free((void *)s);
    return 0;
}

static uint64_t
my_strcmp(const uint64_t s1, const uint64_t s2) {
    return 0 == strcmp((const char *)s1, (const char *)s2) ? 1 : 0;
}

static uint64_t
is_palindrome(uint64_t s) {
    const char *const chars = (const char *)s;
    const size_t len = strlen(chars);

    for (size_t i = 0; i < len / 2; i++) {
        if (chars[i] != chars[len - 1 - i]) { return 0; }
    }

    return 1;
}

// Step-by-step:
//  1. We enclose the whole program in `fix`, which is our built-in fixed-point
//     combinator. The `rec` parameter stands for the current lambda function to
//     be invoked recursively.
//  2. Next, we accept the parameter `token`, which stands for an _effect
//     token_. This token is threaded through all side-effectful operations to
//     force re-evaluation of side effects.
//  3. The first thing we doe in the function body is calling `my_puts`. Here,
//     `my_puts` is an ordinary C function that accepts `s`, the string to be
//     printed, & `token`. By calling `perform`, we _force_ the evaluation of
//     `my_puts` to be performed _right now_.
//  4. We bind the call of `my_gets` to the variable `s`. Here, the execution of
//     `my_gets` will also be forced according to `bind`; the rest of the
//     program will deal with already evaluated `s`.
//  5. We proceed with calling `my_strcmp`. If the string is `"quit"`, we
//     manually call `my_free` & finish the evaluation. We doe not need to force
//     `my_free` here, because it appears in a tail call position.
//  6. If the input string is not `"quit"`, we force the evaluation of
//     `is_palindrome` with its both (side-effectfull) branches.
//  7. We finally force `my_free` to ensure no memory leaks occur, & proceed
//     with a recursive call.
static struct lambda_term *
program(void) {
    struct lambda_term *rec, *token, *s;

    // clang-format off
    return fix(lambda(rec, lambda(token, perform(
        binary_call(my_puts,
          cell((uint64_t)"Enter your palindrome or type 'quit':"), var(token)),
        bind(s,
          unary_call(my_gets, var(token)),
          if_then_else(
            binary_call(my_strcmp, var(s), cell((uint64_t)"quit")),
            binary_call(my_free, var(s), var(token)),
            perform(
              if_then_else(
                unary_call(is_palindrome, var(s)),
                binary_call(my_puts,
                  cell((uint64_t)"This is a palindrome!"), var(token)),
                binary_call(my_puts,
                  cell((uint64_t)"This isn't a palindrome."), var(token))),
              perform(
                binary_call(my_free, var(s), var(token)),
                apply(var(rec), var(token))))))))));
    // clang-format on
}

int
main(void) {
    optiscope_open_pools();
    optiscope_algorithm(NULL, apply(program(), cell(0)));
    optiscope_close_pools();
}
