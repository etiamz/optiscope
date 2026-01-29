/*
BSD 3-Clause License

Copyright (c) 2025, Louis F. Ashfield

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

// Header Inclusions
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#include "optiscope.h"

#include <assert.h>
#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Miscellaneouse Macros
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#define ARRAY_LENGTH(array) (sizeof((array)) / sizeof((array)[0]))

#ifndef NDEBUG
#define CLEAR_MEMORY(object) memset((object), '\0', sizeof *(object))
#else
#define CLEAR_MEMORY(object) ((void)0)
#endif

#define ITERATE_ONCE(finish, before, after)                                    \
    for (bool finish = ((before), false); !finish; (after), finish = true)

#define CAT_PRIMITIVE(a, b) a##b
#define CAT(a, b)           CAT_PRIMITIVE(a, b)

#define STRINGIFY_PRIMITIVE(...) #__VA_ARGS__
#define STRINGIFY(...)           STRINGIFY_PRIMITIVE(__VA_ARGS__)

// Compiler Functionality Detection
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#define STANDARD_C99_OR_HIGHER (__STDC_VERSION__ >= 199901L)
#define STANDARD_C11_OR_HIGHER (__STDC_VERSION__ >= 201112L)

#if !STANDARD_C99_OR_HIGHER
#error C99 or higher is required!
#endif

#if !defined(NDEBUG) && defined(__has_feature) // Clang
#if __has_feature(address_sanitizer)
#define COMPILER_ASAN_AVAILABLE
#endif
#elif !defined(NDEBUG) && defined(__SANITIZE_ADDRESS__) // GCC & MSVC
#define COMPILER_ASAN_AVAILABLE
#endif

#ifdef COMPILER_ASAN_AVAILABLE
#include <sanitizer/asan_interface.h>
#endif

#ifdef __GNUC__
#include <sys/mman.h>
#include <unistd.h>
#endif

// Perhaps for future use...
#if defined(_OPENMP) && defined(NDEBUG)
#include <omp.h>
#define MY_OPENMP(...) _Pragma(__VA_ARGS__)
#else
#define MY_OPENMP(...) /* empty */
#endif

// Compiler-Specific Builtins
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#ifdef __GNUC__

// We generally trust the compiler whether or not to inline a function.
// However, we utilize a number of other attributes, to help both the human
// reader & the compiler.
// See <https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html> for
// the list of GNU C function attributes.
// Note: please, keep `.clang-format` up-to-date with the macros below.

#define COMPILER_UNUSED             __attribute__((unused))
#define COMPILER_NORETURN           __attribute__((noreturn))
#define COMPILER_COLD               __attribute__((cold))
#define COMPILER_HOT                __attribute__((hot))
#define COMPILER_FLATTEN            __attribute__((flatten))
#define COMPILER_ALWAYS_INLINE      __attribute__((always_inline))
#define COMPILER_RETURNS_NONNULL    __attribute__((returns_nonnull))
#define COMPILER_WARN_UNUSED_RESULT __attribute__((warn_unused_result))

#ifndef __clang__

#define COMPILER_MALLOC(deallocator, ptr_index)                                \
    __attribute__((malloc(deallocator, ptr_index)))

#endif // __clang__

#define COMPILER_FORMAT(archetype, string_index, first_to_check)               \
    __attribute__((format(archetype, string_index, first_to_check)))

#ifdef NDEBUG

#define COMPILER_UNREACHABLE() __builtin_unreachable()
#define COMPILER_NONNULL(...)  __attribute__((nonnull(__VA_ARGS__)))
#define COMPILER_CONST         __attribute__((const))
#define COMPILER_PURE          __attribute__((pure))

#else

#define COMPILER_UNREACHABLE() assert(false)
#define COMPILER_NONNULL(...)  /* checked by `assert` */
#define COMPILER_CONST         /* may invoke side-effecting `assert` */
#define COMPILER_PURE          /* may invoke side-effecting `assert` */

#endif // NDEBUG

#endif // __GNUC__

#ifdef COMPILER_ASAN_AVAILABLE
#define COMPILER_POISON_MEMORY       ASAN_POISON_MEMORY_REGION
#define COMPILER_UNPOISON_MEMORY     ASAN_UNPOISON_MEMORY_REGION
#define COMPILER_IS_POISONED_ADDRESS __asan_address_is_poisoned
#define COMPILER_IS_POISONED_MEMORY  __asan_region_is_poisoned
#endif

#define COMPILER_IGNORE                /* empty, object-like */
#define COMPILER_IGNORE_WITH_ARGS(...) /* empty, function-like */

#ifndef COMPILER_UNUSED
#define COMPILER_UNUSED COMPILER_IGNORE
#endif

#ifndef COMPILER_NORETURN
#if STANDARD_C11_OR_HIGHER
#define COMPILER_NORETURN _Noreturn
#else
#define COMPILER_NORETURN COMPILER_IGNORE
#endif
#endif

#ifndef COMPILER_COLD
#define COMPILER_COLD COMPILER_IGNORE
#endif

#ifndef COMPILER_HOT
#define COMPILER_HOT COMPILER_IGNORE
#endif

#ifndef COMPILER_FLATTEN
#define COMPILER_FLATTEN COMPILER_IGNORE
#endif

#ifndef COMPILER_ALWAYS_INLINE
#define COMPILER_ALWAYS_INLINE COMPILER_IGNORE
#endif

#ifndef COMPILER_RETURNS_NONNULL
#define COMPILER_RETURNS_NONNULL COMPILER_IGNORE
#endif

#ifndef COMPILER_WARN_UNUSED_RESULT
#define COMPILER_WARN_UNUSED_RESULT COMPILER_IGNORE
#endif

#ifndef COMPILER_MALLOC
#define COMPILER_MALLOC COMPILER_IGNORE_WITH_ARGS
#endif

#ifndef COMPILER_FORMAT
#define COMPILER_FORMAT COMPILER_IGNORE_WITH_ARGS
#endif

#ifndef COMPILER_UNREACHABLE
#define COMPILER_UNREACHABLE COMPILER_IGNORE_WITH_ARGS
#endif

#ifndef COMPILER_NONNULL
#define COMPILER_NONNULL COMPILER_IGNORE
#endif

#ifndef COMPILER_CONST
#define COMPILER_CONST COMPILER_IGNORE
#endif

#ifndef COMPILER_PURE
#define COMPILER_PURE COMPILER_IGNORE
#endif

#ifndef COMPILER_POISON_MEMORY
#define COMPILER_POISON_MEMORY COMPILER_IGNORE_WITH_ARGS
#endif

#ifndef COMPILER_UNPOISON_MEMORY
#define COMPILER_UNPOISON_MEMORY COMPILER_IGNORE_WITH_ARGS
#endif

#ifndef COMPILER_IS_POISONED_ADDRESS
#define COMPILER_IS_POISONED_ADDRESS COMPILER_IGNORE_WITH_ARGS
#endif

#ifndef COMPILER_IS_POISONED_MEMORY
#define COMPILER_IS_POISONED_MEMORY COMPILER_IGNORE_WITH_ARGS
#endif

// Debugging Assertions
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

// Used for communicating logic invariants to the compiler.
#if defined(__GNUC__) && defined(NDEBUG)
#define XASSERT(condition) (!(condition) ? __builtin_unreachable() : (void)0)
#else
#define XASSERT assert
#endif

// Assertions that are checked at compile-time.
#if defined(__GNUC__) || STANDARD_C11_OR_HIGHER
#define STATIC_ASSERT(constant_expression)                                     \
    _Static_assert((constant_expression), #constant_expression)
#else
// clang-format off
#define STATIC_ASSERT(constant_expression) \
    COMPILER_UNUSED /* */ \
    static const char CAT(c99_static_assert_, __LINE__) \
        [(constant_expression) ? 1 : -1] = {0}
// clang-format on
#endif

// File-Related I/O
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#define IO_CALL(f, ...) (f(__VA_ARGS__) < 0 ? (perror(#f), abort()) : (void)0)
#define IO_CALL_ASSIGN(x, f, ...)                                              \
    ((x = f(__VA_ARGS__)) < 0 ? (perror(#f), abort()) : (void)0)

// Logging & Panicking
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#define PRINTER(name, stream, culmination)                                     \
    COMPILER_NONNULL(1) COMPILER_FORMAT(printf, 1, 2) /* */                    \
    static void                                                                \
    name(const char *const restrict format, ...) {                             \
        assert(format);                                                        \
                                                                               \
        va_list args;                                                          \
        va_start(args, format);                                                \
        vfprintf(stream, format, args);                                        \
        fputs("\n", stream);                                                   \
        va_end(args);                                                          \
                                                                               \
        culmination;                                                           \
    }

#ifdef OPTISCOPE_ENABLE_TRACING
PRINTER(debug, stdout, /* empty */)
#else
#define debug(...) ((void)0)
#endif

COMPILER_COLD COMPILER_NORETURN //
PRINTER(panic, stderr, abort())

#undef PRINTER

COMPILER_HOT COMPILER_ALWAYS_INLINE //
inline static uint64_t
checked_add(const uint64_t value, const uint64_t offset) {
    if (value > UINT64_MAX - offset) {
        panic("Maximum `uint64_t` value is reached!");
    }

    return value + offset;
}

// Checked Memory Allocation
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

COMPILER_MALLOC(free, 1) COMPILER_RETURNS_NONNULL COMPILER_WARN_UNUSED_RESULT //
static void *
xmalloc(const size_t size) {
    XASSERT(size > 0);

    void *const object = malloc(size);
    if (NULL == object) { panic("Failed `xmalloc`!"); }

    return object;
}

COMPILER_MALLOC(free, 1) COMPILER_RETURNS_NONNULL COMPILER_WARN_UNUSED_RESULT //
static void *
xcalloc(const size_t n, const size_t size) {
    XASSERT(size > 0);

    void *const object = calloc(n, size);
    if (NULL == object) { panic("Failed `xcalloc`!"); }

    return object;
}

COMPILER_MALLOC(free, 1) COMPILER_RETURNS_NONNULL COMPILER_WARN_UNUSED_RESULT //
static void *
xrealloc(void *restrict object, const size_t size) {
    XASSERT(size > 0);

    object = realloc(object, size);
    if (NULL == object) { panic("Failed `xrealloc`!"); }

    return object;
}

COMPILER_WARN_UNUSED_RESULT COMPILER_RETURNS_NONNULL COMPILER_NONNULL(1)
COMPILER_FORMAT(printf, 1, 2) //
static char *
format_string(const char *const format, ...) {
    assert(format);

    va_list args, args_copy;
    va_start(args, format);
    va_copy(args_copy, args);
    const int length = vsnprintf(NULL, 0, format, args);
    va_end(args);
    char *const result = xmalloc(length + 1);
    vsprintf(result, format, args_copy);
    va_end(args_copy);

    return result;
}

// Lambda Term Interface
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

enum lambda_term_type {
    LAMBDA_TERM_APPLY,
    LAMBDA_TERM_LAMBDA,
    LAMBDA_TERM_VAR,
    LAMBDA_TERM_CELL,
    LAMBDA_TERM_UNARY_CALL,
    LAMBDA_TERM_BINARY_CALL,
    LAMBDA_TERM_IF_THEN_ELSE,
    LAMBDA_TERM_PERFORM,
    LAMBDA_TERM_REFERENCE,
};

struct apply_data {
    struct lambda_term *rator, *rand;
};

struct lambda_data {
    struct lambda_term *body;
    uint64_t nusages;        // the number of times the lambda body refers to
                             // its binder
    uint64_t **binder_ports; // the pointer to the next binder port; dynamically
                             // mutated
    uint64_t lvl;            // the de Bruijn level; dynamically mutated
};

struct unary_call_data {
    uint64_t (*function)(uint64_t);
    struct lambda_term *rand;
};

struct binary_call_data {
    uint64_t (*function)(uint64_t, uint64_t);
    struct lambda_term *lhs, *rhs;
};

struct if_then_else_data {
    struct lambda_term *condition;
    struct lambda_term *if_then, *if_else;
};

struct perform_data {
    struct lambda_term *action, *k;
};

struct reference_data {
    struct lambda_term *(*function)(void);
};

union lambda_term_data {
    struct apply_data app;
    struct lambda_data *lam;
    struct lambda_data **var;
    uint64_t cell;
    struct unary_call_data ucall;
    struct binary_call_data bcall;
    struct if_then_else_data ite;
    struct perform_data perf;
    struct reference_data ref;
};

struct lambda_term {
    enum lambda_term_type ty;
    union lambda_term_data data;
    uint64_t fv_count;    // the number of free variables in the term; assigned
                          // during construction
    uint64_t *connect_to; // the port addresse to be connected with the
                          // interface; dynamically mutated
};

extern LambdaTerm
apply(const restrict LambdaTerm rator, const restrict LambdaTerm rand) {
    assert(rator);
    assert(rand);

    struct lambda_term *const term = xmalloc(sizeof *term);
    term->ty = LAMBDA_TERM_APPLY;
    term->data.app.rator = rator;
    term->data.app.rand = rand;
    term->fv_count = rator->fv_count + rand->fv_count;

    return term;
}

extern LambdaTerm
prelambda(void) {
    struct lambda_term *const term = xmalloc(sizeof *term);
    term->ty = LAMBDA_TERM_LAMBDA;
    term->data.lam = xcalloc(1, sizeof *term->data.lam);
    // All the data fields are zeroed out.

    return term;
}

extern LambdaTerm
link_lambda_body(
    const restrict LambdaTerm binder, const restrict LambdaTerm body) {
    assert(binder);
    assert(body);
    assert(LAMBDA_TERM_LAMBDA == binder->ty);

    binder->data.lam->body = body;
    binder->fv_count = body->fv_count - binder->data.lam->nusages;

    return binder;
}

extern LambdaTerm
var(const restrict LambdaTerm binder) {
    assert(binder);
    assert(LAMBDA_TERM_LAMBDA == binder->ty);

    struct lambda_term *const term = xmalloc(sizeof *term);
    term->ty = LAMBDA_TERM_VAR;
    term->data.var = &binder->data.lam;
    term->fv_count = 1;

    binder->data.lam->nusages++;

    return term;
}

extern LambdaTerm
cell(const uint64_t value) {
    struct lambda_term *const term = xmalloc(sizeof *term);
    term->ty = LAMBDA_TERM_CELL;
    term->data.cell = value;
    term->fv_count = 0;

    return term;
}

extern LambdaTerm
unary_call(
    uint64_t (*const function)(uint64_t), const restrict LambdaTerm rand) {
    assert(function);
    assert(rand);

    struct lambda_term *const term = xmalloc(sizeof *term);
    term->ty = LAMBDA_TERM_UNARY_CALL;
    term->data.ucall.function = function;
    term->data.ucall.rand = rand;
    term->fv_count = rand->fv_count;

    return term;
}

extern LambdaTerm
binary_call(
    uint64_t (*const function)(uint64_t, uint64_t),
    const restrict LambdaTerm lhs,
    const restrict LambdaTerm rhs) {
    assert(function);
    assert(lhs);
    assert(rhs);

    struct lambda_term *const term = xmalloc(sizeof *term);
    term->ty = LAMBDA_TERM_BINARY_CALL;
    term->data.bcall.function = function;
    term->data.bcall.lhs = lhs;
    term->data.bcall.rhs = rhs;
    term->fv_count = lhs->fv_count + rhs->fv_count;

    return term;
}

extern LambdaTerm
if_then_else(
    const restrict LambdaTerm condition,
    const restrict LambdaTerm if_then,
    const restrict LambdaTerm if_else) {
    assert(condition);
    assert(if_then);
    assert(if_else);

    struct lambda_term *const term = xmalloc(sizeof *term);
    term->ty = LAMBDA_TERM_IF_THEN_ELSE;
    term->data.ite.condition = condition;
    term->data.ite.if_then = if_then;
    term->data.ite.if_else = if_else;
    term->fv_count =
        condition->fv_count + if_then->fv_count + if_else->fv_count;

    return term;
}

extern LambdaTerm
perform(const restrict LambdaTerm action, const restrict LambdaTerm k) {
    assert(action);
    assert(k);

    struct lambda_term *const term = xmalloc(sizeof *term);
    term->ty = LAMBDA_TERM_PERFORM;
    term->data.perf.action = action;
    term->data.perf.k = k;
    term->fv_count = action->fv_count + k->fv_count;

    return term;
}

extern LambdaTerm
expand(struct lambda_term *(*const function)(void)) {
    assert(function);

    struct lambda_term *const term = xmalloc(sizeof *term);
    term->ty = LAMBDA_TERM_REFERENCE;
    term->data.ref.function = function;
    term->fv_count = 0;

    return term;
}

COMPILER_NONNULL(1) COMPILER_COLD //
static void
free_lambda_term(struct lambda_term *const restrict term) {
    assert(term);

    switch (term->ty) {
    case LAMBDA_TERM_APPLY:
        free_lambda_term(term->data.app.rator);
        free_lambda_term(term->data.app.rand);
        break;
    case LAMBDA_TERM_LAMBDA:
        free_lambda_term(term->data.lam->body);
        free(term->data.lam->binder_ports);
        free(term->data.lam);
        break;
    case LAMBDA_TERM_UNARY_CALL: free_lambda_term(term->data.ucall.rand); break;
    case LAMBDA_TERM_BINARY_CALL:
        free_lambda_term(term->data.bcall.lhs);
        free_lambda_term(term->data.bcall.rhs);
        break;
    case LAMBDA_TERM_IF_THEN_ELSE:
        free_lambda_term(term->data.ite.condition);
        free_lambda_term(term->data.ite.if_then);
        free_lambda_term(term->data.ite.if_else);
        break;
    case LAMBDA_TERM_PERFORM:
        free_lambda_term(term->data.perf.action);
        free_lambda_term(term->data.perf.k);
        break;
    case LAMBDA_TERM_VAR:
    case LAMBDA_TERM_CELL:
    case LAMBDA_TERM_REFERENCE: break;
    default: COMPILER_UNREACHABLE();
    }

    free(term);
}

// Ports & Symbols Functionality
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#define MACHINE_WORD_BITS    UINT64_C(64)
#define OFFSET_METADATA_BITS UINT64_C(2)
#define PHASE_METADATA_BITS  UINT64_C(4)
#define EFFECTIVE_ADDRESS_BITS                                                 \
    (MACHINE_WORD_BITS - OFFSET_METADATA_BITS - PHASE_METADATA_BITS)
#define UNUSED_ADDRESS_BITS (MACHINE_WORD_BITS - EFFECTIVE_ADDRESS_BITS)
#define ADDRESS_MASK        (~UINT64_C(0) >> UNUSED_ADDRESS_BITS)

#define SIGN_EXTEND(n)                                                         \
    ((uint64_t)((int64_t)((n) << UNUSED_ADDRESS_BITS) >> UNUSED_ADDRESS_BITS))

#define ENCODE_METADATA(offset, phase)                                         \
    ((((offset) << PHASE_METADATA_BITS) | (phase)) << EFFECTIVE_ADDRESS_BITS)
#define DECODE_OFFSET_METADATA(address)                                        \
    ((address) >> (EFFECTIVE_ADDRESS_BITS + PHASE_METADATA_BITS))
#define DECODE_PHASE_METADATA(address)                                         \
    (((address) << OFFSET_METADATA_BITS) >>                                    \
     (EFFECTIVE_ADDRESS_BITS + OFFSET_METADATA_BITS))

#ifdef __linux__

// The kernel returnes userspace addresses with sign-extended bits always set to
// zeroes, so they will not corrupt our metadata.
// Source:
// <https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/Documentation/arch/x86/x86_64/mm.rst>.
// Workes with both 4- & 5-level page tables.
#define ENCODE_ADDRESS(metadata, address) ((address) | (metadata))

#define DECODE_ADDRESS(address) ((uint64_t *)((address) & ADDRESS_MASK))

#else

// In the fallback case, we need to properly maske the highermost addresse bits
// to avoid metadata corruption.
#define ENCODE_ADDRESS(metadata, address)                                      \
    (((address) & ADDRESS_MASK) | (metadata))

#define DECODE_ADDRESS(address)                                                \
    ((uint64_t *)(SIGN_EXTEND((address) & ADDRESS_MASK)))

#endif // __linux__

#define DECODE_ADDRESS_METADATA(address) (((address) & ~ADDRESS_MASK))

#define PORT_VALUE(offset, phase, address)                                     \
    ENCODE_ADDRESS(ENCODE_METADATA((offset), (phase)), (address))

#define IS_PRINCIPAL_PORT(port) (0 == DECODE_OFFSET_METADATA((port)))

#define MAX_REGULAR_SYMBOL   UINT64_C(63)
#define INDEX_RANGE          UINT64_C(9223372036854775776)
#define MAX_DUPLICATOR_INDEX (MAX_REGULAR_SYMBOL + INDEX_RANGE)
#define MAX_DELIMITER_INDEX  (MAX_DUPLICATOR_INDEX + INDEX_RANGE)
#define MAX_PORTS            UINT64_C(4)
#define MAX_AUXILIARY_PORTS  (MAX_PORTS - 1)

STATIC_ASSERT(CHAR_BIT == 8);
STATIC_ASSERT(sizeof(uint64_t *) == sizeof(uint64_t));
STATIC_ASSERT(sizeof(uint64_t (*)(uint64_t)) <= sizeof(uint64_t));
STATIC_ASSERT(sizeof(uint64_t (*)(uint64_t, uint64_t)) <= sizeof(uint64_t));
STATIC_ASSERT(sizeof(struct lambda_term *(*)(void)) <= sizeof(uint64_t));
STATIC_ASSERT(UINT64_MAX == UINT64_C(18446744073709551615));
STATIC_ASSERT(UINT64_MAX == MAX_DELIMITER_INDEX);

#define SYMBOL_ROOT                UINT64_C(0)
#define SYMBOL_APPLICATOR          UINT64_C(1)
#define SYMBOL_LAMBDA              UINT64_C(2)
#define SYMBOL_ERASER              UINT64_C(3)
#define SYMBOL_CELL                UINT64_C(4)
#define SYMBOL_UNARY_CALL          UINT64_C(5)
#define SYMBOL_BINARY_CALL         UINT64_C(6)
#define SYMBOL_BINARY_CALL_AUX     UINT64_C(7)
#define SYMBOL_IF_THEN_ELSE        UINT64_C(8)
#define SYMBOL_PERFORM             UINT64_C(9)
#define SYMBOL_IDENTITY_LAMBDA     UINT64_C(10) // the identity lambda
#define SYMBOL_GC_LAMBDA           UINT64_C(11) // a lambda discarding its parameter
#define SYMBOL_LAMBDA_C            UINT64_C(12) // a closed lambda
#define SYMBOL_REFERENCE           UINT64_C(13)
#define SYMBOL_BARRIER             UINT64_C(14)
#define SYMBOL_GC_DUPLICATOR_LEFT  UINT64_C(15)
#define SYMBOL_GC_DUPLICATOR_RIGHT UINT64_C(16)
#define SYMBOL_DUPLICATOR(i)       (MAX_REGULAR_SYMBOL + 1 + (i))
#define SYMBOL_DELIMITER(i)        (MAX_DUPLICATOR_INDEX + 1 + (i))

#define IS_ANY_LAMBDA(symbol)                                                  \
    (SYMBOL_LAMBDA == (symbol) || SYMBOL_IDENTITY_LAMBDA == (symbol) ||        \
     SYMBOL_GC_LAMBDA == (symbol) || SYMBOL_LAMBDA_C == (symbol))

#define IS_RELEVANT_LAMBDA(symbol)                                             \
    (SYMBOL_LAMBDA == (symbol) || SYMBOL_LAMBDA_C == (symbol))

// clang-format off
#define IS_DUPLICATOR(symbol) \
    ((symbol) >= SYMBOL_DUPLICATOR(UINT64_C(0)) && \
     (symbol) <= MAX_DUPLICATOR_INDEX)
#define IS_DELIMITER(symbol) \
    ((symbol) >= SYMBOL_DELIMITER(UINT64_C(0)))
// clang-format on

#define IS_GC_DUPLICATOR(symbol)                                               \
    (SYMBOL_GC_DUPLICATOR_LEFT == (symbol) ||                                  \
     SYMBOL_GC_DUPLICATOR_RIGHT == (symbol))

#define IS_ANY_DUPLICATOR(symbol)                                              \
    (IS_DUPLICATOR((symbol)) || IS_GC_DUPLICATOR((symbol)))

#define IS_INTERFACE_SYMBOL(symbol)                                            \
    (IS_ANY_LAMBDA((symbol)) || SYMBOL_CELL == (symbol))

COMPILER_CONST COMPILER_WARN_UNUSED_RESULT COMPILER_HOT //
inline static bool
is_atomic_symbol(const uint64_t symbol) {
    switch (symbol) {
    case SYMBOL_ERASER:
    case SYMBOL_CELL:
    case SYMBOL_IDENTITY_LAMBDA:
    case SYMBOL_REFERENCE: return true;
    default: return false;
    }
}

#define FOR_ALL_PORTS(node, i, seed)                                           \
    for (uint8_t i = seed; i < ports_count((node).ports[-1]); i++)

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function" // may be unused

COMPILER_CONST COMPILER_WARN_UNUSED_RESULT //
static uint8_t
ports_count(const uint64_t symbol) {
    switch (symbol) {
    case SYMBOL_ROOT:
    case SYMBOL_ERASER:
    case SYMBOL_CELL:
    case SYMBOL_IDENTITY_LAMBDA:
    case SYMBOL_REFERENCE: //
        return 1;
    case SYMBOL_UNARY_CALL:
    case SYMBOL_BINARY_CALL_AUX:
    case SYMBOL_GC_LAMBDA:
    case SYMBOL_BARRIER:
    case SYMBOL_GC_DUPLICATOR_LEFT:
    case SYMBOL_GC_DUPLICATOR_RIGHT:
    delimiter:
        return 2;
    case SYMBOL_APPLICATOR:
    case SYMBOL_LAMBDA:
    case SYMBOL_BINARY_CALL:
    case SYMBOL_PERFORM:
    case SYMBOL_LAMBDA_C:
    duplicator:
        return 3;
    case SYMBOL_IF_THEN_ELSE: //
        return 4;
    default:
        if (IS_DUPLICATOR(symbol)) goto duplicator;
        else if (IS_DELIMITER(symbol)) goto delimiter;
        else COMPILER_UNREACHABLE();
    }
}

#pragma GCC diagnostic pop // "-Wunused-function"

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT COMPILER_RETURNS_NONNULL
COMPILER_NONNULL(1) COMPILER_HOT COMPILER_ALWAYS_INLINE //
inline static uint64_t *
get_principal_port(uint64_t *const restrict port) {
    assert(port);

    return (port - DECODE_OFFSET_METADATA(port[0]));
}

COMPILER_NONNULL(1, 2) COMPILER_HOT COMPILER_ALWAYS_INLINE //
inline static void
connect_ports(uint64_t *const restrict lhs, uint64_t *const restrict rhs) {
    debug("%p 🔗 %p", (void *)lhs, (void *)rhs);

    assert(lhs);
    assert(rhs);
    XASSERT(lhs != rhs);

    *lhs = ENCODE_ADDRESS(DECODE_ADDRESS_METADATA(*lhs), (uint64_t)rhs);
    *rhs = ENCODE_ADDRESS(DECODE_ADDRESS_METADATA(*rhs), (uint64_t)lhs);
}

#define SYMBOL_INDEX(symbol)                                                   \
    /* Extract the symbol without branching, considering that duplicators &    \
     * delimiters share the same symbol range. */                              \
    (((symbol) - MAX_REGULAR_SYMBOL - 1) % INDEX_RANGE)

#if defined(OPTISCOPE_ENABLE_TRACING) || defined(OPTISCOPE_ENABLE_GRAPHVIZ)

COMPILER_MALLOC(free, 1) COMPILER_RETURNS_NONNULL COMPILER_WARN_UNUSED_RESULT //
static char *
print_symbol(const uint64_t symbol) {
    switch (symbol) {
    case SYMBOL_ROOT: return format_string("root");
    case SYMBOL_APPLICATOR: return format_string("@");
    case SYMBOL_LAMBDA: return format_string("λ");
    case SYMBOL_ERASER: return format_string("◉");
    case SYMBOL_CELL: return format_string("cell");
    case SYMBOL_UNARY_CALL: return format_string("unary-call");
    case SYMBOL_BINARY_CALL: return format_string("binary-call");
    case SYMBOL_BINARY_CALL_AUX: return format_string("binary-call-aux");
    case SYMBOL_IF_THEN_ELSE: return format_string("if-then-else");
    case SYMBOL_PERFORM: return format_string("perform");
    case SYMBOL_IDENTITY_LAMBDA: return format_string("identity");
    case SYMBOL_GC_LAMBDA: return format_string("λ◉");
    case SYMBOL_LAMBDA_C: return format_string("λc");
    case SYMBOL_REFERENCE: return format_string("&");
    case SYMBOL_BARRIER: return format_string("🚧");
    case SYMBOL_GC_DUPLICATOR_LEFT: return format_string("◉δ");
    case SYMBOL_GC_DUPLICATOR_RIGHT: return format_string("δ◉");
    default:
        if (IS_DUPLICATOR(symbol)) goto duplicator;
        else if (IS_DELIMITER(symbol)) goto delimiter;
        else COMPILER_UNREACHABLE();
    duplicator:
        return format_string("δ/%" PRIi64, SYMBOL_INDEX(symbol));
    delimiter:
        return format_string("⊔/%" PRIi64, SYMBOL_INDEX(symbol));
    }
}

#endif

#define INDEX_OVERFLOW()                                                       \
    panic("Maximum index of %" PRIu64 " is reached!", INDEX_RANGE - 1)

COMPILER_HOT COMPILER_ALWAYS_INLINE //
inline static uint64_t
bump_index(const uint64_t symbol, const uint64_t offset) {
    XASSERT(symbol > MAX_REGULAR_SYMBOL);

    if ((IS_DUPLICATOR(symbol) && offset > MAX_DUPLICATOR_INDEX - symbol) ||
        (IS_DELIMITER(symbol) && offset > MAX_DELIMITER_INDEX - symbol)) {
        INDEX_OVERFLOW();
    }

    return symbol + offset;
}

COMPILER_HOT COMPILER_ALWAYS_INLINE //
inline static uint64_t
bump_raw_index(const uint64_t index, const uint64_t offset) {
    XASSERT(index < INDEX_RANGE);

    if (offset > INDEX_RANGE - 1 - index) { INDEX_OVERFLOW(); }

    return index + offset;
}

#undef INDEX_OVERFLOW

#define PHASE_DEFAULT  UINT64_C(0) // all nodes not in the reduction stack
#define PHASE_GC       UINT64_C(1) // active GC erasers
#define PHASE_GC_AUX   UINT64_C(2) // GC erasers scheduled for deletion
#define PHASE_IN_STACK UINT64_C(3) // operators in the reduction stack

#ifdef OPTISCOPE_ENABLE_GRAPHVIZ
#define PHASE_CURRENT_PAIR UINT64_C(4) // Grapdhviz: the current active pair
#endif

#define PHASE_MASK                                                             \
    UINT64_C(0xC3FFFFFFFFFFFFFF) /* clear the phase bits (61-58) */

COMPILER_NONNULL(1) COMPILER_HOT COMPILER_ALWAYS_INLINE //
inline static void
set_phase(uint64_t *const restrict port, const uint64_t phase) {
    assert(port);
    assert(IS_PRINCIPAL_PORT(*port));

    *port = (*port & PHASE_MASK) | (phase << EFFECTIVE_ADDRESS_BITS);

    assert(DECODE_PHASE_METADATA(*port) == phase);
}

// Native Function Pointers
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

// Casting a function pointer to `void *` is not safe by the standard, but many
// implementations permit it because of dynamic loading (e.g., `dlopen`).
// Here, we performe a two-step conversion: we first cast the user-provided
// pointer to `void *` & then to `uint64_t`.
#define U64_OF_FUNCTION(function) ((uint64_t)(void *)(function))

#define UNARY_FUNCTION_OF_U64(function)                                        \
    ((uint64_t (*)(uint64_t))(void *)(function))
#define BINARY_FUNCTION_OF_U64(function)                                       \
    ((uint64_t (*)(uint64_t, uint64_t))(void *)(function))

// O(1) Pool Allocation & Deallocation
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#if defined(OPTISCOPE_ENABLE_HUGE_PAGES) && defined(__linux__)

#define HUGE_PAGE_SIZE_2MB (2 * 1024 * 1024)

#define POOL_CHUNK_LIST_SIZE(chunk_size) (HUGE_PAGE_SIZE_2MB / (chunk_size))

COMPILER_NONNULL(1) COMPILER_COLD //
void
free_chunk(void *const memory);

// clang-format off
COMPILER_WARN_UNUSED_RESULT COMPILER_MALLOC(free_chunk, 1) COMPILER_COLD
// clang-format on
static void *
alloc_chunk(const size_t size) {
    assert(size <= HUGE_PAGE_SIZE_2MB);

    const int prot = PROT_READ | PROT_WRITE,
              flags = MAP_PRIVATE | MAP_ANONYMOUS | MAP_HUGETLB;
    void *const memory = mmap(NULL, HUGE_PAGE_SIZE_2MB, prot, flags, -1, 0);
    if (MAP_FAILED == memory) {
        // Cannot allocate memory, defaulting to `xmalloc`.
        perror("mmap");
        return xmalloc(size);
    }

    return memory;
}

COMPILER_NONNULL(1) COMPILER_COLD //
void
free_chunk(void *const memory) {
    assert(memory);

    if (munmap(memory, HUGE_PAGE_SIZE_2MB) < 0) {
        if (EINVAL == errno) {
            // This block was allocated with `malloc`, not `mmap`.
            free(memory);
        } else {
            perror("munmap");
        }
    }
}

#else // either huge pages are not requested or it is not Linux

#define POOL_CHUNK_LIST_SIZE(chunk_size) 1024

#define alloc_chunk xmalloc
#define free_chunk  free

#endif

#define POOL_ALLOCATOR(prefix, chunk_size)                                     \
    union prefix##_chunk {                                                     \
        char data[chunk_size];                                                 \
        union prefix##_chunk *next;                                            \
    };                                                                         \
                                                                               \
    struct prefix##_chunks_bucket {                                            \
        union prefix##_chunk *chunks;                                          \
        struct prefix##_chunks_bucket *next;                                   \
    };                                                                         \
                                                                               \
    struct prefix##_pool {                                                     \
        union prefix##_chunk *next_free_chunk;                                 \
        struct prefix##_chunks_bucket *buckets;                                \
    };                                                                         \
                                                                               \
    COMPILER_NONNULL(1) COMPILER_COLD /* */                                    \
    static void                                                                \
    prefix##_pool_close(struct prefix##_pool *const restrict self);            \
                                                                               \
    COMPILER_MALLOC(prefix##_pool_close, 1) COMPILER_RETURNS_NONNULL           \
    COMPILER_WARN_UNUSED_RESULT COMPILER_COLD /* */                            \
    static struct prefix##_pool *                                              \
    prefix##_pool_create(void) {                                               \
        struct prefix##_pool *const self = xmalloc(sizeof *self);              \
                                                                               \
        union prefix##_chunk *const chunks =                                   \
            alloc_chunk(POOL_CHUNK_LIST_SIZE(chunk_size) * chunk_size);        \
        for (size_t i = 0; i < POOL_CHUNK_LIST_SIZE(chunk_size) - 1; i++) {    \
            chunks[i].next = &chunks[i + 1];                                   \
        }                                                                      \
        chunks[POOL_CHUNK_LIST_SIZE(chunk_size) - 1].next = NULL;              \
        self->next_free_chunk = chunks;                                        \
                                                                               \
        struct prefix##_chunks_bucket *const buckets =                         \
            xmalloc(sizeof buckets[0]);                                        \
        buckets->chunks = chunks, buckets->next = NULL;                        \
        self->buckets = buckets;                                               \
                                                                               \
        return self;                                                           \
    }                                                                          \
                                                                               \
    COMPILER_NONNULL(1) COMPILER_COLD /* */                                    \
    static void                                                                \
    prefix##_pool_close(struct prefix##_pool *const restrict self) {           \
        assert(self);                                                          \
        XASSERT(self->buckets);                                                \
                                                                               \
        struct prefix##_chunks_bucket *iter = self->buckets;                   \
        while (iter) {                                                         \
            struct prefix##_chunks_bucket *next = iter->next;                  \
            XASSERT(iter->chunks);                                             \
            free_chunk(iter->chunks);                                          \
            free(iter);                                                        \
            iter = next;                                                       \
        }                                                                      \
                                                                               \
        free(self);                                                            \
    }                                                                          \
                                                                               \
    COMPILER_NONNULL(1) COMPILER_COLD /* */                                    \
    static void                                                                \
    prefix##_pool_expand(struct prefix##_pool *const restrict self) {          \
        assert(self);                                                          \
        XASSERT(self->buckets);                                                \
                                                                               \
        union prefix##_chunk *const extra_chunks =                             \
            alloc_chunk(POOL_CHUNK_LIST_SIZE(chunk_size) * chunk_size);        \
        for (size_t i = 0; i < POOL_CHUNK_LIST_SIZE(chunk_size) - 1; i++) {    \
            extra_chunks[i].next = &extra_chunks[i + 1];                       \
        }                                                                      \
        extra_chunks[POOL_CHUNK_LIST_SIZE(chunk_size) - 1].next =              \
            self->next_free_chunk;                                             \
        self->next_free_chunk = extra_chunks;                                  \
                                                                               \
        struct prefix##_chunks_bucket *const buckets =                         \
            xmalloc(sizeof buckets[0]);                                        \
        buckets->chunks = extra_chunks, buckets->next = self->buckets;         \
        self->buckets = buckets;                                               \
    }                                                                          \
                                                                               \
    COMPILER_NONNULL(1, 2) COMPILER_HOT /* */                                  \
    static void                                                                \
    prefix##_pool_free(                                                        \
        struct prefix##_pool *const restrict self, uint64_t *restrict object); \
                                                                               \
    COMPILER_MALLOC(prefix##_pool_free, 1) COMPILER_RETURNS_NONNULL            \
    COMPILER_WARN_UNUSED_RESULT COMPILER_NONNULL(1) COMPILER_HOT /* */         \
    static uint64_t *                                                          \
    prefix##_pool_alloc(struct prefix##_pool *const restrict self) {           \
        assert(self);                                                          \
        XASSERT(self->buckets);                                                \
                                                                               \
        if (NULL == self->next_free_chunk) { prefix##_pool_expand(self); }     \
        XASSERT(self->next_free_chunk);                                        \
                                                                               \
        COMPILER_UNPOISON_MEMORY(self->next_free_chunk, chunk_size);           \
        void *const object = (void *)self->next_free_chunk;                    \
        self->next_free_chunk = self->next_free_chunk->next;                   \
                                                                               \
        return (uint64_t *)object + 1 /* passe the symbol */;                  \
    }                                                                          \
                                                                               \
    COMPILER_NONNULL(1, 2) COMPILER_HOT /* */                                  \
    static void                                                                \
    prefix##_pool_free(                                                        \
        struct prefix##_pool *const restrict self,                             \
        uint64_t *restrict object) {                                           \
        assert(self);                                                          \
        XASSERT(self->buckets);                                                \
        assert(object);                                                        \
                                                                               \
        object--; /* back to the symbol addresse */                            \
        union prefix##_chunk *const freed = (union prefix##_chunk *)object;    \
        CLEAR_MEMORY(freed);                                                   \
        freed->next = self->next_free_chunk;                                   \
        self->next_free_chunk = freed;                                         \
        COMPILER_POISON_MEMORY(freed, chunk_size);                             \
    }

POOL_ALLOCATOR(u64x2, sizeof(uint64_t) * 2)
POOL_ALLOCATOR(u64x3, sizeof(uint64_t) * 3)
POOL_ALLOCATOR(u64x4, sizeof(uint64_t) * 4)
POOL_ALLOCATOR(u64x5, sizeof(uint64_t) * 5)

#define ALLOC_POOL_OBJECT(pool_name) pool_name##_alloc(pool_name)
#define FREE_POOL_OBJECT(pool_name, object)                                    \
    pool_name##_free(pool_name, (object))

#define POOLS X(u64x2_pool) X(u64x3_pool) X(u64x4_pool) X(u64x5_pool)

#define X(pool_name) static struct pool_name *pool_name = NULL;
POOLS
#undef X

#define X(pool_name)                                                           \
    {                                                                          \
        XASSERT(NULL == pool_name);                                            \
        pool_name = pool_name##_create();                                      \
        XASSERT(pool_name);                                                    \
    }

// clang-format off
extern void optiscope_open_pools(void) { POOLS }
// clang-format on

#undef X

#define X(pool_name)                                                           \
    {                                                                          \
        XASSERT(pool_name);                                                    \
        pool_name##_close(pool_name);                                          \
        pool_name = NULL;                                                      \
    }

// clang-format off
extern void optiscope_close_pools(void) { POOLS }
// clang-format on

#undef X

#undef POOLS

#undef POOL_ALLOCATOR
#undef POOL_CHUNK_LIST_SIZE

// Nodes Functionality
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

struct node {
    uint64_t *ports;
};

STATIC_ASSERT(sizeof(struct node) == sizeof(uint64_t *));

#define node_of_port(port) ((struct node){get_principal_port((port))})

#define follow_port(node, i) (node_of_port(DECODE_ADDRESS((node).ports[(i)])))

#define points_to(f, g) (DECODE_ADDRESS((f).ports[0]) == &(g).ports[0])

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function" // may be unused

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT //
inline static bool
is_interaction(const struct node f, const struct node g) {
    XASSERT(f.ports);
    XASSERT(g.ports);

    return points_to(f, g) && points_to(g, f);
}

#pragma GCC diagnostic pop // "-Wunused-function"

#ifdef OPTISCOPE_ENABLE_TRACING

COMPILER_MALLOC(free, 1) COMPILER_RETURNS_NONNULL COMPILER_WARN_UNUSED_RESULT //
static char *
print_node(const struct node node) {
    XASSERT(node.ports);

    const uint64_t *const p = node.ports;

    char *const ssymbol = print_symbol(p[-1]);

    char *result = NULL;

    switch (ports_count(p[-1])) {
    case 1: result = format_string("%s [%p]", ssymbol, (void *)&p[0]); break;
    case 2:
        result =
            format_string("%s [%p, %p]", ssymbol, (void *)&p[0], (void *)&p[1]);
        break;
    case 3:
        result = format_string(
            "%s [%p, %p, %p]",
            ssymbol,
            (void *)&p[0],
            (void *)&p[1],
            (void *)&p[2]);
        break;
    case 4:
        result = format_string(
            "%s [%p, %p, %p, %p]",
            ssymbol,
            (void *)&p[0],
            (void *)&p[1],
            (void *)&p[2],
            (void *)&p[3]);
        break;
    default: COMPILER_UNREACHABLE();
    }

    free(ssymbol);

    return result;
}

#endif // OPTISCOPE_ENABLE_TRACING

// Bytecode Definitions
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

enum bc_instruction_type {
    INSTRUCTION_ATTACH_NODE,
    INSTRUCTION_SAVE_PORT,
    INSTRUCTION_CONNECT,
    INSTRUCTION_DELIMIT,
    INSTRUCTION_TREE,
};

struct bc_attach_node_data {
    struct node template;
    uint64_t interface_port_idx;
    uint64_t **connect_to;
};

struct bc_save_port_data {
    uint64_t **location;
    uint64_t port_idx;
};

struct bc_connect_data {
    uint64_t **lhs, **rhs;
};

struct bc_delimit_data {
    uint64_t **points_to, **goes_from;
    uint64_t n;
};

struct bc_tree_data {
    uint64_t n;
    uint64_t **locations;
};

union bc_instruction_data {
    struct bc_attach_node_data attach_node;
    struct bc_save_port_data save_port;
    struct bc_connect_data connect;
    struct bc_delimit_data delimit;
    struct bc_tree_data tree;
};

struct bc_instruction {
    enum bc_instruction_type ty;
    union bc_instruction_data data;
};

#define INITIAL_BYTECODE_CAPACITY 1024

struct bytecode {
    size_t count, capacity;
    struct bc_instruction *instructions;
};

#define alloc_bytecode(initial_capacity)                                       \
    ((struct bytecode){                                                        \
        .count = 0,                                                            \
        .capacity = (initial_capacity),                                        \
        .instructions =                                                        \
            xmalloc(sizeof(struct bc_instruction) * (initial_capacity)),       \
    })

#define free_bytecode(bc) free((bc).instructions)

COMPILER_NONNULL(1) //
static void
expand_bytecode(struct bytecode *const restrict bc) {
    assert(bc);
    XASSERT(bc->count == bc->capacity);
    XASSERT(bc->instructions);

    bc->instructions = xrealloc(
        bc->instructions, sizeof bc->instructions[0] * (bc->capacity *= 2));
}

COMPILER_NONNULL(1) //
inline static void
emit_instruction(
    struct bytecode *const restrict bc,
    const struct bc_instruction instruction) {
    assert(bc);
    XASSERT(bc->count <= bc->capacity);
    XASSERT(bc->instructions);

    if (bc->count == bc->capacity) { expand_bytecode(bc); }

    bc->instructions[bc->count++] = instruction;
}

#define BC_ATTACH_NODE(bc, ...)                                                \
    emit_instruction(                                                          \
        (bc),                                                                  \
        (struct bc_instruction){.ty = INSTRUCTION_ATTACH_NODE,                 \
                                .data.attach_node = {__VA_ARGS__}})

#define BC_SAVE_PORT(bc, ...)                                                  \
    emit_instruction(                                                          \
        (bc),                                                                  \
        (struct bc_instruction){.ty = INSTRUCTION_SAVE_PORT,                   \
                                .data.save_port = {__VA_ARGS__}})

#define BC_CONNECT(bc, ...)                                                    \
    emit_instruction(                                                          \
        (bc),                                                                  \
        (struct bc_instruction){.ty = INSTRUCTION_CONNECT,                     \
                                .data.connect = {__VA_ARGS__}})

#define BC_DELIMIT(bc, ...)                                                    \
    emit_instruction(                                                          \
        (bc),                                                                  \
        (struct bc_instruction){.ty = INSTRUCTION_DELIMIT,                     \
                                .data.delimit = {__VA_ARGS__}})

#define BC_TREE(bc, ...)                                                       \
    emit_instruction(                                                          \
        (bc),                                                                  \
        (struct bc_instruction){.ty = INSTRUCTION_TREE,                        \
                                .data.tree = {__VA_ARGS__}})

// Multifocuses Functionality
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#define INITIAL_MULTIFOCUS_CAPACITY 4096

struct multifocus {
    size_t count, capacity;
    struct node *nodes;
};

#define alloc_focus(initial_capacity)                                          \
    ((struct multifocus){                                                      \
        .count = 0,                                                            \
        .capacity = (initial_capacity),                                        \
        .nodes = xmalloc(sizeof(struct node) * (initial_capacity)),            \
    })

#define free_focus(focus) free((focus).nodes)

COMPILER_NONNULL(1) COMPILER_COLD //
static void
expand_focus(struct multifocus *const restrict focus) {
    assert(focus);
    XASSERT(focus->count == focus->capacity);
    XASSERT(focus->nodes);

    focus->nodes =
        xrealloc(focus->nodes, sizeof focus->nodes[0] * (focus->capacity *= 2));
}

COMPILER_NONNULL(1) COMPILER_HOT COMPILER_ALWAYS_INLINE //
inline static void
focus_on(struct multifocus *const restrict focus, const struct node node) {
    assert(focus);
    XASSERT(node.ports);
    XASSERT(focus->count <= focus->capacity);
    XASSERT(focus->nodes);

    if (focus->count == focus->capacity) { expand_focus(focus); }

    focus->nodes[focus->count++] = node;
}

#ifdef OPTISCOPE_ENABLE_GRAPHVIZ

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT //
static bool
is_focused_on(const struct multifocus focus, const struct node node) {
    XASSERT(node.ports);
    XASSERT(focus.nodes);

    for (size_t i = 0; i < focus.count; i++) {
        if (focus.nodes[i].ports == node.ports) { return true; }
    }

    return false;
}

#endif // OPTISCOPE_ENABLE_GRAPHVIZ

COMPILER_NONNULL(1) COMPILER_HOT COMPILER_ALWAYS_INLINE //
inline static struct node
unfocus(struct multifocus *const restrict focus) {
    assert(focus);
    XASSERT(focus->count > 0);
    XASSERT(focus->count <= focus->capacity);
    XASSERT(focus->nodes);

    return focus->nodes[--focus->count];
}

#define CONSUME_MULTIFOCUS(focus, f)                                           \
    for (struct node f = {NULL};                                               \
         (focus)->count > 0 ? (f = unfocus((focus)), true) : false;            \
         (void)0)

// Dynamic Book of Expansions
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#define INITIAL_BOOK_CAPACITY 64

struct expansion {
    // A pointer to a user-provided function for expansion.
    struct lambda_term *(*function)(void);

    // It is crucial to store an expansion itself, as the bytecode will use
    // memory allocated for the expansion.
    struct lambda_term *expansion;

    // The bytecode that describes how to build a net.
    struct bytecode bc;
};

struct book {
    size_t count, capacity;
    struct expansion *entries;
};

#define alloc_expansion(user_function)                                         \
    ((struct expansion){                                                       \
        .function = (user_function),                                           \
        .expansion = NULL,                                                     \
        .bc = alloc_bytecode(INITIAL_BYTECODE_CAPACITY),                       \
    })

#define alloc_book(initial_capacity)                                           \
    ((struct book){                                                            \
        .count = 0,                                                            \
        .capacity = (initial_capacity),                                        \
        .entries = xcalloc((initial_capacity), sizeof(struct expansion)),      \
    })

COMPILER_COLD //
static void
free_expansion(const struct expansion entry) {
    XASSERT(entry.function);

    if (entry.expansion) {
        // The function was actually called; free the term it generated.
        free_lambda_term(entry.expansion);
    }

    // The bytecode is allocated unconditionally.
    free_bytecode(entry.bc);
}

COMPILER_COLD //
static void
free_book(const struct book book) {
    XASSERT(book.entries);

    for (size_t i = 0; i < book.count; i++) { free_expansion(book.entries[i]); }

    free(book.entries);
}

COMPILER_NONNULL(1) COMPILER_COLD //
static void
expand_book(struct book *const restrict book) {
    assert(book);
    XASSERT(book->count == book->capacity);
    XASSERT(book->entries);

    book->entries = xrealloc(
        book->entries, sizeof book->entries[0] * (book->capacity *= 2));
}

// Returnes the index of a `function` entry occurring in `book`, creating a new
// entry if there is no such function yet.
COMPILER_WARN_UNUSED_RESULT COMPILER_NONNULL(1, 2) COMPILER_COLD //
static size_t
lookup_function(
    struct book *const restrict book,
    struct lambda_term *(*const function)(void)) {
    assert(book);
    XASSERT(book->entries);
    assert(function);

    for (size_t i = 0; i < book->count; i++) {
        if (function == book->entries[i].function) { return i; }
    }

    if (book->count == book->capacity) { expand_book(book); }

    const size_t index = book->count++;
    book->entries[index] = alloc_expansion(function);
    return index;
}

// Main Context Functionality
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

struct context {
    // The root node of the graph (always `SYMBOL_ROOT`).
    struct node root;

    // The cache that stores already performed expansions.
    struct book book;

    // The multifocus for garbage collection purposes.
    struct multifocus gc_focus;

    // Whether a full rescan from the root is needed after garbage collection.
    bool rescan;

#ifdef OPTISCOPE_ENABLE_STATS
    // The number of all proper interactions.
    uint64_t ninteractions;
    // The numbers of all interactions involving duplicators/delimiters.
    uint64_t nduplicator_itrs, ndelimiter_itrs;
    // The numbers of all non-interaction graph rewrites.
    uint64_t nmergings, nextrusions, ngc;
    // The memory usage statistics.
    uint64_t ntotal, nmax_total;
#endif
};

COMPILER_NONNULL(1) COMPILER_COLD //
static void
free_context(struct context *const restrict graph);

// clang-format off
COMPILER_MALLOC(free_context, 1) COMPILER_RETURNS_NONNULL
COMPILER_WARN_UNUSED_RESULT COMPILER_COLD
// clang-format on
static struct context *
alloc_context(void) {
    const struct node root = {(uint64_t *)xcalloc(2, sizeof(uint64_t)) + 1};
    root.ports[-1] = SYMBOL_ROOT;
    root.ports[0] = PORT_VALUE(UINT64_C(0), PHASE_DEFAULT, UINT64_C(0));

    struct context *const graph = xcalloc(1, sizeof *graph);
    graph->root = root;
    graph->book = alloc_book(INITIAL_BOOK_CAPACITY);
    graph->gc_focus = alloc_focus(INITIAL_MULTIFOCUS_CAPACITY);
    graph->rescan = false;
    // The rest of the fields are zeroed out by `xcalloc`.

    return graph;
}

COMPILER_NONNULL(1) COMPILER_COLD //
static void
free_context(struct context *const restrict graph) {
    debug("%s()", __func__);

    assert(graph);
    XASSERT(graph->root.ports);

    free(graph->root.ports - 1 /* back to the symbol */);
    free_book(graph->book);
    free_focus(graph->gc_focus);
    free(graph);
}

#ifdef OPTISCOPE_ENABLE_STATS

COMPILER_NONNULL(1) //
static void
print_stats(const struct context *const restrict graph) {
    assert(graph);

    const uint64_t ntotal_rewrites = graph->ninteractions + graph->nmergings +
                                     graph->nextrusions + graph->ngc;

    const uint64_t nbookkeeping_rewrites =
        graph->ndelimiter_itrs + graph->nmergings + graph->nextrusions;

    const double sharing_work =
        ((double)graph->nduplicator_itrs / (double)ntotal_rewrites) * 100.0;

    const double bookkeeping_work =
        ((double)nbookkeeping_rewrites / (double)ntotal_rewrites) * 100.0;

    const double gc_work =
        ((double)graph->ngc / (double)ntotal_rewrites) * 100.0;

    printf("    Total rewrites: %" PRIu64 "\n", ntotal_rewrites);
    printf("Total interactions: %" PRIu64 "\n", graph->ninteractions);
    printf("      Sharing work: %.2f%%\n", sharing_work);
    printf("  Bookkeeping work: %.2f%%\n", bookkeeping_work);
    printf("           GC work: %.2f%%\n", gc_work);
    printf("   Peak node count: %" PRIu64 "\n", graph->nmax_total);
}

#else

#define print_stats(graph) ((void)0)

#endif // OPTISCOPE_ENABLE_STATS

COMPILER_WARN_UNUSED_RESULT COMPILER_NONNULL(1) COMPILER_HOT //
static struct node
alloc_node_from(
    struct context *const restrict graph,
    const uint64_t symbol,
    const struct node *const restrict prototype) {
    assert(graph);
    XASSERT(SYMBOL_ROOT != symbol);
    if (prototype) { XASSERT(prototype->ports); }

#ifndef NDEBUG
    if (prototype) {
        if (IS_DUPLICATOR(symbol)) {
            assert(IS_DUPLICATOR(prototype->ports[-1]));
        } else if (IS_DELIMITER(symbol)) {
            assert(IS_DELIMITER(prototype->ports[-1]));
        } else {
            assert(symbol == prototype->ports[-1]);
        }
    }
#endif

    (void)graph; // `graph` is onely needed for `OPTISCOPE_ENABLE_STATS`

    uint64_t *p = NULL;

#define SET_SYMBOL() (p[-1] = symbol)
#define SET_PORTS_0()                                                          \
    (SET_SYMBOL(), p[0] = PORT_VALUE(UINT64_C(0), PHASE_DEFAULT, UINT64_C(0)))
#define SET_PORTS_1()                                                          \
    (SET_PORTS_0(), p[1] = PORT_VALUE(UINT64_C(1), UINT64_C(0), UINT64_C(0)))
#define SET_PORTS_2()                                                          \
    (SET_PORTS_1(), p[2] = PORT_VALUE(UINT64_C(2), UINT64_C(0), UINT64_C(0)))
#define SET_PORTS_3()                                                          \
    (SET_PORTS_2(), p[3] = PORT_VALUE(UINT64_C(3), UINT64_C(0), UINT64_C(0)))

    switch (symbol) {
    case SYMBOL_ERASER:
    case SYMBOL_IDENTITY_LAMBDA:
        p = ALLOC_POOL_OBJECT(u64x2_pool);
        SET_PORTS_0();
        break;
    case SYMBOL_GC_LAMBDA:
        p = ALLOC_POOL_OBJECT(u64x3_pool);
        SET_PORTS_1();
        break;
    case SYMBOL_CELL:
    case SYMBOL_REFERENCE:
        p = ALLOC_POOL_OBJECT(u64x3_pool);
        if (prototype) { p[1] = prototype->ports[1]; }
        SET_PORTS_0();
        break;
    case SYMBOL_APPLICATOR:
    case SYMBOL_LAMBDA:
    case SYMBOL_LAMBDA_C:
    case SYMBOL_PERFORM:
    duplicator:
        p = ALLOC_POOL_OBJECT(u64x4_pool);
        SET_PORTS_2();
        break;
    case SYMBOL_UNARY_CALL:
    case SYMBOL_BARRIER:
    case SYMBOL_GC_DUPLICATOR_LEFT:
    case SYMBOL_GC_DUPLICATOR_RIGHT:
    delimiter:
        p = ALLOC_POOL_OBJECT(u64x4_pool);
        if (prototype) { p[2] = prototype->ports[2]; }
        SET_PORTS_1();
        break;
    case SYMBOL_IF_THEN_ELSE:
        p = ALLOC_POOL_OBJECT(u64x5_pool);
        SET_PORTS_3();
        break;
    case SYMBOL_BINARY_CALL:
        p = ALLOC_POOL_OBJECT(u64x5_pool);
        if (prototype) { p[3] = prototype->ports[3]; }
        SET_PORTS_2();
        break;
    case SYMBOL_BINARY_CALL_AUX:
        p = ALLOC_POOL_OBJECT(u64x5_pool);
        if (prototype) {
            p[2] = prototype->ports[2];
            p[3] = prototype->ports[3];
        }
        SET_PORTS_1();
        break;
    default:
        if (IS_DUPLICATOR(symbol)) goto duplicator;
        else if (IS_DELIMITER(symbol)) goto delimiter;
        else COMPILER_UNREACHABLE();
    }

#undef SET_PORTS_3
#undef SET_PORTS_2
#undef SET_PORTS_1
#undef SET_PORTS_0
#undef SET_SYMBOL

#ifdef OPTISCOPE_ENABLE_STATS
    graph->ntotal++;
    if (graph->ntotal > graph->nmax_total) {
        graph->nmax_total = graph->ntotal;
    }
#endif

#ifdef OPTISCOPE_ENABLE_TRACING
    char *const snode = print_node((struct node){p});
    debug("🔨 %s", snode);
    free(snode);
#endif

    return (struct node){p};
}

#define alloc_node(graph, symbol) alloc_node_from((graph), (symbol), NULL)

COMPILER_WARN_UNUSED_RESULT COMPILER_NONNULL(1, 2) COMPILER_HOT //
static struct node
alloc_gc_node(
    struct context *const restrict graph, uint64_t *const restrict points_to) {
    assert(graph);
    assert(points_to);

    const struct node node = alloc_node(graph, SYMBOL_ERASER);
    // Mark this eraser as garbage-collecting, which is necessary for
    // successfull operation of the garbage collector.
    set_phase(&node.ports[0], PHASE_GC);
    connect_ports(&node.ports[0], points_to);

    return node;
}

COMPILER_NONNULL(1) COMPILER_HOT //
static void
free_node(struct context *const restrict graph, const struct node node) {
    debug("🧹 %p", (void *)node.ports);

    assert(graph);
    XASSERT(node.ports);

    (void)graph; // `graph` is onely needed for `OPTISCOPE_ENABLE_STATS`

    const uint64_t symbol = node.ports[-1];
    XASSERT(SYMBOL_ROOT != symbol);

    uint64_t *const p = node.ports;

#ifdef COMPILER_ASAN_AVAILABLE
    {
        if (COMPILER_IS_POISONED_ADDRESS(p - 1)) {
            // Invoke AddressSanitizer's use-after-poison report.
            p[-1] = 666;
        }
    }
#endif

    switch (symbol) {
    case SYMBOL_ERASER:
    case SYMBOL_IDENTITY_LAMBDA: FREE_POOL_OBJECT(u64x2_pool, p); break;
    case SYMBOL_GC_LAMBDA:
    case SYMBOL_CELL:
    case SYMBOL_REFERENCE: FREE_POOL_OBJECT(u64x3_pool, p); break;
    case SYMBOL_APPLICATOR:
    case SYMBOL_LAMBDA:
    case SYMBOL_LAMBDA_C:
    case SYMBOL_UNARY_CALL:
    case SYMBOL_PERFORM:
    case SYMBOL_BARRIER:
    case SYMBOL_GC_DUPLICATOR_LEFT:
    case SYMBOL_GC_DUPLICATOR_RIGHT:
    duplicator:
    delimiter:
        FREE_POOL_OBJECT(u64x4_pool, p);
        break;
    case SYMBOL_BINARY_CALL:
    case SYMBOL_BINARY_CALL_AUX:
    case SYMBOL_IF_THEN_ELSE: FREE_POOL_OBJECT(u64x5_pool, p); break;
    default:
        if (IS_DUPLICATOR(symbol)) goto duplicator;
        else if (IS_DELIMITER(symbol)) goto delimiter;
        else COMPILER_UNREACHABLE();
    }

#ifdef OPTISCOPE_ENABLE_STATS
    graph->ntotal--;
#endif
}

// Delimiter Functionality
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

struct delimiter {
    const uint64_t idx;
    uint64_t *const restrict points_to, *const restrict goes_from;
};

#ifndef NDEBUG

static void
assert_delimiter_template(const struct delimiter template) {
    assert(template.idx < INDEX_RANGE);
    assert(template.points_to);
    assert(template.goes_from);
}

#else

#define assert_delimiter_template(template) ((void)0)

#endif // NDEBUG

COMPILER_NONNULL(1) COMPILER_HOT //
static void
inst_delimiter_as_is(
    struct context *const restrict graph,
    const struct delimiter template,
    const uint64_t count) {
    assert(graph);
    assert_delimiter_template(template);

    const struct node node = alloc_node(graph, SYMBOL_DELIMITER(template.idx));
    node.ports[2] = count;
    connect_ports(&node.ports[0], template.points_to);
    connect_ports(&node.ports[1], template.goes_from);
}

COMPILER_NONNULL(1) COMPILER_HOT //
static void
inst_delimiter(
    struct context *const restrict graph,
    const struct delimiter template,
    const uint64_t count) {
    assert(graph);
    assert_delimiter_template(template);

    if (0 == count) {
        connect_ports(template.points_to, template.goes_from);
        return;
    }

    const struct node g = node_of_port(template.points_to);
    XASSERT(g.ports);

    const bool condition = IS_DELIMITER(g.ports[-1]) &&
                           1 == DECODE_OFFSET_METADATA(*template.points_to) &&
                           SYMBOL_DELIMITER(template.idx) == g.ports[-1];
    if (condition) {
        g.ports[2] = checked_add(g.ports[2], count);
        connect_ports(&g.ports[1], template.goes_from);
#ifdef OPTISCOPE_ENABLE_STATS
        graph->nmergings++;
#endif
    } else {
        inst_delimiter_as_is(graph, template, count);
    }
}

COMPILER_NONNULL(1) COMPILER_HOT //
static void
try_merge_delimiter(struct context *const restrict graph, const struct node f) {
    assert(graph);
    XASSERT(f.ports);
    XASSERT(IS_DELIMITER(f.ports[-1]));

    uint64_t *const points_to = DECODE_ADDRESS(f.ports[0]);
    XASSERT(points_to);

    const struct node g = node_of_port(points_to);
    XASSERT(g.ports);

    const bool condition = IS_DELIMITER(g.ports[-1]) &&
                           1 == DECODE_OFFSET_METADATA(*points_to) &&
                           f.ports[-1] == g.ports[-1];
    if (condition) {
        g.ports[2] = checked_add(g.ports[2], f.ports[2]);
        connect_ports(&g.ports[1], DECODE_ADDRESS(f.ports[1]));
#ifdef OPTISCOPE_ENABLE_STATS
        graph->nmergings++;
#endif
    } else if (is_atomic_symbol(g.ports[-1])) {
        connect_ports(&g.ports[0], DECODE_ADDRESS(f.ports[1]));
#ifdef OPTISCOPE_ENABLE_STATS
        graph->ninteractions++;
        graph->ndelimiter_itrs++;
#endif
    } else {
        return;
    }

    free_node(graph, f);
}

COMPILER_NONNULL(1) COMPILER_HOT //
static void
try_merge_if_delimiter(
    struct context *const restrict graph, const struct node f) {
    assert(graph);
    XASSERT(f.ports);

    if (IS_DELIMITER(f.ports[-1])) { try_merge_delimiter(graph, f); }
}

// Immediate Duplication
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

COMPILER_NONNULL(1) COMPILER_HOT //
static void
try_duplicate(struct context *const restrict graph, const struct node f) {
    assert(graph);
    XASSERT(f.ports);
    XASSERT(IS_DUPLICATOR(f.ports[-1]));

    const struct node g = follow_port(f, 0);
    XASSERT(g.ports);

    const bool condition =
        is_atomic_symbol(g.ports[-1]) && SYMBOL_REFERENCE != g.ports[-1];
    if (condition) {
        const struct node gx = alloc_node_from(graph, g.ports[-1], &g);
        connect_ports(&g.ports[0], DECODE_ADDRESS(f.ports[1]));
        connect_ports(&gx.ports[0], DECODE_ADDRESS(f.ports[2]));
        free_node(graph, f);
#ifdef OPTISCOPE_ENABLE_STATS
        graph->ninteractions++;
        graph->nduplicator_itrs++;
#endif
    }
}

// Graphviz Graph Generation
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#ifdef OPTISCOPE_ENABLE_GRAPHVIZ

#define GRAPHVIZ_INDENT "    "

COMPILER_MALLOC(free, 1) COMPILER_RETURNS_NONNULL COMPILER_WARN_UNUSED_RESULT //
static char *
graphviz_node_xlabel(const struct node node) {
    XASSERT(node.ports);

    static const char address_line[] = "+----------------+";

    const uint64_t *const p = node.ports;

#define FORMAT_BLOCK(fmt, ...)                                                 \
    format_string("%s" fmt "%s", address_line, __VA_ARGS__, address_line)

    switch (ports_count(p[-1])) {
    case 1: return FORMAT_BLOCK("<BR/>| %p |<BR/>", (void *)&p[0]);
    case 2:
        return FORMAT_BLOCK(
            "<BR/>| %p |<BR/>| %p |<BR/>", (void *)&p[0], (void *)&p[1]);
    case 3:
        return FORMAT_BLOCK(
            "<BR/>| %p |<BR/>| %p |<BR/>| %p |<BR/>",
            (void *)&p[0],
            (void *)&p[1],
            (void *)&p[2]);
    case 4:
        return FORMAT_BLOCK(
            "<BR/>| %p |<BR/>| %p |<BR/>| %p |<BR/>| %p |<BR/>",
            (void *)&p[0],
            (void *)&p[1],
            (void *)&p[2],
            (void *)&p[3]);
    default: COMPILER_UNREACHABLE();
    }

#undef FORMAT_BLOCK
}

COMPILER_MALLOC(free, 1) COMPILER_RETURNS_NONNULL COMPILER_WARN_UNUSED_RESULT //
static char *
graphviz_edge_label(const struct node node, const uint8_t i) {
    XASSERT(node.ports);

    switch (node.ports[-1]) {
    case SYMBOL_APPLICATOR:
        switch (i) {
        case 0: return format_string("rator (\\#%" PRIu8 ")", i); break;
        case 1: return format_string("\\#%" PRIu8, i); break;
        case 2: return format_string("rand (\\#%" PRIu8 ")", i); break;
        default: COMPILER_UNREACHABLE();
        }
    case SYMBOL_LAMBDA:
    case SYMBOL_LAMBDA_C:
        switch (i) {
        case 0: return format_string("\\#%" PRIu8, i); break;
        case 1: return format_string("binder (\\#%" PRIu8 ")", i); break;
        case 2: return format_string("body (\\#%" PRIu8 ")", i); break;
        default: COMPILER_UNREACHABLE();
        }
    case SYMBOL_GC_LAMBDA:
        switch (i) {
        case 0: return format_string("\\#%" PRIu8, i); break;
        case 1: return format_string("body (\\#%" PRIu8 ")", i); break;
        default: COMPILER_UNREACHABLE();
        }
    default: return format_string("\\#%" PRIu8, i);
    }
}

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT COMPILER_RETURNS_NONNULL //
static const char *
graphviz_edge_tailport(const struct node node, const uint8_t i) {
    XASSERT(node.ports);

    switch (node.ports[-1]) {
    case SYMBOL_ROOT:
        switch (i) {
        case 0: return "s";
        default: COMPILER_UNREACHABLE();
        }
    case SYMBOL_ERASER:
    case SYMBOL_CELL:
    case SYMBOL_IDENTITY_LAMBDA:
    case SYMBOL_REFERENCE:
        switch (i) {
        case 0: return "n";
        default: COMPILER_UNREACHABLE();
        }
    case SYMBOL_GC_LAMBDA:
        switch (i) {
        case 0: return "n";
        case 1: return "s";
        default: COMPILER_UNREACHABLE();
        }
    case SYMBOL_UNARY_CALL:
    case SYMBOL_BINARY_CALL_AUX:
    case SYMBOL_BARRIER:
    case SYMBOL_GC_DUPLICATOR_LEFT:
    case SYMBOL_GC_DUPLICATOR_RIGHT:
    delimiter:
        switch (i) {
        case 0: return "s";
        case 1: return "n";
        default: COMPILER_UNREACHABLE();
        }
    case SYMBOL_APPLICATOR:
        switch (i) {
        case 0: return "s";
        case 1: return "n";
        case 2: return "e";
        default: COMPILER_UNREACHABLE();
        }
    case SYMBOL_LAMBDA:
    case SYMBOL_LAMBDA_C:
        switch (i) {
        case 0: return "n";
        case 1: return "e";
        case 2: return "s";
        default: COMPILER_UNREACHABLE();
        }
    case SYMBOL_BINARY_CALL:
    case SYMBOL_PERFORM:
        switch (i) {
        case 0: return "sw";
        case 1: return "n";
        case 2: return "se";
        default: COMPILER_UNREACHABLE();
        }
    case SYMBOL_IF_THEN_ELSE:
        switch (i) {
        case 0: return "sw";
        case 1: return "n";
        case 2: return "se";
        case 3: return "s";
        default: COMPILER_UNREACHABLE();
        }
    duplicator:
        switch (i) {
        case 0: return "s";
        case 1: return "nw";
        case 2: return "ne";
        default: COMPILER_UNREACHABLE();
        }
    default:
        if (IS_DUPLICATOR(node.ports[-1])) goto duplicator;
        else if (IS_DELIMITER(node.ports[-1])) goto delimiter;
        else COMPILER_UNREACHABLE();
    }
}

COMPILER_MALLOC(free, 1) COMPILER_RETURNS_NONNULL COMPILER_WARN_UNUSED_RESULT //
static char *
graphviz_print_symbol(const struct node node) {
    XASSERT(node.ports);

    const uint64_t *const p = node.ports;

    char *const ssymbol = print_symbol(p[-1]);

    char *result = NULL;

    if (SYMBOL_CELL == p[-1]) {
        result = format_string("%s %" PRIu64, ssymbol, p[1]);
    } else if (SYMBOL_BINARY_CALL_AUX == p[-1]) {
        result = format_string("%s %" PRIu64, ssymbol, p[3]);
    } else if (SYMBOL_BARRIER == p[-1]) {
        result = format_string("%s %" PRIu64, ssymbol, p[2]);
    } else if (IS_GC_DUPLICATOR(p[-1])) {
        result = format_string("%s/%" PRIu64, ssymbol, p[2]);
    } else if (IS_DELIMITER(p[-1])) {
        result = format_string("%s %" PRIu64, ssymbol, p[2]);
    } else {
        result = format_string("%s", ssymbol);
    }

    free(ssymbol);

    return result;
}

struct graphviz_context {
    struct context *graph;
    struct multifocus history;
    FILE *stream;
};

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT //
inline static bool
graphviz_is_current_node(const struct node node) {
    XASSERT(node.ports);

    return PHASE_CURRENT_PAIR == DECODE_PHASE_METADATA(node.ports[0]);
}

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT //
inline static bool
graphviz_is_active_node(const struct node node) {
    XASSERT(node.ports);

    const struct node f = node, g = follow_port(node, 0);

    return points_to(g, f) && SYMBOL_ROOT != node.ports[-1];
}

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT //
inline static bool
graphviz_is_active_edge(const struct node node, const uint8_t i) {
    XASSERT(node.ports);

    const struct node f = node, g = follow_port(node, i);

    return is_interaction(f, g);
}

COMPILER_NONNULL(1) //
static void
graphviz_draw_node(
    struct graphviz_context *const restrict ctx, const struct node node) {
    assert(ctx);
    XASSERT(ctx->stream);
    XASSERT(node.ports);

    const uint64_t *const p = node.ports;

    const bool is_active = graphviz_is_active_node(node),
               is_current = graphviz_is_current_node(node),
               is_root = SYMBOL_ROOT == p[-1];

    char *const ssymbol = graphviz_print_symbol(node);
    char *const xlabel = graphviz_node_xlabel(node);

    fprintf(
        ctx->stream,
        // clang-format off
        GRAPHVIZ_INDENT "n%p"
        " [label=\"%s\""
        ", xlabel=<<FONT FACE=\"Courier\" COLOR=\"blue\" POINT-SIZE=\"8\">%s</FONT>>"
        "%s%s%s];\n",
        // clang-format on
        (void *)p,
        ssymbol,
        xlabel,
        (is_current && !is_root
             ? ", color=darkred"
             : (is_active && !is_root ? ", color=darkgreen" : "")),
        (is_active ? ", penwidth=2.3" : ""),
        (is_root ? ", style=filled" : ""));

    free(ssymbol);
    free(xlabel);
}

COMPILER_NONNULL(1) //
static void
graphviz_draw_edge(
    struct graphviz_context *const restrict ctx,
    const struct node source,
    const uint8_t i) {
    assert(ctx);
    XASSERT(source.ports);

    uint64_t *const target_port = DECODE_ADDRESS(source.ports[i]);
    const struct node target = node_of_port(target_port);
    const bool is_active = graphviz_is_active_edge(source, i);
    char *const edge_label = graphviz_edge_label(source, i);

    fprintf(
        ctx->stream,
        // clang-format off
        GRAPHVIZ_INDENT "n%p -> n%p [label=\" %s \", tailport=%s%s%s%s%s];\n",
        // clang-format on
        (void *)source.ports,
        (void *)target.ports,
        edge_label,
        graphviz_edge_tailport(source, i),
        (is_active && graphviz_is_current_node(source)
             ? ", color=darkred"
             : (is_active ? ", color=darkgreen" : "")),
        (is_active ? ", penwidth=1.5" : ""),
        (IS_PRINCIPAL_PORT(*target_port) ? ", arrowhead=dot" : ""),
        (0 == i ? ", style=dashed" : ""));

    free(edge_label);
}

COMPILER_NONNULL(1) //
static void
go_graphviz(
    struct graphviz_context *const restrict ctx,
    const struct node source,
    const uint8_t i) {
    assert(ctx);
    XASSERT(ctx->stream);
    XASSERT(source.ports);

    const struct node node = follow_port(source, i);

    if (is_focused_on(ctx->history, node)) { return; }

    focus_on(&ctx->history, node);

    graphviz_draw_node(ctx, node);

    FOR_ALL_PORTS (node, j, 0) { graphviz_draw_edge(ctx, node, j); }

    FOR_ALL_PORTS (node, j, 0) { go_graphviz(ctx, node, j); }
}

COMPILER_NONNULL(1, 2) //
static void
graphviz(
    struct context *const restrict graph, //
    const char *const restrict filename) {
    debug("%s(\"%s\")", __func__, filename);

    assert(graph);
    assert(filename);

    FILE *fp = fopen(filename, "w");
    if (NULL == fp) {
        perror("fopen");
        abort();
    }

    fprintf(fp, "digraph {\n");
    fprintf(fp, GRAPHVIZ_INDENT "graph [nodesep=0.5, ranksep=0.8];\n");
    fprintf(fp, GRAPHVIZ_INDENT "node [fontname=\"bold helvetica\"];\n");
    fprintf(
        fp,
        GRAPHVIZ_INDENT
        "edge [fontname=\"bold helvetica\", fontsize=11, fontcolor=darkblue];\n");
    fprintf(
        fp, GRAPHVIZ_INDENT "{ rank=min; n%p }\n", (void *)graph->root.ports);
    struct graphviz_context ctx = {
        .graph = graph,
        .history = alloc_focus(INITIAL_MULTIFOCUS_CAPACITY),
        .stream = fp,
    };
    go_graphviz(&ctx, graph->root, 0);
    free_focus(ctx.history);
    fprintf(fp, "}\n");

    IO_CALL(fclose, fp);
}

#undef GRAPHVIZ_INDENT

#else

#define graphviz(graph, filename) ((void)0)

#endif // OPTISCOPE_ENABLE_GRAPHVIZ

#if !defined(NDEBUG) && defined(OPTISCOPE_ENABLE_STEP_BY_STEP)

COMPILER_NONNULL(1) //
static void
wait_for_user(
    struct context *const restrict graph,
    const struct node f,
    const struct node g) {
    assert(graph);

#ifdef OPTISCOPE_ENABLE_GRAPHVIZ
    set_phase(&f.ports[0], PHASE_CURRENT_PAIR);
    set_phase(&g.ports[0], PHASE_CURRENT_PAIR);
    graphviz(graph, "target/state.dot");
    set_phase(&f.ports[0], PHASE_DEFAULT);
    set_phase(&g.ports[0], PHASE_DEFAULT);
    if (system("./command/graphviz-state.sh") != 0) {
        panic("Failed to run `./command/graphviz-state.sh`!");
    }
#endif

    printf("Press ENTER to proceed...");
    fflush(stdout);
    if (EOF == getchar()) {
        perror("getchar");
        abort();
    }
}

#else

#define wait_for_user(graph) ((void)0)

#endif // !defined(NDEBUG) && defined(OPTISCOPE_ENABLE_STEP_BY_STEP)

// Bytecode Emission
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

COMPILER_CONST COMPILER_WARN_UNUSED_RESULT //
inline static uint64_t
de_bruijn_level_to_index(const uint64_t lvl, const uint64_t var) {
    return lvl - var - 1;
}

COMPILER_NONNULL(1, 2, 3) COMPILER_COLD //
static void
emit_bytecode(
    struct context *const restrict graph,
    struct bytecode *const restrict bc,
    struct lambda_term *const restrict term,
    const uint64_t lvl) {
    assert(graph);
    assert(term);

    switch (term->ty) {
    case LAMBDA_TERM_LAMBDA: {
        struct lambda_data *const binder = term->data.lam;
        struct lambda_term *const body = term->data.lam->body;
        XASSERT(binder);
        XASSERT(body);

        const bool is_identity =
            LAMBDA_TERM_VAR == body->ty && binder == *body->data.var;
        if (is_identity) {
            const struct node lam = alloc_node(graph, SYMBOL_IDENTITY_LAMBDA);
            BC_ATTACH_NODE(bc, lam, 0, &term->connect_to);
            break;
        }

        if (0 == binder->nusages) {
            // This is a lambda that "garbage-collects" its argument.
            const struct node lam = alloc_node(graph, SYMBOL_GC_LAMBDA);
            BC_ATTACH_NODE(bc, lam, 0, &term->connect_to);
            BC_SAVE_PORT(bc, &body->connect_to, 1);
            emit_bytecode(graph, bc, body, lvl + 1);
            break;
        }

        const struct node lam = alloc_node(
            graph, term->fv_count > 0 ? SYMBOL_LAMBDA : SYMBOL_LAMBDA_C);

        binder->binder_ports = xmalloc(sizeof(uint64_t *) * binder->nusages);
        binder->lvl = lvl;
        BC_ATTACH_NODE(bc, lam, 0, &term->connect_to);
        if (1 == binder->nusages) {
            // This is a linear non-self-referential lambda.
            BC_SAVE_PORT(bc, &binder->binder_ports[0], 1);
        } else {
            // This is a non-linear lambda that needs a duplicator tree.
            BC_TREE(bc, binder->nusages, binder->binder_ports);
        }
        uint64_t **const binder_ports = binder->binder_ports;
        BC_SAVE_PORT(bc, &body->connect_to, 2);
        emit_bytecode(graph, bc, body, lvl + 1);
        binder->binder_ports = binder_ports;

        break;
    }
    case LAMBDA_TERM_VAR: {
        struct lambda_data *const binder = *term->data.var;
        XASSERT(binder);
        XASSERT(binder->binder_ports);

        const uint64_t idx = de_bruijn_level_to_index(lvl, binder->lvl);
        if (0 == idx) {
            BC_CONNECT(bc, binder->binder_ports++, &term->connect_to);
        } else {
            BC_DELIMIT(bc, binder->binder_ports++, &term->connect_to, idx);
        }

        break;
    }
    case LAMBDA_TERM_APPLY: {
        struct lambda_term *const rator = term->data.app.rator, //
            *const rand = term->data.app.rand;

        const struct node app = alloc_node(graph, SYMBOL_APPLICATOR);

        BC_ATTACH_NODE(bc, app, 1, &term->connect_to);
        BC_SAVE_PORT(bc, &rator->connect_to, 0);
        BC_SAVE_PORT(bc, &rand->connect_to, 2);
        emit_bytecode(graph, bc, rator, lvl);
        emit_bytecode(graph, bc, rand, lvl);

        break;
    }
    case LAMBDA_TERM_CELL: {
        const struct node cell = alloc_node(graph, SYMBOL_CELL);
        cell.ports[1] = term->data.cell;

        BC_ATTACH_NODE(bc, cell, 0, &term->connect_to);

        break;
    }
    case LAMBDA_TERM_UNARY_CALL: {
        uint64_t (*const function)(uint64_t) = term->data.ucall.function;
        struct lambda_term *const rand = term->data.ucall.rand;

        const struct node ucall = alloc_node(graph, SYMBOL_UNARY_CALL);
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
        ucall.ports[2] = U64_OF_FUNCTION(function);
#pragma GCC diagnostic pop

        BC_ATTACH_NODE(bc, ucall, 1, &term->connect_to);
        BC_SAVE_PORT(bc, &rand->connect_to, 0);
        emit_bytecode(graph, bc, rand, lvl);

        break;
    }
    case LAMBDA_TERM_BINARY_CALL: {
        uint64_t (*const function)(uint64_t, uint64_t) = //
            term->data.bcall.function;
        struct lambda_term *const lhs = term->data.bcall.lhs, //
            *const rhs = term->data.bcall.rhs;

        const struct node bcall = alloc_node(graph, SYMBOL_BINARY_CALL);
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
        bcall.ports[3] = U64_OF_FUNCTION(function);
#pragma GCC diagnostic pop

        BC_ATTACH_NODE(bc, bcall, 1, &term->connect_to);
        BC_SAVE_PORT(bc, &lhs->connect_to, 0);
        BC_SAVE_PORT(bc, &rhs->connect_to, 2);
        emit_bytecode(graph, bc, lhs, lvl);
        emit_bytecode(graph, bc, rhs, lvl);

        break;
    }
    case LAMBDA_TERM_IF_THEN_ELSE: {
        struct lambda_term *const condition = term->data.ite.condition, //
            *const if_then = term->data.ite.if_then,                    //
                *const if_else = term->data.ite.if_else;

        const struct node ite = alloc_node(graph, SYMBOL_IF_THEN_ELSE);

        BC_ATTACH_NODE(bc, ite, 1, &term->connect_to);
        BC_SAVE_PORT(bc, &condition->connect_to, 0);
        BC_SAVE_PORT(bc, &if_else->connect_to, 2);
        BC_SAVE_PORT(bc, &if_then->connect_to, 3);
        emit_bytecode(graph, bc, condition, lvl);
        emit_bytecode(graph, bc, if_else, lvl);
        emit_bytecode(graph, bc, if_then, lvl);

        break;
    }
    case LAMBDA_TERM_PERFORM: {
        struct lambda_term *const action = term->data.perf.action, //
            *const k = term->data.perf.k;

        const struct node perf = alloc_node(graph, SYMBOL_PERFORM);

        BC_ATTACH_NODE(bc, perf, 1, &term->connect_to);
        BC_SAVE_PORT(bc, &action->connect_to, 0);
        BC_SAVE_PORT(bc, &k->connect_to, 2);
        emit_bytecode(graph, bc, action, lvl);
        emit_bytecode(graph, bc, k, lvl);

        break;
    }
    case LAMBDA_TERM_REFERENCE: {
        struct lambda_term *(*const function)(void) = term->data.ref.function;

        const struct node ref = alloc_node(graph, SYMBOL_REFERENCE);
        ref.ports[1] = lookup_function(&graph->book, function);

        BC_ATTACH_NODE(bc, ref, 0, &term->connect_to);

        break;
    }
    default: COMPILER_UNREACHABLE();
    }
}

// Bytecode Execution
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

// Connect the interface of an expanded net to `target`.
#define plug_into(entry, target) ((entry)->expansion->connect_to = (target))

COMPILER_NONNULL(1, 2, 4) //
static void
build_duplicator_tree(
    struct context *const restrict graph,
    uint64_t *const restrict binder_port,
    const uint64_t n,
    uint64_t **const restrict ports) {
    assert(graph);
    assert(binder_port);
    assert(ports);
    XASSERT(n >= 2);

    struct node current = alloc_node(graph, SYMBOL_DUPLICATOR(0));
    ports[0] = &current.ports[1];
    ports[1] = &current.ports[2];

    for (uint64_t i = 2; i < n; i++) {
        const struct node dup = alloc_node(graph, SYMBOL_DUPLICATOR(0));
        ports[i] = &dup.ports[1];
        connect_ports(&dup.ports[2], &current.ports[0]);
        current = dup;
    }

    connect_ports(&current.ports[0], binder_port);
}

COMPILER_NONNULL(1) COMPILER_HOT //
static void
execute_bytecode(
    struct context *const restrict graph, const struct bytecode bc) {
    assert(graph);
    XASSERT(bc.instructions);

    struct node current = {NULL};

    for (size_t i = 0; i < bc.count; i++) {
        const struct bc_instruction instr = bc.instructions[i];

        switch (instr.ty) {
        case INSTRUCTION_ATTACH_NODE: {
            // clang-format off
            const struct node template = instr.data.attach_node.template;
            const uint64_t interface_port_idx = instr.data.attach_node.interface_port_idx;
            uint64_t **const connect_to = instr.data.attach_node.connect_to;
            // clang-format on

            current = alloc_node_from(graph, template.ports[-1], &template);
            connect_ports(&current.ports[interface_port_idx], *connect_to);

            break;
        }
        case INSTRUCTION_SAVE_PORT: {
            uint64_t **const location = instr.data.save_port.location;
            const uint64_t port_idx = instr.data.save_port.port_idx;

            *location = &current.ports[port_idx];

            break;
        }
        case INSTRUCTION_CONNECT: {
            uint64_t **const lhs = instr.data.connect.lhs, //
                **const rhs = instr.data.connect.rhs;

            connect_ports(*lhs, *rhs);

            break;
        }
        case INSTRUCTION_DELIMIT: {
            uint64_t **const points_to = instr.data.delimit.points_to, //
                **const goes_from = instr.data.delimit.goes_from;
            const uint64_t n = instr.data.delimit.n;

            inst_delimiter_as_is(
                graph, (struct delimiter){.idx = 0, *points_to, *goes_from}, n);

            break;
        }
        case INSTRUCTION_TREE:
            build_duplicator_tree(
                graph,
                &current.ports[1],
                instr.data.tree.n,
                instr.data.tree.locations);
            break;
        default: COMPILER_UNREACHABLE();
        }
    }
}

// Eraser-Passing Garbage Collection
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

// When the eraser `f` faces the binder port `i` of the lambda `g`.
COMPILER_NONNULL(1) COMPILER_HOT //
static void
gc_lambda_binder(
    struct context *const restrict graph,
    const struct node f,
    const struct node g,
    const ptrdiff_t i) {
    assert(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    XASSERT(1 == i);
    XASSERT(SYMBOL_ERASER == f.ports[-1]);
    XASSERT(IS_RELEVANT_LAMBDA(g.ports[-1]));

    const struct node replacement = alloc_node(graph, SYMBOL_GC_LAMBDA);

    connect_ports(&replacement.ports[0], DECODE_ADDRESS(g.ports[0]));
    connect_ports(&replacement.ports[1], DECODE_ADDRESS(g.ports[2]));

    free_node(graph, f);
    free_node(graph, g);
}

// When the eraser `f` faces the auxiliary port `i` of the duplicator `g`.
COMPILER_NONNULL(1) COMPILER_HOT //
static void
gc_duplicator_aux(
    struct context *const restrict graph,
    const struct node f,
    const struct node g,
    const ptrdiff_t i) {
    assert(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    XASSERT(1 == i || 2 == i);
    XASSERT(SYMBOL_ERASER == f.ports[-1]);
    XASSERT(IS_DUPLICATOR(g.ports[-1]));

    uint64_t *const points_to = DECODE_ADDRESS(g.ports[0]), //
        *const shares_with = DECODE_ADDRESS(g.ports[1 == i ? 2 : 1]);

    const struct node h = node_of_port(shares_with),
                      shared = node_of_port(points_to);

    if (SYMBOL_ERASER == h.ports[-1]) {
        assert(PHASE_GC == DECODE_PHASE_METADATA(h.ports[0]));
        connect_ports(&f.ports[0], points_to);
        focus_on(&graph->gc_focus, f);
        free_node(graph, g);
        set_phase(&h.ports[0], PHASE_GC_AUX);
    } else if (is_atomic_symbol(shared.ports[-1])) {
        connect_ports(&shared.ports[0], shares_with);
        free_node(graph, f);
        free_node(graph, g);
    } else if (SYMBOL_DUPLICATOR(UINT64_C(0)) == g.ports[-1]) {
        connect_ports(points_to, shares_with);
        free_node(graph, g);
    } else {
        // clang-format off
        struct node replacement = alloc_node(graph,
            1 == i ?
            SYMBOL_GC_DUPLICATOR_LEFT :
            SYMBOL_GC_DUPLICATOR_RIGHT);
        // clang-format on

        replacement.ports[2] = SYMBOL_INDEX(g.ports[-1]);
        connect_ports(&replacement.ports[0], points_to);
        connect_ports(&replacement.ports[1], shares_with);

        free_node(graph, f);
        free_node(graph, g);
    }
}

COMPILER_NONNULL(1) COMPILER_HOT //
static void
gc_step(
    struct context *const restrict graph,
    const struct node f,
    const struct node g,
    const ptrdiff_t i) {
    assert(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    XASSERT(i >= 0 && (uint64_t)i < MAX_PORTS);
    XASSERT(SYMBOL_ERASER == f.ports[-1]);

    switch (g.ports[-1]) {
    commute_1_2: {
        connect_ports(&f.ports[0], DECODE_ADDRESS(g.ports[0 == i ? 1 : 0]));

        focus_on(&graph->gc_focus, f);

        free_node(graph, g);

        break;
    }
    commute_1_3: {
        const uint8_t indices[] = {1, 2, 0};
        ptrdiff_t j = i;

        connect_ports(&f.ports[0], DECODE_ADDRESS(g.ports[indices[j++ % 3]]));

        const struct node fx =
            alloc_gc_node(graph, DECODE_ADDRESS(g.ports[indices[j % 3]]));

        focus_on(&graph->gc_focus, f);
        focus_on(&graph->gc_focus, fx);

        free_node(graph, g);

        break;
    }
    commute_1_4: {
        const uint8_t indices[] = {1, 2, 3, 0};
        ptrdiff_t j = i;

        connect_ports(&f.ports[0], DECODE_ADDRESS(g.ports[indices[j++ % 4]]));

        const struct node fx =
            alloc_gc_node(graph, DECODE_ADDRESS(g.ports[indices[j++ % 4]]));
        const struct node fxx =
            alloc_gc_node(graph, DECODE_ADDRESS(g.ports[indices[j % 4]]));

        focus_on(&graph->gc_focus, f);
        focus_on(&graph->gc_focus, fx);
        focus_on(&graph->gc_focus, fxx);

        free_node(graph, g);

        break;
    }
    annihilate: {
        free_node(graph, f);
        free_node(graph, g);
        break;
    }
    case SYMBOL_UNARY_CALL:
    case SYMBOL_BINARY_CALL_AUX:
    case SYMBOL_GC_LAMBDA:
    case SYMBOL_BARRIER:
    case SYMBOL_GC_DUPLICATOR_LEFT:
    case SYMBOL_GC_DUPLICATOR_RIGHT:
    delimiter:
        goto commute_1_2;
    case SYMBOL_APPLICATOR:
    case SYMBOL_BINARY_CALL:
    case SYMBOL_PERFORM: goto commute_1_3;
    case SYMBOL_IF_THEN_ELSE: goto commute_1_4;
    case SYMBOL_ERASER:
    case SYMBOL_CELL:
    case SYMBOL_IDENTITY_LAMBDA:
    case SYMBOL_REFERENCE: goto annihilate;
    case SYMBOL_LAMBDA:
    case SYMBOL_LAMBDA_C:
        if (1 == i) {
            gc_lambda_binder(graph, f, g, i);
            break;
        } else {
            goto commute_1_3;
        }
    duplicator:
        switch (i) {
        case 1:
        case 2: gc_duplicator_aux(graph, f, g, i); break;
        case 0: goto commute_1_3;
        default: COMPILER_UNREACHABLE();
        }
        break;
    default:
        if (IS_DUPLICATOR(g.ports[-1])) goto duplicator;
        else if (IS_DELIMITER(g.ports[-1])) goto delimiter;
        else COMPILER_UNREACHABLE();
    }

#ifdef OPTISCOPE_ENABLE_STATS
    graph->ngc++;
#endif
}

COMPILER_NONNULL(1, 2) COMPILER_HOT //
static void
gc(struct context *const restrict graph, uint64_t *const restrict port) {
    debug("%s(%p)", __func__, (void *)port);

    assert(graph);
    assert(port);

    const struct node launch = alloc_gc_node(graph, port);
    focus_on(&graph->gc_focus, launch);

    CONSUME_MULTIFOCUS (&graph->gc_focus, f) {
        XASSERT(f.ports);

        if (PHASE_GC_AUX == DECODE_PHASE_METADATA(f.ports[0])) {
            free_node(graph, f);
        } else {
            uint64_t *const points_to = DECODE_ADDRESS(f.ports[0]);

            const struct node g = node_of_port(points_to);
            XASSERT(g.ports);

            switch (DECODE_PHASE_METADATA(g.ports[0])) {
            case PHASE_GC:
                free_node(graph, f);
                set_phase(&g.ports[0], PHASE_GC_AUX);
                break;
            case PHASE_IN_STACK:
                graph->rescan = true;
                // fallthrough
            default: //
                gc_step(graph, f, g, points_to - g.ports);
            }
        }
    }
}

// Computational Interaction Rules
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

// clang-format off
#define COMPUTATION_RULE(name, graph, f, g) \
    COMPILER_NONNULL(1) COMPILER_HOT \
    static void \
    name(struct context *const restrict graph, const struct node f, const struct node g)
// clang-format on

COMPUTATION_RULE(beta, graph, f, g) {
    assert(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    assert(is_interaction(f, g));
    XASSERT(SYMBOL_APPLICATOR == f.ports[-1]);
    XASSERT(SYMBOL_LAMBDA == g.ports[-1]);

    inst_delimiter(
        graph,
        (struct delimiter){
            .idx = 0, DECODE_ADDRESS(f.ports[1]), DECODE_ADDRESS(g.ports[2])},
        1);
    inst_delimiter(
        graph,
        (struct delimiter){
            .idx = 0, DECODE_ADDRESS(f.ports[2]), DECODE_ADDRESS(g.ports[1])},
        1);

    free_node(graph, f);
    free_node(graph, g);
}

COMPUTATION_RULE(beta_c, graph, f, g) {
    assert(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    assert(is_interaction(f, g));
    XASSERT(SYMBOL_APPLICATOR == f.ports[-1]);
    XASSERT(SYMBOL_LAMBDA_C == g.ports[-1]);

    connect_ports(DECODE_ADDRESS(f.ports[1]), DECODE_ADDRESS(g.ports[2]));
    connect_ports(DECODE_ADDRESS(f.ports[2]), DECODE_ADDRESS(g.ports[1]));

    free_node(graph, f);
    free_node(graph, g);
}

COMPUTATION_RULE(identity_beta, graph, f, g) {
    assert(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    assert(is_interaction(f, g));
    XASSERT(SYMBOL_APPLICATOR == f.ports[-1]);
    XASSERT(SYMBOL_IDENTITY_LAMBDA == g.ports[-1]);

    connect_ports(DECODE_ADDRESS(f.ports[1]), DECODE_ADDRESS(f.ports[2]));

    free_node(graph, f);
    free_node(graph, g);
}

COMPUTATION_RULE(gc_beta, graph, f, g) {
    assert(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    assert(is_interaction(f, g));
    XASSERT(SYMBOL_APPLICATOR == f.ports[-1]);
    XASSERT(SYMBOL_GC_LAMBDA == g.ports[-1]);

    inst_delimiter(
        graph,
        (struct delimiter){
            .idx = 0, DECODE_ADDRESS(f.ports[1]), DECODE_ADDRESS(g.ports[1])},
        1);

    // There is a chance that the argument is fully disconnected from the root;
    // if so, we must garbage-collect it.
    gc(graph, DECODE_ADDRESS(f.ports[2]));

    free_node(graph, f);
    free_node(graph, g);
}

COMPUTATION_RULE(barrier, graph, f, g) {
    assert(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    assert(is_interaction(f, g));
    XASSERT(SYMBOL_BARRIER == f.ports[-1]);
    XASSERT(SYMBOL_DELIMITER(UINT64_C(0)) == g.ports[-1]);

    connect_ports(&f.ports[0], DECODE_ADDRESS(g.ports[1]));
    f.ports[2] = checked_add(f.ports[2], g.ports[2]);

    free_node(graph, g);
}

COMPUTATION_RULE(unbarrier, graph, f, g) {
    assert(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    assert(is_interaction(f, g));
    XASSERT(SYMBOL_BARRIER == f.ports[-1]);
    // `g` is unspecified.

    inst_delimiter(
        graph,
        (struct delimiter){.idx = 0, DECODE_ADDRESS(f.ports[1]), &g.ports[0]},
        f.ports[2]);

    free_node(graph, f);
}

COMPUTATION_RULE(do_expand, graph, f, g) {
    assert(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    assert(is_interaction(f, g));
    XASSERT(SYMBOL_REFERENCE == f.ports[-1]);
    // `g` is unspecified.

    const size_t index = f.ports[1];
    XASSERT(index < graph->book.count);

    struct expansion *const entry = &graph->book.entries[index];
    XASSERT(entry->function);

    free_node(graph, f);

    // Emit bytecode on first expansion.
    if (NULL == entry->expansion) {
        struct lambda_term *const term = entry->function();
        entry->expansion = term;
        emit_bytecode(graph, &entry->bc, term, 0);
    }

    plug_into(entry, &g.ports[0]);
    execute_bytecode(graph, entry->bc);
}

COMPUTATION_RULE(do_unary_call, graph, f, g) {
    assert(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    assert(is_interaction(f, g));
    XASSERT(SYMBOL_UNARY_CALL == f.ports[-1]);
    XASSERT(SYMBOL_CELL == g.ports[-1]);

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
    g.ports[1] = (UNARY_FUNCTION_OF_U64(f.ports[2]))(g.ports[1]);
#pragma GCC diagnostic pop
    connect_ports(&g.ports[0], DECODE_ADDRESS(f.ports[1]));

    free_node(graph, f);
}

COMPUTATION_RULE(do_binary_call, graph, f, g) {
    assert(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    assert(is_interaction(f, g));
    XASSERT(SYMBOL_BINARY_CALL == f.ports[-1]);
    XASSERT(SYMBOL_CELL == g.ports[-1]);

    const struct node aux = alloc_node(graph, SYMBOL_BINARY_CALL_AUX);
    connect_ports(&aux.ports[1], DECODE_ADDRESS(f.ports[1]));
    aux.ports[2] = f.ports[3];
    aux.ports[3] = g.ports[1];
    connect_ports(&aux.ports[0], DECODE_ADDRESS(f.ports[2]));

    free_node(graph, f);
    free_node(graph, g);
}

COMPUTATION_RULE(do_binary_call_aux, graph, f, g) {
    assert(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    assert(is_interaction(f, g));
    XASSERT(SYMBOL_BINARY_CALL_AUX == f.ports[-1]);
    XASSERT(SYMBOL_CELL == g.ports[-1]);

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
    g.ports[1] = (BINARY_FUNCTION_OF_U64(f.ports[2]))(f.ports[3], g.ports[1]);
#pragma GCC diagnostic pop
    connect_ports(&g.ports[0], DECODE_ADDRESS(f.ports[1]));

    free_node(graph, f);
}

COMPUTATION_RULE(do_if_then_else, graph, f, g) {
    assert(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    assert(is_interaction(f, g));
    XASSERT(SYMBOL_IF_THEN_ELSE == f.ports[-1]);
    XASSERT(SYMBOL_CELL == g.ports[-1]);

    uint64_t *const if_then = DECODE_ADDRESS(f.ports[3]), //
        *const if_else = DECODE_ADDRESS(f.ports[2]);

    uint64_t *choose, *discard;
    if (g.ports[1]) choose = if_then, discard = if_else;
    else choose = if_else, discard = if_then;

    connect_ports(DECODE_ADDRESS(f.ports[1]), choose);
    gc(graph, discard);

    free_node(graph, f);
    free_node(graph, g);
}

COMPUTATION_RULE(do_perform, graph, f, g) {
    assert(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    assert(is_interaction(f, g));
    XASSERT(SYMBOL_PERFORM == f.ports[-1]);
    XASSERT(SYMBOL_CELL == g.ports[-1]);

    connect_ports(DECODE_ADDRESS(f.ports[1]), DECODE_ADDRESS(f.ports[2]));

    free_node(graph, f);
    free_node(graph, g);
}

#undef COMPUTATION_RULE

// Generic Annihilation Helpers
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

// clang-format off
#define ANNIHILATION_HELPER(name, graph, f, g) \
    COMPILER_NONNULL(1) COMPILER_HOT \
    static void \
    name(struct context *const restrict graph, const struct node f, const struct node g)
// clang-format on

#define ANNIHILATION_PROLOGUE(graph, f, g)                                     \
    do {                                                                       \
        assert(graph);                                                         \
        XASSERT(f.ports);                                                      \
        XASSERT(g.ports);                                                      \
    } while (false)

ANNIHILATION_HELPER(annihilate_2_2_helper, graph, f, g) {
    ANNIHILATION_PROLOGUE(graph, f, g);

    connect_ports(DECODE_ADDRESS(f.ports[1]), DECODE_ADDRESS(g.ports[1]));

    free_node(graph, f);
    free_node(graph, g);
}

ANNIHILATION_HELPER(annihilate_3_3_helper, graph, f, g) {
    ANNIHILATION_PROLOGUE(graph, f, g);

    connect_ports(DECODE_ADDRESS(f.ports[1]), DECODE_ADDRESS(g.ports[1]));
    connect_ports(DECODE_ADDRESS(f.ports[2]), DECODE_ADDRESS(g.ports[2]));

    free_node(graph, f);
    free_node(graph, g);
}

#undef ANNIHILATION_PROLOGUE
#undef ANNIHILATION_HELPER

// Generic Commutation Helpers
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

// clang-format off
#define COMMUTATION_HELPER(name, graph, f, g) \
    COMPILER_NONNULL(1) COMPILER_HOT \
    static void \
    name(struct context *const restrict graph, const struct node f, const struct node g)
// clang-format on

#define COMMUTATION_PROLOGUE(graph, f, g)                                      \
    do {                                                                       \
        assert(graph);                                                         \
        XASSERT(f.ports);                                                      \
        XASSERT(g.ports);                                                      \
    } while (false)

COMMUTATION_HELPER(commute_1_2_helper, graph, f, g) {
    COMMUTATION_PROLOGUE(graph, f, g);

    connect_ports(&f.ports[0], DECODE_ADDRESS(g.ports[1]));

    free_node(graph, g);
}

#define commute_2_1_helper(graph, f, g) commute_1_2_helper((graph), (g), (f))

COMMUTATION_HELPER(commute_1_3_helper, graph, f, g) {
    COMMUTATION_PROLOGUE(graph, f, g);

    const struct node fx = alloc_node_from(graph, f.ports[-1], &f);

    connect_ports(&f.ports[0], DECODE_ADDRESS(g.ports[1]));
    connect_ports(&fx.ports[0], DECODE_ADDRESS(g.ports[2]));

    free_node(graph, g);
}

#define commute_3_1_helper(graph, f, g) commute_1_3_helper((graph), (g), (f))

COMMUTATION_HELPER(commute_2_2_helper, graph, f, g) {
    COMMUTATION_PROLOGUE(graph, f, g);

    connect_ports(&f.ports[0], DECODE_ADDRESS(g.ports[1]));
    connect_ports(&g.ports[0], DECODE_ADDRESS(f.ports[1]));

    connect_ports(&f.ports[1], &g.ports[1]);

    try_merge_if_delimiter(graph, f);
    try_merge_if_delimiter(graph, g);
}

COMMUTATION_HELPER(commute_2_3_helper, graph, f, g) {
    COMMUTATION_PROLOGUE(graph, f, g);

    const struct node fx = alloc_node_from(graph, f.ports[-1], &f);

    connect_ports(&g.ports[0], DECODE_ADDRESS(f.ports[1]));
    connect_ports(&f.ports[0], DECODE_ADDRESS(g.ports[1]));
    connect_ports(&fx.ports[0], DECODE_ADDRESS(g.ports[2]));

    connect_ports(&g.ports[1], &f.ports[1]);
    connect_ports(&g.ports[2], &fx.ports[1]);

    if (IS_DELIMITER(f.ports[-1])) {
        try_merge_delimiter(graph, f);
        try_merge_delimiter(graph, fx);
    }

    if (IS_DUPLICATOR(g.ports[-1])) { try_duplicate(graph, g); }
}

#define commute_3_2_helper(graph, f, g) commute_2_3_helper((graph), (g), (f))

COMMUTATION_HELPER(commute_2_4_helper, graph, f, g) {
    COMMUTATION_PROLOGUE(graph, f, g);

    const struct node fx = alloc_node_from(graph, f.ports[-1], &f);
    const struct node fxx = alloc_node_from(graph, f.ports[-1], &f);

    connect_ports(&g.ports[0], DECODE_ADDRESS(f.ports[1]));
    connect_ports(&f.ports[0], DECODE_ADDRESS(g.ports[1]));
    connect_ports(&fx.ports[0], DECODE_ADDRESS(g.ports[2]));
    connect_ports(&fxx.ports[0], DECODE_ADDRESS(g.ports[3]));

    connect_ports(&g.ports[1], &f.ports[1]);
    connect_ports(&g.ports[2], &fx.ports[1]);
    connect_ports(&g.ports[3], &fxx.ports[1]);

    if (IS_DELIMITER(f.ports[-1])) {
        try_merge_delimiter(graph, f);
        try_merge_delimiter(graph, fx);
        try_merge_delimiter(graph, fxx);
    }
}

#define commute_4_2_helper(graph, f, g) commute_2_4_helper((graph), (g), (f))

COMMUTATION_HELPER(commute_3_3_helper, graph, f, g) {
    COMMUTATION_PROLOGUE(graph, f, g);

    const struct node fx = alloc_node_from(graph, f.ports[-1], &f);
    const struct node gx = alloc_node_from(graph, g.ports[-1], &g);

    connect_ports(&f.ports[0], DECODE_ADDRESS(g.ports[1]));
    connect_ports(&fx.ports[0], DECODE_ADDRESS(g.ports[2]));
    connect_ports(&g.ports[0], DECODE_ADDRESS(f.ports[1]));
    connect_ports(&gx.ports[0], DECODE_ADDRESS(f.ports[2]));

    connect_ports(&g.ports[1], &f.ports[1]);
    connect_ports(&g.ports[2], &fx.ports[1]);
    connect_ports(&gx.ports[1], &f.ports[2]);
    connect_ports(&gx.ports[2], &fx.ports[2]);

    if (IS_DUPLICATOR(f.ports[-1])) {
        try_duplicate(graph, f);
        try_duplicate(graph, fx);
    }

    if (IS_DUPLICATOR(g.ports[-1])) {
        try_duplicate(graph, g);
        try_duplicate(graph, gx);
    }
}

COMMUTATION_HELPER(commute_3_4_helper, graph, f, g) {
    COMMUTATION_PROLOGUE(graph, f, g);

    const struct node gx = alloc_node_from(graph, g.ports[-1], &g);
    const struct node fx = alloc_node_from(graph, f.ports[-1], &f);
    const struct node fxx = alloc_node_from(graph, f.ports[-1], &f);

    connect_ports(&g.ports[0], DECODE_ADDRESS(f.ports[1]));
    connect_ports(&gx.ports[0], DECODE_ADDRESS(f.ports[2]));
    connect_ports(&f.ports[0], DECODE_ADDRESS(g.ports[1]));
    connect_ports(&fx.ports[0], DECODE_ADDRESS(g.ports[2]));
    connect_ports(&fxx.ports[0], DECODE_ADDRESS(g.ports[3]));

    connect_ports(&f.ports[1], &g.ports[1]);
    connect_ports(&f.ports[2], &gx.ports[1]);
    connect_ports(&fx.ports[1], &g.ports[2]);
    connect_ports(&fx.ports[2], &gx.ports[2]);
    connect_ports(&fxx.ports[1], &g.ports[3]);
    connect_ports(&fxx.ports[2], &gx.ports[3]);

    if (IS_DUPLICATOR(f.ports[-1])) {
        try_duplicate(graph, f);
        try_duplicate(graph, fx);
        try_duplicate(graph, fxx);
    }
}

#define commute_4_3_helper(graph, f, g) commute_3_4_helper((graph), (g), (f))

#undef COMMUTATION_PROLOGUE
#undef COMMUTATION_HELPER

// Generic Extrusion Rules
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

// TODO: maybe print delimiter extrusions if tracing is enabled?

// clang-format off
#define EXTRUSION_RULE(name, graph, f, g) \
    COMPILER_NONNULL(1) COMPILER_HOT \
    static void \
    name(struct context *const restrict graph, const struct node f, const struct node g)
// clang-format on

#define EXTRUSION_PROLOGUE(graph, f, g)                                        \
    do {                                                                       \
        assert(graph);                                                         \
        XASSERT(f.ports);                                                      \
        XASSERT(g.ports);                                                      \
        assert(IS_DELIMITER(f.ports[-1]));                                     \
        assert(DECODE_ADDRESS(f.ports[0]) == &g.ports[1]);                     \
    } while (false)

EXTRUSION_RULE(extrude_2_2, graph, f, g) {
    EXTRUSION_PROLOGUE(graph, f, g);

    connect_ports(&g.ports[1], DECODE_ADDRESS(f.ports[1]));
    connect_ports(&f.ports[0], DECODE_ADDRESS(g.ports[0]));

    connect_ports(&g.ports[0], &f.ports[1]);

    try_merge_delimiter(graph, f);

#ifdef OPTISCOPE_ENABLE_STATS
    graph->nextrusions++;
#endif
}

EXTRUSION_RULE(extrude_2_3, graph, f, g) {
    EXTRUSION_PROLOGUE(graph, f, g);

    const struct node fx = alloc_node_from(graph, f.ports[-1], &f);

    connect_ports(&g.ports[1], DECODE_ADDRESS(f.ports[1]));
    connect_ports(&f.ports[0], DECODE_ADDRESS(g.ports[0]));
    connect_ports(&fx.ports[0], DECODE_ADDRESS(g.ports[2]));

    connect_ports(&g.ports[0], &f.ports[1]);
    connect_ports(&g.ports[2], &fx.ports[1]);

    try_merge_delimiter(graph, f);
    try_merge_delimiter(graph, fx);

#ifdef OPTISCOPE_ENABLE_STATS
    graph->nextrusions++;
#endif
}

EXTRUSION_RULE(extrude_2_4, graph, f, g) {
    EXTRUSION_PROLOGUE(graph, f, g);

    const struct node fx = alloc_node_from(graph, f.ports[-1], &f);
    const struct node fxx = alloc_node_from(graph, f.ports[-1], &f);

    connect_ports(&g.ports[1], DECODE_ADDRESS(f.ports[1]));
    connect_ports(&f.ports[0], DECODE_ADDRESS(g.ports[0]));
    connect_ports(&fx.ports[0], DECODE_ADDRESS(g.ports[2]));
    connect_ports(&fxx.ports[0], DECODE_ADDRESS(g.ports[3]));

    connect_ports(&g.ports[0], &f.ports[1]);
    connect_ports(&g.ports[2], &fx.ports[1]);
    connect_ports(&g.ports[3], &fxx.ports[1]);

    try_merge_delimiter(graph, f);
    try_merge_delimiter(graph, fx);
    try_merge_delimiter(graph, fxx);

#ifdef OPTISCOPE_ENABLE_STATS
    graph->nextrusions++;
#endif
}

#undef EXTRUSION_PROLOGUE
#undef EXTRUSION_RULE

// Specialized Annihilation Helpers
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#define annihilate_dup_gc_dup(graph, f, g, keep, discard)                      \
    do {                                                                       \
        connect_ports(                                                         \
            DECODE_ADDRESS(f.ports[(keep)]), DECODE_ADDRESS(g.ports[1]));      \
        gc(graph, DECODE_ADDRESS(f.ports[(discard)]));                         \
        free_node(graph, f);                                                   \
        free_node(graph, g);                                                   \
    } while (0)

#define annihilate_gc_dup_dup(graph, f, g, keep, discard)                      \
    do {                                                                       \
        connect_ports(                                                         \
            DECODE_ADDRESS(f.ports[1]), DECODE_ADDRESS(g.ports[(keep)]));      \
        gc(graph, DECODE_ADDRESS(g.ports[(discard)]));                         \
        free_node(graph, f);                                                   \
        free_node(graph, g);                                                   \
    } while (0)

#define gc_both_directions(graph, f, g)                                        \
    do {                                                                       \
        gc(graph, DECODE_ADDRESS(f.ports[1]));                                 \
        gc(graph, DECODE_ADDRESS(g.ports[1]));                                 \
        free_node(graph, f);                                                   \
        free_node(graph, g);                                                   \
    } while (0)

#define annihilate_delimiter(graph, survivor, deceased)                        \
    do {                                                                       \
        survivor.ports[2] -= deceased.ports[2];                                \
        connect_ports(&survivor.ports[0], DECODE_ADDRESS(deceased.ports[1]));  \
        try_merge_delimiter(graph, survivor);                                  \
        free_node(graph, deceased);                                            \
    } while (0)

#define absorb_delimiter(graph, f, g)                                          \
    do {                                                                       \
        connect_ports(&g.ports[0], DECODE_ADDRESS(f.ports[1]));                \
        free_node(graph, f);                                                   \
    } while (0)

// Specialized Commutation Helpers
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#define commute_gc_dup_lam(graph, f, g)                                        \
    do {                                                                       \
        f.ports[2] = bump_raw_index(f.ports[2], 1);                            \
        commute_2_3_helper(graph, f, g);                                       \
    } while (0)

#define commute_gc_dup_gc_lam(graph, f, g)                                     \
    do {                                                                       \
        f.ports[2] = bump_raw_index(f.ports[2], 1);                            \
        commute_2_2_helper(graph, f, g);                                       \
    } while (0)

#define commute_gc_dup_del(graph, f, g)                                        \
    do {                                                                       \
        if (f.ports[2] >= SYMBOL_INDEX(g.ports[-1])) {                         \
            f.ports[2] = bump_raw_index(f.ports[2], g.ports[2]);               \
        }                                                                      \
        commute_2_2_helper(graph, f, g);                                       \
    } while (0)

#define commute_dup_lam(graph, f, g)                                           \
    do {                                                                       \
        f.ports[-1] = bump_index(f.ports[-1], 1);                              \
        commute_3_3_helper(graph, f, g);                                       \
    } while (0)

#define commute_dup_gc_lam(graph, f, g)                                        \
    do {                                                                       \
        f.ports[-1] = bump_index(f.ports[-1], 1);                              \
        commute_3_2_helper(graph, f, g);                                       \
    } while (0)

#define commute_dup_del(graph, f, g)                                           \
    do {                                                                       \
        if (SYMBOL_INDEX(f.ports[-1]) >= SYMBOL_INDEX(g.ports[-1])) {          \
            f.ports[-1] = bump_index(f.ports[-1], g.ports[2]);                 \
        }                                                                      \
        commute_3_2_helper(graph, f, g);                                       \
    } while (0)

#define commute_del_lam(graph, f, g)                                           \
    do {                                                                       \
        f.ports[-1] = bump_index(f.ports[-1], 1);                              \
        commute_2_3_helper(graph, f, g);                                       \
    } while (0)

#define commute_del_gc_lam(graph, f, g)                                        \
    do {                                                                       \
        f.ports[-1] = bump_index(f.ports[-1], 1);                              \
        commute_2_2_helper(graph, f, g);                                       \
    } while (0)

#define commute_del_del(graph, f, g)                                           \
    do {                                                                       \
        if (f.ports[-1] > g.ports[-1]) {                                       \
            f.ports[-1] = bump_index(f.ports[-1], g.ports[2]);                 \
        } else {                                                               \
            g.ports[-1] = bump_index(g.ports[-1], f.ports[2]);                 \
        }                                                                      \
        commute_2_2_helper(graph, f, g);                                       \
    } while (0)

// Interaction Rules per Operator Type
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#ifdef OPTISCOPE_ENABLE_TRACING

COMPILER_NONNULL(1, 2) //
static void
debug_interaction(
    const char *const restrict caller,
    struct context *const restrict graph,
    const struct node f,
    const struct node g) {
    assert(caller);
    assert(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    assert(is_interaction(f, g));

    char *const f_ssymbol = print_symbol(f.ports[-1]), //
        *const g_ssymbol = print_symbol(g.ports[-1]);

    // clang-format off
    debug(
        "%s(%p %s, %p %s)", caller, (void *)f.ports, f_ssymbol, (void *)g.ports, g_ssymbol);
    // clang-format on

    free(f_ssymbol);
    free(g_ssymbol);

    wait_for_user(graph, f, g);
}

#else

#define debug_interaction(caller, graph, f, g) ((void)0)

#endif // OPTISCOPE_ENABLE_TRACING

enum reduce_action {
    // Pop a node from the reduction stack, continue with the popped node.
    REDUCE_POP,
    // Pop, but check if rescan is needed first.
    REDUCE_POP_WITH_CHECK,
    // Push the current node, continue with the node it points to.
    REDUCE_PUSH,
    // Continue from the current node.
    REDUCE_LOOP,
    // Stop the reduction loop.
    REDUCE_STOP,
};

// clang-format off
#define CONTROL_FUNCTION(name, graph, f, g) \
    COMPILER_WARN_UNUSED_RESULT COMPILER_NONNULL(1) COMPILER_HOT \
    static enum reduce_action \
    name(struct context *const restrict graph, const struct node f, const struct node g)
// clang-format on

#ifdef OPTISCOPE_ENABLE_STATS
#define NINTERACTIONS_PLUS_PLUS(graph) ((graph)->ninteractions++)
#else
#define NINTERACTIONS_PLUS_PLUS(graph) ((void)0)
#endif

#define INTERACTION(graph, f, g, action, ...)                                  \
    do {                                                                       \
        debug_interaction(__func__, (graph), (f), (g));                        \
        do __VA_ARGS__ while (false);                                          \
        NINTERACTIONS_PLUS_PLUS(graph);                                        \
        return (action);                                                       \
    } while (false)

COMPILER_WARN_UNUSED_RESULT COMPILER_NONNULL(1) COMPILER_HOT //
static bool
try_inst_barrier(
    struct context *const restrict graph,
    const struct node f,
    const struct node g) {
    assert(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);

    if (SYMBOL_DELIMITER(UINT64_C(0)) != g.ports[-1]) { return false; }

    uint64_t *const h_port = DECODE_ADDRESS(g.ports[1]);
    const struct node h = node_of_port(h_port);

    if (DECODE_ADDRESS(h.ports[0]) != &g.ports[1]) {
        const struct node barr = alloc_node(graph, SYMBOL_BARRIER);
        barr.ports[2] = g.ports[2];
        connect_ports(&barr.ports[0], h_port);
        connect_ports(&barr.ports[1], &f.ports[0]);
        free_node(graph, g);
        return true;
    }

    return false;
}

COMPILER_WARN_UNUSED_RESULT COMPILER_NONNULL(1) COMPILER_HOT //
static bool
try_extrude(
    struct context *const restrict graph,
    const struct node f,
    const struct node g) {
    assert(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    assert(IS_DELIMITER(f.ports[-1]));

    uint64_t *const points_to = DECODE_ADDRESS(f.ports[0]);

    if (1 != DECODE_OFFSET_METADATA(*points_to)) { return false; }

    switch (g.ports[-1]) {
    case SYMBOL_UNARY_CALL:
    case SYMBOL_BINARY_CALL_AUX: extrude_2_2(graph, f, g); return true;
    case SYMBOL_APPLICATOR:
    case SYMBOL_BINARY_CALL:
    case SYMBOL_PERFORM: extrude_2_3(graph, f, g); return true;
    case SYMBOL_IF_THEN_ELSE: extrude_2_4(graph, f, g); return true;
    default: return false;
    }
}

CONTROL_FUNCTION(interact_with_gc_dup, graph, f, g) {
    assert(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    XASSERT(IS_GC_DUPLICATOR(f.ports[-1]));

    const uint64_t fsym = f.ports[-1], gsym = g.ports[-1];

    if (!points_to(g, f)) {
        return REDUCE_PUSH;
    } else if (IS_RELEVANT_LAMBDA(gsym)) {
        INTERACTION(
            graph, f, g, REDUCE_POP, { commute_gc_dup_lam(graph, f, g); });
    } else if (SYMBOL_GC_LAMBDA == gsym) {
        INTERACTION(
            graph, f, g, REDUCE_POP, { commute_gc_dup_gc_lam(graph, f, g); });
    } else if (SYMBOL_CELL == gsym) {
        INTERACTION(
            graph, f, g, REDUCE_POP, { commute_2_1_helper(graph, f, g); });
    } else if (SYMBOL_IDENTITY_LAMBDA == gsym) {
        INTERACTION(
            graph, f, g, REDUCE_POP, { commute_2_1_helper(graph, f, g); });
    } else if (SYMBOL_REFERENCE == gsym) {
        INTERACTION(graph, f, g, REDUCE_LOOP, { do_expand(graph, g, f); });
    } else if (fsym == gsym && f.ports[2] == g.ports[2]) {
        INTERACTION(
            graph, f, g, REDUCE_POP, { annihilate_2_2_helper(graph, f, g); });
    } else if (
        IS_GC_DUPLICATOR(gsym) && fsym != gsym && f.ports[2] == g.ports[2]) {
        INTERACTION(graph, f, g, REDUCE_POP_WITH_CHECK, {
            gc_both_directions(graph, f, g);
        });
    } else if (IS_GC_DUPLICATOR(gsym)) {
        INTERACTION(
            graph, f, g, REDUCE_POP, { commute_2_2_helper(graph, f, g); });
    } else if (
        fsym == SYMBOL_GC_DUPLICATOR_LEFT && //
        IS_DUPLICATOR(gsym) &&               //
        f.ports[2] == SYMBOL_INDEX(gsym)) {
        INTERACTION(graph, f, g, REDUCE_POP_WITH_CHECK, {
            annihilate_gc_dup_dup(graph, f, g, 2, 1);
        });
    } else if (
        fsym == SYMBOL_GC_DUPLICATOR_RIGHT && //
        IS_DUPLICATOR(gsym) &&                //
        f.ports[2] == SYMBOL_INDEX(gsym)) {
        INTERACTION(graph, f, g, REDUCE_POP_WITH_CHECK, {
            annihilate_gc_dup_dup(graph, f, g, 1, 2);
        });
    } else if (IS_DUPLICATOR(gsym)) {
        INTERACTION(
            graph, f, g, REDUCE_POP, { commute_2_3_helper(graph, f, g); });
    } else if (IS_DELIMITER(gsym)) {
        if (!try_inst_barrier(graph, f, g)) {
            INTERACTION(
                graph, f, g, REDUCE_POP, { commute_gc_dup_del(graph, f, g); });
        }
    } else {
        COMPILER_UNREACHABLE();
    }

    return REDUCE_POP;
}

CONTROL_FUNCTION(interact_with_dup, graph, f, g) {
    assert(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    XASSERT(IS_DUPLICATOR(f.ports[-1]));

    const uint64_t fsym = f.ports[-1], gsym = g.ports[-1];

    if (!points_to(g, f)) {
        return REDUCE_PUSH;
    } else if (IS_RELEVANT_LAMBDA(gsym)) {
        INTERACTION(graph, f, g, REDUCE_POP, { commute_dup_lam(graph, f, g); });
    } else if (SYMBOL_GC_LAMBDA == gsym) {
        INTERACTION(
            graph, f, g, REDUCE_POP, { commute_dup_gc_lam(graph, f, g); });
    } else if (SYMBOL_CELL == gsym) {
        INTERACTION(
            graph, f, g, REDUCE_POP, { commute_3_1_helper(graph, f, g); });
    } else if (SYMBOL_IDENTITY_LAMBDA == gsym) {
        INTERACTION(
            graph, f, g, REDUCE_POP, { commute_3_1_helper(graph, f, g); });
    } else if (SYMBOL_REFERENCE == gsym) {
        INTERACTION(graph, f, g, REDUCE_LOOP, { do_expand(graph, g, f); });
    } else if (
        SYMBOL_GC_DUPLICATOR_LEFT == gsym && //
        SYMBOL_INDEX(fsym) == g.ports[2]) {
        INTERACTION(graph, f, g, REDUCE_POP_WITH_CHECK, {
            annihilate_dup_gc_dup(graph, f, g, 2, 1);
        });
    } else if (
        SYMBOL_GC_DUPLICATOR_RIGHT == gsym && //
        SYMBOL_INDEX(fsym) == g.ports[2]) {
        INTERACTION(graph, f, g, REDUCE_POP_WITH_CHECK, {
            annihilate_dup_gc_dup(graph, f, g, 1, 2);
        });
    } else if (IS_GC_DUPLICATOR(gsym)) {
        INTERACTION(
            graph, f, g, REDUCE_POP, { commute_3_2_helper(graph, f, g); });
    } else if (fsym == gsym) {
        INTERACTION(
            graph, f, g, REDUCE_POP, { annihilate_3_3_helper(graph, f, g); });
    } else if (IS_DUPLICATOR(gsym)) {
        INTERACTION(
            graph, f, g, REDUCE_POP, { commute_3_3_helper(graph, f, g); });
    } else if (IS_DELIMITER(gsym)) {
        if (!try_inst_barrier(graph, f, g)) {
            INTERACTION(
                graph, f, g, REDUCE_POP, { commute_dup_del(graph, f, g); });
        }
    } else {
        COMPILER_UNREACHABLE();
    }

    return REDUCE_POP;
}

CONTROL_FUNCTION(interact_with_del, graph, f, g) {
    assert(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    XASSERT(IS_DELIMITER(f.ports[-1]));

    const uint64_t fsym = f.ports[-1], gsym = g.ports[-1];

    if (SYMBOL_CELL == gsym) {
        INTERACTION(
            graph, f, g, REDUCE_POP, { commute_2_1_helper(graph, f, g); });
    } else if (SYMBOL_IDENTITY_LAMBDA == gsym) {
        INTERACTION(
            graph, f, g, REDUCE_POP, { commute_2_1_helper(graph, f, g); });
    } else if (SYMBOL_REFERENCE == gsym) {
        INTERACTION(
            graph, f, g, REDUCE_POP, { commute_2_1_helper(graph, f, g); });
    } else if (SYMBOL_LAMBDA == gsym) {
        INTERACTION(graph, f, g, REDUCE_POP, { commute_del_lam(graph, f, g); });
    } else if (SYMBOL_GC_LAMBDA == gsym) {
        INTERACTION(
            graph, f, g, REDUCE_POP, { commute_del_gc_lam(graph, f, g); });
    } else if (SYMBOL_LAMBDA_C == gsym) {
        INTERACTION(
            graph, f, g, REDUCE_POP, { absorb_delimiter(graph, f, g); });
    } else if (try_extrude(graph, f, g)) {
        return REDUCE_POP;
    } else if (!points_to(g, f)) {
        return REDUCE_PUSH;
    } else if (IS_GC_DUPLICATOR(gsym)) {
        INTERACTION(
            graph, f, g, REDUCE_POP, { commute_gc_dup_del(graph, g, f); });
    } else if (IS_DUPLICATOR(gsym)) {
        INTERACTION(graph, f, g, REDUCE_POP, { commute_dup_del(graph, g, f); });
    } else if (fsym == gsym && f.ports[2] == g.ports[2]) {
        INTERACTION(
            graph, f, g, REDUCE_POP, { annihilate_2_2_helper(graph, f, g); });
    } else if (fsym == gsym && f.ports[2] > g.ports[2]) {
        INTERACTION(
            graph, f, g, REDUCE_POP, { annihilate_delimiter(graph, f, g); });
    } else if (fsym == gsym) {
        INTERACTION(
            graph, f, g, REDUCE_POP, { annihilate_delimiter(graph, g, f); });
    } else if (IS_DELIMITER(gsym)) {
        INTERACTION(graph, f, g, REDUCE_POP, { commute_del_del(graph, f, g); });
    } else {
        COMPILER_UNREACHABLE();
    }

    return REDUCE_POP;
}

CONTROL_FUNCTION(interact_with_app, graph, f, g) {
    assert(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    XASSERT(SYMBOL_APPLICATOR == f.ports[-1]);

    const uint64_t gsym = g.ports[-1];

    if (!points_to(g, f)) {
        return REDUCE_PUSH;
    } else if (SYMBOL_LAMBDA == gsym) {
        INTERACTION(graph, f, g, REDUCE_POP, { beta(graph, f, g); });
    } else if (SYMBOL_LAMBDA_C == gsym) {
        INTERACTION(graph, f, g, REDUCE_POP, { beta_c(graph, f, g); });
    } else if (SYMBOL_IDENTITY_LAMBDA == gsym) {
        INTERACTION(graph, f, g, REDUCE_POP, { identity_beta(graph, f, g); });
    } else if (SYMBOL_GC_LAMBDA == gsym) {
        INTERACTION(
            graph, f, g, REDUCE_POP_WITH_CHECK, { gc_beta(graph, f, g); });
    } else if (SYMBOL_REFERENCE == gsym) {
        INTERACTION(graph, f, g, REDUCE_LOOP, { do_expand(graph, g, f); });
    } else if (IS_GC_DUPLICATOR(gsym)) {
        INTERACTION(
            graph, f, g, REDUCE_POP, { commute_3_2_helper(graph, f, g); });
    } else if (IS_DUPLICATOR(gsym)) {
        INTERACTION(
            graph, f, g, REDUCE_POP, { commute_3_3_helper(graph, f, g); });
    } else if (IS_DELIMITER(gsym)) {
        if (!try_inst_barrier(graph, f, g)) {
            INTERACTION(
                graph, f, g, REDUCE_POP, { commute_3_2_helper(graph, f, g); });
        }
    } else {
        COMPILER_UNREACHABLE();
    }

    return REDUCE_POP;
}

CONTROL_FUNCTION(interact_with_ucall, graph, f, g) {
    assert(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    XASSERT(SYMBOL_UNARY_CALL == f.ports[-1]);

    const uint64_t gsym = g.ports[-1];

    if (!points_to(g, f)) {
        return REDUCE_PUSH;
    } else if (SYMBOL_CELL == gsym) {
        INTERACTION(graph, f, g, REDUCE_POP, { do_unary_call(graph, f, g); });
    } else if (SYMBOL_REFERENCE == gsym) {
        INTERACTION(graph, f, g, REDUCE_LOOP, { do_expand(graph, g, f); });
    } else if (IS_GC_DUPLICATOR(gsym)) {
        INTERACTION(
            graph, f, g, REDUCE_POP, { commute_2_2_helper(graph, f, g); });
    } else if (IS_DUPLICATOR(gsym)) {
        INTERACTION(
            graph, f, g, REDUCE_POP, { commute_2_3_helper(graph, f, g); });
    } else if (IS_DELIMITER(gsym)) {
        if (!try_inst_barrier(graph, f, g)) {
            INTERACTION(
                graph, f, g, REDUCE_POP, { commute_2_2_helper(graph, f, g); });
        }
    } else {
        COMPILER_UNREACHABLE();
    }

    return REDUCE_POP;
}

CONTROL_FUNCTION(interact_with_bcall, graph, f, g) {
    assert(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    XASSERT(SYMBOL_BINARY_CALL == f.ports[-1]);

    const uint64_t gsym = g.ports[-1];

    if (!points_to(g, f)) {
        return REDUCE_PUSH;
    } else if (SYMBOL_CELL == gsym) {
        INTERACTION(graph, f, g, REDUCE_POP, { do_binary_call(graph, f, g); });
    } else if (SYMBOL_REFERENCE == gsym) {
        INTERACTION(graph, f, g, REDUCE_LOOP, { do_expand(graph, g, f); });
    } else if (IS_GC_DUPLICATOR(gsym)) {
        INTERACTION(
            graph, f, g, REDUCE_POP, { commute_3_2_helper(graph, f, g); });
    } else if (IS_DUPLICATOR(gsym)) {
        INTERACTION(
            graph, f, g, REDUCE_POP, { commute_3_3_helper(graph, f, g); });
    } else if (IS_DELIMITER(gsym)) {
        if (!try_inst_barrier(graph, f, g)) {
            INTERACTION(
                graph, f, g, REDUCE_POP, { commute_3_2_helper(graph, f, g); });
        }
    } else {
        COMPILER_UNREACHABLE();
    }

    return REDUCE_POP;
}

CONTROL_FUNCTION(interact_with_bcall_aux, graph, f, g) {
    assert(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    XASSERT(SYMBOL_BINARY_CALL_AUX == f.ports[-1]);

    const uint64_t gsym = g.ports[-1];

    if (!points_to(g, f)) {
        return REDUCE_PUSH;
    } else if (SYMBOL_CELL == gsym) {
        INTERACTION(
            graph, f, g, REDUCE_POP, { do_binary_call_aux(graph, f, g); });
    } else if (SYMBOL_REFERENCE == gsym) {
        INTERACTION(graph, f, g, REDUCE_LOOP, { do_expand(graph, g, f); });
    } else if (IS_GC_DUPLICATOR(gsym)) {
        INTERACTION(
            graph, f, g, REDUCE_POP, { commute_2_2_helper(graph, f, g); });
    } else if (IS_DUPLICATOR(gsym)) {
        INTERACTION(
            graph, f, g, REDUCE_POP, { commute_2_3_helper(graph, f, g); });
    } else if (IS_DELIMITER(gsym)) {
        if (!try_inst_barrier(graph, f, g)) {
            INTERACTION(
                graph, f, g, REDUCE_POP, { commute_2_2_helper(graph, f, g); });
        }
    } else {
        COMPILER_UNREACHABLE();
    }

    return REDUCE_POP;
}

CONTROL_FUNCTION(interact_with_ite, graph, f, g) {
    assert(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    XASSERT(SYMBOL_IF_THEN_ELSE == f.ports[-1]);

    const uint64_t gsym = g.ports[-1];

    if (!points_to(g, f)) {
        return REDUCE_PUSH;
    } else if (SYMBOL_CELL == gsym) {
        INTERACTION(graph, f, g, REDUCE_POP_WITH_CHECK, {
            do_if_then_else(graph, f, g);
        });
    } else if (SYMBOL_REFERENCE == gsym) {
        INTERACTION(graph, f, g, REDUCE_LOOP, { do_expand(graph, g, f); });
    } else if (IS_GC_DUPLICATOR(gsym)) {
        INTERACTION(
            graph, f, g, REDUCE_POP, { commute_4_2_helper(graph, f, g); });
    } else if (IS_DUPLICATOR(gsym)) {
        INTERACTION(
            graph, f, g, REDUCE_POP, { commute_4_3_helper(graph, f, g); });
    } else if (IS_DELIMITER(gsym)) {
        if (!try_inst_barrier(graph, f, g)) {
            INTERACTION(
                graph, f, g, REDUCE_POP, { commute_4_2_helper(graph, f, g); });
        }
    } else {
        COMPILER_UNREACHABLE();
    }

    return REDUCE_POP;
}

CONTROL_FUNCTION(interact_with_perf, graph, f, g) {
    assert(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    XASSERT(SYMBOL_PERFORM == f.ports[-1]);

    const uint64_t gsym = g.ports[-1];

    if (!points_to(g, f)) {
        return REDUCE_PUSH;
    } else if (SYMBOL_CELL == gsym) {
        INTERACTION(graph, f, g, REDUCE_POP, { do_perform(graph, f, g); });
    } else if (SYMBOL_REFERENCE == gsym) {
        INTERACTION(graph, f, g, REDUCE_LOOP, { do_expand(graph, g, f); });
    } else if (IS_GC_DUPLICATOR(gsym)) {
        INTERACTION(
            graph, f, g, REDUCE_POP, { commute_3_2_helper(graph, f, g); });
    } else if (IS_DUPLICATOR(gsym)) {
        INTERACTION(
            graph, f, g, REDUCE_POP, { commute_3_3_helper(graph, f, g); });
    } else if (IS_DELIMITER(gsym)) {
        if (!try_inst_barrier(graph, f, g)) {
            INTERACTION(
                graph, f, g, REDUCE_POP, { commute_3_2_helper(graph, f, g); });
        }
    } else {
        COMPILER_UNREACHABLE();
    }

    return REDUCE_POP;
}

CONTROL_FUNCTION(interact_with_barr, graph, f, g) {
    assert(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    XASSERT(SYMBOL_BARRIER == f.ports[-1]);

    const uint64_t gsym = g.ports[-1];

    if (!points_to(g, f)) {
        return REDUCE_PUSH;
    } else if (SYMBOL_DELIMITER(UINT64_C(0)) == gsym) {
        INTERACTION(graph, f, g, REDUCE_LOOP, { barrier(graph, f, g); });
    } else {
        INTERACTION(graph, f, g, REDUCE_POP, { unbarrier(graph, f, g); });
    }
}

CONTROL_FUNCTION(interact_with_root, graph, f, g) {
    assert(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    XASSERT(SYMBOL_ROOT == f.ports[-1]);

    const uint64_t gsym = g.ports[-1];

    if (!points_to(g, f)) {
        return REDUCE_PUSH;
    } else if (IS_INTERFACE_SYMBOL(gsym)) {
        return REDUCE_STOP;
    } else if (IS_DELIMITER(gsym)) {
        INTERACTION(
            graph, f, g, REDUCE_LOOP, { commute_1_2_helper(graph, f, g); });
    } else {
        COMPILER_UNREACHABLE();
    }
}

#undef INTERACTION
#undef NINTERACTIONS_PLUS_PLUS
#undef CONTROL_FUNCTION

// Weak Reduction to Interface Normal Form (WRINF)
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

COMPILER_NONNULL(1) //
static void
reduce(struct context *const restrict graph) {
    debug("%s()", __func__);

    assert(graph);

    struct multifocus stack = alloc_focus(INITIAL_MULTIFOCUS_CAPACITY);

rescan: {
    struct node f = graph->root, g = {NULL};

loop: {
    g = follow_port(f, 0);
    XASSERT(g.ports);

    const uint64_t fsym = f.ports[-1], gsym = g.ports[-1];

    (void)gsym; // `gsym` is onely needed for `OPTISCOPE_ENABLE_STATS`

    // Ideally, these counters need to be incremented _after_ an interaction
    // takes place. Incrementing them beforehand is just following the path of
    // least resistance to keep the code readable.
#ifdef OPTISCOPE_ENABLE_STATS
    if (is_interaction(f, g)) {
        if (IS_ANY_DUPLICATOR(fsym) || IS_ANY_DUPLICATOR(gsym)) {
            graph->nduplicator_itrs++;
        }
        if (IS_DELIMITER(fsym) || IS_DELIMITER(gsym)) {
            graph->ndelimiter_itrs++;
        }
    }
#endif

    enum reduce_action action = REDUCE_POP;

    switch (fsym) {
    case SYMBOL_ROOT: action = interact_with_root(graph, f, g); break;
    case SYMBOL_APPLICATOR: action = interact_with_app(graph, f, g); break;
    case SYMBOL_UNARY_CALL: action = interact_with_ucall(graph, f, g); break;
    case SYMBOL_BINARY_CALL: action = interact_with_bcall(graph, f, g); break;
    case SYMBOL_BINARY_CALL_AUX:
        action = interact_with_bcall_aux(graph, f, g);
        break;
    case SYMBOL_IF_THEN_ELSE: action = interact_with_ite(graph, f, g); break;
    case SYMBOL_PERFORM: action = interact_with_perf(graph, f, g); break;
    case SYMBOL_BARRIER: action = interact_with_barr(graph, f, g); break;
    case SYMBOL_GC_DUPLICATOR_LEFT:
    case SYMBOL_GC_DUPLICATOR_RIGHT:
        action = interact_with_gc_dup(graph, f, g);
        break;
    default:
        if (IS_DUPLICATOR(fsym)) goto duplicator;
        else if (IS_DELIMITER(fsym)) goto delimiter;
        else COMPILER_UNREACHABLE();
    duplicator:
        action = interact_with_dup(graph, f, g);
        break;
    delimiter:
        action = interact_with_del(graph, f, g);
        break;
    }

    switch (action) {
    case REDUCE_LOOP: goto loop;
    case REDUCE_PUSH: goto push;
    case REDUCE_POP_WITH_CHECK: goto pop_with_check;
    case REDUCE_POP: goto pop;
    case REDUCE_STOP: goto stop;
    default: COMPILER_UNREACHABLE();
    }
}

push: {
    set_phase(&f.ports[0], PHASE_IN_STACK);
    focus_on(&stack, f);
    f = g;
    goto loop;
}

pop: {
    f = unfocus(&stack);
    f.ports[0] &= PHASE_MASK;
    goto loop;
}

pop_with_check: {
    if (graph->rescan) {
        // Proceed with resetting the phases of nodes from the stack.
        // clang-format off
        CONSUME_MULTIFOCUS (&stack, node) {
#ifdef COMPILER_ASAN_AVAILABLE
            if (!COMPILER_IS_POISONED_ADDRESS(node.ports)) {
#endif
            if (PHASE_IN_STACK == DECODE_PHASE_METADATA(node.ports[0])) {
                node.ports[0] &= PHASE_MASK;
            }
#ifdef COMPILER_ASAN_AVAILABLE
            }
#endif
        }
        // clang-format on
        graph->rescan = false;
        goto rescan;
    } else {
        goto pop;
    }
}
}

stop:
    free_focus(stack);
}

// Metacircular Interpretation
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

static struct lambda_term *
self_lambda(void) {
    struct lambda_term *body, *my_lambda, *my_apply, *my_var;

    // clang-format off
    return lambda(body,
        lambda(my_lambda, lambda(my_apply, lambda(my_var,
            apply(var(my_lambda), var(body))))));
    // clang-format on
}

static struct lambda_term *
self_apply(void) {
    struct lambda_term *rator, *rand, *my_lambda, *my_apply, *my_var;

    // clang-format off
    return lambda(rator, lambda(rand,
        lambda(my_lambda, lambda(my_apply, lambda(my_var,
            apply(apply(var(my_apply), var(rator)), var(rand)))))));
    // clang-format on
}

static struct lambda_term *
self_var(void) {
    struct lambda_term *lvl, *my_lambda, *my_apply, *my_var;

    // clang-format off
    return lambda(lvl,
        lambda(my_lambda, lambda(my_apply, lambda(my_var,
            apply(var(my_var), var(lvl))))));
    // clang-format on
}

static uint64_t
print_lambda(const uint64_t body) {
    const char *const result = format_string("(λ %s)", (const char *)body);
    free((void *)body);
    return (uint64_t)result;
}

static uint64_t
print_apply(const uint64_t m, const uint64_t n) {
    const char *const result =
        format_string("(%s %s)", (const char *)m, (const char *)n);
    free((void *)m);
    free((void *)n);
    return (uint64_t)result;
}

static uint64_t
print_var(const uint64_t lvl, const uint64_t var) {
    return (uint64_t)format_string("%" PRIu64, lvl - var - 1);
}

static uint64_t
plus_one(const uint64_t x) {
    return x + 1;
}

static struct lambda_term *
print_out(void) {
    struct lambda_term *input, *lvl, *f, *m, *n, *v;

    // clang-format off
    return lambda(input, lambda(lvl,
        apply(apply(apply(var(input),
            lambda(f, unary_call(print_lambda,
                apply(apply(expand(print_out),
                    apply(var(f), apply(self_var(), var(lvl)))),
                    unary_call(plus_one, var(lvl)))))),
            lambda(m, lambda(n, binary_call(print_apply,
                apply(apply(expand(print_out), var(m)), var(lvl)),
                apply(apply(expand(print_out), var(n)), var(lvl)))))),
            lambda(v,
                binary_call(print_var, var(lvl), var(v))))));
    // clang-format on
}

static struct lambda_term *
applying(void) {
    struct lambda_term *m, *n, *mx, *nx, *f, *v, *non_lambda_case;

    // clang-format off
    return lambda(m, lambda(n,
        apply(
            lambda(non_lambda_case,
                apply(apply(apply(var(m),
                    lambda(f, apply(var(f), var(n)))),
                    lambda(mx, lambda(nx, var(non_lambda_case)))),
                    lambda(v, var(non_lambda_case)))),
            apply(apply(self_apply(), var(m)), var(n)))));
    // clang-format on
}

COMPILER_RETURNS_NONNULL COMPILER_NONNULL(1) //
static struct lambda_term *
metacode(struct lambda_term *const restrict term) {
    assert(term);

    switch (term->ty) {
    case LAMBDA_TERM_APPLY: {
        struct apply_data *const data = &term->data.app;

        data->rator = metacode(data->rator);
        data->rand = metacode(data->rand);
        struct lambda_term *const result =
            apply(apply(expand(applying), data->rator), data->rand);
        free(term);

        return result;
    }
    case LAMBDA_TERM_LAMBDA: {
        struct lambda_data *const data = term->data.lam;

        data->body = metacode(data->body);

        return apply(expand(self_lambda), term);
    }
    case LAMBDA_TERM_VAR: return term;
    default: panic("Non-LC term in metacoding!");
    }
}

// Complete Algorithm
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

extern uint64_t
optiscope_algorithm(
    FILE *const restrict stream,      // full reduction if not `NULL`
    struct lambda_term *restrict term // must not be `NULL`
) {
    debug("%s()", __func__);

    assert(term);

    struct context *const graph = alloc_context();

    if (stream) {
        term = apply(apply(expand(print_out), metacode(term)), cell(0));
    }
    struct bytecode bc = alloc_bytecode(INITIAL_BYTECODE_CAPACITY);
    emit_bytecode(graph, &bc, term, 0);
    term->connect_to = &graph->root.ports[0];
    execute_bytecode(graph, bc);
    free_bytecode(bc);
    free_lambda_term(term);
    reduce(graph);
    const struct node result = follow_port(graph->root, 0);
    const uint64_t value =
        SYMBOL_CELL == result.ports[-1] ? result.ports[1] : 0;
    if (stream) {
        char *const s = (char *)value;
        IO_CALL(fputs, s, stream);
        free(s);
    }
    print_stats(graph);
    free_context(graph);
    return stream ? 0 : value;
}
