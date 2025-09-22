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

#define COMPILER_UNREACHABLE() MY_ASSERT(false)
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

// Assertions that are checked at program run-time.
#ifndef NDEBUG
#define MY_ASSERT assert
#else
#define MY_ASSERT(condition) ((void)(condition))
#endif

// Used for communicating logic invariants to the compiler.
#if defined(__GNUC__) && defined(NDEBUG)
#define XASSERT(condition) (!(condition) ? __builtin_unreachable() : (void)0)
#else
#define XASSERT MY_ASSERT
#endif

// Assertions that are checked at compile-time.
#if defined(__GNUC__) || STANDARD_C11_OR_HIGHER
#define STATIC_ASSERT _Static_assert
#else
// clang-format off
#define STATIC_ASSERT(constant_expression, string_literal) \
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

extern void
optiscope_redirect_stream(
    FILE *const restrict source, FILE *const restrict destination) {
    MY_ASSERT(source);
    MY_ASSERT(destination);

    int c;
    while (EOF != (c = fgetc(source))) {
        if (EOF == fputc(c, destination)) { perror("fputc"), abort(); }
    }

    if (ferror(source) != 0) { perror("fgetc"), abort(); }
}

// Logging & Panicking
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#define PRINTER(name, stream, culmination)                                     \
    COMPILER_NONNULL(1) COMPILER_FORMAT(printf, 1, 2) /* */                    \
    static void                                                                \
    name(const char *const restrict format, ...) {                             \
        MY_ASSERT(format);                                                     \
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

// Checked Memory Allocation
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

COMPILER_MALLOC(free, 1) COMPILER_RETURNS_NONNULL COMPILER_WARN_UNUSED_RESULT //
static void *
xmalloc(const size_t size) {
    XASSERT(size > 0);

    void *const object = malloc(size);
    if (NULL == object) { panic("Failed allocation!"); }

    return object;
}

COMPILER_MALLOC(free, 1) COMPILER_RETURNS_NONNULL COMPILER_WARN_UNUSED_RESULT //
static void *
xcalloc(const size_t n, const size_t size) {
    XASSERT(size > 0);

    void *const object = calloc(n, size);
    if (NULL == object) { panic("Failed allocation!"); }

    return object;
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
    LAMBDA_TERM_FIX,
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

struct fix_data {
    struct lambda_term *f;
};

struct perform_data {
    struct lambda_term *action, *k;
};

struct reference_data {
    struct lambda_term *(*function)(void);
};

union lambda_term_data {
    struct apply_data apply;
    struct lambda_data *lambda;
    struct lambda_data **var;
    uint64_t cell;
    struct unary_call_data u_call;
    struct binary_call_data b_call;
    struct if_then_else_data ite;
    struct fix_data fix;
    struct perform_data perform;
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
    MY_ASSERT(rator);
    MY_ASSERT(rand);

    struct lambda_term *const term = xmalloc(sizeof *term);
    term->ty = LAMBDA_TERM_APPLY;
    term->data.apply.rator = rator;
    term->data.apply.rand = rand;
    term->fv_count = rator->fv_count + rand->fv_count;

    return term;
}

extern LambdaTerm
prelambda(void) {
    struct lambda_term *const term = xmalloc(sizeof *term);
    term->ty = LAMBDA_TERM_LAMBDA;
    term->data.lambda = xcalloc(1, sizeof *term->data.lambda);
    // All the data fields are zeroed out.

    return term;
}

extern LambdaTerm
link_lambda_body(
    const restrict LambdaTerm binder, const restrict LambdaTerm body) {
    MY_ASSERT(binder);
    MY_ASSERT(body);
    MY_ASSERT(LAMBDA_TERM_LAMBDA == binder->ty);

    binder->data.lambda->body = body;
    binder->fv_count = body->fv_count - binder->data.lambda->nusages;

    return binder;
}

extern LambdaTerm
var(const restrict LambdaTerm binder) {
    MY_ASSERT(binder);
    MY_ASSERT(LAMBDA_TERM_LAMBDA == binder->ty);

    struct lambda_term *const term = xmalloc(sizeof *term);
    term->ty = LAMBDA_TERM_VAR;
    term->data.var = &binder->data.lambda;
    term->fv_count = 1;

    binder->data.lambda->nusages++;

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
    MY_ASSERT(function);
    MY_ASSERT(rand);

    struct lambda_term *const term = xmalloc(sizeof *term);
    term->ty = LAMBDA_TERM_UNARY_CALL;
    term->data.u_call.function = function;
    term->data.u_call.rand = rand;
    term->fv_count = rand->fv_count;

    return term;
}

extern LambdaTerm
binary_call(
    uint64_t (*const function)(uint64_t, uint64_t),
    const restrict LambdaTerm lhs,
    const restrict LambdaTerm rhs) {
    MY_ASSERT(function);
    MY_ASSERT(lhs);
    MY_ASSERT(rhs);

    struct lambda_term *const term = xmalloc(sizeof *term);
    term->ty = LAMBDA_TERM_BINARY_CALL;
    term->data.b_call.function = function;
    term->data.b_call.lhs = lhs;
    term->data.b_call.rhs = rhs;
    term->fv_count = lhs->fv_count + rhs->fv_count;

    return term;
}

extern LambdaTerm
if_then_else(
    const restrict LambdaTerm condition,
    const restrict LambdaTerm if_then,
    const restrict LambdaTerm if_else) {
    MY_ASSERT(condition);
    MY_ASSERT(if_then);
    MY_ASSERT(if_else);

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
fix(const restrict LambdaTerm f) {
    MY_ASSERT(f);

    struct lambda_term *const term = xmalloc(sizeof *term);
    term->ty = LAMBDA_TERM_FIX;
    term->data.fix.f = f;
    term->fv_count = f->fv_count;

    return term;
}

extern LambdaTerm
perform(const restrict LambdaTerm action, const restrict LambdaTerm k) {
    MY_ASSERT(action);
    MY_ASSERT(k);

    struct lambda_term *const term = xmalloc(sizeof *term);
    term->ty = LAMBDA_TERM_PERFORM;
    term->data.perform.action = action;
    term->data.perform.k = k;
    term->fv_count = action->fv_count + k->fv_count;

    return term;
}

extern LambdaTerm
expand(struct lambda_term *(*const function)(void)) {
    MY_ASSERT(function);

    struct lambda_term *const term = xmalloc(sizeof *term);
    term->ty = LAMBDA_TERM_REFERENCE;
    term->data.ref.function = function;
    term->fv_count = 0;

    return term;
}

COMPILER_NONNULL(1) COMPILER_COLD //
static void
free_lambda_term(struct lambda_term *const restrict term) {
    MY_ASSERT(term);

    switch (term->ty) {
    case LAMBDA_TERM_APPLY:
        free_lambda_term(term->data.apply.rator);
        free_lambda_term(term->data.apply.rand);
        break;
    case LAMBDA_TERM_LAMBDA:
        free_lambda_term(term->data.lambda->body);
        free(term->data.lambda->binder_ports);
        free(term->data.lambda);
        break;
    case LAMBDA_TERM_UNARY_CALL:
        free_lambda_term(term->data.u_call.rand);
        break;
    case LAMBDA_TERM_BINARY_CALL:
        free_lambda_term(term->data.b_call.lhs);
        free_lambda_term(term->data.b_call.rhs);
        break;
    case LAMBDA_TERM_IF_THEN_ELSE:
        free_lambda_term(term->data.ite.condition);
        free_lambda_term(term->data.ite.if_then);
        free_lambda_term(term->data.ite.if_else);
        break;
    case LAMBDA_TERM_FIX: free_lambda_term(term->data.fix.f); break;
    case LAMBDA_TERM_PERFORM:
        free_lambda_term(term->data.perform.action);
        free_lambda_term(term->data.perform.k);
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

#define ENCODE_METADATA(offset, phase)                                         \
    ((((offset) << PHASE_METADATA_BITS) | (phase)) << EFFECTIVE_ADDRESS_BITS)
#define DECODE_OFFSET_METADATA(address)                                        \
    ((address) >> (EFFECTIVE_ADDRESS_BITS + PHASE_METADATA_BITS))
#define DECODE_PHASE_METADATA(address)                                         \
    (((address) << OFFSET_METADATA_BITS) >>                                    \
     (EFFECTIVE_ADDRESS_BITS + OFFSET_METADATA_BITS))

#define ENCODE_ADDRESS(metadata, address)                                      \
    (((address) & ADDRESS_MASK) | (metadata))
#define SIGN_EXTEND(n)                                                         \
    ((uint64_t)((int64_t)((n) << UNUSED_ADDRESS_BITS) >> UNUSED_ADDRESS_BITS))
#define DECODE_ADDRESS(address)                                                \
    ((uint64_t *)(SIGN_EXTEND((address) & ADDRESS_MASK)))
#define DECODE_ADDRESS_METADATA(address) (((address) & ~ADDRESS_MASK))

#define PORT_VALUE(offset, phase, address)                                     \
    ENCODE_ADDRESS(ENCODE_METADATA((offset), (phase)), (address))

#define IS_PRINCIPAL_PORT(port) (0 == DECODE_OFFSET_METADATA((port)))

STATIC_ASSERT(CHAR_BIT == 8, "The byte width must be 8 bits!");
STATIC_ASSERT(sizeof(uint64_t *) == sizeof(uint64_t), "The machine word width must be 64 bits!");
STATIC_ASSERT(sizeof(uint64_t (*)(uint64_t value)) <= sizeof(uint64_t), "Function handles must fit in `uint64_t`!");

#define MAX_REGULAR_SYMBOL   UINT64_C(15)
#define INDEX_RANGE          UINT64_C(9223372036854775800)
#define MAX_DUPLICATOR_INDEX (MAX_REGULAR_SYMBOL + INDEX_RANGE)
#define MAX_DELIMITER_INDEX  (MAX_DUPLICATOR_INDEX + INDEX_RANGE)
#define MAX_PORTS            UINT64_C(4)
#define MAX_AUXILIARY_PORTS  (MAX_PORTS - 1)

STATIC_ASSERT(UINT64_MAX == UINT64_C(18446744073709551615), "`uint64_t` must have the expected range of [0; 2^64 - 1]!");
STATIC_ASSERT(UINT64_MAX == MAX_DELIMITER_INDEX, "Every bit of a symbol must be used!");

#define SYMBOL_ROOT            UINT64_C(0)
#define SYMBOL_APPLICATOR      UINT64_C(1)
#define SYMBOL_LAMBDA          UINT64_C(2)
#define SYMBOL_ERASER          UINT64_C(3)
#define SYMBOL_CELL            UINT64_C(4)
#define SYMBOL_UNARY_CALL      UINT64_C(5)
#define SYMBOL_BINARY_CALL     UINT64_C(6)
#define SYMBOL_BINARY_CALL_AUX UINT64_C(7)
#define SYMBOL_IF_THEN_ELSE    UINT64_C(8)
#define SYMBOL_PERFORM         UINT64_C(9)
#define SYMBOL_IDENTITY_LAMBDA UINT64_C(10) // the identity lambda
#define SYMBOL_GC_LAMBDA       UINT64_C(11) // a lambda discarding its parameter
#define SYMBOL_LAMBDA_C        UINT64_C(12) // a closed lambda
#define SYMBOL_REFERENCE       UINT64_C(13)
#define SYMBOL_BARRIER         UINT64_C(14)
#define SYMBOL_UNUSED          UINT64_C(15)
#define SYMBOL_DUPLICATOR(i)   (MAX_REGULAR_SYMBOL + 1 + (i))
#define SYMBOL_DELIMITER(i)    (MAX_DUPLICATOR_INDEX + 1 + (i))

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

COMPILER_CONST COMPILER_WARN_UNUSED_RESULT COMPILER_HOT //
inline static bool
is_operator_symbol(const uint64_t symbol) {
    switch (symbol) {
    case SYMBOL_APPLICATOR:
    case SYMBOL_UNARY_CALL:
    case SYMBOL_BINARY_CALL:
    case SYMBOL_BINARY_CALL_AUX:
    case SYMBOL_IF_THEN_ELSE:
    case SYMBOL_PERFORM: return true;
    default: return false;
    }
}

#define FOR_ALL_PORTS(node, i, seed)                                           \
    for (uint8_t i = seed; i < ports_count((node).ports[-1]); i++)

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

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT COMPILER_RETURNS_NONNULL
COMPILER_NONNULL(1) COMPILER_HOT COMPILER_ALWAYS_INLINE //
inline static uint64_t *
get_principal_port(uint64_t *const restrict port) {
    MY_ASSERT(port);

    return (port - DECODE_OFFSET_METADATA(port[0]));
}

COMPILER_NONNULL(1, 2) COMPILER_HOT COMPILER_ALWAYS_INLINE //
inline static void
connect_port_to(
    uint64_t *const restrict port, const uint64_t *const restrict another) {
    MY_ASSERT(port);
    MY_ASSERT(another);
    XASSERT(port != another);

    const uint64_t port_metadata = DECODE_ADDRESS_METADATA(*port);

    *port = ENCODE_ADDRESS(port_metadata, (uint64_t)another);

    MY_ASSERT(DECODE_ADDRESS(*port) == another);
    MY_ASSERT(DECODE_ADDRESS_METADATA(*port) == port_metadata);
}

COMPILER_NONNULL(1, 2) COMPILER_HOT COMPILER_ALWAYS_INLINE //
inline static void
connect_ports(uint64_t *const restrict lhs, uint64_t *const restrict rhs) {
    debug("%p 🔗 %p", (void *)lhs, (void *)rhs);

    // Delegate the assertions to `connect_port_to`.
    MY_ASSERT(true);

    connect_port_to(lhs, rhs), connect_port_to(rhs, lhs);
}

COMPILER_CONST COMPILER_WARN_UNUSED_RESULT COMPILER_HOT
COMPILER_ALWAYS_INLINE //
inline static int64_t
symbol_index(const uint64_t symbol) {
    STATIC_ASSERT(INDEX_RANGE <= (uint64_t)INT64_MAX, "Indices must fit in `int64_t`!");

    if (symbol <= MAX_REGULAR_SYMBOL) { return -1; }

    if (symbol <= MAX_DUPLICATOR_INDEX) {
        return (int64_t)(symbol - MAX_REGULAR_SYMBOL - 1);
    } else if (symbol <= MAX_DELIMITER_INDEX) {
        return (int64_t)(symbol - MAX_DUPLICATOR_INDEX - 1);
    } else {
        COMPILER_UNREACHABLE();
    }
}

#if defined(OPTISCOPE_ENABLE_TRACING) || defined(OPTISCOPE_ENABLE_GRAPHVIZ)

#define MAX_SSYMBOL_SIZE 64

COMPILER_CONST COMPILER_WARN_UNUSED_RESULT COMPILER_RETURNS_NONNULL //
static const char *
print_symbol(const uint64_t symbol) {
    static char buffer[MAX_SSYMBOL_SIZE] = {0};

    switch (symbol) {
    case SYMBOL_ROOT: sprintf(buffer, "root"); break;
    case SYMBOL_APPLICATOR: sprintf(buffer, "@"); break;
    case SYMBOL_LAMBDA: sprintf(buffer, "λ"); break;
    case SYMBOL_ERASER: sprintf(buffer, "◉"); break;
    case SYMBOL_CELL: sprintf(buffer, "cell"); break;
    case SYMBOL_UNARY_CALL: sprintf(buffer, "unary-call"); break;
    case SYMBOL_BINARY_CALL: sprintf(buffer, "binary-call"); break;
    case SYMBOL_BINARY_CALL_AUX: sprintf(buffer, "binary-call-aux"); break;
    case SYMBOL_IF_THEN_ELSE: sprintf(buffer, "if-then-else"); break;
    case SYMBOL_PERFORM: sprintf(buffer, "perform"); break;
    case SYMBOL_IDENTITY_LAMBDA: sprintf(buffer, "identity"); break;
    case SYMBOL_GC_LAMBDA: sprintf(buffer, "λ◉"); break;
    case SYMBOL_LAMBDA_C: sprintf(buffer, "λc"); break;
    case SYMBOL_REFERENCE: sprintf(buffer, "&"); break;
    case SYMBOL_BARRIER: sprintf(buffer, "🚧"); break;
    default:
        if (IS_DUPLICATOR(symbol)) goto duplicator;
        else if (IS_DELIMITER(symbol)) goto delimiter;
        else COMPILER_UNREACHABLE();
    duplicator:
        sprintf(buffer, "δ/%" PRIi64, symbol_index(symbol));
        break;
    delimiter:
        sprintf(buffer, "⊔/%" PRIi64, symbol_index(symbol));
        break;
    }

    return buffer;
}

#endif

COMPILER_WARN_UNUSED_RESULT COMPILER_HOT COMPILER_ALWAYS_INLINE //
inline static uint64_t
bump_index(const uint64_t symbol, const uint64_t offset) {
    XASSERT(symbol > MAX_REGULAR_SYMBOL);

    if ((IS_DUPLICATOR(symbol) && offset > MAX_DUPLICATOR_INDEX - symbol) ||
        (IS_DELIMITER(symbol) && offset > MAX_DELIMITER_INDEX - symbol)) {
        panic("Maximum index of %" PRIu64 " is reached!", INDEX_RANGE);
    }

    return symbol + offset;
}

#define PHASE_REDUCTION UINT64_C(0)
#define PHASE_GC        UINT64_C(1)
#define PHASE_GC_AUX    UINT64_C(2)
#define PHASE_STACK     UINT64_C(3)

#define PHASE_MASK                                                             \
    UINT64_C(0xC3FFFFFFFFFFFFFF) /* clear the phase bits (61-58) */

COMPILER_NONNULL(1) COMPILER_HOT COMPILER_ALWAYS_INLINE //
inline static void
set_phase(uint64_t *const restrict port, const uint64_t phase) {
    MY_ASSERT(port);
    MY_ASSERT(IS_PRINCIPAL_PORT(*port));

    *port = (*port & PHASE_MASK) | (phase << EFFECTIVE_ADDRESS_BITS);

    MY_ASSERT(DECODE_PHASE_METADATA(*port) == phase);
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

#define USER_FUNCTION_OF_U64(function)                                         \
    ((struct lambda_term * (*)(void))(void *)(function))

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
    MY_ASSERT(size <= HUGE_PAGE_SIZE_2MB);

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
    MY_ASSERT(memory);

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
        union prefix##_chunk *chunks =                                         \
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
        MY_ASSERT(self);                                                       \
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
        MY_ASSERT(self);                                                       \
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
        MY_ASSERT(self);                                                       \
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
        MY_ASSERT(self);                                                       \
        XASSERT(self->buckets);                                                \
        MY_ASSERT(object);                                                     \
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

STATIC_ASSERT(sizeof(struct node) == sizeof(uint64_t *), "`struct node` must be as tiny as a pointer!");

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT COMPILER_NONNULL(1) COMPILER_HOT
COMPILER_ALWAYS_INLINE //
inline static struct node
node_of_port(uint64_t *const restrict port) {
    MY_ASSERT(port);

    const struct node node = {get_principal_port(port)};
    XASSERT(node.ports);

    return node;
}

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT COMPILER_NONNULL(1) COMPILER_HOT
COMPILER_ALWAYS_INLINE //
inline static struct node
follow_port(uint64_t *const restrict port) {
    MY_ASSERT(port);

    return node_of_port(DECODE_ADDRESS(*port));
}

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT COMPILER_HOT COMPILER_ALWAYS_INLINE //
inline static bool
is_interacting_with(const struct node f, const struct node g) {
    XASSERT(f.ports);
    XASSERT(g.ports);

    // Supposing that `g` is derived from `f` by `follow_port(&f.ports[0])`.
    return DECODE_ADDRESS(g.ports[0]) == &f.ports[0];
}

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT //
inline static bool
is_interaction(const struct node f, const struct node g) {
    XASSERT(f.ports);
    XASSERT(g.ports);

    return is_interacting_with(f, g) && is_interacting_with(g, f);
}

#ifdef OPTISCOPE_ENABLE_TRACING

#define MAX_SNODE_SIZE 256

COMPILER_CONST COMPILER_WARN_UNUSED_RESULT COMPILER_RETURNS_NONNULL //
static const char *
print_node(const struct node node) {
    XASSERT(node.ports);

    static char buffer[MAX_SNODE_SIZE] = {0};

    const uint64_t *const p = node.ports;
    const char *const ssymbol = print_symbol(p[-1]);

    switch (ports_count(p[-1])) {
    case 1: sprintf(buffer, "%s [%p]", ssymbol, (void *)&p[0]); break;
    case 2:
        sprintf(buffer, "%s [%p, %p]", ssymbol, (void *)&p[0], (void *)&p[1]);
        break;
    case 3:
        // clang-format off
        sprintf(
            buffer, "%s [%p, %p, %p]", ssymbol,
            (void *)&p[0], (void *)&p[1], (void *)&p[2]
        );
        // clang-format on
        break;
    case 4:
        // clang-format off
        sprintf(
            buffer, "%s [%p, %p, %p, %p]", ssymbol,
            (void *)&p[0], (void *)&p[1], (void *)&p[2], (void *)&p[3]
        );
        // clang-format on
        break;
    default: COMPILER_UNREACHABLE();
    }

    return buffer;
}

#endif // OPTISCOPE_ENABLE_TRACING

#ifdef OPTISCOPE_ENABLE_GRAPHVIZ

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT //
inline static bool
is_active(const struct node node) {
    XASSERT(node.ports);

    return is_interacting_with(node, follow_port(&node.ports[0]));
}

#endif // OPTISCOPE_ENABLE_GRAPHVIZ

// Bytecode Definitions
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

enum bc_instruction_type {
    INSTRUCTION_ATTACH_NODE,
    INSTRUCTION_SAVE_PORT,
    INSTRUCTION_DELIMIT,
    INSTRUCTION_TREE,
    INSTRUCTION_FIX,
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

struct bc_delimit_data {
    uint64_t **points_to, **goes_from;
    uint64_t n;
};

struct bc_tree_data {
    uint64_t n;
    uint64_t **locations;
};

struct bc_fix_data {
    uint64_t **connect_to;
};

union bc_instruction_data {
    struct bc_attach_node_data attach_node;
    struct bc_save_port_data save_port;
    struct bc_delimit_data delimit;
    struct bc_tree_data tree;
    struct bc_fix_data fix;
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
    MY_ASSERT(bc);
    XASSERT(bc->count == bc->capacity);
    XASSERT(bc->instructions);

    bc->instructions = realloc(
        bc->instructions, sizeof bc->instructions[0] * (bc->capacity *= 2));
    if (NULL == bc->instructions) { //
        panic("Failed to reallocate the bytecode!");
    }
}

COMPILER_NONNULL(1) //
inline static void
emit_instruction(
    struct bytecode *const restrict bc,
    const struct bc_instruction instruction) {
    MY_ASSERT(bc);
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

#define BC_FIX(bc, ...)                                                        \
    emit_instruction(                                                          \
        (bc),                                                                  \
        (struct bc_instruction){.ty = INSTRUCTION_FIX,                         \
                                .data.fix = {__VA_ARGS__}})

// Multifocuses Functionality
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#define INITIAL_MULTIFOCUS_CAPACITY 4096

struct multifocus {
    size_t count, capacity;
    struct node *array;
};

#define alloc_focus(initial_capacity)                                          \
    ((struct multifocus){                                                      \
        .count = 0,                                                            \
        .capacity = (initial_capacity),                                        \
        .array = xmalloc(sizeof(struct node) * (initial_capacity)),            \
    })

#define free_focus(focus) free((focus).array)

COMPILER_NONNULL(1) COMPILER_COLD //
static void
expand_focus(struct multifocus *const restrict focus) {
    MY_ASSERT(focus);
    XASSERT(focus->count == focus->capacity);
    XASSERT(focus->array);

    focus->array =
        realloc(focus->array, sizeof focus->array[0] * (focus->capacity *= 2));
    if (NULL == focus->array) { //
        panic("Failed to reallocate the multifocus!");
    }
}

COMPILER_NONNULL(1) COMPILER_HOT COMPILER_ALWAYS_INLINE //
inline static void
focus_on(struct multifocus *const restrict focus, const struct node node) {
    MY_ASSERT(focus);
    XASSERT(node.ports);
    XASSERT(focus->count <= focus->capacity);
    XASSERT(focus->array);

    if (focus->count == focus->capacity) { expand_focus(focus); }

    focus->array[focus->count++] = node;
}

#ifdef OPTISCOPE_ENABLE_GRAPHVIZ

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT //
static bool
is_focused_on(const struct multifocus focus, const struct node node) {
    XASSERT(node.ports);
    XASSERT(focus.array);

    for (size_t i = 0; i < focus.count; i++) {
        if (focus.array[i].ports == node.ports) { return true; }
    }

    return false;
}

#endif // OPTISCOPE_ENABLE_GRAPHVIZ

COMPILER_NONNULL(1) COMPILER_HOT COMPILER_ALWAYS_INLINE //
inline static struct node
unfocus(struct multifocus *const restrict focus) {
    MY_ASSERT(focus);
    XASSERT(focus->count > 0);
    XASSERT(focus->count <= focus->capacity);
    XASSERT(focus->array);

    return focus->array[--focus->count];
}

COMPILER_NONNULL(1) COMPILER_HOT COMPILER_ALWAYS_INLINE //
inline static struct node
unfocus_or(
    struct multifocus *const restrict focus, //
    const struct node fallback) {
    MY_ASSERT(focus);
    XASSERT(focus->count <= focus->capacity);
    XASSERT(focus->array);

    return focus->count > 0 ? unfocus(focus) : fallback;
}

#define CONSUME_MULTIFOCUS(focus, f)                                           \
    for (struct node f = {NULL};                                               \
         (focus)->count > 0 ? (f = unfocus((focus)), true) : false;            \
         (void)0)

// Dynamic Book of Expansions
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#ifndef OPTISCOPE_BOOK_SIZE
#define OPTISCOPE_BOOK_SIZE 4096
#endif

struct expansion {
    // A pointer to a user-provided function for lookup purposes.
    struct lambda_term *(*function)(void);

    // It is crucial to store an expansion itselfe, as the bytecode will use
    // memory allocated for the expansion.
    struct lambda_term *expansion;

    // The bytecode that describes how to build a net.
    struct bytecode bc;
};

struct book {
    struct expansion *array;
};

#define alloc_book()                                                           \
    ((struct book){                                                            \
        .array = xcalloc(OPTISCOPE_BOOK_SIZE, sizeof(struct expansion)),       \
    })

COMPILER_COLD //
static void
free_book(const struct book book) {
    XASSERT(book.array);

    for (size_t i = 0; i < OPTISCOPE_BOOK_SIZE; i++) {
        const struct expansion entry = book.array[i];

        if (entry.expansion) {
            MY_ASSERT(entry.function);
            free_lambda_term(entry.expansion);
            free_bytecode(entry.bc);
        }
    }

    free(book.array);
}

// Main Context Functionality
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

struct context {
    // The root node of the graph (alwaies `SYMBOL_ROOT`).
    struct node root;

    // Indicates whether the interface normal form has been reached.
    bool time_to_stop;

    // The cache that stores already performed expansions.
    struct book book;

    // The multifocus for garbage collection purposes.
    struct multifocus gc_focus;

#ifdef OPTISCOPE_ENABLE_STATS
    // The numbers of proper interactions.
    uint64_t nbetas, ncommutations, nannihilations, nexpansions,
        ncell_operations, nbarrier_operations;
    // The numbers of duplication & delimiter interactions.
    uint64_t nduplication_itrs, ndelimiter_itrs;
    // The numbers of non-interaction graph rewrites.
    uint64_t nmergings, nextrusions, ngc;
    // The memory usage statistics.
    uint64_t ntotal, nmax_total;
#endif

#ifdef OPTISCOPE_ENABLE_GRAPHVIZ
    // The active pair to be reduced next (Graphviz onely).
    struct node current_pair[2];
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
    root.ports[0] = PORT_VALUE(UINT64_C(0), PHASE_REDUCTION, UINT64_C(0));

    struct context *const graph = xcalloc(1, sizeof *graph);
    graph->root = root;
    graph->time_to_stop = false;
    graph->book = alloc_book();
    graph->gc_focus = alloc_focus(INITIAL_MULTIFOCUS_CAPACITY);
    // The statistics counters are zeroed out by `xcalloc`.

#ifdef OPTISCOPE_ENABLE_GRAPHVIZ
    CLEAR_MEMORY(graph->current_pair);
#endif

    return graph;
}

COMPILER_NONNULL(1) COMPILER_COLD //
static void
free_context(struct context *const restrict graph) {
    debug("%s()", __func__);

    MY_ASSERT(graph);
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
    MY_ASSERT(graph);

    const uint64_t ntotal_interactions = //
        graph->nbetas + graph->ncommutations + graph->nannihilations +
        graph->nexpansions + graph->ncell_operations +
        graph->nbarrier_operations;

    const uint64_t ntotal_rewrites = //
        ntotal_interactions + graph->nmergings + graph->nextrusions +
        graph->ngc;

    const uint64_t nbookkeeping_rewrites = //
        graph->ndelimiter_itrs + graph->nmergings + graph->nextrusions;

    const double sharing_work = //
        ((double)graph->nduplication_itrs / (double)ntotal_rewrites) * 100.0;

    const double bookkeeping_work = //
        ((double)nbookkeeping_rewrites / (double)ntotal_rewrites) * 100.0;

    printf("   Family reductions: %" PRIu64 "\n", graph->nbetas);
    printf("        Commutations: %" PRIu64 "\n", graph->ncommutations);
    printf("       Annihilations: %" PRIu64 "\n", graph->nannihilations);
    printf("          Expansions: %" PRIu64 "\n", graph->nexpansions);
    printf("     Cell operations: %" PRIu64 "\n", graph->ncell_operations);
    printf("  Barrier operations: %" PRIu64 "\n", graph->nbarrier_operations);
    printf("  Total interactions: %" PRIu64 "\n", ntotal_interactions);
    printf(" Garbage collections: %" PRIu64 "\n", graph->ngc);
    printf("  Delimiter mergings: %" PRIu64 "\n", graph->nmergings);
    printf("Delimiter extrusions: %" PRIu64 "\n", graph->nextrusions);
    printf("      Total rewrites: %" PRIu64 "\n", ntotal_rewrites);
    printf("        Sharing work: %.2f%%\n", sharing_work);
    printf("    Bookkeeping work: %.2f%%\n", bookkeeping_work);
    printf("     Peak node count: %" PRIu64 "\n", graph->nmax_total);
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
    MY_ASSERT(graph);
    XASSERT(SYMBOL_ROOT != symbol);
    if (prototype) { XASSERT(prototype->ports); }

#ifndef NDEBUG
    if (prototype) {
        if (IS_DUPLICATOR(symbol)) {
            MY_ASSERT(IS_DUPLICATOR(prototype->ports[-1]));
        } else if (IS_DELIMITER(symbol)) {
            MY_ASSERT(IS_DELIMITER(prototype->ports[-1]));
        } else {
            MY_ASSERT(symbol == prototype->ports[-1]);
        }
    }
#endif

    uint64_t *ports = NULL;

#define SET_SYMBOL() (ports[-1] = symbol)
#define SET_PORTS_0()                                                          \
    (SET_SYMBOL(),                                                             \
     ports[0] = PORT_VALUE(UINT64_C(0), PHASE_REDUCTION, UINT64_C(0)))
#define SET_PORTS_1()                                                          \
    (SET_PORTS_0(),                                                            \
     ports[1] = PORT_VALUE(UINT64_C(1), UINT64_C(0), UINT64_C(0)))
#define SET_PORTS_2()                                                          \
    (SET_PORTS_1(),                                                            \
     ports[2] = PORT_VALUE(UINT64_C(2), UINT64_C(0), UINT64_C(0)))
#define SET_PORTS_3()                                                          \
    (SET_PORTS_2(),                                                            \
     ports[3] = PORT_VALUE(UINT64_C(3), UINT64_C(0), UINT64_C(0)))

    switch (symbol) {
    case SYMBOL_APPLICATOR:
    case SYMBOL_LAMBDA:
    case SYMBOL_LAMBDA_C:
    case SYMBOL_PERFORM:
    duplicator:
        ports = ALLOC_POOL_OBJECT(u64x4_pool);
        SET_PORTS_2();
        break;
    case SYMBOL_ERASER:
    case SYMBOL_IDENTITY_LAMBDA:
        ports = ALLOC_POOL_OBJECT(u64x2_pool);
        SET_PORTS_0();
        break;
    case SYMBOL_GC_LAMBDA:
        ports = ALLOC_POOL_OBJECT(u64x3_pool);
        SET_PORTS_1();
        break;
    case SYMBOL_IF_THEN_ELSE:
        ports = ALLOC_POOL_OBJECT(u64x5_pool);
        SET_PORTS_3();
        break;
    case SYMBOL_CELL:
    case SYMBOL_REFERENCE:
        ports = ALLOC_POOL_OBJECT(u64x3_pool);
        if (prototype) { ports[1] = prototype->ports[1]; }
        SET_PORTS_0();
        break;
    case SYMBOL_UNARY_CALL:
        ports = ALLOC_POOL_OBJECT(u64x4_pool);
        if (prototype) { ports[2] = prototype->ports[2]; }
        SET_PORTS_1();
        break;
    case SYMBOL_BINARY_CALL:
        ports = ALLOC_POOL_OBJECT(u64x5_pool);
        if (prototype) { ports[3] = prototype->ports[3]; }
        SET_PORTS_2();
        break;
    case SYMBOL_BINARY_CALL_AUX:
        ports = ALLOC_POOL_OBJECT(u64x5_pool);
        if (prototype) {
            ports[2] = prototype->ports[2];
            ports[3] = prototype->ports[3];
        }
        SET_PORTS_1();
        break;
    case SYMBOL_BARRIER:
    delimiter:
        ports = ALLOC_POOL_OBJECT(u64x4_pool);
        if (prototype) { ports[2] = prototype->ports[2]; }
        SET_PORTS_1();
        break;
    default:
        if (symbol <= MAX_DUPLICATOR_INDEX) goto duplicator;
        else if (symbol <= MAX_DELIMITER_INDEX) goto delimiter;
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

    debug("🔨 %s", print_node((struct node){ports}));

    return (struct node){ports};
}

#define alloc_node(graph, symbol) alloc_node_from((graph), (symbol), NULL)

COMPILER_WARN_UNUSED_RESULT COMPILER_NONNULL(1, 2) COMPILER_HOT //
static struct node
alloc_gc_node(
    struct context *const restrict graph, uint64_t *const restrict points_to) {
    MY_ASSERT(graph);
    MY_ASSERT(points_to);

    const struct node eraser = alloc_node(graph, SYMBOL_ERASER);
    // Mark this eraser as garbage-collecting, which is necessary for
    // successfull operation of the garbage collector.
    set_phase(&eraser.ports[0], PHASE_GC);
    connect_ports(&eraser.ports[0], points_to);

    return eraser;
}

COMPILER_NONNULL(1) COMPILER_HOT //
static void
free_node(struct context *const restrict graph, const struct node node) {
    debug("🧹 %p", (void *)node.ports);

    MY_ASSERT(graph);
    XASSERT(node.ports);

    const uint64_t symbol = node.ports[-1];
    XASSERT(SYMBOL_ROOT != symbol);

    uint64_t *const p = node.ports;

#ifdef COMPILER_ASAN_AVAILABLE
    {
        const size_t region_size = sizeof *p * (ports_count(symbol) + 1);

        if (COMPILER_IS_POISONED_MEMORY(p - 1, region_size)) {
            // Invoke AddressSanitizer's use-after-poison report.
            memset(p - 1, '\0', region_size);
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
    duplicator:
    delimiter:
        FREE_POOL_OBJECT(u64x4_pool, p);
        break;
    case SYMBOL_BINARY_CALL:
    case SYMBOL_BINARY_CALL_AUX:
    case SYMBOL_IF_THEN_ELSE: FREE_POOL_OBJECT(u64x5_pool, p); break;
    default:
        if (symbol <= MAX_DUPLICATOR_INDEX) goto duplicator;
        else if (symbol <= MAX_DELIMITER_INDEX) goto delimiter;
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
    MY_ASSERT(template.idx < INDEX_RANGE);
    MY_ASSERT(template.points_to);
    MY_ASSERT(template.goes_from);
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
    MY_ASSERT(graph);
    assert_delimiter_template(template);

    const struct node delim = alloc_node(graph, SYMBOL_DELIMITER(template.idx));
    delim.ports[2] = count;
    connect_ports(&delim.ports[0], template.points_to);
    connect_ports(&delim.ports[1], template.goes_from);
}

COMPILER_NONNULL(1) COMPILER_HOT //
static void
inst_delimiter(
    struct context *const restrict graph,
    const struct delimiter template,
    const uint64_t count) {
    MY_ASSERT(graph);
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
        g.ports[2] += count;
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
    MY_ASSERT(graph);
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
        g.ports[2] += f.ports[2];
        connect_ports(&g.ports[1], DECODE_ADDRESS(f.ports[1]));
#ifdef OPTISCOPE_ENABLE_STATS
        graph->nmergings++;
#endif
    } else if (is_atomic_symbol(g.ports[-1])) {
        connect_ports(&g.ports[0], DECODE_ADDRESS(f.ports[1]));
#ifdef OPTISCOPE_ENABLE_STATS
        graph->ncommutations++;
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
    MY_ASSERT(graph);
    XASSERT(f.ports);

    if (IS_DELIMITER(f.ports[-1])) { try_merge_delimiter(graph, f); }
}

// Immediate Duplication
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

COMPILER_NONNULL(1) COMPILER_HOT //
static void
try_duplicate(struct context *const restrict graph, const struct node f) {
    MY_ASSERT(graph);
    XASSERT(f.ports);
    XASSERT(IS_DUPLICATOR(f.ports[-1]));

    const struct node g = follow_port(&f.ports[0]);
    XASSERT(g.ports);

    if (is_atomic_symbol(g.ports[-1])) {
        const struct node gx = alloc_node_from(graph, g.ports[-1], &g);
        connect_ports(&g.ports[0], DECODE_ADDRESS(f.ports[1]));
        connect_ports(&gx.ports[0], DECODE_ADDRESS(f.ports[2]));
        free_node(graph, f);
#ifdef OPTISCOPE_ENABLE_STATS
        graph->ncommutations++;
        graph->nduplication_itrs++;
#endif
    }
}

// Graphviz Graph Generation
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#ifdef OPTISCOPE_ENABLE_GRAPHVIZ

#define GRAPHVIZ_INDENT "    "

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT COMPILER_RETURNS_NONNULL //
static const char *
graphviz_node_xlabel(const struct node node) {
    XASSERT(node.ports);

    static const char address_line[] = "+----------------+";

    static char buffer[256] = {0};

    const uint64_t *const p = node.ports;

#define SPRINTF(fmt, ...)                                                      \
    sprintf(buffer, "%s" fmt "%s", address_line, __VA_ARGS__, address_line)

    switch (ports_count(p[-1])) {
    case 1: SPRINTF("<BR/>| %p |<BR/>", (void *)&p[0]); break;
    case 2:
        SPRINTF(
            "<BR/>| %p |<BR/>| %p |<BR/>", //
            (void *)&p[0],
            (void *)&p[1]);
        break;
    case 3:
        SPRINTF(
            "<BR/>| %p |<BR/>| %p |<BR/>| %p |<BR/>",
            (void *)&p[0],
            (void *)&p[1],
            (void *)&p[2]);
        break;
    case 4:
        SPRINTF(
            "<BR/>| %p |<BR/>| %p |<BR/>| %p |<BR/>| %p |<BR/>",
            (void *)&p[0],
            (void *)&p[1],
            (void *)&p[2],
            (void *)&p[3]);
        break;
    default: COMPILER_UNREACHABLE();
    }

#undef SPRINTF

    return buffer;
}

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT COMPILER_RETURNS_NONNULL //
static const char *
graphviz_edge_label(const struct node node, const uint8_t i) {
    XASSERT(node.ports);

    static char buffer[16] = {0};

    switch (node.ports[-1]) {
    case SYMBOL_APPLICATOR:
        switch (i) {
        case 0: sprintf(buffer, "rator (\\#%" PRIu8 ")", i); break;
        case 1: sprintf(buffer, "\\#%" PRIu8, i); break;
        case 2: sprintf(buffer, "rand (\\#%" PRIu8 ")", i); break;
        default: COMPILER_UNREACHABLE();
        }
        break;
    case SYMBOL_LAMBDA:
    case SYMBOL_LAMBDA_C:
        switch (i) {
        case 0: sprintf(buffer, "\\#%" PRIu8, i); break;
        case 1: sprintf(buffer, "binder (\\#%" PRIu8 ")", i); break;
        case 2: sprintf(buffer, "body (\\#%" PRIu8 ")", i); break;
        default: COMPILER_UNREACHABLE();
        }
        break;
    case SYMBOL_GC_LAMBDA:
        switch (i) {
        case 0: sprintf(buffer, "\\#%" PRIu8, i); break;
        case 1: sprintf(buffer, "body (\\#%" PRIu8 ")", i); break;
        default: COMPILER_UNREACHABLE();
        }
        break;
    default: sprintf(buffer, "\\#%" PRIu8, i);
    }

    return buffer;
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
    case SYMBOL_GC_LAMBDA:
        switch (i) {
        case 0: return "n";
        case 1: return "s";
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
    case SYMBOL_ERASER:
    case SYMBOL_CELL:
    case SYMBOL_IDENTITY_LAMBDA:
    case SYMBOL_REFERENCE:
        switch (i) {
        case 0: return "n";
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
    default:
        if (IS_DUPLICATOR(node.ports[-1])) goto duplicator;
        else if (IS_DELIMITER(node.ports[-1])) goto delimiter;
        else COMPILER_UNREACHABLE();
    duplicator:
        switch (i) {
        case 0: return "s";
        case 1: return "nw";
        case 2: return "ne";
        default: COMPILER_UNREACHABLE();
        }
    case SYMBOL_UNARY_CALL:
    case SYMBOL_BINARY_CALL_AUX:
    case SYMBOL_BARRIER:
    delimiter:
        switch (i) {
        case 0: return "s";
        case 1: return "n";
        default: COMPILER_UNREACHABLE();
        }
    }
}

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT COMPILER_RETURNS_NONNULL //
static const char *
graphviz_print_symbol(const struct node node) {
    XASSERT(node.ports);

    static char buffer[MAX_SSYMBOL_SIZE + 64] = {0};

    sprintf(buffer, "%s", print_symbol(node.ports[-1]));

#define SPRINTF(fmt, ...) sprintf(buffer + strlen(buffer), fmt, __VA_ARGS__)

    if (SYMBOL_CELL == node.ports[-1]) {
        SPRINTF(" %" PRIu64, node.ports[1]);
    } else if (SYMBOL_BINARY_CALL_AUX == node.ports[-1]) {
        SPRINTF(" %" PRIu64, node.ports[3]);
    } else if (SYMBOL_BARRIER == node.ports[-1]) {
        SPRINTF(" %" PRIu64, node.ports[2]);
    } else if (IS_DELIMITER(node.ports[-1])) {
        SPRINTF(" %" PRIu64, node.ports[2]);
    }

#undef SPRINTF

    return buffer;
}

struct graphviz_context {
    struct context *graph;
    struct multifocus history;
    FILE *stream;
};

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT COMPILER_NONNULL(1) //
inline static bool
graphviz_is_current_node(
    struct graphviz_context *const restrict ctx, const struct node node) {
    MY_ASSERT(ctx);
    XASSERT(node.ports);

    return ctx->graph->current_pair[0].ports == node.ports ||
           ctx->graph->current_pair[1].ports == node.ports;
}

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT //
inline static bool
graphviz_is_active_node(const struct node node) {
    XASSERT(node.ports);

    const struct node f = node, g = follow_port(&node.ports[0]);

    return is_interacting_with(f, g) && SYMBOL_ROOT != node.ports[-1];
}

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT //
inline static bool
graphviz_is_active_edge(const struct node node, const uint8_t i) {
    XASSERT(node.ports);

    const struct node f = node, g = follow_port(&node.ports[i]);

    return is_interaction(f, g);
}

COMPILER_NONNULL(1) //
static void
graphviz_draw_node(
    struct graphviz_context *const restrict ctx, const struct node node) {
    MY_ASSERT(ctx);
    XASSERT(ctx->stream);
    XASSERT(node.ports);

    const uint64_t *const p = node.ports;

    const bool is_active = graphviz_is_active_node(node),
               is_current = graphviz_is_current_node(ctx, node),
               is_root = SYMBOL_ROOT == p[-1];

    fprintf(
        ctx->stream,
        // clang-format off
        GRAPHVIZ_INDENT "n%p"
        " [label=\"%s\""
        ", xlabel=<<FONT FACE=\"Courier\" COLOR=\"blue\" POINT-SIZE=\"8\">%s</FONT>>"
        "%s%s%s];\n",
        // clang-format on
        (void *)p,
        graphviz_print_symbol(node),
        graphviz_node_xlabel(node),
        (is_current && !is_root
             ? ", color=darkred"
             : (is_active && !is_root ? ", color=darkgreen" : "")),
        (is_active ? ", penwidth=2.3" : ""),
        (is_root ? ", style=filled" : ""));
}

COMPILER_NONNULL(1) //
static void
graphviz_draw_edge(
    struct graphviz_context *const restrict ctx,
    const struct node source,
    const uint8_t i) {
    MY_ASSERT(ctx);
    XASSERT(source.ports);

    uint64_t *const target_port = DECODE_ADDRESS(source.ports[i]);
    const struct node target = node_of_port(target_port);

    const bool is_active = graphviz_is_active_edge(source, i);

    fprintf(
        ctx->stream,
        // clang-format off
        GRAPHVIZ_INDENT "n%p -> n%p [label=\" %s \", tailport=%s%s%s%s%s];\n",
        // clang-format on
        (void *)source.ports,
        (void *)target.ports,
        graphviz_edge_label(source, i),
        graphviz_edge_tailport(source, i),
        (is_active && graphviz_is_current_node(ctx, source)
             ? ", color=darkred"
             : (is_active ? ", color=darkgreen" : "")),
        (is_active ? ", penwidth=1.5" : ""),
        (IS_PRINCIPAL_PORT(*target_port) ? ", arrowhead=dot" : ""),
        (0 == i ? ", style=dashed" : ""));
}

COMPILER_NONNULL(1) //
static void
go_graphviz(
    struct graphviz_context *const restrict ctx,
    const struct node source,
    const uint8_t i) {
    MY_ASSERT(ctx);
    XASSERT(ctx->stream);
    XASSERT(source.ports);

    const struct node node = follow_port(&source.ports[i]);

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

    MY_ASSERT(graph);
    MY_ASSERT(filename);

    FILE *fp = fopen(filename, "w");
    if (NULL == fp) { perror("fopen"), abort(); }

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
    CLEAR_MEMORY(ctx.graph->current_pair);
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
wait_for_user(struct context *const restrict graph) {
    MY_ASSERT(graph);

#ifdef OPTISCOPE_ENABLE_GRAPHVIZ
    graphviz(graph, "target/state.dot");
    if (system("./command/graphviz-state.sh") != 0) {
        panic("Failed to run `./command/graphviz-state.sh`!");
    }
#endif

    printf("Press ENTER to proceed...");
    fflush(stdout);
    if (EOF == getchar()) { perror("getchar"), abort(); }
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
    MY_ASSERT(graph);
    MY_ASSERT(term);

    switch (term->ty) {
    case LAMBDA_TERM_LAMBDA: {
        struct lambda_data *const binder = term->data.lambda;
        struct lambda_term *const body = term->data.lambda->body;
        XASSERT(binder);
        XASSERT(body);

        const bool is_identity =
            LAMBDA_TERM_VAR == body->ty && binder == *body->data.var;
        if (is_identity) {
            // clang-format off
            const struct node lambda = alloc_node(graph, SYMBOL_IDENTITY_LAMBDA);
            // clang-format on
            BC_ATTACH_NODE(bc, lambda, 0, &term->connect_to);
            break;
        }

        if (0 == binder->nusages) {
            // This is a lambda that "garbage-collects" its argument.
            const struct node lambda = alloc_node(graph, SYMBOL_GC_LAMBDA);
            BC_ATTACH_NODE(bc, lambda, 0, &term->connect_to);
            BC_SAVE_PORT(bc, &body->connect_to, 1);
            emit_bytecode(graph, bc, body, lvl + 1);
            break;
        }

        const struct node lambda = alloc_node(
            graph, term->fv_count > 0 ? SYMBOL_LAMBDA : SYMBOL_LAMBDA_C);

        binder->binder_ports = xmalloc(sizeof(uint64_t *) * binder->nusages);
        binder->lvl = lvl;
        BC_ATTACH_NODE(bc, lambda, 0, &term->connect_to);
        if (1 == binder->nusages) {
            // This is a linear non-selfe-referential lambda.
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
        BC_DELIMIT(bc, binder->binder_ports++, &term->connect_to, idx);

        break;
    }
    case LAMBDA_TERM_APPLY: {
        struct lambda_term *const rator = term->data.apply.rator, //
            *const rand = term->data.apply.rand;

        const struct node applicator = alloc_node(graph, SYMBOL_APPLICATOR);

        BC_ATTACH_NODE(bc, applicator, 1, &term->connect_to);
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
        uint64_t (*const function)(uint64_t) = term->data.u_call.function;
        struct lambda_term *const rand = term->data.u_call.rand;

        const struct node call = alloc_node(graph, SYMBOL_UNARY_CALL);
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
        call.ports[2] = U64_OF_FUNCTION(function);
#pragma GCC diagnostic pop

        BC_ATTACH_NODE(bc, call, 1, &term->connect_to);
        BC_SAVE_PORT(bc, &rand->connect_to, 0);
        emit_bytecode(graph, bc, rand, lvl);

        break;
    }
    case LAMBDA_TERM_BINARY_CALL: {
        uint64_t (*const function)(uint64_t, uint64_t) = //
            term->data.b_call.function;
        struct lambda_term *const lhs = term->data.b_call.lhs, //
            *const rhs = term->data.b_call.rhs;

        const struct node call = alloc_node(graph, SYMBOL_BINARY_CALL);
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
        call.ports[3] = U64_OF_FUNCTION(function);
#pragma GCC diagnostic pop

        BC_ATTACH_NODE(bc, call, 1, &term->connect_to);
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
    case LAMBDA_TERM_FIX: {
        struct lambda_term *const f = term->data.fix.f;

        BC_FIX(bc, &term->connect_to);
        BC_SAVE_PORT(bc, &f->connect_to, 0);
        emit_bytecode(graph, bc, f, lvl);

        break;
    }
    case LAMBDA_TERM_PERFORM: {
        struct lambda_term *const action = term->data.perform.action, //
            *const k = term->data.perform.k;

        const struct node perform = alloc_node(graph, SYMBOL_PERFORM);

        BC_ATTACH_NODE(bc, perform, 1, &term->connect_to);
        BC_SAVE_PORT(bc, &action->connect_to, 0);
        BC_SAVE_PORT(bc, &k->connect_to, 2);
        emit_bytecode(graph, bc, action, lvl);
        emit_bytecode(graph, bc, k, lvl);

        break;
    }
    case LAMBDA_TERM_REFERENCE: {
        struct lambda_term *(*const function)(void) = term->data.ref.function;

        const struct node ref = alloc_node(graph, SYMBOL_REFERENCE);
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
        ref.ports[1] = U64_OF_FUNCTION(function);
#pragma GCC diagnostic pop

        BC_ATTACH_NODE(bc, ref, 0, &term->connect_to);

        break;
    }
    default: COMPILER_UNREACHABLE();
    }
}

// Bytecode Execution
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

COMPILER_NONNULL(1, 2, 4) //
static void
build_duplicator_tree(
    struct context *const restrict graph,
    uint64_t *const restrict binder_port,
    const uint64_t n,
    uint64_t **const restrict ports) {
    MY_ASSERT(graph);
    MY_ASSERT(binder_port);
    MY_ASSERT(ports);
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
    MY_ASSERT(graph);
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
        case INSTRUCTION_DELIMIT: {
            uint64_t **const points_to = instr.data.delimit.points_to, //
                **goes_from = instr.data.delimit.goes_from;
            const uint64_t n = instr.data.delimit.n;

            inst_delimiter(
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
        case INSTRUCTION_FIX: {
            uint64_t **const connect_to = instr.data.fix.connect_to;

            const struct node dup = alloc_node(graph, SYMBOL_DUPLICATOR(0));
            const struct node applicator = alloc_node(graph, SYMBOL_APPLICATOR);

            connect_ports(&dup.ports[0], &applicator.ports[1]);
            connect_ports(&dup.ports[1], *connect_to);
            connect_ports(&dup.ports[2], &applicator.ports[2]);

            current = applicator;

            break;
        }
        default: COMPILER_UNREACHABLE();
        }
    }
}

// Eraser-Passing Garbage Collection
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

COMPILER_NONNULL(1) COMPILER_HOT //
static void
gc_step(
    struct context *const restrict graph,
    const struct node f,
    const struct node g,
    const ptrdiff_t i) {
    MY_ASSERT(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    XASSERT(i >= 0 && (uint64_t)i < MAX_PORTS);
    XASSERT(SYMBOL_ERASER == f.ports[-1]);

    switch (g.ports[-1]) {
    commute_1_2: {
        connect_ports(&f.ports[0], DECODE_ADDRESS(g.ports[0 == i ? 1 : 0]));

        focus_on(&graph->gc_focus, f);

        free_node(graph, g);

#ifdef OPTISCOPE_ENABLE_STATS
        graph->ngc++;
#endif
        break;
    }
    commute_1_3: {
        uint8_t j, k;
        (0 == i ? (j = 1, k = 2)
                : (1 == i ? (j = 0, k = 2)
                          : (2 == i ? (j = 0, k = 1)
                                    : (COMPILER_UNREACHABLE(), 0))));

        connect_ports(&f.ports[0], DECODE_ADDRESS(g.ports[j]));
        const struct node fx = alloc_gc_node(graph, DECODE_ADDRESS(g.ports[k]));

        focus_on(&graph->gc_focus, f);
        focus_on(&graph->gc_focus, fx);

        free_node(graph, g);

#ifdef OPTISCOPE_ENABLE_STATS
        graph->ngc++;
#endif
        break;
    }
    commute_1_4: {
        uint8_t j, k, l;
        (0 == i ? (j = 1, k = 2, l = 3)
                : (1 == i ? (j = 0, k = 2, l = 3)
                          : (2 == i ? (j = 0, k = 1, l = 3)
                                    : (3 == i ? (j = 0, k = 1, l = 2)
                                              : (COMPILER_UNREACHABLE(), 0)))));

        connect_ports(&f.ports[0], DECODE_ADDRESS(g.ports[j]));
        const struct node fx = alloc_gc_node(graph, DECODE_ADDRESS(g.ports[k]));
        const struct node fxx =
            alloc_gc_node(graph, DECODE_ADDRESS(g.ports[l]));

        focus_on(&graph->gc_focus, f);
        focus_on(&graph->gc_focus, fx);
        focus_on(&graph->gc_focus, fxx);

        free_node(graph, g);

#ifdef OPTISCOPE_ENABLE_STATS
        graph->ngc++;
#endif
        break;
    }
    annihilate:
        free_node(graph, f);
        free_node(graph, g);

#ifdef OPTISCOPE_ENABLE_STATS
        graph->ngc++;
#endif
        break;
    case SYMBOL_LAMBDA:
    case SYMBOL_LAMBDA_C:
        if (1 == i) {
            const struct node gc_lambda = alloc_node(graph, SYMBOL_GC_LAMBDA);
            connect_ports(&gc_lambda.ports[0], DECODE_ADDRESS(g.ports[0]));
            connect_ports(&gc_lambda.ports[1], DECODE_ADDRESS(g.ports[2]));
            free_node(graph, f);
            free_node(graph, g);
            break;
        } else {
            goto commute_1_3;
        }
    duplicator:
        switch (i) {
        case 1:
        case 2: {
            uint64_t *const points_to = DECODE_ADDRESS(g.ports[0]), //
                *const shares_with = DECODE_ADDRESS(g.ports[1 == i ? 2 : 1]);

            const struct node h = node_of_port(shares_with),
                              shared = node_of_port(points_to);

            if (SYMBOL_ERASER == h.ports[-1]) {
                connect_ports(&f.ports[0], points_to);
                focus_on(&graph->gc_focus, f);
                free_node(graph, g);
                if (PHASE_GC == DECODE_PHASE_METADATA(h.ports[0])) {
                    set_phase(&h.ports[0], PHASE_GC_AUX);
                } else {
                    free_node(graph, h);
                }
#ifdef OPTISCOPE_ENABLE_STATS
                graph->ngc++;
#endif
            } else if (is_atomic_symbol(shared.ports[-1])) {
                connect_ports(&shared.ports[0], shares_with);
                free_node(graph, g), free_node(graph, f);
#ifdef OPTISCOPE_ENABLE_STATS
                graph->ngc++;
#endif
            } else if (SYMBOL_DUPLICATOR(UINT64_C(0)) == g.ports[-1]) {
                connect_ports(points_to, shares_with);
                free_node(graph, g);
#ifdef OPTISCOPE_ENABLE_STATS
                graph->ngc++;
#endif
            } else {
                f.ports[0] &= PHASE_MASK;
            }

            break;
        }
        case 0: goto commute_1_3;
        default: COMPILER_UNREACHABLE();
        }
        break;
    case SYMBOL_UNARY_CALL:
    case SYMBOL_BINARY_CALL_AUX:
    case SYMBOL_GC_LAMBDA:
    case SYMBOL_BARRIER:
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
    default:
        if (IS_DUPLICATOR(g.ports[-1])) goto duplicator;
        else if (IS_DELIMITER(g.ports[-1])) goto delimiter;
        else COMPILER_UNREACHABLE();
    }
}

COMPILER_NONNULL(1, 2) COMPILER_HOT //
static void
gc(struct context *const restrict graph, uint64_t *const restrict port) {
    debug("%s(%p)", __func__, (void *)port);

    MY_ASSERT(graph);
    MY_ASSERT(port);

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
            case PHASE_STACK: //
                f.ports[0] &= PHASE_MASK;
                break;
            default: //
                gc_step(graph, f, g, points_to - g.ports);
            }
        }
    }
}

// Core Interaction Rules
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#ifndef NDEBUG

static void
assert_annihilation(const struct node f, const struct node g) {
    MY_ASSERT(f.ports);
    MY_ASSERT(g.ports);
    MY_ASSERT(is_interaction(f, g));
    MY_ASSERT(f.ports[-1] == g.ports[-1]);
}

static void
assert_beta(
    const struct context *const restrict graph,
    const struct node f,
    const struct node g) {
    MY_ASSERT(graph);
    MY_ASSERT(f.ports);
    MY_ASSERT(g.ports);
    MY_ASSERT(is_interaction(f, g));
    MY_ASSERT(SYMBOL_APPLICATOR == f.ports[-1]);
    MY_ASSERT(SYMBOL_LAMBDA == g.ports[-1]);
}

static void
assert_beta_c(
    const struct context *const restrict graph,
    const struct node f,
    const struct node g) {
    MY_ASSERT(graph);
    MY_ASSERT(f.ports);
    MY_ASSERT(g.ports);
    MY_ASSERT(is_interaction(f, g));
    MY_ASSERT(SYMBOL_APPLICATOR == f.ports[-1]);
    MY_ASSERT(SYMBOL_LAMBDA_C == g.ports[-1]);
}

static void
assert_identity_beta(
    const struct context *const restrict graph,
    const struct node f,
    const struct node g) {
    MY_ASSERT(graph);
    MY_ASSERT(f.ports);
    MY_ASSERT(g.ports);
    MY_ASSERT(is_interaction(f, g));
    MY_ASSERT(SYMBOL_APPLICATOR == f.ports[-1]);
    MY_ASSERT(SYMBOL_IDENTITY_LAMBDA == g.ports[-1]);
}

static void
assert_gc_beta(
    const struct context *const restrict graph,
    const struct node f,
    const struct node g) {
    MY_ASSERT(graph);
    MY_ASSERT(f.ports);
    MY_ASSERT(g.ports);
    MY_ASSERT(is_interaction(f, g));
    MY_ASSERT(SYMBOL_APPLICATOR == f.ports[-1]);
    MY_ASSERT(SYMBOL_GC_LAMBDA == g.ports[-1]);
}

static void
assert_expand(
    const struct context *const restrict graph,
    const struct node f,
    const struct node g) {
    MY_ASSERT(graph);
    MY_ASSERT(f.ports);
    MY_ASSERT(g.ports);
    MY_ASSERT(is_interaction(f, g));
    MY_ASSERT(SYMBOL_REFERENCE == f.ports[-1]);
    MY_ASSERT(is_operator_symbol(g.ports[-1]));
}

static void
assert_commutation(const struct node f, const struct node g) {
    MY_ASSERT(f.ports);
    MY_ASSERT(g.ports);
    MY_ASSERT(is_interaction(f, g));
    MY_ASSERT(f.ports[-1] != g.ports[-1]);
}

static void
assert_unary_call(
    const struct context *const restrict graph,
    const struct node f,
    const struct node g) {
    MY_ASSERT(graph);
    MY_ASSERT(f.ports);
    MY_ASSERT(g.ports);
    MY_ASSERT(is_interaction(f, g));
    MY_ASSERT(SYMBOL_UNARY_CALL == f.ports[-1]);
    MY_ASSERT(SYMBOL_CELL == g.ports[-1]);
}

static void
assert_binary_call(
    const struct context *const restrict graph,
    const struct node f,
    const struct node g) {
    MY_ASSERT(graph);
    MY_ASSERT(f.ports);
    MY_ASSERT(g.ports);
    MY_ASSERT(is_interaction(f, g));
    MY_ASSERT(SYMBOL_BINARY_CALL == f.ports[-1]);
    MY_ASSERT(SYMBOL_CELL == g.ports[-1]);
}

static void
assert_binary_call_aux(
    const struct context *const restrict graph,
    const struct node f,
    const struct node g) {
    MY_ASSERT(graph);
    MY_ASSERT(f.ports);
    MY_ASSERT(g.ports);
    MY_ASSERT(is_interaction(f, g));
    MY_ASSERT(SYMBOL_BINARY_CALL_AUX == f.ports[-1]);
    MY_ASSERT(SYMBOL_CELL == g.ports[-1]);
}

static void
assert_if_then_else(
    const struct context *const restrict graph,
    const struct node f,
    const struct node g) {
    MY_ASSERT(graph);
    MY_ASSERT(f.ports);
    MY_ASSERT(g.ports);
    MY_ASSERT(is_interaction(f, g));
    MY_ASSERT(SYMBOL_IF_THEN_ELSE == f.ports[-1]);
    MY_ASSERT(SYMBOL_CELL == g.ports[-1]);
}

static void
assert_perform(
    const struct context *const restrict graph,
    const struct node f,
    const struct node g) {
    MY_ASSERT(graph);
    MY_ASSERT(f.ports);
    MY_ASSERT(g.ports);
    MY_ASSERT(is_interaction(f, g));
    MY_ASSERT(SYMBOL_PERFORM == f.ports[-1]);
    MY_ASSERT(SYMBOL_CELL == g.ports[-1]);
}

static void
assert_barrier(
    const struct context *const restrict graph,
    const struct node f,
    const struct node g) {
    MY_ASSERT(graph);
    MY_ASSERT(f.ports);
    MY_ASSERT(g.ports);
    MY_ASSERT(is_interaction(f, g));
    MY_ASSERT(SYMBOL_BARRIER == f.ports[-1]);
    MY_ASSERT(SYMBOL_DELIMITER(UINT64_C(0)) == g.ports[-1]);
}

static void
assert_unbarrier(
    const struct context *const restrict graph,
    const struct node f,
    const struct node g) {
    MY_ASSERT(graph);
    MY_ASSERT(f.ports);
    MY_ASSERT(g.ports);
    MY_ASSERT(is_interaction(f, g));
    MY_ASSERT(SYMBOL_BARRIER == f.ports[-1]);
    // `g` is unspecified.
}

#else

#define assert_annihilation(f, g)           ((void)0)
#define assert_beta(graph, f, g)            ((void)0)
#define assert_beta_c(graph, f, g)          ((void)0)
#define assert_identity_beta(graph, f, g)   ((void)0)
#define assert_gc_beta(graph, f, g)         ((void)0)
#define assert_expand(graph, f, g)          ((void)0)
#define assert_commutation(f, g)            ((void)0)
#define assert_unary_call(graph, f, g)      ((void)0)
#define assert_binary_call(graph, f, g)     ((void)0)
#define assert_binary_call_aux(graph, f, g) ((void)0)
#define assert_if_then_else(graph, f, g)    ((void)0)
#define assert_perform(graph, f, g)         ((void)0)
#define assert_barrier(graph, f, g)         ((void)0)
#define assert_unbarrier(graph, f, g)       ((void)0)

#endif // NDEBUG

#ifdef OPTISCOPE_ENABLE_TRACING

COMPILER_NONNULL(1, 2) //
static void
debug_interaction(
    const char *const restrict caller,
    struct context *const restrict graph,
    const struct node f,
    const struct node g) {
    MY_ASSERT(caller);
    MY_ASSERT(graph);

#ifdef OPTISCOPE_ENABLE_GRAPHVIZ
    graph->current_pair[0] = f;
    graph->current_pair[1] = g;
#endif

    char f_ssymbol[MAX_SSYMBOL_SIZE] = {0}, g_ssymbol[MAX_SSYMBOL_SIZE] = {0};
    strcpy(f_ssymbol, print_symbol(f.ports[-1]));
    strcpy(g_ssymbol, print_symbol(g.ports[-1]));
    debug(
        "%s(%p %s, %p %s)",
        caller,
        (void *)f.ports,
        f_ssymbol,
        (void *)g.ports,
        g_ssymbol);
    wait_for_user(graph);
}

#else

#define debug_interaction(caller, graph, f, g) ((void)0)

#endif // OPTISCOPE_ENABLE_TRACING

typedef void (*Rule)(
    struct context *const restrict graph, struct node f, struct node g);

#define RULE_DEFINITION(name, graph, f, g)                                     \
    COMPILER_NONNULL(1) COMPILER_HOT /* */                                     \
    static void                                                                \
    name(struct context *const restrict graph, struct node f, struct node g)

#define TYPE_CHECK_RULE(name)                                                  \
    COMPILER_UNUSED static const Rule name##_type_check = name

RULE_DEFINITION(annihilate, graph, f, g) {
    MY_ASSERT(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    assert_annihilation(f, g);
    debug_interaction(__func__, graph, f, g);

#ifdef OPTISCOPE_ENABLE_STATS
    graph->nannihilations++;
#endif

    const uint64_t n = ports_count(f.ports[-1]) - 1;
    XASSERT(n <= MAX_AUXILIARY_PORTS);

    for (uint8_t i = 1; i <= n; i++) {
        // Respective ports must have the same semantic meaning.
        connect_ports(DECODE_ADDRESS(f.ports[i]), DECODE_ADDRESS(g.ports[i]));
    }

    free_node(graph, f);
    free_node(graph, g);
}

TYPE_CHECK_RULE(annihilate);

RULE_DEFINITION(commute, graph, f, g) {
    MY_ASSERT(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    assert_commutation(f, g);
    debug_interaction(__func__, graph, f, g);

#ifdef OPTISCOPE_ENABLE_STATS
    graph->ncommutations++;
#endif

#ifndef NDEBUG
    {
        const bool with_lambda_or_delim =
            IS_ANY_LAMBDA(g.ports[-1]) || IS_DELIMITER(g.ports[-1]);

        // Ensure that lambdas & delimiters are alwaies `g`, to give `f` the
        // opportunity to increment its index.
        MY_ASSERT(
            !((IS_ANY_LAMBDA(f.ports[-1]) || IS_DELIMITER(f.ports[-1])) &&
              !with_lambda_or_delim));

        // If `f` is a lambda & `g` is a delimiter, swap them so that the index
        // of `g` could be incremented.
        MY_ASSERT(!(IS_ANY_LAMBDA(f.ports[-1]) && IS_DELIMITER(g.ports[-1])));
    }
#endif

    int64_t i = symbol_index(f.ports[-1]), j = symbol_index(g.ports[-1]);

    // If both are delimiters, the one with a higher index should be `f`.
    if (IS_DELIMITER(f.ports[-1]) && IS_DELIMITER(g.ports[-1]) && j > i) {
        const struct node h = f;
        f = g, g = h;
        const int64_t k = i;
        i = j, j = k;
    }

    const bool update_symbol = (IS_ANY_LAMBDA(g.ports[-1]) && i >= 0) ||
                               (IS_DELIMITER(g.ports[-1]) && i >= j);

    const uint64_t fsym =
                       update_symbol ? bump_index(f.ports[-1], 1) : f.ports[-1],
                   gsym = g.ports[-1];

    const uint8_t n = ports_count(f.ports[-1]) - 1,
                  m = ports_count(g.ports[-1]) - 1;

    struct node f_updates[MAX_AUXILIARY_PORTS] = {{NULL}},
                g_updates[MAX_AUXILIARY_PORTS] = {{NULL}};

    for (uint8_t i = 0; i < m; i++) {
        f_updates[i] = alloc_node_from(graph, fsym, &f);
        // clang-format off
        connect_ports(
            &f_updates[i].ports[0], DECODE_ADDRESS(g.ports[m - i]));
        // clang-format on
    }
    for (uint8_t i = 0; i < n; i++) {
        g_updates[i] = alloc_node_from(graph, gsym, &g);
        // clang-format off
        connect_ports(
            &g_updates[i].ports[0], DECODE_ADDRESS(f.ports[i + 1]));
        // clang-format on
    }

    for (uint8_t i = 0; i < m; i++) {
        for (uint8_t j = 0; j < n; j++) {
            connect_ports(
                &f_updates[i].ports[j + 1], &g_updates[j].ports[m - i]);
        }
    }

    free_node(graph, f);
    free_node(graph, g);
}

TYPE_CHECK_RULE(commute);

RULE_DEFINITION(beta, graph, f, g) {
    MY_ASSERT(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    assert_beta(graph, f, g);
    debug_interaction(__func__, graph, f, g);

#ifdef OPTISCOPE_ENABLE_STATS
    graph->nbetas++;
#endif

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

TYPE_CHECK_RULE(beta);

RULE_DEFINITION(beta_c, graph, f, g) {
    MY_ASSERT(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    assert_beta_c(graph, f, g);
    debug_interaction(__func__, graph, f, g);

#ifdef OPTISCOPE_ENABLE_STATS
    graph->nbetas++;
#endif

    connect_ports(DECODE_ADDRESS(f.ports[1]), DECODE_ADDRESS(g.ports[2]));
    connect_ports(DECODE_ADDRESS(f.ports[2]), DECODE_ADDRESS(g.ports[1]));

    free_node(graph, f);
    free_node(graph, g);
}

TYPE_CHECK_RULE(beta_c);

RULE_DEFINITION(identity_beta, graph, f, g) {
    MY_ASSERT(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    assert_identity_beta(graph, f, g);
    debug_interaction(__func__, graph, f, g);

#ifdef OPTISCOPE_ENABLE_STATS
    graph->nbetas++;
#endif

    connect_ports(DECODE_ADDRESS(f.ports[1]), DECODE_ADDRESS(f.ports[2]));

    free_node(graph, f);
    free_node(graph, g);
}

TYPE_CHECK_RULE(identity_beta);

RULE_DEFINITION(gc_beta, graph, f, g) {
    MY_ASSERT(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    assert_gc_beta(graph, f, g);
    debug_interaction(__func__, graph, f, g);

#ifdef OPTISCOPE_ENABLE_STATS
    graph->nbetas++;
#endif

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

TYPE_CHECK_RULE(gc_beta);

RULE_DEFINITION(barrier, graph, f, g) {
    MY_ASSERT(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    assert_barrier(graph, f, g);
    debug_interaction(__func__, graph, f, g);

#ifdef OPTISCOPE_ENABLE_STATS
    graph->nbarrier_operations++;
#endif

    connect_ports(&f.ports[0], DECODE_ADDRESS(g.ports[1]));
    f.ports[2] += g.ports[2];

    free_node(graph, g);
}

TYPE_CHECK_RULE(barrier);

RULE_DEFINITION(unbarrier, graph, f, g) {
    MY_ASSERT(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    assert_unbarrier(graph, f, g);
    debug_interaction(__func__, graph, f, g);

#ifdef OPTISCOPE_ENABLE_STATS
    graph->nbarrier_operations++;
#endif

    inst_delimiter(
        graph,
        (struct delimiter){.idx = 0, DECODE_ADDRESS(f.ports[1]), &g.ports[0]},
        f.ports[2]);

    free_node(graph, f);
}

TYPE_CHECK_RULE(unbarrier);

RULE_DEFINITION(do_expand, graph, f, g) {
    MY_ASSERT(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    assert_expand(graph, f, g);
    debug_interaction(__func__, graph, f, g);

#ifdef OPTISCOPE_ENABLE_STATS
    graph->nexpansions++;
#endif

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
    // clang-format off
    struct lambda_term *(*const function)(void) = USER_FUNCTION_OF_U64(f.ports[1]);
    const size_t idx = U64_OF_FUNCTION(function) % OPTISCOPE_BOOK_SIZE;
    // clang-format on
#pragma GCC diagnostic pop

    free_node(graph, f);

    struct expansion *expansion = NULL;
    struct lambda_term *term = NULL;

    size_t i = idx, limit = OPTISCOPE_BOOK_SIZE;
    while (i < limit) {
        struct expansion *const entry = &graph->book.array[i];

        if (function == entry->function) {
            expansion = entry;
            goto execute;
        } else if (NULL == entry->function) {
            expansion = entry;
            goto emit;
        } else if (OPTISCOPE_BOOK_SIZE - 1 == i) {
            i = 0, limit = idx;
        } else {
            i++;
        }
    }

    if (NULL == expansion) {
        // Evict an old entry from the book.
        struct expansion *const victim = &graph->book.array[idx];
        free_lambda_term(victim->expansion);
        free_bytecode(victim->bc);
        expansion = victim;
    }

emit:;
    {
        term = function();
        expansion->function = function;
        expansion->expansion = term;
        expansion->bc = alloc_bytecode(INITIAL_BYTECODE_CAPACITY);
        emit_bytecode(graph, &expansion->bc, term, 0);
    }

execute:;
    {
        term = expansion->expansion;
        term->connect_to = &g.ports[0];
        execute_bytecode(graph, expansion->bc);
    }
}

TYPE_CHECK_RULE(do_expand);

RULE_DEFINITION(do_unary_call, graph, f, g) {
    MY_ASSERT(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    assert_unary_call(graph, f, g);
    debug_interaction(__func__, graph, f, g);

#ifdef OPTISCOPE_ENABLE_STATS
    graph->ncell_operations++;
#endif

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
    g.ports[1] = (UNARY_FUNCTION_OF_U64(f.ports[2]))(g.ports[1]);
#pragma GCC diagnostic pop
    connect_ports(&g.ports[0], DECODE_ADDRESS(f.ports[1]));

    free_node(graph, f);
}

TYPE_CHECK_RULE(do_unary_call);

RULE_DEFINITION(do_binary_call, graph, f, g) {
    MY_ASSERT(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    assert_binary_call(graph, f, g);
    debug_interaction(__func__, graph, f, g);

#ifdef OPTISCOPE_ENABLE_STATS
    graph->ncell_operations++;
#endif

    const struct node aux = alloc_node(graph, SYMBOL_BINARY_CALL_AUX);
    connect_ports(&aux.ports[1], DECODE_ADDRESS(f.ports[1]));
    aux.ports[2] = f.ports[3];
    aux.ports[3] = g.ports[1];
    connect_ports(&aux.ports[0], DECODE_ADDRESS(f.ports[2]));

    free_node(graph, f);
    free_node(graph, g);
}

TYPE_CHECK_RULE(do_binary_call);

RULE_DEFINITION(do_binary_call_aux, graph, f, g) {
    MY_ASSERT(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    assert_binary_call_aux(graph, f, g);
    debug_interaction(__func__, graph, f, g);

#ifdef OPTISCOPE_ENABLE_STATS
    graph->ncell_operations++;
#endif

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
    g.ports[1] = (BINARY_FUNCTION_OF_U64(f.ports[2]))(f.ports[3], g.ports[1]);
#pragma GCC diagnostic pop
    connect_ports(&g.ports[0], DECODE_ADDRESS(f.ports[1]));

    free_node(graph, f);
}

TYPE_CHECK_RULE(do_binary_call_aux);

RULE_DEFINITION(do_if_then_else, graph, f, g) {
    MY_ASSERT(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    assert_if_then_else(graph, f, g);
    debug_interaction(__func__, graph, f, g);

#ifdef OPTISCOPE_ENABLE_STATS
    graph->ncell_operations++;
#endif

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

TYPE_CHECK_RULE(do_if_then_else);

RULE_DEFINITION(do_perform, graph, f, g) {
    MY_ASSERT(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    assert_perform(graph, f, g);
    debug_interaction(__func__, graph, f, g);

#ifdef OPTISCOPE_ENABLE_STATS
    graph->ncell_operations++;
#endif

    connect_ports(DECODE_ADDRESS(f.ports[1]), DECODE_ADDRESS(f.ports[2]));

    free_node(graph, f);
    free_node(graph, g);
}

TYPE_CHECK_RULE(do_perform);

// Specialized Annihilation Rules
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#define ANNIHILATION_PROLOGUE(graph, f, g)                                     \
    do {                                                                       \
        MY_ASSERT(graph);                                                      \
        XASSERT(f.ports);                                                      \
        XASSERT(g.ports);                                                      \
        assert_annihilation(f, g);                                             \
        debug_interaction(__func__, graph, f, g);                              \
        NANNIHILATIONS_PLUS_PLUS(graph);                                       \
    } while (false)

#ifdef OPTISCOPE_ENABLE_STATS
#define NANNIHILATIONS_PLUS_PLUS(graph) ((graph)->nannihilations++)
#else
#define NANNIHILATIONS_PLUS_PLUS(graph) ((void)0)
#endif

RULE_DEFINITION(annihilate_delim_delim, graph, f, g) {
    ANNIHILATION_PROLOGUE(graph, f, g);
    XASSERT(f.ports[2] > 0);
    XASSERT(g.ports[2] > 0);

    if (f.ports[2] == g.ports[2]) {
        connect_ports(DECODE_ADDRESS(f.ports[1]), DECODE_ADDRESS(g.ports[1]));
        free_node(graph, f);
        free_node(graph, g);
    } else if (f.ports[2] > g.ports[2]) {
        f.ports[2] -= g.ports[2];
        connect_ports(&f.ports[0], DECODE_ADDRESS(g.ports[1]));
        try_merge_delimiter(graph, f);
        free_node(graph, g);
    } else {
        g.ports[2] -= f.ports[2];
        connect_ports(DECODE_ADDRESS(f.ports[1]), &g.ports[0]);
        try_merge_delimiter(graph, g);
        free_node(graph, f);
    }
}

TYPE_CHECK_RULE(annihilate_delim_delim);

RULE_DEFINITION(annihilate_dup_dup, graph, f, g) {
    ANNIHILATION_PROLOGUE(graph, f, g);

    connect_ports(DECODE_ADDRESS(f.ports[1]), DECODE_ADDRESS(g.ports[1]));
    connect_ports(DECODE_ADDRESS(f.ports[2]), DECODE_ADDRESS(g.ports[2]));

    free_node(graph, f);
    free_node(graph, g);
}

TYPE_CHECK_RULE(annihilate_dup_dup);

#undef NANNIHILATIONS_PLUS_PLUS
#undef ANNIHILATION_PROLOGUE

// Specialized Commutation Rules
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#define COMMUTATION_PROLOGUE(graph, f, g)                                      \
    do {                                                                       \
        MY_ASSERT(graph);                                                      \
        XASSERT(f.ports);                                                      \
        XASSERT(g.ports);                                                      \
        assert_commutation(f, g);                                              \
        debug_interaction(__func__, graph, f, g);                              \
        NCOMMUTATIONS_PLUS_PLUS(graph);                                        \
    } while (false)

#ifdef OPTISCOPE_ENABLE_STATS
#define NCOMMUTATIONS_PLUS_PLUS(graph) ((graph)->ncommutations++)
#else
#define NCOMMUTATIONS_PLUS_PLUS(graph) ((void)0)
#endif

RULE_DEFINITION(commute_1_2, graph, f, g) {
    COMMUTATION_PROLOGUE(graph, f, g);

    connect_ports(&f.ports[0], DECODE_ADDRESS(g.ports[1]));

    free_node(graph, g);
}

TYPE_CHECK_RULE(commute_1_2);

#define commute_2_1(graph, f, g) commute_1_2((graph), (g), (f))

RULE_DEFINITION(commute_1_3, graph, f, g) {
    COMMUTATION_PROLOGUE(graph, f, g);

    const struct node fx = alloc_node_from(graph, f.ports[-1], &f);

    connect_ports(&f.ports[0], DECODE_ADDRESS(g.ports[1]));
    connect_ports(&fx.ports[0], DECODE_ADDRESS(g.ports[2]));

    free_node(graph, g);
}

TYPE_CHECK_RULE(commute_1_3);

#define commute_3_1(graph, f, g) commute_1_3((graph), (g), (f))

RULE_DEFINITION(commute_2_2_core, graph, f, g) {
    (void)graph;

    connect_ports(&f.ports[0], DECODE_ADDRESS(g.ports[1]));
    connect_ports(&g.ports[0], DECODE_ADDRESS(f.ports[1]));

    connect_ports(&f.ports[1], &g.ports[1]);

    try_merge_if_delimiter(graph, f);
    try_merge_if_delimiter(graph, g);
}

TYPE_CHECK_RULE(commute_2_2_core);

RULE_DEFINITION(commute_2_2, graph, f, g) {
    COMMUTATION_PROLOGUE(graph, f, g);
    commute_2_2_core(graph, f, g);
}

TYPE_CHECK_RULE(commute_2_2);

RULE_DEFINITION(commute_3_2_core, graph, f, g) {
    const struct node gx = alloc_node_from(graph, g.ports[-1], &g);

    connect_ports(&f.ports[0], DECODE_ADDRESS(g.ports[1]));
    connect_ports(&g.ports[0], DECODE_ADDRESS(f.ports[1]));
    connect_ports(&gx.ports[0], DECODE_ADDRESS(f.ports[2]));

    connect_ports(&f.ports[1], &g.ports[1]);
    connect_ports(&f.ports[2], &gx.ports[1]);

    if (IS_DUPLICATOR(f.ports[-1])) { try_duplicate(graph, f); }

    if (IS_DELIMITER(g.ports[-1])) {
        try_merge_delimiter(graph, g);
        try_merge_delimiter(graph, gx);
    }
}

TYPE_CHECK_RULE(commute_3_2_core);

#define commute_2_3_core(graph, f, g) commute_3_2_core((graph), (g), (f))

RULE_DEFINITION(commute_3_2, graph, f, g) {
    COMMUTATION_PROLOGUE(graph, f, g);
    commute_3_2_core(graph, f, g);
}

TYPE_CHECK_RULE(commute_3_2);

#define commute_2_3(graph, f, g) commute_3_2((graph), (g), (f))

RULE_DEFINITION(commute_3_3_core, graph, f, g) {
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

TYPE_CHECK_RULE(commute_3_3_core);

RULE_DEFINITION(commute_3_3, graph, f, g) {
    COMMUTATION_PROLOGUE(graph, f, g);
    commute_3_3_core(graph, f, g);
}

TYPE_CHECK_RULE(commute_3_3);

RULE_DEFINITION(commute_4_2, graph, f, g) {
    COMMUTATION_PROLOGUE(graph, f, g);

    const struct node gx = alloc_node_from(graph, g.ports[-1], &g);
    const struct node gxx = alloc_node_from(graph, g.ports[-1], &g);

    connect_ports(&f.ports[0], DECODE_ADDRESS(g.ports[1]));
    connect_ports(&g.ports[0], DECODE_ADDRESS(f.ports[1]));
    connect_ports(&gx.ports[0], DECODE_ADDRESS(f.ports[2]));
    connect_ports(&gxx.ports[0], DECODE_ADDRESS(f.ports[3]));

    connect_ports(&f.ports[1], &g.ports[1]);
    connect_ports(&f.ports[2], &gx.ports[1]);
    connect_ports(&f.ports[3], &gxx.ports[1]);

    if (IS_DELIMITER(g.ports[-1])) {
        try_merge_delimiter(graph, g);
        try_merge_delimiter(graph, gx);
        try_merge_delimiter(graph, gxx);
    }
}

TYPE_CHECK_RULE(commute_4_2);

#define commute_2_4(graph, f, g) commute_4_2((graph), (g), (f))

RULE_DEFINITION(commute_4_3, graph, f, g) {
    COMMUTATION_PROLOGUE(graph, f, g);

    const struct node fx = alloc_node_from(graph, f.ports[-1], &f);
    const struct node gx = alloc_node_from(graph, g.ports[-1], &g);
    const struct node gxx = alloc_node_from(graph, g.ports[-1], &g);

    connect_ports(&f.ports[0], DECODE_ADDRESS(g.ports[1]));
    connect_ports(&fx.ports[0], DECODE_ADDRESS(g.ports[2]));
    connect_ports(&g.ports[0], DECODE_ADDRESS(f.ports[1]));
    connect_ports(&gx.ports[0], DECODE_ADDRESS(f.ports[2]));
    connect_ports(&gxx.ports[0], DECODE_ADDRESS(f.ports[3]));

    connect_ports(&g.ports[1], &f.ports[1]);
    connect_ports(&g.ports[2], &fx.ports[1]);
    connect_ports(&gx.ports[1], &f.ports[2]);
    connect_ports(&gx.ports[2], &fx.ports[2]);
    connect_ports(&gxx.ports[1], &f.ports[3]);
    connect_ports(&gxx.ports[2], &fx.ports[3]);

    if (IS_DUPLICATOR(g.ports[-1])) {
        try_duplicate(graph, g);
        try_duplicate(graph, gx);
        try_duplicate(graph, gxx);
    }
}

TYPE_CHECK_RULE(commute_4_3);

#define commute_3_4(graph, f, g) commute_4_3((graph), (g), (f))

RULE_DEFINITION(commute_dup_delim, graph, f, g) {
    COMMUTATION_PROLOGUE(graph, f, g);

    if (symbol_index(f.ports[-1]) >= symbol_index(g.ports[-1])) {
        f.ports[-1] = bump_index(f.ports[-1], g.ports[2]);
    }

    commute_3_2_core(graph, f, g);
}

TYPE_CHECK_RULE(commute_dup_delim);

RULE_DEFINITION(commute_delim_delim, graph, f, g) {
    COMMUTATION_PROLOGUE(graph, f, g);

    if (f.ports[-1] > g.ports[-1]) {
        f.ports[-1] = bump_index(f.ports[-1], g.ports[2]);
    } else {
        g.ports[-1] = bump_index(g.ports[-1], f.ports[2]);
    }

    commute_2_2_core(graph, f, g);
}

TYPE_CHECK_RULE(commute_delim_delim);

RULE_DEFINITION(commute_lambda_delim, graph, f, g) {
    COMMUTATION_PROLOGUE(graph, f, g);
    g.ports[-1] = bump_index(g.ports[-1], 1);
    commute_3_2_core(graph, f, g);
}

TYPE_CHECK_RULE(commute_lambda_delim);

RULE_DEFINITION(commute_lambda_dup, graph, f, g) {
    COMMUTATION_PROLOGUE(graph, f, g);
    g.ports[-1] = bump_index(g.ports[-1], 1);
    commute_3_3_core(graph, f, g);
}

TYPE_CHECK_RULE(commute_lambda_dup);

RULE_DEFINITION(commute_gc_lambda_delim, graph, f, g) {
    COMMUTATION_PROLOGUE(graph, f, g);
    g.ports[-1] = bump_index(g.ports[-1], 1);
    commute_2_2_core(graph, f, g);
}

TYPE_CHECK_RULE(commute_gc_lambda_delim);

RULE_DEFINITION(commute_gc_lambda_dup, graph, f, g) {
    COMMUTATION_PROLOGUE(graph, f, g);
    g.ports[-1] = bump_index(g.ports[-1], 1);
    commute_2_3_core(graph, f, g);
}

TYPE_CHECK_RULE(commute_gc_lambda_dup);

// An excerpt from "8.1. Optimal vs. efficient":
// > For instance, extruding a scope over a closed λ-term costs time linear in
//   the size of the term in our implementation, whereas one observes that in
//   such cases it would be safe to simply remove the scope.
RULE_DEFINITION(commute_lambda_c_delim, graph, f, g) {
    COMMUTATION_PROLOGUE(graph, f, g);

    connect_ports(&f.ports[0], DECODE_ADDRESS(g.ports[1]));

    free_node(graph, g);
}

TYPE_CHECK_RULE(commute_lambda_c_delim);

RULE_DEFINITION(commute_lambda_c_dup, graph, f, g) {
    COMMUTATION_PROLOGUE(graph, f, g);
    g.ports[-1] = bump_index(g.ports[-1], 1);
    commute_3_3_core(graph, f, g);
}

TYPE_CHECK_RULE(commute_lambda_c_dup);

#undef NCOMMUTATIONS_PLUS_PLUS
#undef COMMUTATION_PROLOGUE

#undef TYPE_CHECK_RULE

// Specialized Extrusion Rules
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

// TODO: maybe print delimiter extrusions if tracing is enabled?

#define EXTRUSION_PROLOGUE(graph, f, g)                                        \
    do {                                                                       \
        MY_ASSERT(graph);                                                      \
        XASSERT(f.ports);                                                      \
        XASSERT(g.ports);                                                      \
        NEXTRUSIONS_PLUS_PLUS(graph);                                          \
    } while (false)

#ifdef OPTISCOPE_ENABLE_STATS
#define NEXTRUSIONS_PLUS_PLUS(graph) ((graph)->nextrusions++)
#else
#define NEXTRUSIONS_PLUS_PLUS(graph) ((void)0)
#endif

RULE_DEFINITION(extrude_2_2, graph, f, g) {
    EXTRUSION_PROLOGUE(graph, f, g);

    connect_ports(&g.ports[1], DECODE_ADDRESS(f.ports[1]));
    connect_ports(&f.ports[0], DECODE_ADDRESS(g.ports[0]));

    connect_ports(&g.ports[0], &f.ports[1]);

    try_merge_delimiter(graph, f);
}

RULE_DEFINITION(extrude_2_3, graph, f, g) {
    EXTRUSION_PROLOGUE(graph, f, g);

    const struct node fx = alloc_node_from(graph, f.ports[-1], &f);

    connect_ports(&g.ports[1], DECODE_ADDRESS(f.ports[1]));
    connect_ports(&f.ports[0], DECODE_ADDRESS(g.ports[0]));
    connect_ports(&fx.ports[0], DECODE_ADDRESS(g.ports[2]));

    connect_ports(&g.ports[0], &f.ports[1]);
    connect_ports(&g.ports[2], &fx.ports[1]);

    try_merge_delimiter(graph, f);
    try_merge_delimiter(graph, fx);
}

RULE_DEFINITION(extrude_2_4, graph, f, g) {
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
}

#undef NEXTRUSIONS_PLUS_PLUS
#undef EXTRUSION_PROLOGUE

#undef TYPE_CHECK_RULE

// Interaction Rule Dispatching
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

COMPILER_NONNULL(1) COMPILER_HOT //
static void
interact(
    struct context *const restrict graph,
    const struct node f,
    const struct node g) {
    MY_ASSERT(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-result"
    MY_ASSERT(is_interaction(f, g));
#pragma GCC diagnostic pop

    const uint64_t fsym = f.ports[-1], gsym = g.ports[-1];

    if ((is_operator_symbol(fsym) || IS_DUPLICATOR(fsym)) &&
        SYMBOL_DELIMITER(UINT64_C(0)) == gsym) {
        uint64_t *const h_port = DECODE_ADDRESS(g.ports[1]);
        const struct node h = node_of_port(h_port);

        if (DECODE_ADDRESS(h.ports[0]) != &g.ports[1]) {
            const struct node barrier = alloc_node(graph, SYMBOL_BARRIER);
            barrier.ports[2] = g.ports[2];
            connect_ports(&barrier.ports[0], h_port);
            connect_ports(&barrier.ports[1], &f.ports[0]);
            free_node(graph, g);
            return;
        }
    }

#ifdef OPTISCOPE_ENABLE_STATS
    if (IS_DUPLICATOR(fsym) || IS_DUPLICATOR(gsym)) { //
        graph->nduplication_itrs++;
    }
    if (IS_DELIMITER(fsym) || IS_DELIMITER(gsym)) { //
        graph->ndelimiter_itrs++;
    }
#endif

    switch (fsym) {
    duplicator:
        if (fsym == gsym) annihilate_dup_dup(graph, f, g);
        else if (SYMBOL_APPLICATOR == gsym) commute_3_3(graph, g, f);
        else if (SYMBOL_LAMBDA == gsym) commute_lambda_dup(graph, g, f);
        else if (SYMBOL_IDENTITY_LAMBDA == gsym) commute_1_3(graph, g, f);
        else if (SYMBOL_GC_LAMBDA == gsym) commute_gc_lambda_dup(graph, g, f);
        else if (SYMBOL_LAMBDA_C == gsym) commute_lambda_c_dup(graph, g, f);
        else if (SYMBOL_CELL == gsym) commute_1_3(graph, g, f);
        else if (SYMBOL_UNARY_CALL == gsym) commute_2_3(graph, g, f);
        else if (SYMBOL_BINARY_CALL == gsym) commute_3_3(graph, g, f);
        else if (SYMBOL_BINARY_CALL_AUX == gsym) commute_2_3(graph, g, f);
        else if (SYMBOL_IF_THEN_ELSE == gsym) commute_4_3(graph, g, f);
        else if (SYMBOL_REFERENCE == gsym) commute_1_3(graph, g, f);
        else if (SYMBOL_BARRIER == gsym) unbarrier(graph, g, f);
        else if (IS_DELIMITER(gsym)) commute_dup_delim(graph, f, g);
        else if (IS_DUPLICATOR(gsym)) commute_3_3(graph, f, g);
        else commute(graph, f, g);
        break;
    delimiter:
        if (fsym == gsym) annihilate_delim_delim(graph, f, g);
        else if (SYMBOL_ROOT == gsym) commute_1_2(graph, g, f);
        else if (SYMBOL_APPLICATOR == gsym) commute_3_2(graph, g, f);
        else if (SYMBOL_LAMBDA == gsym) commute_lambda_delim(graph, g, f);
        else if (SYMBOL_IDENTITY_LAMBDA == gsym) commute_1_2(graph, g, f);
        else if (SYMBOL_GC_LAMBDA == gsym) commute_gc_lambda_delim(graph, g, f);
        else if (SYMBOL_LAMBDA_C == gsym) commute_lambda_c_delim(graph, g, f);
        else if (SYMBOL_CELL == gsym) commute_1_2(graph, g, f);
        else if (SYMBOL_UNARY_CALL == gsym) commute_2_2(graph, g, f);
        else if (SYMBOL_BINARY_CALL == gsym) commute_3_2(graph, g, f);
        else if (SYMBOL_BINARY_CALL_AUX == gsym) commute_2_2(graph, g, f);
        else if (SYMBOL_IF_THEN_ELSE == gsym) commute_4_2(graph, g, f);
        else if (SYMBOL_REFERENCE == gsym) commute_1_2(graph, g, f);
        else if (SYMBOL_BARRIER == gsym) unbarrier(graph, g, f);
        else if (IS_DELIMITER(gsym)) commute_delim_delim(graph, f, g);
        else if (IS_DUPLICATOR(gsym)) commute_dup_delim(graph, g, f);
        else
            commute(graph, g, f); /* delimiters must be the second, unless they
                                     commute with lambdas */
        break;
    case SYMBOL_DELIMITER(UINT64_C(0)):
        if (SYMBOL_BARRIER == gsym) barrier(graph, g, f);
        else goto delimiter;
        break;
    case SYMBOL_ROOT:
        if (IS_DELIMITER(gsym)) commute_1_2(graph, f, g);
        else if (IS_ANY_LAMBDA(gsym) || SYMBOL_CELL == gsym)
            graph->time_to_stop = true;
        else COMPILER_UNREACHABLE();
        break;
    case SYMBOL_APPLICATOR:
        if (SYMBOL_LAMBDA == gsym) beta(graph, f, g);
        else if (SYMBOL_LAMBDA_C == gsym) beta_c(graph, f, g);
        else if (SYMBOL_IDENTITY_LAMBDA == gsym) identity_beta(graph, f, g);
        else if (SYMBOL_GC_LAMBDA == gsym) gc_beta(graph, f, g);
        else if (SYMBOL_REFERENCE == gsym) do_expand(graph, g, f);
        else if (IS_DELIMITER(gsym)) commute_3_2(graph, f, g);
        else if (IS_DUPLICATOR(gsym)) commute_3_3(graph, f, g);
        else commute(graph, f, g);
        break;
    case SYMBOL_UNARY_CALL:
        if (SYMBOL_CELL == gsym) do_unary_call(graph, f, g);
        else if (SYMBOL_REFERENCE == gsym) do_expand(graph, g, f);
        else if (IS_DELIMITER(gsym)) commute_2_2(graph, f, g);
        else if (IS_DUPLICATOR(gsym)) commute_2_3(graph, f, g);
        else commute(graph, f, g);
        break;
    case SYMBOL_BINARY_CALL:
        if (SYMBOL_CELL == gsym) do_binary_call(graph, f, g);
        else if (SYMBOL_REFERENCE == gsym) do_expand(graph, g, f);
        else if (IS_DELIMITER(gsym)) commute_3_2(graph, f, g);
        else if (IS_DUPLICATOR(gsym)) commute_3_3(graph, f, g);
        else commute(graph, f, g);
        break;
    case SYMBOL_BINARY_CALL_AUX:
        if (SYMBOL_CELL == gsym) do_binary_call_aux(graph, f, g);
        else if (SYMBOL_REFERENCE == gsym) do_expand(graph, g, f);
        else if (IS_DELIMITER(gsym)) commute_2_2(graph, f, g);
        else if (IS_DUPLICATOR(gsym)) commute_2_3(graph, f, g);
        else commute(graph, f, g);
        break;
    case SYMBOL_IF_THEN_ELSE:
        if (SYMBOL_CELL == gsym) do_if_then_else(graph, f, g);
        else if (SYMBOL_REFERENCE == gsym) do_expand(graph, g, f);
        else if (IS_DELIMITER(gsym)) commute_4_2(graph, f, g);
        else if (IS_DUPLICATOR(gsym)) commute_4_3(graph, f, g);
        else commute(graph, f, g);
        break;
    case SYMBOL_PERFORM:
        if (SYMBOL_CELL == gsym) do_perform(graph, f, g);
        else if (SYMBOL_REFERENCE == gsym) do_expand(graph, g, f);
        else commute(graph, f, g);
        break;
    case SYMBOL_BARRIER:
        if (SYMBOL_DELIMITER(UINT64_C(0)) == gsym) barrier(graph, f, g);
        else unbarrier(graph, f, g);
        break;
    default:
        if (IS_DUPLICATOR(fsym)) goto duplicator;
        else if (IS_DELIMITER(fsym)) goto delimiter;
        else COMPILER_UNREACHABLE();
    }
}

// Metacircular Interpretation
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

static struct lambda_term *
self_lambda(void) {
    struct lambda_term *body, *my_lambda, *my_apply, *my_var;

    return lambda(
        body,
        lambda(
            my_lambda,
            lambda(
                my_apply, lambda(my_var, apply(var(my_lambda), var(body))))));
}

static struct lambda_term *
self_apply(void) {
    struct lambda_term *rator, *rand, *my_lambda, *my_apply, *my_var;

    return lambda(
        rator,
        lambda(
            rand,
            lambda(
                my_lambda,
                lambda(
                    my_apply,
                    lambda(
                        my_var,
                        apply(apply(var(my_apply), var(rator)), var(rand)))))));
}

static struct lambda_term *
self_var(void) {
    struct lambda_term *lvl, *my_lambda, *my_apply, *my_var;

    return lambda(
        lvl,
        lambda(
            my_lambda,
            lambda(my_apply, lambda(my_var, apply(var(my_var), var(lvl))))));
}

static struct lambda_term *
self_denote(void) {
    struct lambda_term *input, *f, *fx, *rator, *ratorx, *ratorxx, *rand,
        *randx, *v, *vx, *argument;

    // clang-format off
    return lambda(input,
        apply(apply(apply(var(input),
            lambda(f,
                apply(
                    self_lambda(),
                    lambda(argument,
                        apply(expand(self_denote), apply(var(f), var(argument))))))),
            lambda(rator, lambda(rand,
                apply(
                    lambda(
                        ratorx,
                        apply(apply(apply(var(ratorx),
                            lambda(fx, apply(var(fx), var(rand)))),
                            lambda(ratorxx, lambda(randx,
                                apply(
                                    apply(self_apply(), var(ratorx)),
                                    apply(expand(self_denote), var(rand)))))),
                            lambda(vx,
                                apply(
                                    apply(self_apply(), var(ratorx)),
                                    apply(expand(self_denote), var(rand)))))),
                    apply(expand(self_denote), var(rator)))))),
            lambda(v, var(input))));
    // clang-format on
}

static uint64_t
print_string(const uint64_t s, const uint64_t stream) {
    fprintf((FILE *)stream, "%s", (const char *)s);
    return 0;
}

static uint64_t
print_int(const uint64_t n, const uint64_t stream) {
    fprintf((FILE *)stream, "%" PRIu64, n);
    return 0;
}

static uint64_t
plus_one(const uint64_t x) {
    return x + 1;
}

#define PRINT_STRING(s, stream)                                                \
    binary_call(print_string, cell((uint64_t)(s)), (stream))

static struct lambda_term *
self_pp(void) {
    // The stream is passed as an effect token here.
    struct lambda_term *stream, *input, *lvl, *f, *rator, *rand, *v;

    // clang-format off
    return lambda(stream, lambda(input, lambda(lvl,
        apply(apply(apply(var(input),
            lambda(f, perform(
                PRINT_STRING("(λ ", var(stream)),
                perform(
                    apply(apply(apply(expand(self_pp),
                        var(stream)), apply(var(f), apply(self_var(), var(lvl)))), unary_call(plus_one, var(lvl))),
                    PRINT_STRING(")", var(stream)))))),
            lambda(rator, lambda(rand, perform(
                PRINT_STRING("(", var(stream)),
                perform(
                    apply(apply(apply(expand(self_pp), var(stream)), var(rator)), var(lvl)),
                    perform(
                        PRINT_STRING(" ", var(stream)),
                        perform(
                            apply(apply(apply(expand(self_pp), var(stream)), var(rand)), var(lvl)),
                            PRINT_STRING(")", var(stream))))))))),
            lambda(v, binary_call(print_int,
                binary_call(de_bruijn_level_to_index, var(lvl), var(v)),
                var(stream)))))));
    // clang-format on
}

COMPILER_RETURNS_NONNULL COMPILER_NONNULL(1) //
static struct lambda_term *
metacode(struct lambda_term *const restrict term) {
    MY_ASSERT(term);

    switch (term->ty) {
    case LAMBDA_TERM_APPLY: {
        term->data.apply.rator = metacode(term->data.apply.rator);
        term->data.apply.rand = metacode(term->data.apply.rand);
        struct lambda_term *const result = apply(
            apply(expand(self_apply), term->data.apply.rator),
            term->data.apply.rand);
        free(term);
        return result;
    }
    case LAMBDA_TERM_LAMBDA: {
        term->data.lambda->body = metacode(term->data.lambda->body);
        return apply(expand(self_lambda), term);
    }
    case LAMBDA_TERM_VAR: return term;
    default: panic("Non-LC term in metacoding!");
    }
}

// WHNF Reduction
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

COMPILER_WARN_UNUSED_RESULT COMPILER_NONNULL(1, 4) COMPILER_HOT //
static bool
try_extrude_if_delimiter(
    struct context *const restrict graph,
    const struct node f,
    const struct node g,
    uint64_t *const restrict points_to) {
    MY_ASSERT(graph);
    XASSERT(f.ports);
    XASSERT(g.ports);
    MY_ASSERT(points_to);

    if (!IS_DELIMITER(f.ports[-1])) { return false; }

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

COMPILER_NONNULL(1) //
static void
reduce(struct context *const restrict graph) {
    debug("%s()", __func__);

    MY_ASSERT(graph);

    struct multifocus stack = alloc_focus(INITIAL_MULTIFOCUS_CAPACITY);

    struct node f = graph->root;

    while (!graph->time_to_stop) {
        uint64_t *const points_to = DECODE_ADDRESS(f.ports[0]);

        const struct node g = node_of_port(points_to);
        XASSERT(g.ports);

        if (is_interacting_with(f, g)) {
            interact(graph, f, g);
            f = unfocus_or(&stack, graph->root);
            f.ports[0] &= PHASE_MASK;
        } else if (try_extrude_if_delimiter(graph, f, g, points_to)) {
            f = unfocus(&stack);
            f.ports[0] &= PHASE_MASK;
        } else {
            set_phase(&f.ports[0], PHASE_STACK);
            focus_on(&stack, f);
            f = g;
        }
    }

    free_focus(stack);
}

// Complete Algorithm
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

extern uint64_t
optiscope_algorithm(
    FILE *const restrict stream,      // full reduction if not `NULL`
    struct lambda_term *restrict term // must not be `NULL`
) {
    debug("%s()", __func__);

    MY_ASSERT(term);

    struct context *const graph = alloc_context();

    if (stream) {
        term = apply(
            apply(
                apply(expand(self_pp), cell((uint64_t)stream)),
                apply(expand(self_denote), metacode(term))),
            cell(0));
    }
    struct bytecode bc = alloc_bytecode(INITIAL_BYTECODE_CAPACITY);
    emit_bytecode(graph, &bc, term, 0);
    term->connect_to = &graph->root.ports[0];
    execute_bytecode(graph, bc);
    free_bytecode(bc);
    free_lambda_term(term);
    reduce(graph);
    const struct node result = follow_port(&graph->root.ports[0]);
    const uint64_t value =
        SYMBOL_CELL == result.ports[-1] ? result.ports[1] : 0;
    print_stats(graph);
    free_context(graph);
    return value;
}
