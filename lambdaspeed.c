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

// Options:
// `NDEBUG` -- disable a plentitude of assertionnes & enable some compiler
// builtins for optimizationne.
// `LAMBDASPEED_ENABLE_TRACING` -- enable detailed log tracing of the algorithm.
// `LAMBDASPEED_ENABLE_STEP_BY_STEP` -- ask the user for ENTER before each
// interactionne step.
// `LAMBDASPEED_ENABLE_STATS` -- enable run-time statistics (currently, onely
// the total number of interactionnes).
// `LAMBDASPEED_ENABLE_GRAPHVIZ` -- generate `target/state.dot(.svg)`, before
// each interactionne step (requires Graphviz).
// `LAMBDASPEED_ENABLE_GRAPHVIZ_CLUSTERS` -- generate Graphviz "clusters" for
// Beta & commutationne interactionnes.

// Option consistency checks
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#if (                                                                          \
    defined(LAMBDASPEED_ENABLE_GRAPHVIZ) ||                                    \
    defined(LAMBDASPEED_ENABLE_GRAPHVIZ_CLUSTERS)) &&                          \
    defined(NDEBUG)
#error `LAMBDASPEED_ENABLE_GRAPHVIZ` and `LAMBDASPEED_ENABLE_GRAPHVIZ_CLUSTERS` \
are not compatible with `NDEBUG`!
#endif

#if defined(LAMBDASPEED_ENABLE_GRAPHVIZ_CLUSTERS) &&                           \
    !defined(LAMBDASPEED_ENABLE_GRAPHVIZ)
#error Define `LAMBDASPEED_ENABLE_GRAPHVIZ` to use `LAMBDASPEED_ENABLE_GRAPHVIZ_CLUSTERS`!
#endif

#if defined(LAMBDASPEED_ENABLE_STEP_BY_STEP) &&                                \
    !defined(LAMBDASPEED_ENABLE_TRACING)
#error `LAMBDASPEED_ENABLE_STEP_BY_STEP` requires `LAMBDASPEED_ENABLE_TRACING`!
#endif

#if defined(LAMBDASPEED_ENABLE_GRAPHVIZ) && !defined(__GNUC__)
#error You are not eligible for Graphviz visualization.
#endif

// Header inclusionnes
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#define _DEFAULT_SOURCE
#define _POSIX_SOURCE // in case we use glibc

#include <assert.h>
#include <inttypes.h>
#include <limits.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Miscellaneous macros
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#define ARRAY_LENGTH(array) (sizeof(array) / sizeof((array)[0]))

#ifndef NDEBUG
#define CLEAR_MEMORY(object) memset((object), '\0', sizeof *(object))
#else
#define CLEAR_MEMORY(object) /* empty */
#endif

#define ITERATE_ONCE(finish, before, after)                                    \
    for (bool finish = (before, false); !finish; after, finish = true)

#define CAT_PRIMITIVE(a, b) a##b
#define CAT(a, b)           CAT_PRIMITIVE(a, b)

#define STRINGIFY_PRIMITIVE(...) #__VA_ARGS__
#define STRINGIFY(...)           STRINGIFY_PRIMITIVE(__VA_ARGS__)

// Compiler functionalitie detectionne
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#define STANDARD_C99_OR_HIGHER (__STDC_VERSION__ >= 199901L)
#define STANDARD_C11_OR_HIGHER (__STDC_VERSION__ >= 201112L)

#if !STANDARD_C99_OR_HIGHER
#error C99 or higher is required!
#endif

#if defined(__has_feature) // Clang
#if __has_feature(address_sanitizer)
#define COMPILER_ASAN_AVAILABLE
#endif
#elif defined(__SANITIZE_ADDRESS__) // GCC & MSVC
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

// Compiler-specific builtins
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#ifdef __GNUC__

// We generally trust the compiler whether or not to inline a functionne.
// However, we utilize a number of other attributes, to help both the human
// reader & the compiler.
// See <https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html> for
// the list of GNU C functionne attributes.
// Note: please, keep `.clang-format` up-to-date with the macros below.

#define COMPILER_UNUSED             __attribute__((unused))
#define COMPILER_NORETURN           __attribute__((noreturn))
#define COMPILER_COLD               __attribute__((cold))
#define COMPILER_HOT                __attribute__((hot))
#define COMPILER_FLATTEN            __attribute__((flatten))
#define COMPILER_RETURNS_NONNULL    __attribute__((returns_nonnull))
#define COMPILER_WARN_UNUSED_RESULT __attribute__((warn_unused_result))

#ifndef __clang__

#define COMPILER_OPTIMIZE(string) __attribute__((optimize(string)))

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
#define COMPILER_POISON_MEMORY      ASAN_POISON_MEMORY_REGION
#define COMPILER_UNPOISON_MEMORY    ASAN_UNPOISON_MEMORY_REGION
#define COMPILER_IS_POISONED_MEMORY __asan_region_is_poisoned
#endif

#define COMPILER_IGNORE                /* empty, object-like */
#define COMPILER_IGNORE_WITH_ARGS(...) /* empty, functionne-like */

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

#ifndef COMPILER_RETURNS_NONNULL
#define COMPILER_RETURNS_NONNULL COMPILER_IGNORE
#endif

#ifndef COMPILER_WARN_UNUSED_RESULT
#define COMPILER_WARN_UNUSED_RESULT COMPILER_IGNORE
#endif

#ifndef COMPILER_OPTIMIZE
#define COMPILER_OPTIMIZE COMPILER_IGNORE_WITH_ARGS
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

#ifndef COMPILER_IS_POISONED_MEMORY
#define COMPILER_IS_POISONED_MEMORY COMPILER_IGNORE_WITH_ARGS
#endif

// Debug assertionnes
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

// Assertionnes that are checked at programme run-time.
#if defined(__GNUC__) && defined(NDEBUG)
#define XASSERT(condition) (!(condition) ? __builtin_unreachable() : (void)0)
#else
#define XASSERT assert
#endif

// Assertionnes that are checked at compile-time.
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

// File-related I/O
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#define IO_CALL(f, ...) (f(__VA_ARGS__) < 0 ? (perror(#f), abort()) : (void)0)
#define IO_CALL_ASSIGN(x, f, ...)                                              \
    ((x = f(__VA_ARGS__)) < 0 ? (perror(#f), abort()) : (void)0)

COMPILER_NONNULL(1, 2) //
static void
redirect_stream(FILE *const restrict from, FILE *const restrict to) {
    assert(from);
    assert(to);

    int c;
    while (EOF != (c = fgetc(from))) {
        if (EOF == fputc(c, to)) { perror("fputc"), abort(); }
    }

    if (ferror(from) != 0) { perror("fgetc"), abort(); }
}

// Logging & panicking
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#define PRINTER(name, stream, culmination)                                     \
    COMPILER_NONNULL(1) COMPILER_FORMAT(printf, 1, 2) /* */                    \
    static void                                                                \
    name(const char format[const restrict], ...) {                             \
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

#ifdef LAMBDASPEED_ENABLE_TRACING
PRINTER(debug, stdout, /* empty */)
#else
#define debug(...) ((void)0)
#endif

COMPILER_COLD COMPILER_NORETURN //
PRINTER(panic, stderr, abort())

#undef PRINTER

// Ports & symbols functionalitie
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#define MACHINE_WORD_BITS    UINT64_C(64)
#define OFFSET_METADATA_BITS UINT64_C(2)
#define PHASE_METADATA_BITS  UINT64_C(2)
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
     (EFFECTIVE_ADDRESS_BITS + PHASE_METADATA_BITS))

#define ENCODE_ADDRESS(metadata, address)                                      \
    (((address) & ADDRESS_MASK) | (metadata))
#define SIGN_EXTEND(n)                                                         \
    ((uint64_t)((int64_t)((n) << UNUSED_ADDRESS_BITS) >> UNUSED_ADDRESS_BITS))
#define DECODE_ADDRESS(address)                                                \
    ((uint64_t *)(SIGN_EXTEND((address) & ADDRESS_MASK)))
#define DECODE_ADDRESS_METADATA(address) (((address) & ~ADDRESS_MASK))

#define SET_PHASE(address, phase)                                              \
    (((address) & 0xCFFFFFFFFFFFFFFF) |                                        \
     ENCODE_METADATA(UINT64_C(0) /* the principal port */, (phase)))

STATIC_ASSERT(CHAR_BIT == 8, "The byte width must be 8 bits!");
STATIC_ASSERT(
    sizeof(uint64_t *) == sizeof(uint64_t),
    "The machine word width must be 64 bits!");

#define MIN_REGULAR_SYMBOL   UINT64_C(0)
#define MAX_REGULAR_SYMBOL   UINT64_C(5)
#define INDEX_RANGE          UINT64_C(9223372036854775805)
#define MAX_DUPLICATOR_INDEX (MAX_REGULAR_SYMBOL + INDEX_RANGE)
#define MAX_DELIMITER_INDEX  (MAX_DUPLICATOR_INDEX + INDEX_RANGE)
#define MAX_PORTS            UINT64_C(3)
#define MAX_AUXILIARY_PORTS  (MAX_PORTS - 1)

STATIC_ASSERT(
    UINT64_MAX == UINT64_C(18446744073709551615),
    "`uint64_t` must have the expected range of [0; 2^64 - 1]!");
STATIC_ASSERT(
    UINT64_MAX == MAX_DELIMITER_INDEX,
    "Every bit of a symbol must be used!");

#define IS_PRINCIPAL_PORT(port) (0 == DECODE_OFFSET_METADATA(*(port)))

#define SYMBOL_ROOT          UINT64_C(0)
#define SYMBOL_GARBAGE       UINT64_C(1) /* may become usefull later */
#define SYMBOL_APPLICATOR    UINT64_C(2)
#define SYMBOL_LAMBDA        UINT64_C(3)
#define SYMBOL_ERASER        UINT64_C(4)
#define SYMBOL_S             UINT64_C(5)
#define SYMBOL_DUPLICATOR(i) (MAX_REGULAR_SYMBOL + 1 + (i))
#define SYMBOL_DELIMITER(i)  (MAX_DUPLICATOR_INDEX + 1 + (i))

// clang-format off
#define IS_DUPLICATOR(symbol) \
    ((symbol) >= SYMBOL_DUPLICATOR(UINT64_C(0)) && \
     (symbol) <= MAX_DUPLICATOR_INDEX)
#define IS_DELIMITER(symbol) \
    ((symbol) >= SYMBOL_DELIMITER(UINT64_C(0)))
// clang-format on

struct symbol_range {
    uint64_t min, max;
};

COMPILER_CONST COMPILER_WARN_UNUSED_RESULT COMPILER_HOT //
inline static bool
symbol_is_in_range(const struct symbol_range range, const uint64_t symbol) {
    return range.min <= symbol && symbol <= range.max;
}

// clang-format off
#define SYMBOL_RANGE(min, max) ((struct symbol_range){min, max})
#define DUPLICATOR_RANGE \
    SYMBOL_RANGE(SYMBOL_DUPLICATOR(UINT64_C(0)), MAX_DUPLICATOR_INDEX)
#define DELIMITER_RANGE \
    SYMBOL_RANGE(SYMBOL_DELIMITER(UINT64_C(0)), MAX_DELIMITER_INDEX)
// clang-format on

#define FOR_ALL_PORTS(node, i, seed)                                           \
    for (uint8_t i = seed; i < ports_count((node).ports[-1]); i++)

COMPILER_CONST COMPILER_WARN_UNUSED_RESULT COMPILER_HOT //
static uint8_t
ports_count(const uint64_t symbol) {
    switch (symbol) {
    case SYMBOL_ROOT: return 2;
    case SYMBOL_APPLICATOR:
    case SYMBOL_LAMBDA: return 3;
    case SYMBOL_ERASER: return 1;
    case SYMBOL_S: return 2;
    default:
        if (symbol <= MAX_DUPLICATOR_INDEX) goto duplicator;
        else if (symbol <= MAX_DELIMITER_INDEX) goto delimiter;
        else COMPILER_UNREACHABLE();
    duplicator:
        return 3;
    delimiter:
        return 2;
    }
}

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT COMPILER_RETURNS_NONNULL
COMPILER_NONNULL(1) //
inline static uint64_t *
get_principal_port(uint64_t *const restrict port) {
    assert(port);

    return (port - DECODE_OFFSET_METADATA(port[0]));
}

COMPILER_NONNULL(1, 2) COMPILER_HOT //
static void
connect_port_to(
    uint64_t *const restrict port,
    const uint64_t *const restrict another) {
    assert(port);
    assert(another);
    XASSERT(port != another);
    XASSERT(DECODE_ADDRESS(*port) != another);

    const uint64_t port_metadata = DECODE_ADDRESS_METADATA(*port);

    *port = ENCODE_ADDRESS(port_metadata, (uint64_t)another);

    XASSERT(DECODE_ADDRESS(*port) == another);
    XASSERT(DECODE_ADDRESS_METADATA(*port) == port_metadata);
}

COMPILER_NONNULL(1, 2) COMPILER_HOT COMPILER_FLATTEN //
static void
connect_ports(uint64_t *const restrict lhs, uint64_t *const restrict rhs) {
    debug("%s(%p, %p)", __func__, (void *)lhs, (void *)rhs);

    // Delegate the assertionnes to `connect_ports_to`.
    assert(true);

    connect_port_to(lhs, rhs), connect_port_to(rhs, lhs);
}

COMPILER_CONST COMPILER_WARN_UNUSED_RESULT COMPILER_HOT //
static int64_t
symbol_index(const uint64_t symbol) {
    STATIC_ASSERT(
        INDEX_RANGE <= (uint64_t)INT64_MAX, "Indices must fit in `int64_t`!");

    switch (symbol) {
    case SYMBOL_ROOT:
    case SYMBOL_GARBAGE:
    case SYMBOL_APPLICATOR:
    case SYMBOL_LAMBDA:
    case SYMBOL_ERASER:
    case SYMBOL_S: return -1;
    default:
        if (symbol <= MAX_DUPLICATOR_INDEX) goto duplicator;
        else if (symbol <= MAX_DELIMITER_INDEX) goto delimiter;
        else COMPILER_UNREACHABLE();
    duplicator:
        return (int64_t)(symbol - MAX_REGULAR_SYMBOL - 1);
    delimiter:
        return (int64_t)(symbol - MAX_DUPLICATOR_INDEX - 1);
    }
}

COMPILER_CONST COMPILER_WARN_UNUSED_RESULT //
inline static bool
is_canonical_symbol(const uint64_t symbol) {
    if (IS_DUPLICATOR(symbol) || IS_DELIMITER(symbol)) {
        return 0 == symbol_index(symbol);
    }

    return true;
}

#define MAX_SSYMBOL_SIZE 64

COMPILER_CONST COMPILER_WARN_UNUSED_RESULT COMPILER_RETURNS_NONNULL //
static const char *
print_symbol(const uint64_t symbol) {
    static char buffer[MAX_SSYMBOL_SIZE] = {0};

    switch (symbol) {
    case SYMBOL_ROOT: sprintf(buffer, "root"); break;
    case SYMBOL_GARBAGE: sprintf(buffer, "garbage"); break;
    case SYMBOL_APPLICATOR: sprintf(buffer, "@"); break;
    case SYMBOL_LAMBDA: sprintf(buffer, "λ"); break;
    case SYMBOL_ERASER: sprintf(buffer, "◉"); break;
    case SYMBOL_S: sprintf(buffer, "S"); break;
    default:
        if (symbol <= MAX_DUPLICATOR_INDEX) goto duplicator;
        else if (symbol <= MAX_DELIMITER_INDEX) goto delimiter;
        else COMPILER_UNREACHABLE();
    duplicator:
        sprintf(buffer, "▽/%" PRIi64, symbol_index(symbol));
        break;
    delimiter:
        sprintf(buffer, "⌒/%" PRIi64, symbol_index(symbol));
        break;
    }

    return buffer;
}

COMPILER_WARN_UNUSED_RESULT //
static uint64_t
bump_index(const uint64_t symbol) {
    if (MAX_DELIMITER_INDEX == symbol || MAX_DUPLICATOR_INDEX == symbol) {
        panic("Maximum index of %" PRIu64 " is reached!", INDEX_RANGE);
    } else if (symbol > MAX_REGULAR_SYMBOL) {
        return symbol + 1;
    } else {
        panic(
            "The symbol `%s` has no index to bump!", //
            print_symbol(symbol));
    }
}

// O(1) pool allocationne & deallocationne
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

#define POOL_CHUNK_LIST_SIZE 1024 /* for all types of objects */

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
    static void prefix##_pool_close(                                           \
        struct prefix##_pool *const restrict self);                            \
                                                                               \
    COMPILER_MALLOC(prefix##_pool_close, 1) COMPILER_RETURNS_NONNULL           \
    COMPILER_WARN_UNUSED_RESULT COMPILER_COLD /* */                            \
    static struct prefix##_pool *prefix##_pool_create(void) {                  \
        struct prefix##_pool *const self = xmalloc(sizeof *self);              \
                                                                               \
        union prefix##_chunk *chunks =                                         \
            xmalloc(POOL_CHUNK_LIST_SIZE * sizeof chunks[0]);                  \
        for (size_t i = 0; i < POOL_CHUNK_LIST_SIZE - 1; i++) {                \
            chunks[i].next = &chunks[i + 1];                                   \
        }                                                                      \
        chunks[POOL_CHUNK_LIST_SIZE - 1].next = NULL;                          \
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
    static void prefix##_pool_close(                                           \
        struct prefix##_pool *const restrict self) {                           \
        assert(self);                                                          \
        XASSERT(self->buckets);                                                \
                                                                               \
        struct prefix##_chunks_bucket *iter = self->buckets;                   \
        while (iter) {                                                         \
            struct prefix##_chunks_bucket *next = iter->next;                  \
            XASSERT(iter->chunks);                                             \
            free(iter->chunks);                                                \
            free(iter);                                                        \
            iter = next;                                                       \
        }                                                                      \
                                                                               \
        free(self);                                                            \
    }                                                                          \
                                                                               \
    COMPILER_NONNULL(1) COMPILER_COLD /* */                                    \
    static void prefix##_pool_expand(                                          \
        struct prefix##_pool *const restrict self) {                           \
        assert(self);                                                          \
        XASSERT(self->buckets);                                                \
                                                                               \
        union prefix##_chunk *const extra_chunks =                             \
            xmalloc(POOL_CHUNK_LIST_SIZE * sizeof extra_chunks[0]);            \
        for (size_t i = 0; i < POOL_CHUNK_LIST_SIZE - 1; i++) {                \
            extra_chunks[i].next = &extra_chunks[i + 1];                       \
        }                                                                      \
        extra_chunks[POOL_CHUNK_LIST_SIZE - 1].next = self->next_free_chunk;   \
        self->next_free_chunk = extra_chunks;                                  \
                                                                               \
        struct prefix##_chunks_bucket *const buckets =                         \
            xmalloc(sizeof buckets[0]);                                        \
        buckets->chunks = extra_chunks, buckets->next = self->buckets;         \
        self->buckets = buckets;                                               \
    }                                                                          \
                                                                               \
    COMPILER_NONNULL(1, 2) COMPILER_HOT /* */                                  \
    static void prefix##_pool_free(                                            \
        struct prefix##_pool *const restrict self, uint64_t *restrict object); \
                                                                               \
    COMPILER_MALLOC(prefix##_pool_free, 1) COMPILER_RETURNS_NONNULL            \
    COMPILER_WARN_UNUSED_RESULT COMPILER_NONNULL(1) COMPILER_HOT /* */         \
    static uint64_t *prefix##_pool_alloc(                                      \
        struct prefix##_pool *const restrict self) {                           \
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
    static void prefix##_pool_free(                                            \
        struct prefix##_pool *const restrict self,                             \
        uint64_t *restrict object) {                                           \
        assert(self);                                                          \
        XASSERT(self->buckets);                                                \
        assert(object);                                                        \
                                                                               \
        object--; /* returne to the symbol addresse */                         \
        union prefix##_chunk *const freed = (union prefix##_chunk *)object;    \
        CLEAR_MEMORY(freed);                                                   \
        freed->next = self->next_free_chunk;                                   \
        self->next_free_chunk = freed;                                         \
        COMPILER_POISON_MEMORY(freed, chunk_size);                             \
    }

POOL_ALLOCATOR(applicator, sizeof(uint64_t) * 4)
POOL_ALLOCATOR(lambda, sizeof(uint64_t) * 4)
POOL_ALLOCATOR(eraser, sizeof(uint64_t) * 2)
POOL_ALLOCATOR(scope, sizeof(uint64_t) * 3)
POOL_ALLOCATOR(duplicator, sizeof(uint64_t) * 4)
POOL_ALLOCATOR(delimiter, sizeof(uint64_t) * 3)

#define POOLS                                                                  \
    X(applicator_pool)                                                         \
    X(lambda_pool)                                                             \
    X(eraser_pool)                                                             \
    X(scope_pool)                                                              \
    X(duplicator_pool)                                                         \
    X(delimiter_pool)

// clang-format off

#define X(pool_name) static struct pool_name *pool_name = NULL;
POOLS
#undef X

#define X(pool_name) \
    { \
        XASSERT(NULL == pool_name); \
        pool_name = pool_name##_create(); \
        XASSERT(pool_name); \
    }

static void open_pools(void) { POOLS }

#undef X

#define X(pool_name) \
    { \
        XASSERT(pool_name); \
        pool_name##_close(pool_name); \
        pool_name = NULL; \
    }

static void close_pools(void) { POOLS }

#undef X

// clang-format on

#undef POOLS

#undef POOL_ALLOCATOR
#undef POOL_CHUNK_LIST_SIZE

// Nodes functionalitie
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

struct node {
    uint64_t *ports;
};

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT COMPILER_NONNULL(1) COMPILER_HOT
COMPILER_FLATTEN //
inline static struct node
node_of_port(uint64_t *const restrict port) {
    assert(port);

    const struct node node = {get_principal_port(port)};
    XASSERT(node.ports);

    return node;
}

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT COMPILER_NONNULL(1) COMPILER_HOT
COMPILER_FLATTEN //
inline static struct node
follow_port(uint64_t *const restrict port) {
    assert(port);

    return node_of_port(DECODE_ADDRESS(*port));
}

COMPILER_WARN_UNUSED_RESULT COMPILER_COLD //
inline static struct node
xmalloc_node(const uint64_t symbol, const uint64_t phase) {
    const uint64_t n = ports_count(symbol);

    uint64_t *ports = xmalloc(sizeof *ports * (n + 1));
    ports++;
    ports[-1] = symbol;

    for (uint64_t offset = 0; offset < n; offset++) {
        ports[offset] =
            ENCODE_ADDRESS(ENCODE_METADATA(offset, phase), UINT64_C(0));
    }

    return (struct node){ports};
}

#ifdef LAMBDASPEED_ENABLE_TRACING

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
        sprintf(
            buffer,
            "%s [%p, %p, %p]",
            ssymbol,
            (void *)&p[0],
            (void *)&p[1],
            (void *)&p[2]);
        break;
    default: COMPILER_UNREACHABLE();
    }

    return buffer;
}

#endif // LAMBDASPEED_ENABLE_TRACING

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT COMPILER_HOT //
inline static bool
is_interaction(const struct node f, const struct node g) {
    XASSERT(f.ports), XASSERT(g.ports);

    return DECODE_ADDRESS(f.ports[0]) == &g.ports[0] &&
           DECODE_ADDRESS(g.ports[0]) == &f.ports[0];
}

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT COMPILER_HOT //
inline static bool
is_interacting_with(const struct node f, const struct node g) {
    XASSERT(f.ports), XASSERT(g.ports);

    // Supposing that `g` is deriued from `f` by `follow_port(&f.ports[0])`.
    return DECODE_ADDRESS(g.ports[0]) == &f.ports[0];
}

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT //
inline static bool
is_active(const struct node node) {
    XASSERT(node.ports);

    return is_interacting_with(node, follow_port(&node.ports[0]));
}

COMPILER_HOT //
static void
free_node(const struct node node);

// A linked list of nodes
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

struct node_list {
    struct node node;
    struct node_list *cons;
};

#define ITERATE_LIST(iter, seed)                                               \
    for (struct node_list *iter = seed; iter; iter = iter->cons)

#define CONSUME_LIST(iter, seed)                                               \
    for (struct node_list *iter = seed, *cons = NULL; iter;                    \
         cons = iter->cons, (free(iter), iter = cons))

COMPILER_WARN_UNUSED_RESULT COMPILER_RETURNS_NONNULL //
static struct node_list *
visit(struct node_list *const restrict self, const struct node node) {
    XASSERT(node.ports);

    struct node_list *const cons = xmalloc(sizeof *cons);
    cons->node = node;
    cons->cons = self;

    return cons;
}

COMPILER_NONNULL(1) //
static struct node
unvisit(struct node_list **const restrict self) {
    assert(self);
    XASSERT(*self);

    const struct node node = (*self)->node;
    struct node_list *const cons = (*self)->cons;

    free(*self), *self = cons;

    return node;
}

#ifdef LAMBDASPEED_ENABLE_GRAPHVIZ

COMPILER_WARN_UNUSED_RESULT //
static struct node_list *
unvisit_all(struct node_list *const restrict self) {
    CONSUME_LIST (iter, self) {}
    return NULL;
}

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT //
static bool
is_visited(
    const struct node_list *const restrict self,
    const struct node node) {
    XASSERT(node.ports);

    ITERATE_LIST (iter, (struct node_list *)self) {
        if (iter->node.ports == node.ports) { return true; }
    }

    return false;
}

#define GUARD_NODE(history, node /* parameter */)                              \
    do {                                                                       \
        if (is_visited((history), node)) { return; }                           \
        (history) = visit((history), node);                                    \
    } while (false)

#endif // LAMBDASPEED_ENABLE_GRAPHVIZ

// Graphs (nets) of nodes
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

struct focus {
    struct node initial[8192];
    struct node_list *rest;
    size_t count;
};

COMPILER_NONNULL(1) COMPILER_HOT COMPILER_FLATTEN //
static void
focus_on(struct focus *const restrict focus, const struct node node) {
    assert(focus);
    XASSERT(node.ports);

    if (SIZE_MAX == focus->count) { panic("The focus is full!"); }

    if (focus->count < ARRAY_LENGTH(focus->initial)) {
        focus->initial[focus->count] = node;
    } else {
        focus->rest = visit(focus->rest, node);
    }

    focus->count++;
}

COMPILER_NONNULL(1) COMPILER_HOT COMPILER_FLATTEN //
static struct node
unfocus(struct focus *const restrict focus) {
    assert(focus);
    XASSERT(focus->count > 0);

    focus->count--;
    return focus->count < ARRAY_LENGTH(focus->initial)
               ? focus->initial[focus->count]
               : unvisit(&focus->rest);
}

#define CONSUME_FOCUS(focus, f)                                                \
    for (struct node f = (focus)->initial[0]; (focus)->count > 0;              \
         f = unfocus(focus))

struct node_graph {
    const struct node root;
    struct focus *annihilations, *commutations, *betas;
    uint64_t current_phase;
    bool is_reading_back;

#ifdef LAMBDASPEED_ENABLE_STATS
    uint64_t nannihilations, ncommutations, nbetas;
#endif
};

COMPILER_PURE COMPILER_NONNULL(1) COMPILER_HOT //
inline static bool
is_normalized_graph(const struct node_graph *const restrict graph) {
    assert(graph);

    return 0 == graph->betas->count &&         //
           0 == graph->annihilations->count && //
           0 == graph->commutations->count;
}

COMPILER_NONNULL(1) COMPILER_COLD //
static void
free_graph(struct node_graph *const restrict graph) {
    debug("%s()", __func__);

    assert(graph);
    XASSERT(graph->root.ports);
    XASSERT(graph->annihilations), XASSERT(0 == graph->annihilations->count);
    XASSERT(graph->commutations), XASSERT(0 == graph->commutations->count);
    XASSERT(graph->betas), XASSERT(0 == graph->betas->count);
    XASSERT(!graph->is_reading_back);

    free(DECODE_ADDRESS(graph->root.ports[0]) - 1 /* back to the symbol */);
    free(graph->root.ports - 1 /* back to the symbol */);

    free(graph->annihilations);
    free(graph->commutations);
    free(graph->betas);
}

COMPILER_WARN_UNUSED_RESULT COMPILER_NONNULL(1) COMPILER_HOT //
static struct node
alloc_node(struct node_graph *const restrict graph, const uint64_t symbol) {
    assert(graph);
    XASSERT(SYMBOL_ROOT != symbol);

    // While it might seem that preallocationne caches can increase performance,
    // in fact, they introduced almost a 2x slowdown.

    uint64_t *ports = NULL;
    switch (symbol) {
    case SYMBOL_APPLICATOR:
        ports = applicator_pool_alloc(applicator_pool);
        goto assign_port_2;
    case SYMBOL_LAMBDA:
        ports = lambda_pool_alloc(lambda_pool);
        goto assign_port_2;
    case SYMBOL_ERASER:
        ports = eraser_pool_alloc(eraser_pool);
        goto assign_port_0;
    case SYMBOL_S:
        ports = scope_pool_alloc(scope_pool); //
        goto assign_port_1;
    default:
        if (symbol <= MAX_DUPLICATOR_INDEX) goto duplicator;
        else if (symbol <= MAX_DELIMITER_INDEX) goto delimiter;
        else COMPILER_UNREACHABLE();
    duplicator:
        ports = duplicator_pool_alloc(duplicator_pool);
        goto assign_port_2;
    delimiter:
        ports = delimiter_pool_alloc(delimiter_pool);
        goto assign_port_1;
    }

    COMPILER_UNREACHABLE();

#define PORT_VALUE(offset)                                                     \
    ENCODE_ADDRESS(                                                            \
        ENCODE_METADATA(UINT64_C(offset), PHASE_VALUE(offset)), UINT64_C(0))
#define PHASE_VALUE(offset)                                                    \
    (0 == (offset) ? graph->current_phase /* the principal port */             \
                   : UINT64_C(0) /* a non-principal port */)

    // clang-format off
assign_port_2: ports[2] = PORT_VALUE(2);
assign_port_1: ports[1] = PORT_VALUE(1);
assign_port_0: ports[0] = PORT_VALUE(0);
    // clang-format on

#undef PHASE_VALUE
#undef PORT_VALUE

    ports[-1] = symbol;

    debug("%s(%s)", __func__, print_node((struct node){ports}));

    return (struct node){ports};
}

COMPILER_HOT //
static void
free_node(const struct node node) {
    debug("%s(%p)", __func__, (void *)node.ports);

    XASSERT(node.ports);

    const uint64_t symbol = node.ports[-1];
    XASSERT(SYMBOL_ROOT != symbol);
    XASSERT(SYMBOL_GARBAGE != symbol);

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
    case SYMBOL_APPLICATOR: applicator_pool_free(applicator_pool, p); break;
    case SYMBOL_LAMBDA: lambda_pool_free(lambda_pool, p); break;
    case SYMBOL_ERASER: eraser_pool_free(eraser_pool, p); break;
    case SYMBOL_S: scope_pool_free(scope_pool, p); break;
    default:
        if (symbol <= MAX_DUPLICATOR_INDEX) goto duplicator;
        else if (symbol <= MAX_DELIMITER_INDEX) goto delimiter;
        else COMPILER_UNREACHABLE();
    duplicator:
        duplicator_pool_free(duplicator_pool, p);
        break;
    delimiter:
        delimiter_pool_free(delimiter_pool, p);
        break;
    }
}

COMPILER_NONNULL(1) COMPILER_HOT //
static void
register_active_pair(
    struct node_graph *const restrict graph,
    const struct node f,
    const struct node g) {
    assert(graph);
    XASSERT(f.ports), XASSERT(g.ports);
    assert(is_interaction(f, g));

    const uint64_t f_symbol = f.ports[-1], g_symbol = g.ports[-1];

    if (f_symbol == SYMBOL_APPLICATOR && g_symbol == SYMBOL_LAMBDA) {
        focus_on(graph->betas, f);
    } else if (g_symbol == SYMBOL_APPLICATOR && f_symbol == SYMBOL_LAMBDA) {
        focus_on(graph->betas, g);
    } else if (f_symbol == g_symbol) {
        focus_on(graph->annihilations, f);
    } else {
        focus_on(graph->commutations, f);
    }
}

COMPILER_NONNULL(1) COMPILER_HOT //
inline static void
register_pair_if_active(
    struct node_graph *const restrict graph,
    const struct node f,
    const struct node g) {
    assert(graph);
    XASSERT(f.ports), XASSERT(g.ports);

    if (is_interaction(f, g)) { register_active_pair(graph, f, g); }
}

COMPILER_NONNULL(1) COMPILER_HOT //
static void
register_node_if_active(
    struct node_graph *const restrict graph,
    const struct node node) {
    assert(graph);
    XASSERT(node.ports);

    const struct node f = node, g = follow_port(&node.ports[0]);

    if (DECODE_ADDRESS(g.ports[0]) != &f.ports[0]) { return; }

    register_active_pair(graph, f, g);
}

// Graphviz graph generationne
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#ifdef LAMBDASPEED_ENABLE_GRAPHVIZ

// We use glibc-specific functionalitie for convenience. Windows support is not
// & will neuer be planned.
STATIC_ASSERT(__GNUC__ >= 1, "GNU C is required!");

#define GRAPHVIZ_INDENT             "    "
#define GRAPHVIZ_INDENT_2X          GRAPHVIZ_INDENT GRAPHVIZ_INDENT
#define GRAPHVIZ_ADDRESS_WIDTH_LINE "+----------------+"
#define GRAPHVIZ_ACTIVE_COLOR       "darkgreen"
#define GRAPHVIZ_STYLE_PLACEHOLDER  "// xxxxxxxxx"
#define GRAPHVIZ_HIDE               "style=invis;"

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT COMPILER_RETURNS_NONNULL //
static const char *
graphviz_node_xlabel(const struct node node) {
    XASSERT(node.ports);

    static char buffer[256] = {0};

    const uint64_t *const p = node.ports;

    switch (ports_count(p[-1])) {
    case 1:
        sprintf(
            buffer,
            GRAPHVIZ_ADDRESS_WIDTH_LINE "<BR/>| %p |<BR/>" //
            GRAPHVIZ_ADDRESS_WIDTH_LINE,
            (void *)&p[0]);
        break;
    case 2:
        sprintf(
            buffer,
            GRAPHVIZ_ADDRESS_WIDTH_LINE "<BR/>| %p |<BR/>| %p |<BR/>" //
            GRAPHVIZ_ADDRESS_WIDTH_LINE,
            (void *)&p[0],
            (void *)&p[1]);
        break;
    case 3:
        sprintf(
            buffer,
            GRAPHVIZ_ADDRESS_WIDTH_LINE
            "<BR/>| %p |<BR/>| %p |<BR/>| %p |<BR/>" //
            GRAPHVIZ_ADDRESS_WIDTH_LINE,
            (void *)&p[0],
            (void *)&p[1],
            (void *)&p[2]);
        break;
    default: COMPILER_UNREACHABLE();
    }

    return buffer;
}

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT COMPILER_RETURNS_NONNULL //
static const char *
graphviz_edge_label(
    const struct node node,
    const uint8_t i,
    const bool is_reading_back) {
    XASSERT(node.ports);

    static char buffer[16] = {0};

    switch (node.ports[-1]) {
    case SYMBOL_APPLICATOR:
        if ((is_reading_back ? 0 : 1) == i) {
            sprintf(buffer, "\\#%" PRIu8, i);
        } else if ((is_reading_back ? 2 : 0) == i) {
            sprintf(buffer, "rator (\\#%" PRIu8 ")", i);
        } else if ((is_reading_back ? 1 : 2) == i) {
            sprintf(buffer, "rand (\\#%" PRIu8 ")", i);
        } else {
            COMPILER_UNREACHABLE();
        }
        break;
    case SYMBOL_LAMBDA:
        switch (i) {
        case 0: sprintf(buffer, "\\#%" PRIu8, i); break;
        case 1: sprintf(buffer, "binder (\\#%" PRIu8 ")", i); break;
        case 2: sprintf(buffer, "body (\\#%" PRIu8 ")", i); break;
        default: COMPILER_UNREACHABLE();
        }
        break;
    default: sprintf(buffer, "\\#%" PRIu8, i);
    }

    return buffer;
}

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT //
static uint8_t
graphviz_edge_weight(
    const struct node node,
    const uint8_t i,
    const bool is_reading_back) {
    if (is_active(node) && 0 == i) { return 3; }

    switch (node.ports[-1]) {
    case SYMBOL_APPLICATOR:
        if ((is_reading_back ? 2 : 0) == i) {
            return 5; // rator
        }
        if ((is_reading_back ? 1 : 2) == i) {
            return 5; // rand
        }
        break;
    case SYMBOL_LAMBDA:
        if (2 == i) {
            return 3; // body
        }
        break;
    default:
        if (IS_DUPLICATOR(node.ports[-1]) && (1 == i || 2 == i)) {
            return 5; // the auxiliary ports
        }
    }

    // The Graphviz default value.
    return 1;
}

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT COMPILER_RETURNS_NONNULL //
static const char *
graphviz_edge_tailport(
    const struct node node,
    const uint8_t i,
    const bool is_reading_back) {
    XASSERT(node.ports);

    static char buffer[4] = {0};

    switch (node.ports[-1]) {
    case SYMBOL_ROOT:
        switch (i) {
        case 0: sprintf(buffer, "n"); goto exit;
        case 1: sprintf(buffer, "s"); goto exit;
        default: COMPILER_UNREACHABLE();
        }
        goto exit;
    case SYMBOL_APPLICATOR:
        if ((is_reading_back ? 0 : 1) == i) {
            sprintf(buffer, "n");
        } else if ((is_reading_back ? 2 : 0) == i) {
            sprintf(buffer, "s");
        } else if ((is_reading_back ? 1 : 2) == i) {
            sprintf(buffer, "e");
        } else {
            COMPILER_UNREACHABLE();
        }
        goto exit;
    case SYMBOL_LAMBDA:
        switch (i) {
        case 0: sprintf(buffer, "n"); goto exit;
        case 1: sprintf(buffer, "e"); goto exit;
        case 2: sprintf(buffer, "s"); goto exit;
        default: COMPILER_UNREACHABLE();
        }
        goto exit;
    case SYMBOL_ERASER:
        switch (i) {
        case 0: sprintf(buffer, "s"); goto exit;
        default: COMPILER_UNREACHABLE();
        }
        goto exit;
    case SYMBOL_S:
        switch (i) {
        case 0: sprintf(buffer, "n"); goto exit;
        case 1: sprintf(buffer, "s"); goto exit;
        default: COMPILER_UNREACHABLE();
        }
        goto exit;
    default: break;
    }

    if (node.ports[-1] <= MAX_DUPLICATOR_INDEX) {
        switch (i) {
        case 0: sprintf(buffer, "s"); goto exit;
        case 1: sprintf(buffer, "nw"); goto exit;
        case 2: sprintf(buffer, "ne"); goto exit;
        default: COMPILER_UNREACHABLE();
        }
    }
    if (node.ports[-1] <= MAX_DELIMITER_INDEX) {
        switch (i) {
        case 0: sprintf(buffer, "s"); goto exit;
        case 1: sprintf(buffer, "n"); goto exit;
        default: COMPILER_UNREACHABLE();
        }
    }

    COMPILER_UNREACHABLE();

exit:
    return buffer;
}

#ifdef LAMBDASPEED_ENABLE_GRAPHVIZ_CLUSTERS

#define GRAPHVIZ_BEGIN_CLUSTER "// begin cluster"
#define GRAPHVIZ_END_CLUSTER   "// end cluster"

// See <https://graphviz.org/Gallery/directed/cluster.html>.
static FILE *graphviz_footer_fp = NULL;

static uint16_t
graphviz_cluster_counter(void) {
    static uint16_t counter = 0;

    if (UINT16_MAX == counter) { panic("Graphviz cluster counter overflow!"); }
    return counter++;
}

COMPILER_NONNULL(1, 2) //
static void
graphviz_commute_cluster(
    const struct node f_updates[const restrict],
    const struct node g_updates[const restrict],
    const uint8_t m,
    const uint8_t n) {
    assert(f_updates), assert(g_updates);

    char connections[1024] = {0}, top_ranks[256] = {0}, bottom_ranks[256] = {0};

    // Initialize the (invisible) connectionnes between the nodes.
    for (uint8_t i = 0; i < m; i++) {
        for (uint8_t j = 0; j < n; j++) {
            const void *const ports[] = {
                (void *)f_updates[i].ports, (void *)g_updates[j].ports};

            for (uint8_t k = 0; k < ARRAY_LENGTH(ports); k++) {
                sprintf(
                    connections + strlen(connections),
                    GRAPHVIZ_INDENT_2X "n%p -> n%p [style=invis];\n",
                    ports[k],
                    ports[(k + 1) % ARRAY_LENGTH(ports)]);
            }
        }
    }

    // Initialize the top & bottom node ranks.
    for (uint8_t i = 0; i < m; i++) {
        sprintf(
            top_ranks + strlen(top_ranks), "; n%p", (void *)f_updates[i].ports);
    }
    for (uint8_t i = 0; i < n; i++) {
        sprintf(
            bottom_ranks + strlen(bottom_ranks),
            "; n%p",
            (void *)g_updates[i].ports);
    }

    // clang-format off
    fprintf(
        graphviz_footer_fp,
        "\n" GRAPHVIZ_INDENT "subgraph cluster_%" PRIu16 " {\n"
        GRAPHVIZ_INDENT_2X GRAPHVIZ_BEGIN_CLUSTER "\n"
                         /* style=invis; */
        GRAPHVIZ_INDENT_2X GRAPHVIZ_STYLE_PLACEHOLDER "\n"
        GRAPHVIZ_INDENT_2X "label=\"commute\";\n"
        GRAPHVIZ_INDENT_2X "color=darkblue;\n"
        GRAPHVIZ_INDENT_2X "margin=16;\n"
        "%s" // node connectionnes
        GRAPHVIZ_INDENT_2X "{rank=same%s}\n"
        GRAPHVIZ_INDENT_2X "{rank=same%s}\n"
        GRAPHVIZ_INDENT_2X GRAPHVIZ_END_CLUSTER "\n"
        GRAPHVIZ_INDENT "}\n",
        graphviz_cluster_counter(),
        connections,
        top_ranks,
        bottom_ranks);
    // clang-format on
}

COMPILER_NONNULL(1, 2) //
static void
graphviz_beta_cluster(
    const uint64_t *const restrict lhs,
    const uint64_t *const restrict rhs) {
    assert(lhs), assert(rhs);

    // clang-format off
    fprintf(
        graphviz_footer_fp,
        "\n" GRAPHVIZ_INDENT "subgraph cluster_%" PRIu16 " {\n"
        GRAPHVIZ_INDENT_2X GRAPHVIZ_BEGIN_CLUSTER "\n"
                         /* style=invis; */
        GRAPHVIZ_INDENT_2X GRAPHVIZ_STYLE_PLACEHOLDER "\n"
        GRAPHVIZ_INDENT_2X "label=\"Beta\";\n"
        GRAPHVIZ_INDENT_2X "color=darkblue;\n"
        GRAPHVIZ_INDENT_2X "margin=16;\n"
        GRAPHVIZ_INDENT_2X "n%p -> n%p [style=invis];\n"
        GRAPHVIZ_INDENT_2X "{rank=same; n%p; n%p}\n"
        GRAPHVIZ_INDENT_2X GRAPHVIZ_END_CLUSTER "\n"
        GRAPHVIZ_INDENT "}\n",
        graphviz_cluster_counter(),
        (void *)lhs,
        (void *)rhs,
        (void *)lhs,
        (void *)rhs);
    // clang-format on
}

#define DEFINED_graphviz_commute_cluster graphviz_commute_cluster
#define DEFINED_graphviz_beta_cluster    graphviz_beta_cluster

COMPILER_NONNULL(1, 2) //
static void *
mmap_graphviz_footer(
    size_t *const restrict mmap_length,
    char *const restrict mmap_backup_char) {
    assert(mmap_length), assert(mmap_backup_char);

    IO_CALL(fflush, graphviz_footer_fp);

    long length = 0;
    IO_CALL_ASSIGN(length, ftell, graphviz_footer_fp);
    if (0 == length) { return NULL; }

    const int fd = fileno(graphviz_footer_fp);

    char *ptr =
        mmap(NULL, (size_t)length, MAP_PRIVATE, PROT_READ | PROT_WRITE, fd, 0);
    if (MAP_FAILED == ptr) { perror("mmap"), abort(); }

    *mmap_length = (size_t)length;
    *mmap_backup_char = ptr[length - 1];
    ptr[length - 1] = '\0';

    return (void *)ptr;
}

// Mark clusters consisting of onely invisible nodes invisible as well.
static void
postprocess_graphviz_footer(void) {
    char *ptr = NULL, *ptrx = NULL;
    size_t length = 0;
    char clast = '\0';
    ptr = ptrx = mmap_graphviz_footer(&length, &clast);
    if (NULL == ptr) { return; }

    while ((ptr = strstr(ptr, GRAPHVIZ_BEGIN_CLUSTER))) {
        char *const end = strstr(ptr, GRAPHVIZ_END_CLUSTER);
        XASSERT(end);

        const char backup_char = *end;
        *end = '\0';
        if (NULL != strstr(ptr, "n0x")) { goto transition; }

        char *const placeholder = strstr(ptr, GRAPHVIZ_STYLE_PLACEHOLDER);
        if (NULL == placeholder) { goto transition; }

        memcpy(placeholder, GRAPHVIZ_HIDE, strlen(GRAPHVIZ_HIDE));

    transition:
        *end = backup_char;
        ptr = end + strlen(GRAPHVIZ_END_CLUSTER);
    }

    ptrx[length - 1] = clast, IO_CALL(munmap, ptrx, length);
    IO_CALL(fflush, graphviz_footer_fp);
}

static uint16_t
graphviz_hide_counter(void) {
    static uint16_t counter = 0;

    if (UINT16_MAX == counter) { panic("Graphviz hide counter overflow!"); }
    return counter++;
}

// Replace the node with an invisible one in the Graphviz clusters.
static void
clear_graphviz_cluster_node(const struct node node) {
    const size_t saddress_length = strlen("n0x000000000000");
    char saddress[32] = {0}, hide_saddress[32] = {0};

    XASSERT(sizeof(saddress) - 1 >= saddress_length);
    XASSERT(sizeof(hide_saddress) - 1 >= saddress_length);

    sprintf(saddress, "n%p", (void *)node.ports);

    char *ptr = NULL, *ptrx = NULL;
    size_t length = 0;
    char clast = '\0';
    ptr = ptrx = mmap_graphviz_footer(&length, &clast);
    if (NULL == ptr) { return; }

    ptr = strstr(ptr, saddress);
    if (NULL == ptr) {
        // The node is not in the Graphviz clusters.
        goto exit;
    }

    sprintf(
        hide_saddress,
        "hide%0*" PRIu16,
        (int)(saddress_length - strlen("hide")),
        graphviz_hide_counter());

    fprintf(
        graphviz_footer_fp,
        "\n" GRAPHVIZ_INDENT "%s [style=invis];\n",
        hide_saddress);

    do {
        memcpy(ptr, hide_saddress, saddress_length);
    } while ((ptr = strstr(ptr, saddress)));

exit:
    ptrx[length - 1] = clast, IO_CALL(munmap, ptrx, length);
    IO_CALL(fflush, graphviz_footer_fp);
}

#define DEFINED_clear_graphviz_cluster_node

#undef GRAPHVIZ_END_CLUSTER
#undef GRAPHVIZ_BEGIN_CLUSTER

#endif // LAMBDASPEED_ENABLE_GRAPHVIZ_CLUSTERS

inline static bool
graphviz_is_either_root(const struct node f, const struct node g) {
    XASSERT(f.ports), XASSERT(g.ports);

    return SYMBOL_ROOT == f.ports[-1] || SYMBOL_ROOT == g.ports[-1];
}

inline static bool
graphviz_is_active_node(const struct node node) {
    XASSERT(node.ports);

    const struct node f = node, g = follow_port(&node.ports[0]);

    return is_interacting_with(f, g) && !graphviz_is_either_root(f, g);
}

inline static bool
graphviz_is_active_edge(const struct node node, const uint8_t i) {
    XASSERT(node.ports);

    const struct node f = node, g = follow_port(&node.ports[i]);

    return is_interaction(f, g) && !graphviz_is_either_root(f, g);
}

struct graphviz_context {
    FILE *stream;
    struct node_list *history;
    const bool is_reading_back;
};

COMPILER_NONNULL(1) //
static void
graphviz_draw_node(
    struct graphviz_context *const restrict ctx,
    const struct node node) {
    assert(ctx), XASSERT(ctx->stream);
    XASSERT(node.ports);

    const bool is_active = graphviz_is_active_node(node),
               is_root = SYMBOL_ROOT == node.ports[-1];

    fprintf(
        ctx->stream,
        // clang-format off
        GRAPHVIZ_INDENT "n%p"
        " [label=\"%s\""
        ", xlabel=<<FONT FACE=\"Courier\" COLOR=\"darkorange2\" POINT-SIZE=\"8\">%s</FONT>>"
        "%s%s%s];\n",
        // clang-format on
        (void *)node.ports,
        print_symbol(node.ports[-1]),
        graphviz_node_xlabel(node),
        (is_active ? ", color=" GRAPHVIZ_ACTIVE_COLOR : ""),
        (is_active ? ", penwidth=2.3" : ""),
        (is_root ? ", style=filled" : ""));
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

    fprintf(
        ctx->stream,
        // clang-format off
        GRAPHVIZ_INDENT "n%p -> n%p [label=\" %s \", weight=%" PRIu8 ", tailport=%s"
        "%s%s%s%s];\n",
        // clang-format on
        (void *)source.ports,
        (void *)target.ports,
        graphviz_edge_label(source, i, ctx->is_reading_back),
        graphviz_edge_weight(source, i, ctx->is_reading_back),
        graphviz_edge_tailport(source, i, ctx->is_reading_back),
        (is_active ? ", color=" GRAPHVIZ_ACTIVE_COLOR : ""),
        (is_active ? ", penwidth=1.5" : ""),
        (IS_PRINCIPAL_PORT(target_port) ? ", arrowhead=dot" : ""),
        (0 == i ? ", style=dashed" : ""));
}

COMPILER_NONNULL(1) //
static void
go_graphviz(
    struct graphviz_context *const restrict ctx,
    const struct node source,
    const uint8_t i) {
    assert(ctx), XASSERT(ctx->stream);
    XASSERT(source.ports);

    const struct node node = follow_port(&source.ports[i]);

    GUARD_NODE(ctx->history, node);

    graphviz_draw_node(ctx, node);

    FOR_ALL_PORTS (node, j, 0) { graphviz_draw_edge(ctx, node, j); }

    FOR_ALL_PORTS (node, j, 0) { go_graphviz(ctx, node, j); }
}

COMPILER_NONNULL(1, 2) //
static void
graphviz(
    const struct node_graph *const restrict graph,
    const char filename[const restrict]) {
    debug("%s(\"%s\")", __func__, filename);

    assert(graph);
    assert(filename);

#ifdef LAMBDASPEED_ENABLE_GRAPHVIZ_CLUSTERS
    if (NULL == graphviz_footer_fp) {
        // The file descriptor will be closed upon programme terminationne.
        if (NULL == (graphviz_footer_fp = tmpfile())) {
            perror("tmpfile"), abort();
        }
    }
#endif

    FILE *fp = fopen(filename, "w");
    if (NULL == fp) { perror("fopen"), abort(); }

    fprintf(fp, "digraph {\n");
    fprintf(fp, GRAPHVIZ_INDENT "graph [nodesep=0.5, ranksep=0.8];\n");
    fprintf(fp, GRAPHVIZ_INDENT "node [fontname=\"bold helvetica\"];\n");
    fprintf(
        fp,
        GRAPHVIZ_INDENT
        "edge [fontname=\"bold helvetica\""
        ", fontsize=11"
        ", fontcolor=darkblue];\n");
    struct graphviz_context ctx = {
        .stream = fp,
        .history = NULL,
        .is_reading_back = graph->is_reading_back,
    };
    go_graphviz(&ctx, graph->root, 0);
    ctx.history = unvisit_all(ctx.history);
#ifdef LAMBDASPEED_ENABLE_GRAPHVIZ_CLUSTERS
    {
        postprocess_graphviz_footer();
        const long length = ftell(graphviz_footer_fp);
        rewind(graphviz_footer_fp);
        redirect_stream(graphviz_footer_fp, fp);
        fseek(graphviz_footer_fp, length, SEEK_SET);
    }
#endif
    fprintf(fp, "}\n");

    IO_CALL(fclose, fp);
}

#undef GRAPHVIZ_HIDE
#undef GRAPHVIZ_STYLE_PLACEHOLDER
#undef GRAPHVIZ_ACTIVE_COLOR
#undef GRAPHVIZ_ADDRESS_WIDTH_LINE
#undef GRAPHVIZ_INDENT_2X
#undef GRAPHVIZ_INDENT

#else

#define graphviz(graph, filename) ((void)0)

#endif // LAMBDASPEED_ENABLE_GRAPHVIZ

#ifndef DEFINED_graphviz_commute_cluster
#define graphviz_commute_cluster(f_updates, g_updates, m, n) ((void)0)
#endif

#ifndef DEFINED_graphviz_beta_cluster
#define graphviz_beta_cluster(lhs, rhs) ((void)0)
#endif

#ifndef DEFINED_clear_graphviz_cluster_node
#define clear_graphviz_cluster_node(node) ((void)0)
#endif

#if !defined(NDEBUG) && defined(LAMBDASPEED_ENABLE_STEP_BY_STEP)

static void
wait_for_user(const struct node_graph graph) {
#ifdef LAMBDASPEED_ENABLE_GRAPHVIZ
    graphviz(&graph, "target/state.dot");
    if (system("./command/graphviz-state.sh") != 0) {
        panic("Failed to run `./command/graphviz-state.sh`!");
    }
#else
    (void)graph;
#endif

    printf("Press ENTER to proceed...");
    fflush(stdout);
    if (EOF == getchar()) { perror("getchar"), abort(); }
}

#else

#define wait_for_user(graph) ((void)0)

#endif // !defined(NDEBUG) && defined(LAMBDASPEED_ENABLE_STEP_BY_STEP)

// Interactionne rules
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT COMPILER_HOT COMPILER_FLATTEN //
inline static bool
is_annihilation(const struct node f, const struct node g) {
    XASSERT(f.ports), XASSERT(g.ports);

    return is_interaction(f, g) && f.ports[-1] == g.ports[-1];
}

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT COMPILER_HOT COMPILER_FLATTEN //
inline static bool
is_beta(const struct node f, const struct node g) {
    XASSERT(f.ports), XASSERT(g.ports);

    return is_interaction(f, g) &&
           (SYMBOL_APPLICATOR == f.ports[-1] && SYMBOL_LAMBDA == g.ports[-1]);
}

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT COMPILER_HOT COMPILER_FLATTEN //
inline static bool
is_commutation(const struct node f, const struct node g) {
    XASSERT(f.ports), XASSERT(g.ports);

    return is_interaction(f, g) && !is_beta(f, g) && !is_beta(g, f) &&
           f.ports[-1] != g.ports[-1];
}

typedef void (*Rule)(
    struct node_graph *const restrict graph,
    struct node f,
    struct node g);

COMPILER_NONNULL(1) COMPILER_HOT //
static void
annihilate(
    struct node_graph *const restrict graph,
    const struct node f,
    const struct node g) {
#ifdef LAMBDASPEED_ENABLE_TRACING
    {
        const char *const ssymbol = print_symbol(f.ports[-1]);
        debug(
            "%s(%p %s, %p %s)",
            __func__,
            (void *)f.ports,
            ssymbol,
            (void *)g.ports,
            ssymbol);
    }
#endif

    assert(graph);
    XASSERT(f.ports), XASSERT(g.ports);
    assert(is_annihilation(f, g));
    wait_for_user(*graph);

    const uint8_t n = ports_count(f.ports[-1]) - 1;

#define BODY(i)                                                                \
    do {                                                                       \
        uint64_t *const f_target = DECODE_ADDRESS(f.ports[i]),                 \
                        *const g_target = DECODE_ADDRESS(g.ports[i]);          \
        const struct node fx = node_of_port(f_target),                         \
                          gx = node_of_port(g_target);                         \
                                                                               \
        /* Respectiue ports must haue the same semantic meaning. */            \
        connect_ports(f_target, g_target);                                     \
        register_pair_if_active(graph, fx, gx);                                \
    } while (false)

    switch (n) {
    case 2: BODY(2); // fall through
    case 1: BODY(1);
    case 0: break;
    default: COMPILER_UNREACHABLE();
    }

#undef BODY

    clear_graphviz_cluster_node(f);
    clear_graphviz_cluster_node(g);

#ifdef LAMBDASPEED_ENABLE_STATS
    graph->nannihilations++;
#endif
}

COMPILER_UNUSED static const Rule annihilate_type_check = annihilate;

COMPILER_NONNULL(1) COMPILER_HOT //
static void
commute(struct node_graph *const restrict graph, struct node f, struct node g) {
    assert(graph);
    XASSERT(f.ports), XASSERT(g.ports);
    assert(is_commutation(f, g));

prologue:;

    const bool with_lambda_or_delim =
        SYMBOL_LAMBDA == g.ports[-1] || IS_DELIMITER(g.ports[-1]);

    // Ensure that lambdas & delimiters are alwaies `g`, to giue `f` the
    // opportunitie to incremente its index.
    if ((SYMBOL_LAMBDA == f.ports[-1] || IS_DELIMITER(f.ports[-1])) &&
        !with_lambda_or_delim) {
        const struct node h = f;
        f = g, g = h;
        goto prologue;
    }

    const int64_t i = symbol_index(f.ports[-1]), j = symbol_index(g.ports[-1]);

    // If both are delimiters, the one with a higher index should be `f`.
    if (IS_DELIMITER(f.ports[-1]) && IS_DELIMITER(g.ports[-1]) && j > i) {
        const struct node h = f;
        f = g, g = h;
        goto prologue;
    }

    // If `f` is a lambda & `g` is a delimiter, swap them so that the index of
    // `g` could be incremented.
    if (SYMBOL_LAMBDA == f.ports[-1] && IS_DELIMITER(g.ports[-1])) {
        const struct node h = f;
        f = g, g = h;
        goto prologue;
    }

#ifdef LAMBDASPEED_ENABLE_TRACING
    {
        char f_ssymbol[MAX_SSYMBOL_SIZE] = {0},
             g_ssymbol[MAX_SSYMBOL_SIZE] = {0};
        strcpy(f_ssymbol, print_symbol(f.ports[-1])),
            strcpy(g_ssymbol, print_symbol(g.ports[-1]));
        debug(
            "%s(%p %s, %p %s)",
            __func__,
            (void *)f.ports,
            f_ssymbol,
            (void *)g.ports,
            g_ssymbol);
    }
#endif

    wait_for_user(*graph);

    const bool update_symbol = (SYMBOL_LAMBDA == g.ports[-1] && i >= 0) ||
                               (IS_DELIMITER(g.ports[-1]) && i >= j);

    const uint64_t f_symbol =
                       (update_symbol ? bump_index(f.ports[-1]) : f.ports[-1]),
                   g_symbol = g.ports[-1];

    const uint8_t n = ports_count(f.ports[-1]) - 1,
                  m = ports_count(g.ports[-1]) - 1;

    struct node f_updates[MAX_AUXILIARY_PORTS] = {{NULL}},
                g_updates[MAX_AUXILIARY_PORTS] = {{NULL}};

    // Allocate memory for the new nodes to be connected among themselves & with
    // the rest of the graph.
    // for (uint8_t i = 0; i < m; i++) {
    //     f_updates[i] = alloc_node(graph, f_symbol);
    // }
    // for (uint8_t i = 0; i < n; i++) {
    //     g_updates[i] = alloc_node(graph, g_symbol);
    // }
    {
        switch (m) {
        case 2:
            f_updates[1] = alloc_node(graph, f_symbol);
            // fall through
        case 1:
            f_updates[0] = alloc_node(graph, f_symbol);
            // fall through
        case 0: break;
        default: COMPILER_UNREACHABLE();
        }

        switch (n) {
        case 2:
            g_updates[1] = alloc_node(graph, g_symbol);
            // fall through
        case 1:
            g_updates[0] = alloc_node(graph, g_symbol);
            // fall through
        case 0: break;
        default: COMPILER_UNREACHABLE();
        }
    }

    // Connecte the principal ports of the new nodes with the old ones.
    // for (uint8_t i = 0; i < m; i++) {
    //     connect_ports(&f_updates[i].ports[0], DECODE_ADDRESS(g.ports[m -
    //     i]));
    // }
    // for (uint8_t i = 0; i < n; i++) {
    //     connect_ports(&g_updates[i].ports[0], DECODE_ADDRESS(f.ports[i +
    //     1]));
    // }
    {
        switch (m) {
        case 2:
            connect_ports(&f_updates[1].ports[0], DECODE_ADDRESS(g.ports[1]));
            connect_ports(&f_updates[0].ports[0], DECODE_ADDRESS(g.ports[2]));
            break;
        case 1:
            connect_ports(&f_updates[0].ports[0], DECODE_ADDRESS(g.ports[1]));
            break;
        case 0: break;
        default: COMPILER_UNREACHABLE();
        }

        switch (n) {
        case 2:
            connect_ports(&g_updates[1].ports[0], DECODE_ADDRESS(f.ports[2]));
            // fall through
        case 1:
            connect_ports(&g_updates[0].ports[0], DECODE_ADDRESS(f.ports[1]));
            break;
        case 0: break;
        default: COMPILER_UNREACHABLE();
        }
    }

    // Connecte the new nodes among themselves.
    // Manually vnrolling this loop into a sequence of conditionnes did not giue
    // us a noticeable performance benefit.
    for (uint8_t i = 0; i < m; i++) {
        for (uint8_t j = 0; j < n; j++) {
            connect_ports(
                &f_updates[i].ports[j + 1], &g_updates[j].ports[m - i]);
        }
    }

    // Register the new actuie nodes in the graph for future interactionne.
    // for (uint8_t i = 0; i < m; i++) {
    //     register_node_if_active(graph, f_updates[i]);
    // }
    // for (uint8_t i = 0; i < n; i++) {
    //     register_node_if_active(graph, g_updates[i]);
    // }
    {
        switch (m) {
        case 2:
            register_node_if_active(graph, f_updates[1]);
            // fall through
        case 1:
            register_node_if_active(graph, f_updates[0]);
            // fall through
        case 0: break;
        default: COMPILER_UNREACHABLE();
        }

        switch (n) {
        case 2:
            register_node_if_active(graph, g_updates[1]);
            // fall through
        case 1:
            register_node_if_active(graph, g_updates[0]);
            // fall through
        case 0: break;
        default: COMPILER_UNREACHABLE();
        }
    }

    clear_graphviz_cluster_node(f);
    clear_graphviz_cluster_node(g);
    graphviz_commute_cluster(f_updates, g_updates, m, n);

#ifdef LAMBDASPEED_ENABLE_STATS
    graph->ncommutations++;
#endif
}

COMPILER_UNUSED static const Rule commute_type_check = commute;

COMPILER_NONNULL(1) COMPILER_HOT //
static void
beta(
    struct node_graph *const restrict graph,
    const struct node f,
    const struct node g) {
    debug("%s(%p @, %p λ)", __func__, (void *)f.ports, (void *)g.ports);
    wait_for_user(*graph);

    assert(graph);
    XASSERT(!graph->is_reading_back);
    XASSERT(f.ports), XASSERT(g.ports);
    assert(is_interaction(f, g));
    XASSERT(SYMBOL_APPLICATOR == f.ports[-1]);
    XASSERT(SYMBOL_LAMBDA == g.ports[-1]);

    struct node lhs = alloc_node(graph, SYMBOL_DELIMITER(UINT64_C(0)));
    struct node rhs = alloc_node(graph, SYMBOL_DELIMITER(UINT64_C(0)));

    // clang-format off
    uint64_t *const targets[] = {
        DECODE_ADDRESS(f.ports[1]), DECODE_ADDRESS(g.ports[2]),
        DECODE_ADDRESS(f.ports[2]), DECODE_ADDRESS(g.ports[1]),
    };
    // clang-format on

    connect_ports(&lhs.ports[0], targets[0]);
    connect_ports(&rhs.ports[0], targets[2]);

    connect_ports(&lhs.ports[1], targets[1]);
    connect_ports(&rhs.ports[1], targets[3]);

    register_node_if_active(graph, lhs);
    register_node_if_active(graph, rhs);

    clear_graphviz_cluster_node(f);
    clear_graphviz_cluster_node(g);
    graphviz_beta_cluster(&lhs.ports[0], &rhs.ports[0]);

#ifdef LAMBDASPEED_ENABLE_STATS
    graph->nbetas++;
#endif
}

COMPILER_UNUSED static const Rule beta_type_check = beta;

// The x-rules normalizationne loop
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

COMPILER_NONNULL(1, 2) COMPILER_HOT //
static void
interact(
    struct node_graph *const restrict graph,
    const Rule rule,
    const struct node f) {
    assert(graph);
    assert(rule);
    XASSERT(f.ports);

    const struct node g = follow_port(&f.ports[0]);
    XASSERT(g.ports);

    assert(is_interaction(f, g));

    rule(graph, f, g);

    free_node(f);
    free_node(g);
}

COMPILER_NONNULL(1) //
static void
normalize_x_rules(struct node_graph *const restrict graph) {
    debug("%s()", __func__);

    assert(graph);

    do {
        if (!graph->is_reading_back) {
            CONSUME_FOCUS (graph->betas, f) { interact(graph, beta, f); }
        }
        CONSUME_FOCUS (graph->annihilations, f) {
            interact(graph, annihilate, f);
        }
        CONSUME_FOCUS (graph->commutations, f) { //
            interact(graph, commute, f);
        }
    } while (!is_normalized_graph(graph));
}

// The read-back phases
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#define PHASE_INITIAL      UINT64_C(0)
#define PHASE_UNWIND       UINT64_C(1)
#define PHASE_SCOPE_REMOVE UINT64_C(2)
#define PHASE_LOOP_CUT     UINT64_C(3)

struct iter_nodes_context {
    const struct node_graph *graph;
    const struct symbol_range range;
    struct node_list *collection;
};

COMPILER_NONNULL(1) COMPILER_HOT //
static void
go_iter_nodes(
    struct iter_nodes_context *const restrict ctx,
    const struct node node) {
    assert(ctx);
    XASSERT(node.ports);

    if (DECODE_PHASE_METADATA(node.ports[0]) == ctx->graph->current_phase) {
        return;
    }
    node.ports[0] = SET_PHASE(node.ports[0], ctx->graph->current_phase);

    if (symbol_is_in_range(ctx->range, node.ports[-1])) {
        ctx->collection = visit(ctx->collection, node);
    }

    switch (ports_count(node.ports[-1])) {
    case 3:
        go_iter_nodes(ctx, follow_port(&node.ports[2]));
        // fall through
    case 2:
        go_iter_nodes(ctx, follow_port(&node.ports[1]));
        // fall through
    case 1:
        go_iter_nodes(ctx, follow_port(&node.ports[0])); //
        break;
    default: COMPILER_UNREACHABLE();
    }
}

COMPILER_NONNULL(1) //
static struct node_list *
iter_nodes(
    struct node_graph *const restrict graph,
    const struct symbol_range range) {
    assert(graph), XASSERT(graph->root.ports);

    struct iter_nodes_context ctx = {
        .graph = graph, .range = range, .collection = NULL};
    go_iter_nodes(&ctx, graph->root);

    return ctx.collection;
}

COMPILER_NONNULL(1) //
static void
unwind(struct node_graph *const restrict graph) {
    assert(graph);
    assert(is_normalized_graph(graph));

    graph->current_phase = PHASE_UNWIND;

    struct node_list *iter_backup =
        iter_nodes(graph, SYMBOL_RANGE(SYMBOL_APPLICATOR, SYMBOL_APPLICATOR));

    ITERATE_LIST (iter, iter_backup) {
        const struct node node = iter->node;
        XASSERT(node.ports);

        debug("%s(%s)", __func__, print_node(node));
        wait_for_user(*graph);

        uint64_t *const rator_port_target = DECODE_ADDRESS(node.ports[0]),
                        *const applicator_port_target =
                            DECODE_ADDRESS(node.ports[1]),
                        *const rand_port_target = DECODE_ADDRESS(node.ports[2]);
        XASSERT(rator_port_target);
        XASSERT(applicator_port_target);
        XASSERT(rand_port_target);

        connect_ports(&node.ports[0], applicator_port_target);
        connect_ports(&node.ports[1], rand_port_target);
        connect_ports(&node.ports[2], rator_port_target);
    }

    CONSUME_LIST (iter, iter_backup) {
        register_node_if_active(graph, iter->node);
    }
}

COMPILER_NONNULL(1) //
static void
scope_remove(struct node_graph *const restrict graph) {
    assert(graph);
    assert(is_normalized_graph(graph));

    graph->current_phase = PHASE_SCOPE_REMOVE;

    struct node_list *new_scopes = NULL;

    CONSUME_LIST (iter, iter_nodes(graph, DELIMITER_RANGE)) {
        const struct node node = iter->node;
        XASSERT(node.ports);

        debug("%s(%s)", __func__, print_node(node));
        wait_for_user(*graph);

        uint64_t *const port_0_target = DECODE_ADDRESS(node.ports[0]),
                        *const port_1_target = DECODE_ADDRESS(node.ports[1]);
        XASSERT(port_0_target);
        XASSERT(port_1_target);

        const struct node scope = alloc_node(graph, SYMBOL_S);
        connect_ports(&scope.ports[0], port_1_target);
        connect_ports(&scope.ports[1], port_0_target);

        free_node(node);
        new_scopes = visit(new_scopes, scope);
    }

    CONSUME_LIST (iter, new_scopes) {
        const struct node other = follow_port(&iter->node.ports[0]);

        // Protecte from focusing on both actiue scopes.
        // See <https://github.com/etiams/lambdaspeed/issues/2>.
        if (!(SYMBOL_S == other.ports[-1] &&
              (intptr_t)iter->node.ports > (intptr_t)other.ports)) {
            register_node_if_active(graph, iter->node);
        }
    }
}

COMPILER_NONNULL(1) //
static void
loop_cut(struct node_graph *const restrict graph) {
    assert(graph);
    assert(is_normalized_graph(graph));

    graph->current_phase = PHASE_LOOP_CUT;

    CONSUME_LIST (
        iter, iter_nodes(graph, SYMBOL_RANGE(SYMBOL_LAMBDA, SYMBOL_LAMBDA))) {
        const struct node node = iter->node;
        XASSERT(node.ports);

        debug("%s(%s)", __func__, print_node(node));
        wait_for_user(*graph);

        struct node side_eraser = alloc_node(graph, SYMBOL_ERASER);
        struct node bottom_eraser = alloc_node(graph, SYMBOL_ERASER);
        uint64_t *const disconnected_port_backup =
            DECODE_ADDRESS(node.ports[1]);
        XASSERT(disconnected_port_backup);

        connect_ports(&node.ports[1], &side_eraser.ports[0]);
        connect_ports(&bottom_eraser.ports[0], disconnected_port_backup);

        XASSERT(DECODE_ADDRESS(node.ports[1]));
        XASSERT(DECODE_ADDRESS(side_eraser.ports[0]));
        XASSERT(DECODE_ADDRESS(bottom_eraser.ports[0]));

        // Here, principal ports doe not change their directionne.
        register_node_if_active(graph, bottom_eraser);
    }
}

// Conversionne to a lambda term string
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

COMPILER_NONNULL(1) //
static void
to_lambda_string(
    FILE *const restrict stream,
    const uint64_t i,
    const struct node node) {
    assert(stream);
    XASSERT(node.ports);

    switch (node.ports[-1]) {
    case SYMBOL_APPLICATOR:
        fprintf(stream, "(");
        to_lambda_string(stream, i, follow_port(&node.ports[2]));
        fprintf(stream, " ");
        to_lambda_string(stream, i, follow_port(&node.ports[1]));
        fprintf(stream, ")");
        return;
    case SYMBOL_LAMBDA:
        fprintf(stream, "(λ ");
        to_lambda_string(stream, i, follow_port(&node.ports[2]));
        fprintf(stream, ")");
        return;
    case SYMBOL_ERASER: fprintf(stream, "%" PRIu64, i); return;
    case SYMBOL_S:
        to_lambda_string(stream, i + 1, follow_port(&node.ports[1]));
        return;
    default: break;
    }

    if (!IS_DUPLICATOR(node.ports[-1])) {
        // Other symbols must be alreadie remoued at this point.
        COMPILER_UNREACHABLE();
    }

    for (uint8_t k = 1, l = 2; k <= 2; k++, l--) {
        const struct node neighbour = follow_port(&node.ports[k]);
        if (SYMBOL_ERASER == neighbour.ports[-1]) {
            const struct node body = follow_port(&node.ports[l]);
            to_lambda_string(stream, i, body);
            return;
        }
    }

    COMPILER_UNREACHABLE();
}

// Conversionne from a lambda term
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

enum lambda_term_type {
    LAMBDA_TERM_APPLICATOR,
    LAMBDA_TERM_LAMBDA,
    LAMBDA_TERM_VAR,
};

struct applicator_payload {
    struct lambda_term *rator;
    struct lambda_term *rand;
};

struct lambda_payload {
    struct lambda_term *body;
    uint64_t **dup_ports; // the pointer to the next duplicator tree
                          // port; dynamically assigned
    uint64_t lvl;         // the de Bruijn level; dynamically assigned
};

union lambda_term_payload {
    struct applicator_payload applicator;
    struct lambda_payload lambda;
    struct lambda_payload *var; // the pointer to the binding lambda
};

struct lambda_term {
    enum lambda_term_type ty;
    union lambda_term_payload payload;
};

COMPILER_MALLOC(free, 1) COMPILER_WARN_UNUSED_RESULT COMPILER_RETURNS_NONNULL //
static struct lambda_term *
xmalloc_lambda_term(struct lambda_term term) {
    struct lambda_term *const heaped = xmalloc(sizeof term);
    *heaped = term;
    return heaped;
}

COMPILER_WARN_UNUSED_RESULT COMPILER_RETURNS_NONNULL COMPILER_NONNULL(1, 2) //
inline static struct lambda_term *
applicator(struct lambda_term *const rator, struct lambda_term *const rand) {
    assert(rator), assert(rand);

    return xmalloc_lambda_term((struct lambda_term){
        .ty = LAMBDA_TERM_APPLICATOR,
        .payload = {.applicator = {rator, rand}},
    });
}

COMPILER_CONST COMPILER_WARN_UNUSED_RESULT COMPILER_NONNULL(1) //
inline static struct lambda_term
prelambda(struct lambda_term *const body) {
    assert(body);

    return (struct lambda_term){
        .ty = LAMBDA_TERM_LAMBDA,
        .payload = {.lambda = {body, .dup_ports = NULL}},
    };
}

#define lambda(x, body)                                                        \
    ((x) = xmalloc(sizeof *(x)), (*(x) = prelambda(body), (x)))

COMPILER_WARN_UNUSED_RESULT COMPILER_RETURNS_NONNULL COMPILER_NONNULL(1) //
inline static struct lambda_term *
prevar(struct lambda_payload *const lambda) {
    assert(lambda);

    return xmalloc_lambda_term((struct lambda_term){
        .ty = LAMBDA_TERM_VAR,
        .payload = {.var = lambda},
    });
}

#define var(x) prevar(&(x)->payload.lambda)

COMPILER_WARN_UNUSED_RESULT COMPILER_NONNULL(2) //
static uint64_t
count_var_occurrences(
    const struct lambda_term term,
    const struct lambda_payload *const restrict lambda) {
    assert(lambda);

    switch (term.ty) {
    case LAMBDA_TERM_APPLICATOR:
        XASSERT(term.payload.applicator.rator);
        XASSERT(term.payload.applicator.rand);
        return count_var_occurrences(*term.payload.applicator.rator, lambda) +
               count_var_occurrences(*term.payload.applicator.rand, lambda);
    case LAMBDA_TERM_LAMBDA:
        XASSERT(term.payload.lambda.body);
        return count_var_occurrences(*term.payload.lambda.body, lambda);
    case LAMBDA_TERM_VAR:
        XASSERT(term.payload.var);
        return lambda == term.payload.var;
    default: COMPILER_UNREACHABLE();
    }
}

COMPILER_RETURNS_NONNULL COMPILER_WARN_UNUSED_RESULT COMPILER_NONNULL(1, 2) //
static uint64_t *
build_delimiter_sequence(
    struct node_graph *const restrict graph,
    uint64_t *const restrict binder_port,
    const uint64_t n) {
    assert(graph);
    assert(binder_port);
    XASSERT(n > 0);

    struct node current = alloc_node(graph, SYMBOL_DELIMITER(UINT64_C(0)));
    uint64_t *const result = &current.ports[1];
    for (uint64_t i = 1; i < n; i++) {
        const struct node delim =
            alloc_node(graph, SYMBOL_DELIMITER(UINT64_C(0)));
        connect_ports(&current.ports[0], &delim.ports[1]);
        current = delim;
    }

    connect_ports(&current.ports[0], binder_port);

    return result;
}

COMPILER_RETURNS_NONNULL COMPILER_NONNULL(1, 2) COMPILER_WARN_UNUSED_RESULT //
static uint64_t **
build_duplicator_tree(
    struct node_graph *const restrict graph,
    uint64_t *const restrict binder_port,
    const uint64_t n) {
    assert(graph);
    assert(binder_port);
    XASSERT(n >= 1);

    uint64_t **const ports = xmalloc(sizeof ports[0] * n);

    struct node current = alloc_node(graph, SYMBOL_DUPLICATOR(UINT64_C(0)));
    struct node eraser = alloc_node(graph, SYMBOL_ERASER);

    ports[0] = &current.ports[1];
    connect_ports(&current.ports[2], &eraser.ports[0]);

    for (uint64_t i = 1; i < n; i++) {
        const struct node dup =
            alloc_node(graph, SYMBOL_DUPLICATOR(UINT64_C(0)));
        ports[i] = &dup.ports[1];
        connect_ports(&dup.ports[2], &current.ports[0]);
        current = dup;
    }

    connect_ports(&current.ports[0], binder_port);

    return ports;
}

COMPILER_CONST COMPILER_WARN_UNUSED_RESULT //
inline static uint64_t
de_bruijn_level_to_index(const uint64_t lvl, const uint64_t var) {
    return lvl - var - 1;
}

COMPILER_NONNULL(1, 2, 3) //
static void
go_of_lambda_term(
    struct node_graph *const restrict graph,
    struct lambda_term *const restrict term,
    uint64_t *const restrict output_port,
    const uint64_t lvl) {
    assert(graph);
    assert(term);
    assert(output_port);

    switch (term->ty) {
    case LAMBDA_TERM_APPLICATOR: {
        XASSERT(term->payload.applicator.rator);
        XASSERT(term->payload.applicator.rand);
        const struct node applicator = alloc_node(graph, SYMBOL_APPLICATOR);
        go_of_lambda_term(
            graph, term->payload.applicator.rator, &applicator.ports[0], lvl);
        go_of_lambda_term(
            graph, term->payload.applicator.rand, &applicator.ports[2], lvl);
        connect_ports(&applicator.ports[1], output_port);
        if (is_active(applicator)) { focus_on(graph->betas, applicator); }
        break;
    }
    case LAMBDA_TERM_LAMBDA: {
        XASSERT(term->payload.lambda.body);
        const struct node lambda = alloc_node(graph, SYMBOL_LAMBDA);
        const uint64_t nvars = count_var_occurrences(
            *term->payload.lambda.body, &term->payload.lambda);
        uint64_t **dup_ports = NULL;
        if (0 == nvars) {
            const struct node eraser = alloc_node(graph, SYMBOL_ERASER);
            connect_ports(&lambda.ports[1], &eraser.ports[0]);
        } else {
            dup_ports = build_duplicator_tree(
                graph, &lambda.ports[1], nvars /* nvars >= 1 */);
        }
        term->payload.lambda.dup_ports = dup_ports;
        term->payload.lambda.lvl = lvl;
        go_of_lambda_term(
            graph, term->payload.lambda.body, &lambda.ports[2], lvl + 1);
        connect_ports(&lambda.ports[0], output_port);
        if (dup_ports) { free(dup_ports); }
        break;
    }
    case LAMBDA_TERM_VAR: {
        XASSERT(term->payload.var);
        struct lambda_payload *const lambda = term->payload.var;
        const uint64_t idx = de_bruijn_level_to_index(lvl, lambda->lvl);
        if (0 == idx) {
            connect_ports(lambda->dup_ports[0], output_port);
        } else {
            uint64_t *const delimiter_port =
                build_delimiter_sequence(graph, lambda->dup_ports[0], idx);
            connect_ports(delimiter_port, output_port);
        }
        lambda->dup_ports++;
        break;
    }
    default: COMPILER_UNREACHABLE();
    }

    // This functionne takes ownership of the whole `term` object.
    free(term);
}

COMPILER_WARN_UNUSED_RESULT COMPILER_NONNULL(1) //
static struct node_graph
of_lambda_term(struct lambda_term *const restrict term) {
    assert(term);

    const struct node root = xmalloc_node(SYMBOL_ROOT, PHASE_INITIAL);
    const struct node eraser = xmalloc_node(SYMBOL_ERASER, PHASE_INITIAL);

    // Since the principle root port is connected to the eraser, the
    // root will neuer interacte with "real" nodes.
    connect_ports(&root.ports[0], &eraser.ports[0]);

    struct node_graph graph = {
        .root = root,
        .annihilations = xcalloc(1, sizeof *graph.annihilations),
        .commutations = xcalloc(1, sizeof *graph.commutations),
        .betas = xcalloc(1, sizeof *graph.commutations),
        .current_phase = PHASE_INITIAL,
        .is_reading_back = false,

#ifdef LAMBDASPEED_ENABLE_STATS
        .nannihilations = 0,
        .ncommutations = 0,
        .nbetas = 0,
#endif
    };

    go_of_lambda_term(&graph, term, &root.ports[1], 0);

    return graph;
}

// The complete algorithm
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

COMPILER_NONNULL(2) COMPILER_OPTIMIZE("O0") /* for benchmarking */ //
static void
algorithm(
    FILE *const restrict stream, // if `NULL`, doe not read back
    struct lambda_term *const restrict term) {
    debug("%s()", __func__);

    assert(term);

    struct node_graph graph = of_lambda_term(term);

    // Initiall normalizationne.
    {
        graphviz(&graph, "target/0-initial.dot");
        normalize_x_rules(&graph);
        graphviz(&graph, "target/0-initialx.dot");
    }

#ifdef LAMBDASPEED_ENABLE_STATS
    printf("Annihilation interactions: %" PRIu64 "\n", graph.nannihilations);
    printf("Commutation interactions: %" PRIu64 "\n", graph.ncommutations);
    printf("Beta interactions: %" PRIu64 "\n", graph.nbetas);
    printf(
        "Total interactions: %" PRIu64 "\n",
        graph.nannihilations + graph.ncommutations + graph.nbetas);
#endif

    if (NULL == stream) { goto cleanup; }

    ITERATE_ONCE(
        finish, graph.is_reading_back = true, graph.is_reading_back = false) {
        // Phase #1:
        {
            unwind(&graph);
            graphviz(&graph, "target/1-unwound.dot");
            normalize_x_rules(&graph);
            graphviz(&graph, "target/1-unwoundx.dot");
        }

        // Phase #2:
        {
            scope_remove(&graph);
            graphviz(&graph, "target/2-unscoped.dot");
            normalize_x_rules(&graph);
            graphviz(&graph, "target/2-unscopedx.dot");
        }

        // Phase #3:
        {
            loop_cut(&graph);
            graphviz(&graph, "target/3-unlooped.dot");
            normalize_x_rules(&graph);
            graphviz(&graph, "target/3-unloopedx.dot");
        }
    }

    to_lambda_string(stream, 0, follow_port(&graph.root.ports[1]));

cleanup:
    assert(is_normalized_graph(&graph));
    free_graph(&graph);
}
