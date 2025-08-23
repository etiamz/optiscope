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

// Miscellaneous Macros
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
#define SYMBOL_S               UINT64_C(4)
#define SYMBOL_CELL            UINT64_C(5)
#define SYMBOL_UNARY_CALL      UINT64_C(6)
#define SYMBOL_BINARY_CALL     UINT64_C(7)
#define SYMBOL_BINARY_CALL_AUX UINT64_C(8)
#define SYMBOL_IF_THEN_ELSE    UINT64_C(9)
#define SYMBOL_PERFORM         UINT64_C(10)
#define SYMBOL_IDENTITY_LAMBDA UINT64_C(11) // the identity lambda
#define SYMBOL_GC_LAMBDA       UINT64_C(12) // a lambda discarding its parameter
#define SYMBOL_LAMBDA_C        UINT64_C(13) // a closed lambda
#define SYMBOL_REFERENCE       UINT64_C(14)
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
    case SYMBOL_S:
    case SYMBOL_UNARY_CALL:
    case SYMBOL_BINARY_CALL_AUX:
    case SYMBOL_GC_LAMBDA:
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
    MY_ASSERT(DECODE_ADDRESS(*port) != another);

    const uint64_t port_metadata = DECODE_ADDRESS_METADATA(*port);

    *port = ENCODE_ADDRESS(port_metadata, (uint64_t)another);

    MY_ASSERT(DECODE_ADDRESS(*port) == another);
    MY_ASSERT(DECODE_ADDRESS_METADATA(*port) == port_metadata);
}

COMPILER_NONNULL(1, 2) COMPILER_HOT COMPILER_ALWAYS_INLINE //
inline static void
connect_ports(uint64_t *const restrict lhs, uint64_t *const restrict rhs) {
    debug("%p 🔗 %p", (void *)lhs, (void *)rhs);

    // Delegate the assertions to `connect_ports_to`.
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
    case SYMBOL_S: sprintf(buffer, "S"); break;
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
    default:
        if (IS_DUPLICATOR(symbol)) goto duplicator;
        else if (IS_DELIMITER(symbol)) goto delimiter;
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

#define PHASE_REDUCE_WEAKLY    UINT64_C(0)
#define PHASE_REDUCE_FULLY     UINT64_C(1)
#define PHASE_REDUCE_FULLY_AUX UINT64_C(2)
#define PHASE_UNWIND           UINT64_C(3)
#define PHASE_SCOPE_REMOVE     UINT64_C(4)
#define PHASE_LOOP_CUT         UINT64_C(5)
#define PHASE_GC               UINT64_C(6)
#define PHASE_GC_AUX           UINT64_C(7)
#define PHASE_STACK            UINT64_C(8)

COMPILER_NONNULL(1) COMPILER_HOT COMPILER_ALWAYS_INLINE //
inline static void
set_phase(uint64_t *const restrict port, const uint64_t phase) {
    MY_ASSERT(port);
    MY_ASSERT(IS_PRINCIPAL_PORT(*port));

    const uint64_t mask =
        UINT64_C(0xC3FFFFFFFFFFFFFF); /* clear the phase bits (61-58) */

    *port = (*port & mask) | (phase << EFFECTIVE_ADDRESS_BITS);

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
        object--; /* back to the symbol address */                             \
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
POOL_ALLOCATOR(delimiter, sizeof(uint64_t) * 4)
POOL_ALLOCATOR(cell, sizeof(uint64_t) * 3)
POOL_ALLOCATOR(unary_call, sizeof(uint64_t) * 4)
POOL_ALLOCATOR(binary_call, sizeof(uint64_t) * 5)
POOL_ALLOCATOR(binary_call_aux, sizeof(uint64_t) * 5)
POOL_ALLOCATOR(if_then_else, sizeof(uint64_t) * 5)
POOL_ALLOCATOR(perform, sizeof(uint64_t) * 4)
POOL_ALLOCATOR(identity_lambda, sizeof(uint64_t) * 2)
POOL_ALLOCATOR(gc_lambda, sizeof(uint64_t) * 3)
POOL_ALLOCATOR(lambda_c, sizeof(uint64_t) * 4)
POOL_ALLOCATOR(reference, sizeof(uint64_t) * 3)

#define ALLOC_POOL_OBJECT(pool_name) pool_name##_alloc(pool_name)
#define FREE_POOL_OBJECT(pool_name, object)                                    \
    pool_name##_free(pool_name, (object))

#define POOLS                                                                  \
    X(applicator_pool)                                                         \
    X(lambda_pool)                                                             \
    X(eraser_pool)                                                             \
    X(scope_pool)                                                              \
    X(duplicator_pool)                                                         \
    X(delimiter_pool)                                                          \
    X(cell_pool)                                                               \
    X(unary_call_pool)                                                         \
    X(binary_call_pool)                                                        \
    X(binary_call_aux_pool)                                                    \
    X(if_then_else_pool)                                                       \
    X(perform_pool)                                                            \
    X(identity_lambda_pool)                                                    \
    X(gc_lambda_pool)                                                          \
    X(lambda_c_pool)                                                           \
    X(reference_pool)

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

COMPILER_CONST COMPILER_WARN_UNUSED_RESULT //
inline static int
compare_node_ptrs(const struct node f, const struct node g) {
    XASSERT(f.ports), XASSERT(g.ports);

    if ((intptr_t)f.ports < (intptr_t)g.ports) return -1;
    else if ((intptr_t)f.ports > (intptr_t)g.ports) return 1;
    else return 0;
}

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT //
inline static bool
is_either_root(const struct node f, const struct node g) {
    XASSERT(f.ports), XASSERT(g.ports);

    return SYMBOL_ROOT == f.ports[-1] || SYMBOL_ROOT == g.ports[-1];
}

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT COMPILER_HOT COMPILER_ALWAYS_INLINE //
inline static bool
is_interacting_with(const struct node f, const struct node g) {
    XASSERT(f.ports), XASSERT(g.ports);

    // Supposing that `g` is derived from `f` by `follow_port(&f.ports[0])`.
    return DECODE_ADDRESS(g.ports[0]) == &f.ports[0];
}

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT //
inline static bool
is_interaction(const struct node f, const struct node g) {
    XASSERT(f.ports), XASSERT(g.ports);

    return is_interacting_with(f, g) && is_interacting_with(g, f);
}

#define CONNECT_NODE(node, ...)                                                \
    do {                                                                       \
        uint64_t *const ports[] = {__VA_ARGS__};                               \
        for (uint8_t i = 0; i < ARRAY_LENGTH(ports); i++) {                    \
            connect_ports(&(node).ports[i], ports[i]);                         \
        }                                                                      \
    } while (0)

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

// Multifocuses Functionality
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#define INITIAL_MULTIFOCUS_CAPACITY 4096

struct multifocus {
    size_t count, capacity;
    struct node *array;
};

COMPILER_RETURNS_NONNULL COMPILER_COLD //
static struct multifocus *
alloc_focus(const size_t initial_capacity) {
    XASSERT(initial_capacity > 0);

    struct multifocus *const focus = xmalloc(sizeof *focus);
    focus->count = 0;
    focus->capacity = initial_capacity;
    focus->array = xmalloc(sizeof focus->array[0] * initial_capacity);

    return focus;
}

COMPILER_COLD //
static void
free_focus(struct multifocus *const restrict focus) {
    if (focus) {
        XASSERT(focus->count <= focus->capacity);
        free(focus->array);
        free(focus);
    }
}

COMPILER_NONNULL(1) COMPILER_COLD //
static void
expand_focus(struct multifocus *const restrict focus) {
    MY_ASSERT(focus);
    XASSERT(focus->count == focus->capacity);

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

    if (focus->count == focus->capacity) { expand_focus(focus); }

    focus->array[focus->count++] = node;
}

#ifdef OPTISCOPE_ENABLE_GRAPHVIZ

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT //
static bool
is_focused_on(struct multifocus *const restrict focus, const struct node node) {
    MY_ASSERT(focus);
    XASSERT(node.ports);

    for (size_t i = 0; i < focus->count; i++) {
        if (focus->array[i].ports == node.ports) { return true; }
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

    return focus->array[--focus->count];
}

COMPILER_NONNULL(1) COMPILER_HOT COMPILER_ALWAYS_INLINE //
inline static struct node
unfocus_or(
    struct multifocus *const restrict focus, //
    const struct node fallback) {
    MY_ASSERT(focus);
    XASSERT(focus->count <= focus->capacity);

    return focus->count > 0 ? unfocus(focus) : fallback;
}

#define CONSUME_MULTIFOCUS(focus, f)                                           \
    for (struct node f = {NULL};                                               \
         (focus)->count > 0 ? (f = unfocus((focus)), true) : false;            \
         (void)0)

// Main Context Functionality
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

// clang-format off
#define CONTEXT_MULTIFOCUSES \
    X(betas) X(closed_betas) X(identity_betas) X(gc_betas) \
        X(expansions) \
    X(unary_calls) X(binary_calls) X(binary_calls_aux) X(if_then_elses) \
        X(performs) \
    X(commutations) X(annihilations)
// clang-format on

struct context {
    // The root node of the graph (alwaies `SYMBOL_ROOT`).
    struct node root;

    // The current global phase of the reduction.
    uint64_t phase;

    // Indicates whether the interface normal form has been reached.
    bool time_to_stop;

    // The multifocuses for the respective procedures.
    struct multifocus *gc_focus, *unshare_focus;

#define X(focus_name) struct multifocus *focus_name;
    // The multifocuses for full reduction.
    CONTEXT_MULTIFOCUSES
#undef X

#ifdef OPTISCOPE_ENABLE_STATS
#define X(focus_name) uint64_t n##focus_name;
    // The numbers of proper interactions.
    CONTEXT_MULTIFOCUSES
#undef X
    // The number of bookkeeping ("oracle") interactions.
    uint64_t nbookkeeping_interactions;
    // The numbers of non-interaction graph rewrites.
    uint64_t nmergings, ngc;
    // The memory usage statistics.
    uint64_t nduplicators, ndelimiters, ntotal, //
        nmax_duplicators, nmax_delimiters, nmax_total;
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
    root.ports[0] = PORT_VALUE(UINT64_C(0), PHASE_REDUCE_WEAKLY, UINT64_C(0));

    struct context *const graph = xmalloc(sizeof *graph);
    graph->root = root;
    graph->phase = PHASE_REDUCE_WEAKLY;
    graph->time_to_stop = false;

#define X(focus_name) graph->focus_name = NULL;
    CONTEXT_MULTIFOCUSES
#undef X

#ifdef OPTISCOPE_ENABLE_STATS
#define X(focus_name) graph->n##focus_name = 0;
    CONTEXT_MULTIFOCUSES
#undef X
    graph->nbookkeeping_interactions = 0;
    graph->nmergings = graph->ngc = 0;
    graph->nduplicators = graph->ndelimiters = graph->ntotal = //
        graph->nmax_duplicators = graph->nmax_delimiters = graph->nmax_total =
            0;
#endif

    graph->gc_focus = alloc_focus(INITIAL_MULTIFOCUS_CAPACITY);
    graph->unshare_focus = alloc_focus(INITIAL_MULTIFOCUS_CAPACITY);

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

#define X(focus_name) free_focus(graph->focus_name);
    CONTEXT_MULTIFOCUSES
    X(gc_focus)
    X(unshare_focus)
#undef X

    free(graph);
}

COMPILER_PURE COMPILER_NONNULL(1) //
inline static bool
is_normalized_graph(const struct context *const restrict graph) {
    MY_ASSERT(graph);

#define X(focus_name) (0 == graph->focus_name->count) &&
    return CONTEXT_MULTIFOCUSES true;
#undef X
}

#ifdef OPTISCOPE_ENABLE_STATS

COMPILER_NONNULL(1) //
static void
print_stats(const struct context *const restrict graph) {
    MY_ASSERT(graph);

    const uint64_t ncell_operations = //
        graph->nunary_calls + graph->nbinary_calls + graph->nbinary_calls_aux +
        graph->nif_then_elses;

    const uint64_t ntotal_interactions = //
        graph->nbetas + graph->ncommutations + graph->nannihilations +
        graph->nexpansions + ncell_operations;

    const uint64_t ntotal_rewrites = //
        ntotal_interactions + graph->nmergings + graph->ngc;

    const uint64_t nbookkeeping_rewrites = //
        graph->nbookkeeping_interactions + graph->nmergings;

    const double bookkeeping_percentage = //
        ((double)nbookkeeping_rewrites / (double)ntotal_rewrites) * 100.0;

    printf("  Family reductions: %" PRIu64 "\n", graph->nbetas);
    printf("       Commutations: %" PRIu64 "\n", graph->ncommutations);
    printf("      Annihilations: %" PRIu64 "\n", graph->nannihilations);
    printf("         Expansions: %" PRIu64 "\n", graph->nexpansions);
    printf("    Cell operations: %" PRIu64 "\n", ncell_operations);
    printf(" Total interactions: %" PRIu64 "\n", ntotal_interactions);
    printf("Garbage collections: %" PRIu64 "\n", graph->ngc);
    printf(" Delimiter mergings: %" PRIu64 "\n", graph->nmergings);
    printf("     Total rewrites: %" PRIu64 "\n", ntotal_rewrites);
    printf("   Bookkeeping work: %.2f%%\n", bookkeeping_percentage);
    printf("    Max duplicators: %" PRIu64 "\n", graph->nmax_duplicators);
    printf("     Max delimiters: %" PRIu64 "\n", graph->nmax_delimiters);
    printf("    Max total nodes: %" PRIu64 "\n", graph->nmax_total);
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

    // While it might seem that preallocation caches can increase performance,
    // in fact, they introduced almost a 2x slowdown.
    (void)0;

    uint64_t *ports = NULL;

#define SET_PORTS_0()                                                          \
    (ports[0] = PORT_VALUE(UINT64_C(0), graph->phase, UINT64_C(0)))
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
        ports = ALLOC_POOL_OBJECT(applicator_pool), SET_PORTS_2();
        break;
    case SYMBOL_LAMBDA:
        ports = ALLOC_POOL_OBJECT(lambda_pool), SET_PORTS_2();
        break;
    case SYMBOL_ERASER:
        ports = ALLOC_POOL_OBJECT(eraser_pool), SET_PORTS_0();
        break;
    case SYMBOL_S:
        ports = ALLOC_POOL_OBJECT(scope_pool), SET_PORTS_1();
        break;
        // clang-format on
    case SYMBOL_CELL:
        ports = ALLOC_POOL_OBJECT(cell_pool);
        if (prototype) { ports[1] = prototype->ports[1]; }
        SET_PORTS_0();
        break;
    case SYMBOL_UNARY_CALL:
        ports = ALLOC_POOL_OBJECT(unary_call_pool);
        if (prototype) { ports[2] = prototype->ports[2]; }
        SET_PORTS_1();
        break;
    case SYMBOL_BINARY_CALL:
        ports = ALLOC_POOL_OBJECT(binary_call_pool);
        if (prototype) { ports[3] = prototype->ports[3]; }
        SET_PORTS_2();
        break;
    case SYMBOL_BINARY_CALL_AUX:
        ports = ALLOC_POOL_OBJECT(binary_call_aux_pool);
        if (prototype) {
            ports[2] = prototype->ports[2], ports[3] = prototype->ports[3];
        }
        SET_PORTS_1();
        break;
    case SYMBOL_IF_THEN_ELSE:
        ports = ALLOC_POOL_OBJECT(if_then_else_pool), SET_PORTS_3();
        break;
    case SYMBOL_PERFORM:
        ports = ALLOC_POOL_OBJECT(perform_pool), SET_PORTS_2();
        break;
    case SYMBOL_IDENTITY_LAMBDA:
        ports = ALLOC_POOL_OBJECT(identity_lambda_pool), SET_PORTS_0();
        break;
    case SYMBOL_GC_LAMBDA:
        ports = ALLOC_POOL_OBJECT(gc_lambda_pool), SET_PORTS_1();
        break;
    case SYMBOL_LAMBDA_C:
        ports = ALLOC_POOL_OBJECT(lambda_c_pool), SET_PORTS_2();
        break;
    case SYMBOL_REFERENCE:
        ports = ALLOC_POOL_OBJECT(reference_pool), SET_PORTS_0();
        if (prototype) { ports[1] = prototype->ports[1]; }
        break;
    duplicator:
        ports = ALLOC_POOL_OBJECT(duplicator_pool), SET_PORTS_2();
#ifdef OPTISCOPE_ENABLE_STATS
        graph->nduplicators++;
        if (graph->nmax_duplicators < graph->nduplicators) {
            graph->nmax_duplicators = graph->nduplicators;
        }
#endif
        break;
    delimiter:
        ports = ALLOC_POOL_OBJECT(delimiter_pool);
        if (prototype) { ports[2] = prototype->ports[2]; }
        SET_PORTS_1();
#ifdef OPTISCOPE_ENABLE_STATS
        graph->ndelimiters++;
        if (graph->nmax_delimiters < graph->ndelimiters) {
            graph->nmax_delimiters = graph->ndelimiters;
        }
#endif
        break;
    default:
        if (symbol <= MAX_DUPLICATOR_INDEX) goto duplicator;
        else if (symbol <= MAX_DELIMITER_INDEX) goto delimiter;
        else COMPILER_UNREACHABLE();
    }

#ifdef OPTISCOPE_ENABLE_STATS
    graph->ntotal++;
    if (graph->ntotal > graph->nmax_total) {
        graph->nmax_total = graph->ntotal;
    }
#endif

    ports[-1] = symbol;

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
    case SYMBOL_APPLICATOR: FREE_POOL_OBJECT(applicator_pool, p); break;
    case SYMBOL_LAMBDA: FREE_POOL_OBJECT(lambda_pool, p); break;
    case SYMBOL_ERASER: FREE_POOL_OBJECT(eraser_pool, p); break;
    case SYMBOL_S: FREE_POOL_OBJECT(scope_pool, p); break;
    case SYMBOL_CELL: FREE_POOL_OBJECT(cell_pool, p); break;
    case SYMBOL_UNARY_CALL: FREE_POOL_OBJECT(unary_call_pool, p); break;
    case SYMBOL_BINARY_CALL: FREE_POOL_OBJECT(binary_call_pool, p); break;
    case SYMBOL_BINARY_CALL_AUX:
        FREE_POOL_OBJECT(binary_call_aux_pool, p);
        break;
    case SYMBOL_IF_THEN_ELSE: FREE_POOL_OBJECT(if_then_else_pool, p); break;
    case SYMBOL_PERFORM: FREE_POOL_OBJECT(perform_pool, p); break;
    case SYMBOL_IDENTITY_LAMBDA:
        FREE_POOL_OBJECT(identity_lambda_pool, p);
        break;
    case SYMBOL_GC_LAMBDA: FREE_POOL_OBJECT(gc_lambda_pool, p); break;
    case SYMBOL_LAMBDA_C: FREE_POOL_OBJECT(lambda_c_pool, p); break;
    case SYMBOL_REFERENCE: FREE_POOL_OBJECT(reference_pool, p); break;
    default:
        if (symbol <= MAX_DUPLICATOR_INDEX) goto duplicator;
        else if (symbol <= MAX_DELIMITER_INDEX) goto delimiter;
        else COMPILER_UNREACHABLE();
    duplicator:
        FREE_POOL_OBJECT(duplicator_pool, p);
#ifdef OPTISCOPE_ENABLE_STATS
        graph->nduplicators--;
#endif
        break;
    delimiter:
        FREE_POOL_OBJECT(delimiter_pool, p);
#ifdef OPTISCOPE_ENABLE_STATS
        graph->ndelimiters--;
#endif
        break;
    }

#ifdef OPTISCOPE_ENABLE_STATS
    graph->ntotal--;
#endif
}

// Delimiter Compression
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

struct delimiter {
    const uint64_t idx;
    uint64_t *const restrict points_to, *const restrict goes_from;
};

#ifndef NDEBUG

static void
assert_delimiter_template(const struct delimiter template) {
    MY_ASSERT(template.idx < INDEX_RANGE);
    MY_ASSERT(template.points_to), MY_ASSERT(template.goes_from);
}

#else

#define assert_delimiter_template(template) ((void)0)

#endif // NDEBUG

COMPILER_NONNULL(1) COMPILER_HOT //
static void
inst_delimiter_as_is(
    struct context *const restrict graph, const struct delimiter template) {
    MY_ASSERT(graph);
    assert_delimiter_template(template);

    const struct node delim = alloc_node(graph, SYMBOL_DELIMITER(template.idx));
    delim.ports[2] = 1;
    connect_ports(&delim.ports[0], template.points_to);
    connect_ports(&delim.ports[1], template.goes_from);
}

COMPILER_NONNULL(1) COMPILER_HOT //
static void
inst_delimiter(
    struct context *const restrict graph, const struct delimiter template) {
    MY_ASSERT(graph);
    assert_delimiter_template(template);

    const struct node g = node_of_port(&template.points_to[0]);
    XASSERT(g.ports);

    const bool condition = PHASE_REDUCE_WEAKLY == graph->phase &&
                           IS_DELIMITER(g.ports[-1]) &&
                           1 == DECODE_OFFSET_METADATA(*template.points_to) &&
                           SYMBOL_DELIMITER(template.idx) == g.ports[-1];
    if (condition) {
        g.ports[2]++;
        connect_ports(&g.ports[1], template.goes_from);
#ifdef OPTISCOPE_ENABLE_STATS
        graph->nmergings++;
#endif
    } else {
        inst_delimiter_as_is(graph, template);
    }
}

COMPILER_NONNULL(1) COMPILER_HOT //
static void
try_merge_delimiter(struct context *const restrict graph, const struct node f) {
    MY_ASSERT(graph);
    XASSERT(f.ports);
    XASSERT(IS_DELIMITER(f.ports[-1]));
    MY_ASSERT(PHASE_REDUCE_WEAKLY == graph->phase);

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
    MY_ASSERT(PHASE_REDUCE_WEAKLY == graph->phase);

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
    MY_ASSERT(PHASE_REDUCE_WEAKLY == graph->phase);

    const struct node g = follow_port(&f.ports[0]);
    XASSERT(g.ports);

    if (is_atomic_symbol(g.ports[-1])) {
        const struct node gx = alloc_node_from(graph, g.ports[-1], &g);
        connect_ports(&g.ports[0], DECODE_ADDRESS(f.ports[1]));
        connect_ports(&gx.ports[0], DECODE_ADDRESS(f.ports[2]));
        free_node(graph, f);
#ifdef OPTISCOPE_ENABLE_STATS
        graph->ncommutations++;
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
graphviz_edge_label(
    const struct node node, const uint8_t i, const uint64_t phase) {
    XASSERT(node.ports);

    static char buffer[16] = {0};

    switch (node.ports[-1]) {
    case SYMBOL_APPLICATOR:
        if ((phase >= PHASE_UNWIND ? 0 : 1) == i) {
            sprintf(buffer, "\\#%" PRIu8, i);
        } else if ((phase >= PHASE_UNWIND ? 2 : 0) == i) {
            sprintf(buffer, "rator (\\#%" PRIu8 ")", i);
        } else if ((phase >= PHASE_UNWIND ? 1 : 2) == i) {
            sprintf(buffer, "rand (\\#%" PRIu8 ")", i);
        } else {
            COMPILER_UNREACHABLE();
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
    default: sprintf(buffer, "\\#%" PRIu8, i);
    }

    return buffer;
}

COMPILER_PURE COMPILER_WARN_UNUSED_RESULT //
static uint8_t
graphviz_edge_weight(
    const struct node node, const uint8_t i, const uint64_t phase) {
    if (is_active(node) && 0 == i) { return 3; }

    switch (node.ports[-1]) {
    case SYMBOL_APPLICATOR:
        if ((phase >= PHASE_UNWIND ? 2 : 0) == i) return 5; // rator
        if ((phase >= PHASE_UNWIND ? 1 : 2) == i) return 5; // rand
        break;
    case SYMBOL_LAMBDA:
    case SYMBOL_LAMBDA_C:
        if (2 == i) return 3; // body
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
    const struct node node, const uint8_t i, const uint64_t phase) {
    XASSERT(node.ports);

    switch (node.ports[-1]) {
    case SYMBOL_ROOT:
        switch (i) {
        case 0: return "s";
        default: COMPILER_UNREACHABLE();
        }
    case SYMBOL_S:
    case SYMBOL_GC_LAMBDA:
        switch (i) {
        case 0: return "n";
        case 1: return "s";
        default: COMPILER_UNREACHABLE();
        }
    case SYMBOL_APPLICATOR:
        if ((phase >= PHASE_UNWIND ? 0 : 1) == i) return "n";
        else if ((phase >= PHASE_UNWIND ? 2 : 0) == i) return "s";
        else if ((phase >= PHASE_UNWIND ? 1 : 2) == i) return "e";
        else COMPILER_UNREACHABLE();
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
    delimiter:
    case SYMBOL_UNARY_CALL:
    case SYMBOL_BINARY_CALL_AUX:
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
    } else if (IS_DELIMITER(node.ports[-1])) {
        SPRINTF(" %" PRIu64, node.ports[2]);
    }

#undef SPRINTF

    return buffer;
}

struct graphviz_context {
    struct context *graph;
    struct multifocus *history;
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
    MY_ASSERT(ctx), XASSERT(ctx->stream);
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
        GRAPHVIZ_INDENT "n%p -> n%p [label=\" %s \", weight=%" PRIu8 ", tailport=%s"
        "%s%s%s%s];\n",
        // clang-format on
        (void *)source.ports,
        (void *)target.ports,
        graphviz_edge_label(source, i, ctx->graph->phase),
        graphviz_edge_weight(source, i, ctx->graph->phase),
        graphviz_edge_tailport(source, i, ctx->graph->phase),
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
    MY_ASSERT(ctx), XASSERT(ctx->stream);
    XASSERT(source.ports);

    const struct node node = follow_port(&source.ports[i]);

    if (is_focused_on(ctx->history, node)) { return; }

    focus_on(ctx->history, node);

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
    XASSERT(f.ports), XASSERT(g.ports);
    XASSERT(i >= 0 && (uint64_t)i < MAX_PORTS);
    XASSERT(graph->gc_focus);
    XASSERT(SYMBOL_ERASER == f.ports[-1]);

    switch (g.ports[-1]) {
    commute_1_2: {
        connect_ports(&f.ports[0], DECODE_ADDRESS(g.ports[0 == i ? 1 : 0]));

        focus_on(graph->gc_focus, f);

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

        focus_on(graph->gc_focus, f);
        focus_on(graph->gc_focus, fx);

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

        focus_on(graph->gc_focus, f);
        focus_on(graph->gc_focus, fx);
        focus_on(graph->gc_focus, fxx);

        free_node(graph, g);

#ifdef OPTISCOPE_ENABLE_STATS
        graph->ngc++;
#endif
        break;
    }
    annihilate:
        free_node(graph, f), free_node(graph, g);

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
                              shareable = node_of_port(points_to);

            if (SYMBOL_ERASER == h.ports[-1]) {
                connect_ports(&f.ports[0], points_to);
                focus_on(graph->gc_focus, f);
                free_node(graph, g);
                if (PHASE_GC == DECODE_PHASE_METADATA(h.ports[0])) {
                    set_phase(&h.ports[0], PHASE_GC_AUX);
                } else {
                    free_node(graph, h);
                }
#ifdef OPTISCOPE_ENABLE_STATS
                graph->ngc++;
#endif
            } else if (is_atomic_symbol(shareable.ports[-1])) {
                connect_ports(&shareable.ports[0], shares_with);
                free_node(graph, g), free_node(graph, f);
#ifdef OPTISCOPE_ENABLE_STATS
                graph->ngc++;
#endif
            } else if (0 == symbol_index(g.ports[-1])) {
                connect_ports(points_to, shares_with);
                free_node(graph, g);
#ifdef OPTISCOPE_ENABLE_STATS
                graph->ngc++;
#endif
            } else {
                set_phase(&f.ports[0], PHASE_REDUCE_WEAKLY);
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
    XASSERT(graph->gc_focus);

    const struct node top_eraser = alloc_gc_node(graph, port);

    // On later algorithmic phases, we just connect the erasable port with a new
    // eraser, for garbage collection does not provide considerable benefit
    // after the weak reduction phase.
    if (PHASE_REDUCE_WEAKLY != graph->phase) {
        set_phase(&top_eraser.ports[0], graph->phase);
        return;
    }

    focus_on(graph->gc_focus, top_eraser);

    CONSUME_MULTIFOCUS (graph->gc_focus, f) {
        XASSERT(f.ports);

        if (PHASE_GC_AUX == DECODE_PHASE_METADATA(f.ports[0])) {
            free_node(graph, f);
        } else {
            uint64_t *const points_to = DECODE_ADDRESS(f.ports[0]);

            const struct node g = node_of_port(points_to);
            XASSERT(g.ports);

            switch (DECODE_PHASE_METADATA(g.ports[0])) {
            // Two garbage-collecting erasers are interacting. The algorithm is
            // as follows: free `f` & mark `g` for future encountering; when the
            // multifocus encounters `g`, it will instantly free it without
            // following its port.
            case PHASE_GC:
                free_node(graph, f);
                set_phase(&g.ports[0], PHASE_GC_AUX);
                break;
            // We shall not deallocate nodes from the reduction stack.
            case PHASE_STACK:
                set_phase(&f.ports[0], PHASE_REDUCE_WEAKLY);
                break;
            default: //
                gc_step(graph, f, g, points_to - g.ports);
            }
        }
    }
}

// Eager Atomic Unsharing
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

// Eliminates a (higher-order) sharing structure, thus reducing the graph size.
COMPILER_WARN_UNUSED_RESULT COMPILER_NONNULL(1, 2) //
static bool
try_unshare(
    struct context *const restrict graph,
    uint64_t *const restrict port,
    const struct node atom) {
    debug("%s(%p, %s)", __func__, (void *)port, print_node(atom));

    MY_ASSERT(graph);
    MY_ASSERT(port);
    XASSERT(atom.ports);
    XASSERT(graph->unshare_focus);

    if (!is_atomic_symbol(atom.ports[-1])) { return false; }

    connect_ports(&atom.ports[0], port);

    focus_on(graph->unshare_focus, atom);

    CONSUME_MULTIFOCUS (graph->unshare_focus, f) {
        XASSERT(f.ports);

        const struct node g = follow_port(&f.ports[0]);
        XASSERT(g.ports);

        if (!is_interacting_with(f, g)) { continue; }

        if (IS_DUPLICATOR(g.ports[-1])) {
            const struct node fx = alloc_node_from(graph, f.ports[-1], &f);

            connect_ports(&f.ports[0], DECODE_ADDRESS(g.ports[1]));
            connect_ports(&fx.ports[0], DECODE_ADDRESS(g.ports[2]));

            focus_on(graph->unshare_focus, f),
                focus_on(graph->unshare_focus, fx);

#ifdef OPTISCOPE_ENABLE_STATS
            graph->ncommutations++;
#endif
            free_node(graph, g);
        } else if (IS_DELIMITER(g.ports[-1])) {
            connect_ports(&f.ports[0], DECODE_ADDRESS(g.ports[1]));

            focus_on(graph->unshare_focus, f);

#ifdef OPTISCOPE_ENABLE_STATS
            graph->ncommutations++;
#endif
            free_node(graph, g);
        }
    }

    return true;
}

// Core Interaction Rules
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

COMPILER_NONNULL(1, 2, 3) // forward declaration for expanding references
static void
of_lambda_term(
    struct context *const restrict graph,
    struct lambda_term *const restrict term,
    uint64_t *const restrict output_port,
    const uint64_t lvl);

#ifndef NDEBUG

static void
assert_annihilation(const struct node f, const struct node g) {
    MY_ASSERT(f.ports), MY_ASSERT(g.ports);
    MY_ASSERT(is_interaction(f, g));
    MY_ASSERT(f.ports[-1] == g.ports[-1]);
}

static void
assert_beta(
    const struct context *const restrict graph,
    const struct node f,
    const struct node g) {
    MY_ASSERT(graph);
    MY_ASSERT(graph->phase < PHASE_UNWIND);
    MY_ASSERT(f.ports), MY_ASSERT(g.ports);
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
    MY_ASSERT(graph->phase < PHASE_UNWIND);
    MY_ASSERT(f.ports), MY_ASSERT(g.ports);
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
    MY_ASSERT(graph->phase < PHASE_UNWIND);
    MY_ASSERT(f.ports), MY_ASSERT(g.ports);
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
    MY_ASSERT(graph->phase < PHASE_UNWIND);
    MY_ASSERT(f.ports), MY_ASSERT(g.ports);
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
    MY_ASSERT(graph->phase < PHASE_UNWIND);
    MY_ASSERT(f.ports), MY_ASSERT(g.ports);
    MY_ASSERT(is_interaction(f, g));
    MY_ASSERT(SYMBOL_REFERENCE == f.ports[-1]);
    MY_ASSERT(is_operator_symbol(g.ports[-1]));
}

static void
assert_commutation(const struct node f, const struct node g) {
    MY_ASSERT(f.ports), MY_ASSERT(g.ports);
    MY_ASSERT(is_interaction(f, g));
    MY_ASSERT(f.ports[-1] != g.ports[-1]);
}

static void
assert_unary_call(
    const struct context *const restrict graph,
    const struct node f,
    const struct node g) {
    MY_ASSERT(graph);
    MY_ASSERT(graph->phase < PHASE_UNWIND);
    MY_ASSERT(f.ports), MY_ASSERT(g.ports);
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
    MY_ASSERT(graph->phase < PHASE_UNWIND);
    MY_ASSERT(f.ports), MY_ASSERT(g.ports);
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
    MY_ASSERT(graph->phase < PHASE_UNWIND);
    MY_ASSERT(f.ports), MY_ASSERT(g.ports);
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
    MY_ASSERT(graph->phase < PHASE_UNWIND);
    MY_ASSERT(f.ports), MY_ASSERT(g.ports);
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
    MY_ASSERT(graph->phase < PHASE_UNWIND);
    MY_ASSERT(f.ports), MY_ASSERT(g.ports);
    MY_ASSERT(is_interaction(f, g));
    MY_ASSERT(SYMBOL_PERFORM == f.ports[-1]);
    MY_ASSERT(SYMBOL_CELL == g.ports[-1]);
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
    graph->current_pair[0] = f, graph->current_pair[1] = g;
#endif

    char f_ssymbol[MAX_SSYMBOL_SIZE] = {0}, g_ssymbol[MAX_SSYMBOL_SIZE] = {0};
    strcpy(f_ssymbol, print_symbol(f.ports[-1])),
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
    XASSERT(f.ports), XASSERT(g.ports);
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

    free_node(graph, f), free_node(graph, g);
}

TYPE_CHECK_RULE(annihilate);

RULE_DEFINITION(commute, graph, f, g) {
    MY_ASSERT(graph);
    XASSERT(f.ports), XASSERT(g.ports);
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

    // Hint the compiler to automatically unroll the following loops.
    XASSERT(m <= MAX_AUXILIARY_PORTS), XASSERT(n <= MAX_AUXILIARY_PORTS);

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

    free_node(graph, f), free_node(graph, g);
}

TYPE_CHECK_RULE(commute);

RULE_DEFINITION(beta, graph, f, g) {
    MY_ASSERT(graph);
    XASSERT(f.ports), XASSERT(g.ports);
    assert_beta(graph, f, g);
    debug_interaction(__func__, graph, f, g);

#ifdef OPTISCOPE_ENABLE_STATS
    graph->nbetas++;
#endif

    inst_delimiter(
        graph,
        (struct delimiter){
            .idx = 0, DECODE_ADDRESS(f.ports[1]), DECODE_ADDRESS(g.ports[2])});

    uint64_t *const binder_port = DECODE_ADDRESS(g.ports[1]), //
        *const rand_port = DECODE_ADDRESS(f.ports[2]);
    const struct node rand = node_of_port(rand_port);
    if (SYMBOL_LAMBDA_C == rand.ports[-1]) {
        connect_ports(binder_port, rand_port);
    } else if (!try_unshare(graph, binder_port, rand)) {
        inst_delimiter(
            graph, (struct delimiter){.idx = 0, rand_port, binder_port});
    }

    free_node(graph, f), free_node(graph, g);
}

TYPE_CHECK_RULE(beta);

RULE_DEFINITION(beta_c, graph, f, g) {
    MY_ASSERT(graph);
    XASSERT(f.ports), XASSERT(g.ports);
    assert_beta_c(graph, f, g);
    debug_interaction(__func__, graph, f, g);

#ifdef OPTISCOPE_ENABLE_STATS
    graph->nbetas++;
#endif

    connect_ports(DECODE_ADDRESS(f.ports[1]), DECODE_ADDRESS(g.ports[2]));

    uint64_t *const binder_port = DECODE_ADDRESS(g.ports[1]), //
        *const rand_port = DECODE_ADDRESS(f.ports[2]);
    const struct node rand = node_of_port(rand_port);
    if (!try_unshare(graph, binder_port, rand)) {
        connect_ports(rand_port, binder_port);
    }

    free_node(graph, f), free_node(graph, g);
}

TYPE_CHECK_RULE(beta_c);

RULE_DEFINITION(identity_beta, graph, f, g) {
    MY_ASSERT(graph);
    XASSERT(f.ports), XASSERT(g.ports);
    assert_identity_beta(graph, f, g);
    debug_interaction(__func__, graph, f, g);

#ifdef OPTISCOPE_ENABLE_STATS
    graph->nbetas++;
#endif

    connect_ports(DECODE_ADDRESS(f.ports[1]), DECODE_ADDRESS(f.ports[2]));

    free_node(graph, f), free_node(graph, g);
}

TYPE_CHECK_RULE(identity_beta);

RULE_DEFINITION(gc_beta, graph, f, g) {
    MY_ASSERT(graph);
    XASSERT(f.ports), XASSERT(g.ports);
    assert_gc_beta(graph, f, g);
    debug_interaction(__func__, graph, f, g);

#ifdef OPTISCOPE_ENABLE_STATS
    graph->nbetas++;
#endif

    inst_delimiter(
        graph,
        (struct delimiter){
            .idx = 0, DECODE_ADDRESS(f.ports[1]), DECODE_ADDRESS(g.ports[1])});

    // There is a chance that the argument is fully disconnected from the root;
    // if so, we must garbage-collect it.
    gc(graph, DECODE_ADDRESS(f.ports[2]));

    free_node(graph, f), free_node(graph, g);
}

TYPE_CHECK_RULE(gc_beta);

RULE_DEFINITION(do_expand, graph, f, g) {
    MY_ASSERT(graph);
    XASSERT(f.ports), XASSERT(g.ports);
    assert_expand(graph, f, g);
    debug_interaction(__func__, graph, f, g);

#ifdef OPTISCOPE_ENABLE_STATS
    graph->nexpansions++;
#endif

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
    of_lambda_term(graph, USER_FUNCTION_OF_U64(f.ports[1])(), &g.ports[0], 0);
#pragma GCC diagnostic pop

    free_node(graph, f);
}

TYPE_CHECK_RULE(do_expand);

RULE_DEFINITION(do_unary_call, graph, f, g) {
    MY_ASSERT(graph);
    XASSERT(f.ports), XASSERT(g.ports);
    assert_unary_call(graph, f, g);
    debug_interaction(__func__, graph, f, g);

#ifdef OPTISCOPE_ENABLE_STATS
    graph->nunary_calls++;
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
    XASSERT(f.ports), XASSERT(g.ports);
    assert_binary_call(graph, f, g);
    debug_interaction(__func__, graph, f, g);

#ifdef OPTISCOPE_ENABLE_STATS
    graph->nbinary_calls++;
#endif

    const struct node aux = alloc_node(graph, SYMBOL_BINARY_CALL_AUX);
    connect_ports(&aux.ports[1], DECODE_ADDRESS(f.ports[1]));
    aux.ports[2] = f.ports[3];
    aux.ports[3] = g.ports[1];
    connect_ports(&aux.ports[0], DECODE_ADDRESS(f.ports[2]));

    free_node(graph, f), free_node(graph, g);
}

TYPE_CHECK_RULE(do_binary_call);

RULE_DEFINITION(do_binary_call_aux, graph, f, g) {
    MY_ASSERT(graph);
    XASSERT(f.ports), XASSERT(g.ports);
    assert_binary_call_aux(graph, f, g);
    debug_interaction(__func__, graph, f, g);

#ifdef OPTISCOPE_ENABLE_STATS
    graph->nbinary_calls_aux++;
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
    XASSERT(f.ports), XASSERT(g.ports);
    assert_if_then_else(graph, f, g);
    debug_interaction(__func__, graph, f, g);

#ifdef OPTISCOPE_ENABLE_STATS
    graph->nif_then_elses++;
#endif

    uint64_t *const if_then = DECODE_ADDRESS(f.ports[3]), //
        *const if_else = DECODE_ADDRESS(f.ports[2]);

    uint64_t *choose, *discard;
    if (g.ports[1]) choose = if_then, discard = if_else;
    else choose = if_else, discard = if_then;

    connect_ports(DECODE_ADDRESS(f.ports[1]), choose);
    gc(graph, discard);

    free_node(graph, f), free_node(graph, g);
}

TYPE_CHECK_RULE(do_if_then_else);

RULE_DEFINITION(do_perform, graph, f, g) {
    MY_ASSERT(graph);
    XASSERT(f.ports), XASSERT(g.ports);
    assert_perform(graph, f, g);
    debug_interaction(__func__, graph, f, g);

    if (PHASE_REDUCE_WEAKLY != graph->phase) {
        panic("Side effects are only allowed during weak reduction!");
    }

#ifdef OPTISCOPE_ENABLE_STATS
    graph->nperforms++;
#endif

    connect_ports(DECODE_ADDRESS(f.ports[1]), DECODE_ADDRESS(f.ports[2]));

    free_node(graph, f), free_node(graph, g);
}

TYPE_CHECK_RULE(do_perform);

COMPILER_NONNULL(1, 2) COMPILER_HOT //
static void
interact(
    struct context *const restrict graph,
    const Rule rule,
    const struct node f) {
    MY_ASSERT(graph);
    MY_ASSERT(rule);
    XASSERT(f.ports);
    MY_ASSERT(graph->phase > PHASE_REDUCE_WEAKLY);

    const struct node g = follow_port(&f.ports[0]);
    XASSERT(g.ports);

    rule(graph, f, g);
}

// Specialized Annihilation Rules
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#define ANNIHILATION_PROLOGUE(graph, f, g)                                     \
    do {                                                                       \
        MY_ASSERT(graph);                                                      \
        XASSERT(f.ports), XASSERT(g.ports);                                    \
        MY_ASSERT(PHASE_REDUCE_WEAKLY == graph->phase);                        \
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
    XASSERT(f.ports[2] > 0), XASSERT(g.ports[2] > 0);

    if (f.ports[2] == g.ports[2]) {
        connect_ports(DECODE_ADDRESS(f.ports[1]), DECODE_ADDRESS(g.ports[1]));
        free_node(graph, f), free_node(graph, g);
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

    free_node(graph, f), free_node(graph, g);
}

TYPE_CHECK_RULE(annihilate_dup_dup);

#undef NANNIHILATIONS_PLUS_PLUS
#undef ANNIHILATION_PROLOGUE

// Specialized Commutation Rules
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#define COMMUTATION_PROLOGUE(graph, f, g)                                      \
    do {                                                                       \
        MY_ASSERT(graph);                                                      \
        XASSERT(f.ports), XASSERT(g.ports);                                    \
        MY_ASSERT(PHASE_REDUCE_WEAKLY == graph->phase);                        \
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

    if (symbol_index(f.ports[-1]) > symbol_index(g.ports[-1])) {
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

// Rule Dispatching
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#ifdef OPTISCOPE_ENABLE_STATS

#define COUNT_BOOKKEEPING(graph, fsym, gsym)                                   \
    do {                                                                       \
        if (IS_DELIMITER((fsym)) || IS_DELIMITER((gsym))) {                    \
            (graph)->nbookkeeping_interactions++;                              \
        }                                                                      \
    } while (false)

#else

#define COUNT_BOOKKEEPING(graph, fsym, gsym) ((void)0)

#endif // OPTISCOPE_ENABLE_STATS

#define DISPATCH_ACTIVE_PAIR(graph, f, g)                                      \
    do {                                                                       \
        const uint64_t fsym = f.ports[-1], gsym = g.ports[-1];                 \
                                                                               \
        COUNT_BOOKKEEPING(graph, fsym, gsym);                                  \
                                                                               \
        switch (fsym) {                                                        \
        duplicator:                                                            \
            if (fsym == gsym) ANNIHILATE_DUP_DUP(graph, f, g);                 \
            else if (SYMBOL_APPLICATOR == gsym) COMMUTE_APPL_DUP(graph, g, f); \
            else if (SYMBOL_LAMBDA == gsym) COMMUTE_LAMBDA_DUP(graph, g, f);   \
            else if (SYMBOL_IDENTITY_LAMBDA == gsym)                           \
                COMMUTE_IDENTITY_LAMBDA_DUP(graph, g, f);                      \
            else if (SYMBOL_GC_LAMBDA == gsym)                                 \
                COMMUTE_GC_LAMBDA_DUP(graph, g, f);                            \
            else if (SYMBOL_LAMBDA_C == gsym)                                  \
                COMMUTE_LAMBDA_C_DUP(graph, g, f);                             \
            else if (SYMBOL_CELL == gsym) COMMUTE_CELL_DUP(graph, g, f);       \
            else if (SYMBOL_UNARY_CALL == gsym)                                \
                COMMUTE_UCALL_DUP(graph, g, f);                                \
            else if (SYMBOL_BINARY_CALL == gsym)                               \
                COMMUTE_BCALL_DUP(graph, g, f);                                \
            else if (SYMBOL_BINARY_CALL_AUX == gsym)                           \
                COMMUTE_BCALL_AUX_DUP(graph, g, f);                            \
            else if (SYMBOL_IF_THEN_ELSE == gsym)                              \
                COMMUTE_ITE_DUP(graph, g, f);                                  \
            else if (SYMBOL_REFERENCE == gsym) COMMUTE_REF_DUP(graph, g, f);   \
            else if (IS_DELIMITER(gsym)) COMMUTE_DUP_DELIM(graph, f, g);       \
            else if (IS_DUPLICATOR(gsym)) COMMUTE_DUP_DUP(graph, f, g);        \
            else COMMUTE(graph, f, g);                                         \
            break;                                                             \
        delimiter:                                                             \
            if (fsym == gsym) ANNIHILATE_DELIM_DELIM(graph, f, g);             \
            else if (SYMBOL_ROOT == gsym) COMMUTE_ROOT_DELIM(graph, g, f);     \
            else if (SYMBOL_APPLICATOR == gsym)                                \
                COMMUTE_APPL_DELIM(graph, g, f);                               \
            else if (SYMBOL_LAMBDA == gsym) COMMUTE_LAMBDA_DELIM(graph, g, f); \
            else if (SYMBOL_IDENTITY_LAMBDA == gsym)                           \
                COMMUTE_IDENTITY_LAMBDA_DELIM(graph, g, f);                    \
            else if (SYMBOL_GC_LAMBDA == gsym)                                 \
                COMMUTE_GC_LAMBDA_DELIM(graph, g, f);                          \
            else if (SYMBOL_LAMBDA_C == gsym)                                  \
                COMMUTE_LAMBDA_C_DELIM(graph, g, f);                           \
            else if (SYMBOL_CELL == gsym) COMMUTE_CELL_DELIM(graph, g, f);     \
            else if (SYMBOL_UNARY_CALL == gsym)                                \
                COMMUTE_UCALL_DELIM(graph, g, f);                              \
            else if (SYMBOL_BINARY_CALL == gsym)                               \
                COMMUTE_BCALL_DELIM(graph, g, f);                              \
            else if (SYMBOL_BINARY_CALL_AUX == gsym)                           \
                COMMUTE_BCALL_AUX_DELIM(graph, g, f);                          \
            else if (SYMBOL_IF_THEN_ELSE == gsym)                              \
                COMMUTE_ITE_DELIM(graph, g, f);                                \
            else if (SYMBOL_REFERENCE == gsym) COMMUTE_REF_DELIM(graph, g, f); \
            else if (IS_DELIMITER(gsym)) COMMUTE_DELIM_DELIM(graph, f, g);     \
            else if (IS_DUPLICATOR(gsym)) COMMUTE_DUP_DELIM(graph, g, f);      \
            else                                                               \
                COMMUTE(graph, g, f); /* delimiters must be the second,        \
                                         unlesse they commute with lambdas */  \
            break;                                                             \
        case SYMBOL_ROOT:                                                      \
            if (IS_DELIMITER(gsym)) COMMUTE_ROOT_DELIM(graph, f, g);           \
            else if (IS_ANY_LAMBDA(gsym) || SYMBOL_CELL == gsym)               \
                graph->time_to_stop = true;                                    \
            else COMPILER_UNREACHABLE();                                       \
            break;                                                             \
        case SYMBOL_APPLICATOR:                                                \
            if (SYMBOL_LAMBDA == gsym) BETA(graph, f, g);                      \
            else if (SYMBOL_LAMBDA_C == gsym) BETA_C(graph, f, g);             \
            else if (SYMBOL_IDENTITY_LAMBDA == gsym)                           \
                IDENTITY_BETA(graph, f, g);                                    \
            else if (SYMBOL_GC_LAMBDA == gsym) GC_BETA(graph, f, g);           \
            else if (SYMBOL_REFERENCE == gsym) EXPAND(graph, g, f);            \
            else if (IS_DELIMITER(gsym)) COMMUTE_APPL_DELIM(graph, f, g);      \
            else if (IS_DUPLICATOR(gsym)) COMMUTE_APPL_DUP(graph, f, g);       \
            else COMMUTE(graph, f, g);                                         \
            break;                                                             \
        case SYMBOL_LAMBDA:                                                    \
            if (SYMBOL_APPLICATOR == gsym) BETA(graph, g, f);                  \
            else if (IS_DELIMITER(gsym)) COMMUTE_LAMBDA_DELIM(graph, f, g);    \
            else if (IS_DUPLICATOR(gsym)) COMMUTE_LAMBDA_DUP(graph, f, g);     \
            else if (SYMBOL_ROOT == gsym) graph->time_to_stop = true;          \
            else                                                               \
                COMMUTE(graph, g, f); /* lambdas must alwaies be the second */ \
            break;                                                             \
        case SYMBOL_IDENTITY_LAMBDA:                                           \
            if (SYMBOL_APPLICATOR == gsym) IDENTITY_BETA(graph, g, f);         \
            else if (IS_DELIMITER(gsym))                                       \
                COMMUTE_IDENTITY_LAMBDA_DELIM(graph, f, g);                    \
            else if (IS_DUPLICATOR(gsym))                                      \
                COMMUTE_IDENTITY_LAMBDA_DUP(graph, f, g);                      \
            else if (SYMBOL_ROOT == gsym) graph->time_to_stop = true;          \
            else COMMUTE(graph, g, f); /* same as for `SYMBOL_LAMBDA` */       \
            break;                                                             \
        case SYMBOL_GC_LAMBDA:                                                 \
            if (SYMBOL_APPLICATOR == gsym) GC_BETA(graph, g, f);               \
            else if (IS_DELIMITER(gsym)) COMMUTE_GC_LAMBDA_DELIM(graph, f, g); \
            else if (IS_DUPLICATOR(gsym)) COMMUTE_GC_LAMBDA_DUP(graph, f, g);  \
            else if (SYMBOL_ROOT == gsym) graph->time_to_stop = true;          \
            else COMMUTE(graph, g, f); /* same as for `SYMBOL_LAMBDA` */       \
            break;                                                             \
        case SYMBOL_LAMBDA_C:                                                  \
            if (SYMBOL_APPLICATOR == gsym) BETA_C(graph, g, f);                \
            else if (IS_DELIMITER(gsym)) COMMUTE_LAMBDA_C_DELIM(graph, f, g);  \
            else if (IS_DUPLICATOR(gsym)) COMMUTE_LAMBDA_C_DUP(graph, f, g);   \
            else if (SYMBOL_ROOT == gsym) graph->time_to_stop = true;          \
            else COMMUTE(graph, g, f); /* same as for `SYMBOL_LAMBDA` */       \
            break;                                                             \
        case SYMBOL_ERASER: COMMUTE(graph, f, g); break;                       \
        case SYMBOL_S:                                                         \
            if (SYMBOL_S == gsym) ANNIHILATE_DELIM_DELIM(graph, f, g);         \
            else COMMUTE(graph, f, g);                                         \
            break;                                                             \
        case SYMBOL_CELL:                                                      \
            if (SYMBOL_UNARY_CALL == gsym) DO_UNARY_CALL(graph, g, f);         \
            else if (SYMBOL_BINARY_CALL == gsym) DO_BINARY_CALL(graph, g, f);  \
            else if (SYMBOL_BINARY_CALL_AUX == gsym)                           \
                DO_BINARY_CALL_AUX(graph, g, f);                               \
            else if (SYMBOL_IF_THEN_ELSE == gsym)                              \
                DO_IF_THEN_ELSE(graph, g, f);                                  \
            else if (SYMBOL_PERFORM == gsym) DO_PERFORM(graph, g, f);          \
            else if (IS_DELIMITER(gsym)) COMMUTE_CELL_DELIM(graph, f, g);      \
            else if (IS_DUPLICATOR(gsym)) COMMUTE_CELL_DUP(graph, f, g);       \
            else if (SYMBOL_ROOT == gsym) graph->time_to_stop = true;          \
            else COMMUTE(graph, f, g);                                         \
            break;                                                             \
        case SYMBOL_UNARY_CALL:                                                \
            if (SYMBOL_CELL == gsym) DO_UNARY_CALL(graph, f, g);               \
            else if (SYMBOL_REFERENCE == gsym) EXPAND(graph, g, f);            \
            else if (IS_DELIMITER(gsym)) COMMUTE_UCALL_DELIM(graph, f, g);     \
            else if (IS_DUPLICATOR(gsym)) COMMUTE_UCALL_DUP(graph, f, g);      \
            else COMMUTE(graph, f, g);                                         \
            break;                                                             \
        case SYMBOL_BINARY_CALL:                                               \
            if (SYMBOL_CELL == gsym) DO_BINARY_CALL(graph, f, g);              \
            else if (SYMBOL_REFERENCE == gsym) EXPAND(graph, g, f);            \
            else if (IS_DELIMITER(gsym)) COMMUTE_BCALL_DELIM(graph, f, g);     \
            else if (IS_DUPLICATOR(gsym)) COMMUTE_BCALL_DUP(graph, f, g);      \
            else COMMUTE(graph, f, g);                                         \
            break;                                                             \
        case SYMBOL_BINARY_CALL_AUX:                                           \
            if (SYMBOL_CELL == gsym) DO_BINARY_CALL_AUX(graph, f, g);          \
            else if (SYMBOL_REFERENCE == gsym) EXPAND(graph, g, f);            \
            else if (IS_DELIMITER(gsym)) COMMUTE_BCALL_AUX_DELIM(graph, f, g); \
            else if (IS_DUPLICATOR(gsym)) COMMUTE_BCALL_AUX_DUP(graph, f, g);  \
            else COMMUTE(graph, f, g);                                         \
            break;                                                             \
        case SYMBOL_IF_THEN_ELSE:                                              \
            if (SYMBOL_CELL == gsym) DO_IF_THEN_ELSE(graph, f, g);             \
            else if (SYMBOL_REFERENCE == gsym) EXPAND(graph, g, f);            \
            else if (IS_DELIMITER(gsym)) COMMUTE_ITE_DELIM(graph, f, g);       \
            else if (IS_DUPLICATOR(gsym)) COMMUTE_ITE_DUP(graph, f, g);        \
            else COMMUTE(graph, f, g);                                         \
            break;                                                             \
        case SYMBOL_PERFORM:                                                   \
            if (SYMBOL_CELL == gsym) DO_PERFORM(graph, f, g);                  \
            else if (SYMBOL_REFERENCE == gsym) EXPAND(graph, g, f);            \
            else COMMUTE(graph, f, g);                                         \
            break;                                                             \
        case SYMBOL_REFERENCE:                                                 \
            if (is_operator_symbol(gsym)) EXPAND(graph, f, g);                 \
            else if (IS_DELIMITER(gsym)) COMMUTE_REF_DELIM(graph, f, g);       \
            else if (IS_DUPLICATOR(gsym)) COMMUTE_REF_DUP(graph, f, g);        \
            else COMMUTE(graph, f, g);                                         \
            break;                                                             \
        default:                                                               \
            if (fsym <= MAX_DUPLICATOR_INDEX) goto duplicator;                 \
            else if (fsym <= MAX_DELIMITER_INDEX) goto delimiter;              \
            else COMPILER_UNREACHABLE();                                       \
        }                                                                      \
    } while (false)

COMPILER_NONNULL(1) COMPILER_HOT //
static void
fire_rule(
    struct context *const restrict graph,
    const struct node f,
    const struct node g) {
    MY_ASSERT(graph);
    XASSERT(f.ports), XASSERT(g.ports);
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-result"
    MY_ASSERT(is_interaction(f, g));
#pragma GCC diagnostic pop

#define BETA                          beta
#define BETA_C                        beta_c
#define IDENTITY_BETA                 identity_beta
#define GC_BETA                       gc_beta
#define EXPAND                        do_expand
#define DO_UNARY_CALL                 do_unary_call
#define DO_BINARY_CALL                do_binary_call
#define DO_BINARY_CALL_AUX            do_binary_call_aux
#define DO_IF_THEN_ELSE               do_if_then_else
#define DO_PERFORM                    do_perform
#define ANNIHILATE_DELIM_DELIM        annihilate_delim_delim
#define ANNIHILATE_DUP_DUP            annihilate_dup_dup
#define COMMUTE                       commute
#define COMMUTE_ROOT_DELIM            commute_1_2
#define COMMUTE_APPL_DELIM            commute_3_2
#define COMMUTE_CELL_DELIM            commute_1_2
#define COMMUTE_UCALL_DELIM           commute_2_2
#define COMMUTE_BCALL_DELIM           commute_3_2
#define COMMUTE_BCALL_AUX_DELIM       commute_2_2
#define COMMUTE_ITE_DELIM             commute_4_2
#define COMMUTE_REF_DELIM             commute_1_2
#define COMMUTE_APPL_DUP              commute_3_3
#define COMMUTE_CELL_DUP              commute_1_3
#define COMMUTE_UCALL_DUP             commute_2_3
#define COMMUTE_BCALL_DUP             commute_3_3
#define COMMUTE_BCALL_AUX_DUP         commute_2_3
#define COMMUTE_ITE_DUP               commute_4_3
#define COMMUTE_REF_DUP               commute_1_3
#define COMMUTE_DUP_DELIM             commute_dup_delim
#define COMMUTE_DELIM_DELIM           commute_delim_delim
#define COMMUTE_DUP_DUP               commute_3_3
#define COMMUTE_LAMBDA_DELIM          commute_lambda_delim
#define COMMUTE_LAMBDA_DUP            commute_lambda_dup
#define COMMUTE_IDENTITY_LAMBDA_DELIM commute_1_2
#define COMMUTE_IDENTITY_LAMBDA_DUP   commute_1_3
#define COMMUTE_GC_LAMBDA_DELIM       commute_gc_lambda_delim
#define COMMUTE_GC_LAMBDA_DUP         commute_gc_lambda_dup
#define COMMUTE_LAMBDA_C_DELIM        commute_lambda_c_delim
#define COMMUTE_LAMBDA_C_DUP          commute_lambda_c_dup

    DISPATCH_ACTIVE_PAIR(graph, f, g);

#undef COMMUTE_LAMBDA_C_DUP
#undef COMMUTE_LAMBDA_C_DELIM
#undef COMMUTE_GC_LAMBDA_DUP
#undef COMMUTE_GC_LAMBDA_DELIM
#undef COMMUTE_IDENTITY_LAMBDA_DUP
#undef COMMUTE_IDENTITY_LAMBDA_DELIM
#undef COMMUTE_LAMBDA_DUP
#undef COMMUTE_LAMBDA_DELIM
#undef COMMUTE_DUP_DUP
#undef COMMUTE_DELIM_DELIM
#undef COMMUTE_DUP_DELIM
#undef COMMUTE_REF_DUP
#undef COMMUTE_ITE_DUP
#undef COMMUTE_BCALL_AUX_DUP
#undef COMMUTE_BCALL_DUP
#undef COMMUTE_UCALL_DUP
#undef COMMUTE_CELL_DUP
#undef COMMUTE_APPL_DUP
#undef COMMUTE_REF_DELIM
#undef COMMUTE_ITE_DELIM
#undef COMMUTE_BCALL_AUX_DELIM
#undef COMMUTE_BCALL_DELIM
#undef COMMUTE_UCALL_DELIM
#undef COMMUTE_CELL_DELIM
#undef COMMUTE_APPL_DELIM
#undef COMMUTE_ROOT_DELIM
#undef COMMUTE
#undef ANNIHILATE_DUP_DUP
#undef ANNIHILATE_DELIM_DELIM
#undef DO_PERFORM
#undef DO_IF_THEN_ELSE
#undef DO_BINARY_CALL_AUX
#undef DO_BINARY_CALL
#undef DO_UNARY_CALL
#undef EXPAND
#undef GC_BETA
#undef IDENTITY_BETA
#undef BETA_C
#undef BETA
}

COMPILER_NONNULL(1) COMPILER_HOT //
static void
register_active_pair(
    struct context *const restrict graph,
    const struct node f,
    const struct node g) {
    MY_ASSERT(graph);
    XASSERT(f.ports), XASSERT(g.ports);
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-result"
    MY_ASSERT(is_interaction(f, g));
#pragma GCC diagnostic pop

#define BETA(graph, f, g)                   focus_on(graph->betas, f)
#define BETA_C(graph, f, g)                 focus_on(graph->closed_betas, f)
#define IDENTITY_BETA(graph, f, g)          focus_on(graph->identity_betas, f)
#define GC_BETA(graph, f, g)                focus_on(graph->gc_betas, f)
#define EXPAND(graph, f, g)                 focus_on(graph->expansions, f)
#define DO_UNARY_CALL(graph, f, g)          focus_on(graph->unary_calls, f)
#define DO_BINARY_CALL(graph, f, g)         focus_on(graph->binary_calls, f)
#define DO_BINARY_CALL_AUX(graph, f, g)     focus_on(graph->binary_calls_aux, f)
#define DO_IF_THEN_ELSE(graph, f, g)        focus_on(graph->if_then_elses, f)
#define DO_PERFORM(graph, f, g)             focus_on(graph->performs, f)
#define ANNIHILATE_DELIM_DELIM(graph, f, g) focus_on(graph->annihilations, f)
#define ANNIHILATE_DUP_DUP(graph, f, g)     focus_on(graph->annihilations, f)
#define COMMUTE(graph, f, g)                focus_on(graph->commutations, f)
#define COMMUTE_ROOT_DELIM                  COMMUTE
#define COMMUTE_APPL_DELIM                  COMMUTE
#define COMMUTE_CELL_DELIM                  COMMUTE
#define COMMUTE_UCALL_DELIM                 COMMUTE
#define COMMUTE_BCALL_DELIM                 COMMUTE
#define COMMUTE_BCALL_AUX_DELIM             COMMUTE
#define COMMUTE_ITE_DELIM                   COMMUTE
#define COMMUTE_REF_DELIM                   COMMUTE
#define COMMUTE_APPL_DUP                    COMMUTE
#define COMMUTE_CELL_DUP                    COMMUTE
#define COMMUTE_UCALL_DUP                   COMMUTE
#define COMMUTE_BCALL_DUP                   COMMUTE
#define COMMUTE_BCALL_AUX_DUP               COMMUTE
#define COMMUTE_ITE_DUP                     COMMUTE
#define COMMUTE_REF_DUP                     COMMUTE
#define COMMUTE_DUP_DELIM                   COMMUTE
#define COMMUTE_DELIM_DELIM                 COMMUTE
#define COMMUTE_DUP_DUP                     COMMUTE

#define COMMUTE_LAMBDA_DELIM(graph, f, g)                                      \
    COMMUTE(graph, g, f) // delimiters take precedence over lambdas
#define COMMUTE_IDENTITY_LAMBDA_DELIM(graph, f, g) COMMUTE(graph, g, f)
#define COMMUTE_GC_LAMBDA_DELIM(graph, f, g)       COMMUTE(graph, g, f)
#define COMMUTE_LAMBDA_C_DELIM(graph, f, g)        COMMUTE(graph, g, f)

#define COMMUTE_LAMBDA_DUP(graph, f, g)                                        \
    COMMUTE(graph, g, f) // lambdas must alwaies be the second
#define COMMUTE_IDENTITY_LAMBDA_DUP(graph, f, g) COMMUTE(graph, g, f)
#define COMMUTE_GC_LAMBDA_DUP(graph, f, g)       COMMUTE(graph, g, f)
#define COMMUTE_LAMBDA_C_DUP(graph, f, g)        COMMUTE(graph, g, f)

    DISPATCH_ACTIVE_PAIR(graph, f, g);

#undef COMMUTE_LAMBDA_C_DUP
#undef COMMUTE_GC_LAMBDA_DUP
#undef COMMUTE_IDENTITY_LAMBDA_DUP
#undef COMMUTE_LAMBDA_DUP

#undef COMMUTE_LAMBDA_C_DELIM
#undef COMMUTE_GC_LAMBDA_DELIM
#undef COMMUTE_IDENTITY_LAMBDA_DELIM
#undef COMMUTE_LAMBDA_DELIM

#undef COMMUTE_DUP_DUP
#undef COMMUTE_DELIM_DELIM
#undef COMMUTE_DUP_DELIM
#undef COMMUTE_REF_DUP
#undef COMMUTE_ITE_DUP
#undef COMMUTE_BCALL_AUX_DUP
#undef COMMUTE_BCALL_DUP
#undef COMMUTE_UCALL_DUP
#undef COMMUTE_CELL_DUP
#undef COMMUTE_APPL_DUP
#undef COMMUTE_REF_DELIM
#undef COMMUTE_ITE_DELIM
#undef COMMUTE_BCALL_AUX_DELIM
#undef COMMUTE_BCALL_DELIM
#undef COMMUTE_UCALL_DELIM
#undef COMMUTE_CELL_DELIM
#undef COMMUTE_APPL_DELIM
#undef COMMUTE_ROOT_DELIM
#undef COMMUTE
#undef ANNIHILATE_DUP_DUP
#undef ANNIHILATE_DELIM_DELIM
#undef DO_PERFORM
#undef DO_IF_THEN_ELSE
#undef DO_BINARY_CALL_AUX
#undef DO_BINARY_CALL
#undef DO_UNARY_CALL
#undef EXPAND
#undef GC_BETA
#undef IDENTITY_BETA
#undef BETA_C
#undef BETA
}

#undef DISPATCH_ACTIVE_PAIR
#undef COUNT_BOOKKEEPING

// Higher-Order Control Structures
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

COMPILER_RETURNS_NONNULL COMPILER_WARN_UNUSED_RESULT COMPILER_NONNULL(1, 2) //
static uint64_t *
build_delimiter_sequence(
    struct context *const restrict graph,
    uint64_t *const restrict binder_port,
    const uint64_t idx,
    const uint64_t n) {
    MY_ASSERT(graph);
    MY_ASSERT(binder_port);
    XASSERT(n > 0);

    struct node current = alloc_node(graph, SYMBOL_DELIMITER(idx));
    current.ports[2] = 1;
    uint64_t *const result = &current.ports[1];
    for (uint64_t i = 1; i < n; i++) {
        const struct node delim = alloc_node(graph, SYMBOL_DELIMITER(idx));
        delim.ports[2] = 1;
        connect_ports(&current.ports[0], &delim.ports[1]);
        current = delim;
    }

    connect_ports(&current.ports[0], binder_port);

    return result;
}

COMPILER_RETURNS_NONNULL COMPILER_WARN_UNUSED_RESULT COMPILER_NONNULL(1, 2) //
static uint64_t **
build_duplicator_tree(
    struct context *const restrict graph,
    uint64_t *const restrict binder_port,
    const uint64_t idx,
    const uint64_t n) {
    MY_ASSERT(graph);
    MY_ASSERT(binder_port);
    XASSERT(n >= 2);

    uint64_t **const ports = xmalloc(sizeof ports[0] * n);

    struct node current = alloc_node(graph, SYMBOL_DUPLICATOR(idx));
    ports[0] = &current.ports[1];
    ports[1] = &current.ports[2];

    for (uint64_t i = 2; i < n; i++) {
        const struct node dup = alloc_node(graph, SYMBOL_DUPLICATOR(idx));
        ports[i] = &dup.ports[1];
        connect_ports(&dup.ports[2], &current.ports[0]);
        current = dup;
    }

    connect_ports(&current.ports[0], binder_port);

    return ports;
}

// Graph Traversal Procedure
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

// This procedure is onely used _after_ weak reduction, in order to transforme
// nodes & collect active pairs.
COMPILER_NONNULL(1) //
static void
walk_graph(
    struct context *const graph,
    void (*const cb)(struct context *const, const struct node)) {
    MY_ASSERT(graph);
    XASSERT(graph->root.ports);

    struct multifocus *const focus = alloc_focus(INITIAL_MULTIFOCUS_CAPACITY);

    focus_on(focus, graph->root);
    set_phase(&graph->root.ports[0], graph->phase);

    CONSUME_MULTIFOCUS (focus, f) {
        XASSERT(f.ports);

        FOR_ALL_PORTS (f, i, 0) {
            const struct node g = follow_port(&f.ports[i]);

            if (DECODE_PHASE_METADATA(g.ports[0]) != graph->phase) {
                set_phase(&g.ports[0], graph->phase);
                focus_on(focus, g);
            }
        }

        if (cb) { cb(graph, f); }
    }

    free_focus(focus);
}

// Graph Traversal Callbacks
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

COMPILER_NONNULL(1) //
static void
unwind_cb(struct context *const graph, const struct node node) {
    MY_ASSERT(graph);
    XASSERT(node.ports);

    if (SYMBOL_APPLICATOR != node.ports[-1]) { return; }

    debug("%s(%s)", __func__, print_node(node));
    wait_for_user(graph);

    // clang-format off
    CONNECT_NODE(node,
        DECODE_ADDRESS(node.ports[1]), DECODE_ADDRESS(node.ports[2]), DECODE_ADDRESS(node.ports[0]));
    // clang-format on
}

COMPILER_NONNULL(1) //
static void
scope_remove_cb(struct context *const graph, const struct node node) {
    MY_ASSERT(graph);
    XASSERT(node.ports);

    if (!IS_DELIMITER(node.ports[-1])) { return; }

    debug("%s(%s)", __func__, print_node(node));
    wait_for_user(graph);

    const struct node scope = alloc_node(graph, SYMBOL_S);
    // clang-format off
    CONNECT_NODE(scope,
        DECODE_ADDRESS(node.ports[1]), DECODE_ADDRESS(node.ports[0]));
    // clang-format on

    free_node(graph, node);
}

COMPILER_NONNULL(1) //
static void
loop_cut_cb(struct context *const graph, const struct node node) {
    MY_ASSERT(graph);
    XASSERT(node.ports);

    if (!IS_RELEVANT_LAMBDA(node.ports[-1])) { return; }

    debug("%s(%s)", __func__, print_node(node));
    wait_for_user(graph);

    struct node side_eraser = alloc_node(graph, SYMBOL_ERASER);
    struct node bottom_eraser = alloc_node(graph, SYMBOL_ERASER);

    uint64_t *const binder_port = DECODE_ADDRESS(node.ports[1]);

    connect_ports(&node.ports[1], &side_eraser.ports[0]);
    connect_ports(&bottom_eraser.ports[0], binder_port);
}

COMPILER_NONNULL(1) //
static void
normalize_delimiters_cb(struct context *const graph, const struct node node) {
    MY_ASSERT(graph);
    XASSERT(node.ports);

    if (!IS_DELIMITER(node.ports[-1])) { return; }

    uint64_t *const output_port = build_delimiter_sequence(
        graph,
        DECODE_ADDRESS(node.ports[0]),
        (uint64_t)symbol_index(node.ports[-1]),
        node.ports[2]);

    connect_ports(output_port, DECODE_ADDRESS(node.ports[1]));

    free_node(graph, node);
}

COMPILER_NONNULL(1) //
static void
multifocus_cb(struct context *const graph, const struct node f) {
    MY_ASSERT(graph);
    XASSERT(f.ports);

    const struct node g = follow_port(&f.ports[0]);
    XASSERT(g.ports);

    const bool condition = //
        !is_either_root(f, g) && is_interacting_with(f, g) &&
        // Protect from focusing on both active nodes.
        // Thanks to Marvin Borner <git@marvinborner.de> for pointing
        // this out!
        compare_node_ptrs(f, g) < 0;

    if (condition) { register_active_pair(graph, f, g); }
}

// Conversion to Lambda Term String
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

COMPILER_NONNULL(1) //
static void
to_lambda_string(
    FILE *const restrict stream, const uint64_t i, const struct node node) {
    MY_ASSERT(stream);
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
    case SYMBOL_GC_LAMBDA:
    case SYMBOL_LAMBDA_C: {
        const uint8_t body_port_idx =
            IS_RELEVANT_LAMBDA(node.ports[-1]) ? 2 : 1;
        fprintf(stream, "(λ ");
        to_lambda_string(stream, i, follow_port(&node.ports[body_port_idx]));
        fprintf(stream, ")");
        return;
    }
    case SYMBOL_IDENTITY_LAMBDA: fprintf(stream, "(λ 0)"); return;
    case SYMBOL_ERASER: fprintf(stream, "%" PRIu64, i); return;
    case SYMBOL_S:
        to_lambda_string(stream, i + 1, follow_port(&node.ports[1]));
        return;
    case SYMBOL_CELL:
        fprintf(stream, "cell[%" PRIu64 "]", node.ports[1]);
        return;
    default: break;
    }

    if (!IS_DUPLICATOR(node.ports[-1])) {
        // Other symbols must be already removed at this point.
        panic("Unexpected node symbol!: %s", print_symbol(node.ports[-1]));
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
    uint64_t nusages; // the number of times the lambda body refers to
                      // its binder
    struct lambda_term
        *usage; // the pointer to one arbitrary usage of the lambda binder
    uint64_t **dup_ports; // the pointer to the next duplicator tree
                          // port; dynamically assigned
    uint64_t lvl;         // the de Bruijn level; dynamically assigned
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
    uint64_t fv_count;
};

extern LambdaTerm
apply(const restrict LambdaTerm rator, const restrict LambdaTerm rand) {
    MY_ASSERT(rator), MY_ASSERT(rand);

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
    MY_ASSERT(binder), MY_ASSERT(body);
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
    binder->data.lambda->usage = term;

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
    MY_ASSERT(lhs), MY_ASSERT(rhs);

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
    MY_ASSERT(if_then), MY_ASSERT(if_else);

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
    MY_ASSERT(action), MY_ASSERT(k);

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

// Conversion From Lambda Terms
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

COMPILER_CONST COMPILER_WARN_UNUSED_RESULT //
inline static uint64_t
de_bruijn_level_to_index(const uint64_t lvl, const uint64_t var) {
    return lvl - var - 1;
}

COMPILER_NONNULL(1, 2, 3) //
static void
of_lambda_term(
    struct context *const restrict graph,
    struct lambda_term *const restrict term,
    uint64_t *const restrict output_port,
    const uint64_t lvl) {
    MY_ASSERT(graph);
    MY_ASSERT(term);
    MY_ASSERT(output_port);

    switch (term->ty) {
    case LAMBDA_TERM_APPLY: {
        struct lambda_term *const rator = term->data.apply.rator, //
            *const rand = term->data.apply.rand;
        XASSERT(rator), XASSERT(rand);

        const struct node applicator = alloc_node(graph, SYMBOL_APPLICATOR);
        connect_ports(&applicator.ports[1], output_port);
        of_lambda_term(graph, rator, &applicator.ports[0], lvl);
        of_lambda_term(graph, rand, &applicator.ports[2], lvl);

        break;
    }
    case LAMBDA_TERM_LAMBDA: {
        struct lambda_data *const tlambda = term->data.lambda;
        struct lambda_term *const body = term->data.lambda->body;
        XASSERT(tlambda);
        XASSERT(body);

        const bool is_identity =
            LAMBDA_TERM_VAR == body->ty && tlambda == *body->data.var;
        if (is_identity) {
            // clang-format off
            const struct node lambda = alloc_node(graph, SYMBOL_IDENTITY_LAMBDA);
            // clang-format on
            connect_ports(&lambda.ports[0], output_port);
            free(body);
            goto done_with_lambda;
        }

        if (0 == tlambda->nusages) {
            // This is lambda that "garbage-collects" its argument.
            const struct node lambda = alloc_node(graph, SYMBOL_GC_LAMBDA);
            of_lambda_term(graph, body, &lambda.ports[1], lvl + 1);
            connect_ports(&lambda.ports[0], output_port);
            goto done_with_lambda;
        }

        const uint64_t symbol =
            term->fv_count > 0 ? SYMBOL_LAMBDA : SYMBOL_LAMBDA_C;

        const struct node lambda = alloc_node(graph, symbol);
        connect_ports(&lambda.ports[0], output_port);
        uint64_t **dup_ports = NULL;
        if (1 == tlambda->nusages) {
            // This is a linear non-self-referential lambda.
            dup_ports = xmalloc(sizeof dup_ports[0] * 1);
            dup_ports[0] = &lambda.ports[1];
        } else {
            // This is a non-linear lambda that needs a duplicator tree.
            dup_ports = build_duplicator_tree(
                graph, &lambda.ports[1], 0, tlambda->nusages /* >= 2 */);
        }
        tlambda->dup_ports = dup_ports;
        tlambda->lvl = lvl;
        of_lambda_term(graph, body, &lambda.ports[2], lvl + 1);
        free(dup_ports);

    done_with_lambda:
        free(tlambda);
        break;
    }
    case LAMBDA_TERM_VAR: {
        struct lambda_data *const lambda = *term->data.var;
        XASSERT(lambda), XASSERT(lambda->dup_ports);

        const uint64_t idx = de_bruijn_level_to_index(lvl, lambda->lvl);
        if (0 == idx) {
            connect_ports(lambda->dup_ports[0], output_port);
        } else {
            struct node delim =
                alloc_node(graph, SYMBOL_DELIMITER(UINT64_C(0)));
            delim.ports[2] = idx;
            connect_ports(&delim.ports[0], lambda->dup_ports[0]);
            connect_ports(&delim.ports[1], output_port);
        }
        lambda->dup_ports++;

        break;
    }
    case LAMBDA_TERM_CELL: {
        const uint64_t value = term->data.cell;

        const struct node cell = alloc_node(graph, SYMBOL_CELL);
        connect_ports(&cell.ports[0], output_port);
        cell.ports[1] = value;

        break;
    }
    case LAMBDA_TERM_UNARY_CALL: {
        uint64_t (*const function)(uint64_t) = term->data.u_call.function;
        struct lambda_term *const rand = term->data.u_call.rand;
        XASSERT(function);
        XASSERT(rand);

        const struct node call = alloc_node(graph, SYMBOL_UNARY_CALL);
        connect_ports(&call.ports[1], output_port);
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
        call.ports[2] = U64_OF_FUNCTION(function);
#pragma GCC diagnostic pop
        of_lambda_term(graph, rand, &call.ports[0], lvl);

        break;
    }
    case LAMBDA_TERM_BINARY_CALL: {
        uint64_t (*const function)(uint64_t, uint64_t) = //
            term->data.b_call.function;
        struct lambda_term *const lhs = term->data.b_call.lhs, //
            *const rhs = term->data.b_call.rhs;
        XASSERT(function);
        XASSERT(lhs), XASSERT(rhs);

        const struct node call = alloc_node(graph, SYMBOL_BINARY_CALL);
        connect_ports(&call.ports[1], output_port);
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
        call.ports[3] = U64_OF_FUNCTION(function);
#pragma GCC diagnostic pop
        of_lambda_term(graph, lhs, &call.ports[0], lvl);
        of_lambda_term(graph, rhs, &call.ports[2], lvl);

        break;
    }
    case LAMBDA_TERM_IF_THEN_ELSE: {
        struct lambda_term *const condition = term->data.ite.condition, //
            *const if_then = term->data.ite.if_then,                    //
                *const if_else = term->data.ite.if_else;
        XASSERT(condition);
        XASSERT(if_then), XASSERT(if_else);

        const struct node ite = alloc_node(graph, SYMBOL_IF_THEN_ELSE);
        connect_ports(&ite.ports[1], output_port);
        of_lambda_term(graph, condition, &ite.ports[0], lvl);
        of_lambda_term(graph, if_then, &ite.ports[3], lvl);
        of_lambda_term(graph, if_else, &ite.ports[2], lvl);

        break;
    }
    case LAMBDA_TERM_FIX: {
        struct lambda_term *const f = term->data.fix.f;
        XASSERT(f);

        const struct node dup = alloc_node(graph, SYMBOL_DUPLICATOR(0));
        const struct node applicator = alloc_node(graph, SYMBOL_APPLICATOR);

        connect_ports(&dup.ports[0], &applicator.ports[1]);
        connect_ports(&dup.ports[1], output_port);
        connect_ports(&dup.ports[2], &applicator.ports[2]);
        of_lambda_term(graph, f, &applicator.ports[0], lvl);

        break;
    }
    case LAMBDA_TERM_PERFORM: {
        struct lambda_term *const action = term->data.perform.action, //
            *const k = term->data.perform.k;
        XASSERT(action), XASSERT(k);

        const struct node perform = alloc_node(graph, SYMBOL_PERFORM);
        connect_ports(&perform.ports[1], output_port);
        of_lambda_term(graph, action, &perform.ports[0], lvl);
        of_lambda_term(graph, k, &perform.ports[2], lvl);

        break;
    }
    case LAMBDA_TERM_REFERENCE: {
        struct lambda_term *(*const function)(void) = term->data.ref.function;
        XASSERT(function);

        const struct node ref = alloc_node(graph, SYMBOL_REFERENCE);
        connect_ports(&ref.ports[0], output_port);
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
        ref.ports[1] = U64_OF_FUNCTION(function);
#pragma GCC diagnostic pop

        break;
    }
    default: COMPILER_UNREACHABLE();
    }

    // This function takes ownership of the whole `term` object.
    free(term);
}

// Complete Algorithm
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

COMPILER_NONNULL(1) //
static void
weak_reduction(struct context *const restrict graph) {
    debug("%s()", __func__);

    MY_ASSERT(graph);

    struct multifocus *const stack = alloc_focus(INITIAL_MULTIFOCUS_CAPACITY);

    struct node f = graph->root;

    while (!graph->time_to_stop) {
        const struct node g = follow_port(&f.ports[0]);
        XASSERT(g.ports);

        if (is_interacting_with(f, g)) {
            fire_rule(graph, f, g);
            f = unfocus_or(stack, graph->root);
            set_phase(&f.ports[0], PHASE_REDUCE_WEAKLY);
        } else {
            set_phase(&f.ports[0], PHASE_STACK);
            focus_on(stack, f);
            f = g;
        }
    }

    free_focus(stack);
}

COMPILER_NONNULL(1) //
static void
normalize_x_rules(struct context *const restrict graph) {
    debug("%s()", __func__);

    MY_ASSERT(graph);

repeat:
    graph->phase = PHASE_REDUCE_FULLY != graph->phase ? PHASE_REDUCE_FULLY
                                                      : PHASE_REDUCE_FULLY_AUX;
    walk_graph(graph, multifocus_cb);

    if (is_normalized_graph(graph)) { return; }

    // clang-format off
    CONSUME_MULTIFOCUS (graph->betas, f) { interact(graph, beta, f); }
    CONSUME_MULTIFOCUS (graph->closed_betas, f) { interact(graph, beta_c, f); }
    CONSUME_MULTIFOCUS (graph->identity_betas, f) { interact(graph, identity_beta, f); }
    CONSUME_MULTIFOCUS (graph->gc_betas, f) { interact(graph, gc_beta, f); }
    CONSUME_MULTIFOCUS (graph->expansions, f) { interact(graph, do_expand, f); }
    CONSUME_MULTIFOCUS (graph->unary_calls, f) { interact(graph, do_unary_call, f); }
    CONSUME_MULTIFOCUS (graph->binary_calls, f) { interact(graph, do_binary_call, f); }
    CONSUME_MULTIFOCUS (graph->binary_calls_aux, f) { interact(graph, do_binary_call_aux, f); }
    CONSUME_MULTIFOCUS (graph->if_then_elses, f) { interact(graph, do_if_then_else, f); }
    CONSUME_MULTIFOCUS (graph->annihilations, f) { interact(graph, annihilate, f); }
    CONSUME_MULTIFOCUS (graph->commutations, f) { interact(graph, commute, f); }
    // clang-format on

    MY_ASSERT(is_normalized_graph(graph));

    goto repeat;
}

extern void
optiscope_algorithm(
    FILE *const restrict stream,            // if `NULL`, doe not read back
    struct lambda_term *const restrict term // must not be `NULL`
) {
    debug("%s()", __func__);

    MY_ASSERT(term);

    struct context *const graph = alloc_context();

    of_lambda_term(graph, term, &graph->root.ports[0], 0);

    // Phase #1: weak reduction.
    {
        graphviz(graph, "target/1-initial.dot");
        weak_reduction(graph);
        graphviz(graph, "target/1-weakly-reduced.dot");
    }

    if (NULL == stream) { goto finish; }

#define X(focus_name)                                                          \
    graph->focus_name = alloc_focus(INITIAL_MULTIFOCUS_CAPACITY);
    CONTEXT_MULTIFOCUSES
#undef X

    // Phase #2: full reduction.
    {
        graph->phase = PHASE_REDUCE_FULLY;
        walk_graph(graph, normalize_delimiters_cb);
        normalize_x_rules(graph);
        graphviz(graph, "target/2-fully-reduced.dot");
    }

    // Phase #3: unwinding.
    {
        graph->phase = PHASE_UNWIND;
        walk_graph(graph, unwind_cb);
        graphviz(graph, "target/3-unwound.dot");
        normalize_x_rules(graph);
        graphviz(graph, "target/3-unwoundx.dot");
    }

    // Phase #4: scope removal.
    {
        graph->phase = PHASE_SCOPE_REMOVE;
        walk_graph(graph, scope_remove_cb);
        graphviz(graph, "target/4-unscoped.dot");
        normalize_x_rules(graph);
        graphviz(graph, "target/4-unscopedx.dot");
    }

    // Phase #5: loop cutting.
    {
        graph->phase = PHASE_LOOP_CUT;
        walk_graph(graph, loop_cut_cb);
        graphviz(graph, "target/5-unlooped.dot");
        normalize_x_rules(graph);
        graphviz(graph, "target/5-unloopedx.dot");
    }

    MY_ASSERT(is_normalized_graph(graph));

    to_lambda_string(stream, 0, follow_port(&graph->root.ports[0]));

finish:
    print_stats(graph);
    free_context(graph);
}
