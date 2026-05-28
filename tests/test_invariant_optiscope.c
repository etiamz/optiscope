#include <check.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdint.h>
#include <sys/mman.h>
#include <unistd.h>

/*
 * We simulate the vulnerable pattern: vsprintf(result, format, args)
 * writing into a fixed-size buffer, and test that a safe replacement
 * (vsnprintf) never exceeds the declared buffer length.
 *
 * Invariant: Buffer reads/writes never exceed the declared buffer length.
 * Any formatted output that would overflow must be truncated or rejected.
 */

#define SAFE_BUFFER_SIZE 64

/* Safe wrapper that uses vsnprintf instead of vsprintf */
static int safe_format(char *buf, size_t buf_size, const char *fmt, ...)
{
    va_list args;
    int ret;
    va_start(args, fmt);
    ret = vsnprintf(buf, buf_size, fmt, args);
    va_end(args);
    return ret;
}

/* Helper: create a canary-guarded buffer to detect overflows */
typedef struct {
    uint8_t  pre_canary[16];
    char     buffer[SAFE_BUFFER_SIZE];
    uint8_t  post_canary[16];
} guarded_buffer_t;

static void init_guarded_buffer(guarded_buffer_t *gb)
{
    memset(gb->pre_canary,  0xAB, sizeof(gb->pre_canary));
    memset(gb->buffer,      0x00, sizeof(gb->buffer));
    memset(gb->post_canary, 0xCD, sizeof(gb->post_canary));
}

static int check_canaries(const guarded_buffer_t *gb)
{
    for (size_t i = 0; i < sizeof(gb->pre_canary); i++) {
        if (gb->pre_canary[i] != 0xAB) return 0;
    }
    for (size_t i = 0; i < sizeof(gb->post_canary); i++) {
        if (gb->post_canary[i] != 0xCD) return 0;
    }
    return 1;
}

START_TEST(test_buffer_no_overflow_on_oversized_input)
{
    /* Invariant: formatted output never exceeds declared buffer size;
     * canary bytes surrounding the buffer must remain intact. */
    const char *payloads[] = {
        /* 2x buffer size: 128 'A's */
        "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
        "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
        /* 10x buffer size: 640 'B's */
        "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
        "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
        "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
        "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
        "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
        "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
        "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
        "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
        "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
        "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB",
        /* Format string attack attempt */
        "%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s",
        /* %n format string attack */
        "%n%n%n%n%n%n%n%n%n%n%n%n%n%n%n%n%n%n%n%n%n%n%n%n%n%n%n%n%n%n",
        /* Mixed format specifiers */
        "%x%x%x%x%x%x%x%x%x%x%x%x%x%x%x%x%x%x%x%x%x%x%x%x%x%x%x%x%x%x",
        /* Null bytes embedded in long string (treated as C string: just null) */
        "",
        /* Single very long token */
        "CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC"
        "CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC"
        "CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC"
        "CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC",
        /* Unicode-like byte sequences */
        "\xc3\xa9\xc3\xa0\xc3\xbc\xc3\xa9\xc3\xa0\xc3\xbc"
        "\xc3\xa9\xc3\xa0\xc3\xbc\xc3\xa9\xc3\xa0\xc3\xbc"
        "\xc3\xa9\xc3\xa0\xc3\xbc\xc3\xa9\xc3\xa0\xc3\xbc"
        "\xc3\xa9\xc3\xa0\xc3\xbc\xc3\xa9\xc3\xa0\xc3\xbc"
        "\xc3\xa9\xc3\xa0\xc3\xbc\xc3\xa9\xc3\xa0\xc3\xbc"
        "\xc3\xa9\xc3\xa0\xc3\xbc\xc3\xa9\xc3\xa0\xc3\xbc",
        /* Repeated percent signs */
        "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
        "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%",
        /* Path traversal payload */
        "../../../../../../../../../../../../../../../../etc/passwd"
        "../../../../../../../../../../../../../../../../etc/shadow",
    };
    int num_payloads = sizeof(payloads) / sizeof(payloads[0]);

    for (int i = 0; i < num_payloads; i++) {
        guarded_buffer_t gb;
        init_guarded_buffer(&gb);

        /* Use safe vsnprintf-based formatting with "%s" to pass the payload */
        int ret = safe_format(gb.buffer, SAFE_BUFFER_SIZE, "%s", payloads[i]);

        /* 1. Canaries must be intact — no overflow occurred */
        ck_assert_msg(check_canaries(&gb),
            "Canary corruption detected for payload index %d: buffer overflow occurred", i);

        /* 2. The buffer must be null-terminated within bounds */
        int null_found = 0;
        for (int j = 0; j < SAFE_BUFFER_SIZE; j++) {
            if (gb.buffer[j] == '\0') {
                null_found = 1;
                break;
            }
        }
        ck_assert_msg(null_found,
            "Buffer not null-terminated within bounds for payload index %d", i);

        /* 3. The actual string length in the buffer must be < SAFE_BUFFER_SIZE */
        size_t actual_len = strnlen(gb.buffer, SAFE_BUFFER_SIZE);
        ck_assert_msg(actual_len < SAFE_BUFFER_SIZE,
            "Buffer content length %zu >= declared size %d for payload index %d",
            actual_len, SAFE_BUFFER_SIZE, i);

        /* 4. If ret > 0 and payload was longer than buffer, output must be truncated */
        size_t payload_len = strlen(payloads[i]);
        if (payload_len >= SAFE_BUFFER_SIZE) {
            ck_assert_msg(actual_len == SAFE_BUFFER_SIZE - 1,
                "Expected truncation to %d chars for oversized payload index %d, got %zu",
                SAFE_BUFFER_SIZE - 1, i, actual_len);
        }

        /* 5. vsnprintf return value indicates what would have been written;
         *    actual bytes written must never exceed buf_size - 1 */
        if (ret > 0) {
            ck_assert_msg((size_t)ret >= actual_len,
                "vsnprintf return value inconsistency for payload index %d", i);
        }
    }
}
END_TEST

START_TEST(test_buffer_no_overflow_numeric_formats)
{
    /* Invariant: numeric format specifiers also cannot overflow the buffer */
    guarded_buffer_t gb;
    init_guarded_buffer(&gb);

    /* Attempt to write a very large integer repeatedly */
    int ret = safe_format(gb.buffer, SAFE_BUFFER_SIZE,
        "%d%d%d%d%d%d%d%d%d%d%d%d%d%d%d%d%d%d%d%d",
        INT32_MAX, INT32_MAX, INT32_MAX, INT32_MAX, INT32_MAX,
        INT32_MAX, INT32_MAX, INT32_MAX, INT32_MAX, INT32_MAX,
        INT32_MAX, INT32_MAX, INT32_MAX, INT32_MAX, INT32_MAX,
        INT32_MAX, INT32_MAX, INT32_MAX, INT32_MAX, INT32_MAX);

    ck_assert_msg(check_canaries(&gb),
        "Canary corruption detected: numeric format overflow occurred");

    size_t actual_len = strnlen(gb.buffer, SAFE_BUFFER_SIZE);
    ck_assert_msg(actual_len < SAFE_BUFFER_SIZE,
        "Buffer not properly bounded for numeric format: len=%zu", actual_len);

    (void)ret;
}
END_TEST

START_TEST(test_buffer_no_overflow_wide_strings)
{
    /* Invariant: wide/long string arguments cannot overflow fixed buffer */
    const size_t ATTACK_SIZE = SAFE_BUFFER_SIZE * 20;
    char *long_str = (char *)malloc(ATTACK_SIZE + 1);
    ck_assert_ptr_nonnull(long_str);
    memset(long_str, 'X', ATTACK_SIZE);
    long_str[ATTACK_SIZE] = '\0';

    guarded_buffer_t gb;
    init_guarded_buffer(&gb);

    int ret = safe_format(gb.buffer, SAFE_BUFFER_SIZE, "%s", long_str);

    ck_assert_msg(check_canaries(&gb),
        "Canary corruption: wide string caused buffer overflow");

    size_t actual_len = strnlen(gb.buffer, SAFE_BUFFER_SIZE);
    ck_assert_msg(actual_len < SAFE_BUFFER_SIZE,
        "Buffer exceeded declared size with wide string: len=%zu", actual_len);

    ck_assert_msg(actual_len == SAFE_BUFFER_SIZE - 1,
        "Expected truncation to %d, got %zu", SAFE_BUFFER_SIZE - 1, actual_len);

    free(long_str);
    (void)ret;
}
END_TEST

START_TEST(test_buffer_no_overflow_repeated_format)
{
    /* Invariant: repeated format specifiers with large values stay bounded */
    guarded_buffer_t gb;
    init_guarded_buffer(&gb);

    /* %*s with large width argument — potential for large output */
    int ret = safe_format(gb.buffer, SAFE_BUFFER_SIZE,
        "%-*s", SAFE_BUFFER_SIZE * 10, "test");

    ck_assert_msg(check_canaries(&gb),
        "Canary corruption: width-specified format caused overflow");

    size_t actual_len = strnlen(gb.buffer, SAFE_BUFFER_SIZE);
    ck_assert_msg(actual_len < SAFE_BUFFER_SIZE,
        "Buffer exceeded declared size with width format: len=%zu", actual_len);

    (void)ret;
}
END_TEST

Suite *security_suite(void)
{
    Suite *s;
    TCase *tc_core;

    s = suite_create("Security");
    tc_core = tcase_create("Core");

    tcase_set_timeout(tc_core, 30);
    tcase_add_test(tc_core, test_buffer_no_overflow_on_oversized_input);
    tcase_add_test(tc_core, test_buffer_no_overflow_numeric_formats);
    tcase_add_test(tc_core, test_buffer_no_overflow_wide_strings);
    tcase_add_test(tc_core, test_buffer_no_overflow_repeated_format);
    suite_add_tcase(s, tc_core);

    return s;
}

int main(void)
{
    int number_failed;
    Suite *s;
    SRunner *sr;

    s = security_suite();
    sr = srunner_create(s);

    srunner_run_all(sr, CK_NORMAL);
    number_failed = srunner_ntests_failed(sr);
    srunner_free(sr);

    return (number_failed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}