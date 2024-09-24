#ifndef STACUM__UTIL__H
#define STACUM__UTIL__H

#include <stdint.h>
#include <assert.h>

#define MAX(a, b) (a > b) ? (a) : (b)
#define MIN(a, b) (a < b) ? (a) : (b)

typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;
typedef uint8_t byte;
typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef struct {
	u8 *buf;
	u32 size;
	u32 index;
} sized_u8_buffer;

int readwholefile(char *filepath, u8 **output, int *out_size);
u32 u8_buf_get_u32(u8 *buf, int index);
u64 u8_buf_get_u64(u8 *buf, int index);

void init_sized_u8_buffer(sized_u8_buffer *sbuf, u8 *buf, u32 size, u32 index);
u8 sized_u8_buffer_get_next_u8(sized_u8_buffer *buf);
u8 * sized_u8_buffer_get_next_u8_ref(sized_u8_buffer *buf);
u64 * sized_u8_buffer_get_next_u64_ref(sized_u8_buffer *buf);
u32 sized_u8_buffer_get_next_u32(sized_u8_buffer *buf);
void sized_u8_buffer_skip_n_bytes(sized_u8_buffer *buf, u32 n);
u32 sized_u8_buffer_at_end(sized_u8_buffer *buf);

#endif
