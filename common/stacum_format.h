#ifndef STACUM__FORMAT__H
#define STACUM__FORMAT__H

#include "util.h"

typedef struct {
	byte *data; u32 data_n;
	byte *code; u32 code_n;
	u64 *stack; u32 stack_n;
} stacum_format;

int stacum_format_read_from_buffer(stacum_format *format, sized_u8_buffer *buf);

#endif
