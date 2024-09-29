#include "util.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

int
readwholefile(char *filepath, u8 **output, int *out_size) {
	FILE *fp = fopen(filepath, "r");

	if(fp == NULL) {
		goto error;
	}

	if(fseek(fp, 0, SEEK_END)) {
		goto error;
	}

	long size = ftell(fp);

	*output = malloc(size + 1);

	if(*output == NULL) {
		printf("buy more ram\n");
		goto error;
	}

	if(fseek(fp, 0, SEEK_SET)) {
		goto error;
	}

	long out = fread(*output, 1, size, fp);
	if(out != size) {
		goto error;
	}

	*out_size = out;

	fclose(fp);

	return 0;
error:
	if(fp != NULL) {
		fclose(fp);
	}

	if(*output != NULL) {
		free(*output);
	}

	return 1;
}

u16
u8_buf_get_u16(u8 *buf, int index) {
	u16 ret = ((u16*)(&buf[index]))[0];
	return ret;
}

u32
u8_buf_get_u32(u8 *buf, int index) {
	u32 ret = ((u32*)(&buf[index]))[0];
	return ret;
}

u64
u8_buf_get_u64(u8 *buf, int index) {
	u64 ret = ((u64*)(&buf[index]))[0];
	return ret;
}

void
init_sized_u8_buffer(sized_u8_buffer *sbuf, u8 *buf, u32 size, u32 index) {
	sbuf->buf = buf;
	sbuf->size = size;
	sbuf->index = index;
}

u8
sized_u8_buffer_get_next_u8(sized_u8_buffer *buf) {
	assert(buf->index < buf->size);
	u8 ret = buf->buf[buf->index];
	buf->index += sizeof(u8);
	return ret;
}

u8 *
sized_u8_buffer_get_next_u8_ref(sized_u8_buffer *buf) {
	assert(buf->index < buf->size);
	u8 *ret = &buf->buf[buf->index];
	buf->index += sizeof(u8);
	return ret;
}

u64 *
sized_u8_buffer_get_next_u64_ref(sized_u8_buffer *buf) {
	assert(buf->index < buf->size);
	u64 *ret = (u64 *)&buf->buf[buf->index];
	buf->index += sizeof(u64);
	return ret;
}

u32
sized_u8_buffer_get_next_u32(sized_u8_buffer *buf) {
	assert(buf->index + sizeof(u32) - 1 < buf->size);
	u32 ret = u8_buf_get_u32(buf->buf, buf->index);
	buf->index += sizeof(u32);
	return ret;
}

void
sized_u8_buffer_skip_n_bytes(sized_u8_buffer *buf, u32 n) {
	assert(buf->index + n <= buf->size);
	buf->index += n;
}

u32
sized_u8_buffer_at_end(sized_u8_buffer *buf) {
	return buf->size == buf->index;
}
