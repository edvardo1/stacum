#include "stacum_format.h"

int
stacum_format_read_from_buffer(stacum_format *format, sized_u8_buffer *buf) {
	assert(sized_u8_buffer_get_next_u8(buf) == 's');
	assert(sized_u8_buffer_get_next_u8(buf) == 't');
	assert(sized_u8_buffer_get_next_u8(buf) == 'c');

	format->data_n = sized_u8_buffer_get_next_u32(buf);
	if(format->data_n > 0) {
		format->data = sized_u8_buffer_get_next_u8_ref(buf);
		sized_u8_buffer_skip_n_bytes(buf, format->data_n - 1);
	}

	format->code_n = sized_u8_buffer_get_next_u32(buf);
	if(format->code_n > 0) {
		format->code = sized_u8_buffer_get_next_u8_ref(buf);
		sized_u8_buffer_skip_n_bytes(buf, format->code_n - 1);
	}

	format->stack_n = sized_u8_buffer_get_next_u32(buf);
	if(format->stack_n > 0) {
		format->stack = sized_u8_buffer_get_next_u64_ref(buf);
		sized_u8_buffer_skip_n_bytes(buf, (format->stack_n - 1) * sizeof(u64));
	}

	assert(sized_u8_buffer_at_end(buf));

	return 0;
}
