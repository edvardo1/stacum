#include <stdio.h>
#include <unistd.h>
#include <search.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <stdint.h>
#include <assert.h>
#include <dlfcn.h>

#include "../common/util.h"
#include "../common/insts.h"
#include "../common/stacum_format.h"

#define PROGRAM_SIZE 32000

typedef u64 (*dynamic_function)();

//#define PROCEDURE_POINTERS_SIZE (1024)
#define CALL_STACK_SIZE (256)
#define STACK_SIZE (512)
#define HEAP_SIZE (4096 * 8)
typedef struct {
	byte program[PROGRAM_SIZE];
	u64 stack[STACK_SIZE];
	u8 *heap;

	int pc; // program counter
	int sp; // stack pointer

	int stack_n;
	int program_n;

	int csp; // call stack pointer
	u64 call_stack[CALL_STACK_SIZE];

	bool program_loaded;
} Stacum_VM;

void
stacum_load_program(Stacum_VM *vm,
		    u8 *preloaded_program, int preloaded_program_length) {
	assert(preloaded_program != NULL);
	assert(preloaded_program_length < PROGRAM_SIZE);
	memcpy(vm->program, preloaded_program, preloaded_program_length);

	vm->program_loaded = true;
}

void
stacum_load_stack(Stacum_VM *vm,
		  u64 *preloaded_stack, int preloaded_stack_length) {
	assert(preloaded_stack != NULL);
	assert(preloaded_stack_length < STACK_SIZE);
	memcpy(vm->stack, preloaded_stack, preloaded_stack_length);
}
void
stacum_load_heap(Stacum_VM *vm,
		 u8 *preloaded_heap, int preloaded_heap_length) {
	assert(preloaded_heap != NULL);
	assert(preloaded_heap_length < HEAP_SIZE);
	memcpy(vm->heap, preloaded_heap, preloaded_heap_length);
}


void
stacum_init(Stacum_VM *vm) {
	for(int i = 0; i < PROGRAM_SIZE; i++) {
		vm->program[i] = 0;
	}

	for(int i = 0; i < STACK_SIZE; i++) {
		vm->stack[i] = 0;
	}

	for(int i = 0; i < CALL_STACK_SIZE; i++) {
		vm->call_stack[i] = 0;
	}

	vm->heap = calloc(HEAP_SIZE, sizeof(u64));
	assert(vm->heap != NULL);
	
	vm->pc = 0;
	vm->sp = 0;
	vm->csp = 0;
	vm->program_loaded = false;
}

void
stacum_read_from_format(Stacum_VM *vm, stacum_format *format) {
	if(format->data) {
		stacum_load_heap(vm, format->data, format->data_n);
	}
	if(format->code) {
		stacum_load_program(vm, format->code, format->code_n);
	}
	if(format->stack) {
		stacum_load_stack(vm, format->stack, format->stack_n);
	}
}

INST
stacum_get_program_inst(Stacum_VM *vm) {
	return (INST)vm->program[vm->pc++];
}

byte
stacum_get_program_byte(Stacum_VM *vm) {
	return vm->program[vm->pc++];
}

u16
stacum_get_program_u16(Stacum_VM *vm) {
	u16 ret = u8_buf_get_u16(vm->program, vm->pc);
	vm->pc += sizeof(u16) / sizeof(byte);
	return ret;
}

u64
stacum_get_program_u64(Stacum_VM *vm) {
	u64 ret = u8_buf_get_u64(vm->program, vm->pc);
	vm->pc += sizeof(u64) / sizeof(byte);
	return ret;
}

u64
stacum_stack_pop(Stacum_VM *vm) {
	return vm->stack[--vm->sp];
}

void
stacum_stack_push(Stacum_VM *vm, u64 what) {
	vm->stack[vm->sp++] = what;
}

int
stacum_step(Stacum_VM *vm) {
	INST inst = stacum_get_program_inst(vm);

	printf("inst: '%s'\n", inst_string[inst]);

	/* TODO: make stack based operations more efficient */
	switch(inst) {
	case EXIT0 : {
		assert(0 && "EXITED UNSUCCESSFULLY");
		return 1;
	} break;
	case EXIT_GRACEFULLY : {
		return 1;
	} break;
	case NOP : {
		// nothing
	} break;
	case PUSH : {
		u64 pushant = stacum_get_program_u64(vm);
		stacum_stack_push(vm, pushant);
	} break;
	case DROP : {
		(void)stacum_stack_pop(vm);
	} break;
	case DUP : {
		u64 pushant = stacum_stack_pop(vm);
		stacum_stack_push(vm, pushant);
		stacum_stack_push(vm, pushant);
	} break;
	case ROT : { /* a b c - b c a (rotates left)*/
		u64 c = stacum_stack_pop(vm);
		u64 b = stacum_stack_pop(vm);
		u64 a = stacum_stack_pop(vm);
		stacum_stack_push(vm, b);
		stacum_stack_push(vm, c);
		stacum_stack_push(vm, a);
	} break;
	case OVER : { /* a b -- a b a */
		u64 b = stacum_stack_pop(vm);
		u64 a = stacum_stack_pop(vm);
		stacum_stack_push(vm, a);
		stacum_stack_push(vm, b);
		stacum_stack_push(vm, a);
	} break;
	case ADD : {
		u64 a = stacum_stack_pop(vm);
		u64 b = stacum_stack_pop(vm);
		stacum_stack_push(vm, b + a);
	} break;
	case SUB : {
		u64 a = stacum_stack_pop(vm);
		u64 b = stacum_stack_pop(vm);
		stacum_stack_push(vm, b - a);
	} break;
	case MUL : {
		u64 a = stacum_stack_pop(vm);
		u64 b = stacum_stack_pop(vm);
		stacum_stack_push(vm, b * a);
	} break;
	case DIV : {
		u64 a = stacum_stack_pop(vm);
		u64 b = stacum_stack_pop(vm);
		stacum_stack_push(vm, b / a);
	} break;
	case MOD : {
		u64 a = stacum_stack_pop(vm);
		u64 b = stacum_stack_pop(vm);
		stacum_stack_push(vm, b % a);
	} break;
	case AND : {
		u64 a = stacum_stack_pop(vm);
		u64 b = stacum_stack_pop(vm);
		stacum_stack_push(vm, b && a);
	} break;
	case BWAND : {
		u64 a = stacum_stack_pop(vm);
		u64 b = stacum_stack_pop(vm);
		stacum_stack_push(vm, b & a);
	} break;
	case OR : {
		u64 a = stacum_stack_pop(vm);
		u64 b = stacum_stack_pop(vm);
		stacum_stack_push(vm, b || a);
	} break;
	case BWOR : {
		u64 a = stacum_stack_pop(vm);
		u64 b = stacum_stack_pop(vm);
		stacum_stack_push(vm, b | a);
	} break;
	case NOT : {
		u64 a = stacum_stack_pop(vm);
		stacum_stack_push(vm, !a);
	} break;
	case BWNOT : {
		u64 a = stacum_stack_pop(vm);
		stacum_stack_push(vm, ~a);
	} break;
	case XOR : {
		u64 a = stacum_stack_pop(vm);
		u64 b = stacum_stack_pop(vm);
		stacum_stack_push(vm, (a || b) && !(a && b));
	} break;
	case BWXOR : {
		u64 a = stacum_stack_pop(vm);
		u64 b = stacum_stack_pop(vm);
		stacum_stack_push(vm, b ^ a);
	} break;
	case NOR : {
		u64 a = stacum_stack_pop(vm);
		u64 b = stacum_stack_pop(vm);
		stacum_stack_push(vm, !(a || b));
	} break;
	case BWNOR : {
		u64 a = stacum_stack_pop(vm);
		u64 b = stacum_stack_pop(vm);
		stacum_stack_push(vm, ~(a | b));
	} break;
	case XNOR : {
		u64 a = stacum_stack_pop(vm);
		u64 b = stacum_stack_pop(vm);
		stacum_stack_push(vm, !(a || b) || (a && b));
	} break;
	case BWXNOR : {
		u64 a = stacum_stack_pop(vm);
		u64 b = stacum_stack_pop(vm);
		stacum_stack_push(vm, ~(b ^ a));
	} break;
	case STACKALLOC : {
		assert(0 && "unimplemented");
	} break;
	case ALLOC : {
		assert(0 && "unimplemented");
	} break;
	case FREE : {
		assert(0 && "unimplemented");
	} break;
	case MEMGET8 : {
		u64 addr = stacum_stack_pop(vm);
		stacum_stack_push(vm, (u8)vm->heap[addr]);
	} break;
	case MEMSET8 : {
		u64 addr = stacum_stack_pop(vm);
		u8 val  = stacum_stack_pop(vm);
		*(u8 *)(&(vm->heap)[addr]) = val;
	} break;
	case MEMGET16 : {
		u64 addr = stacum_stack_pop(vm);
		stacum_stack_push(vm, (u16)vm->heap[addr]);
	} break;
	case MEMSET16 : {
		u64 addr = stacum_stack_pop(vm);
		u16 val  = stacum_stack_pop(vm);
		*(u16 *)(&(vm->heap)[addr]) = val;
	} break;
	case MEMGET32 : {
		u64 addr = stacum_stack_pop(vm);
		stacum_stack_push(vm, (u32)vm->heap[addr]);
	} break;
	case MEMSET32 : {
		u64 addr = stacum_stack_pop(vm);
		u32 val  = stacum_stack_pop(vm);
		*(u32 *)(&(vm->heap)[addr]) = val;
	} break;
	case MEMGET64 : {
		u64 addr = stacum_stack_pop(vm);
		stacum_stack_push(vm, vm->heap[addr]);
	} break;
	case MEMSET64 : {
		u64 addr = stacum_stack_pop(vm);
		u64 val  = stacum_stack_pop(vm);
		*(u64 *)(&(vm->heap)[addr]) = val;
	} break;
	case SYSCALL : {
		byte which = stacum_get_program_u16(vm);
		switch(which) {
		case 0 : {
			FILE     *fp = (FILE *)stacum_stack_pop(vm);
			size_t nmemb = (size_t)stacum_stack_pop(vm);
			size_t  size = (size_t)stacum_stack_pop(vm);

			u64 index = stacum_stack_pop(vm);
			u8 *place = &(vm->heap[index]);
			void *bufptr = place;

			vm->stack[vm->sp++] =
				fread(bufptr, size, nmemb, fp);
		} break;
		case 1 : {
			FILE     *fp = (FILE *)stacum_stack_pop(vm);
			size_t nmemb = (size_t)stacum_stack_pop(vm);
			size_t  size = (size_t)stacum_stack_pop(vm);

			u64 index = stacum_stack_pop(vm);
			u8 *place = &(vm->heap[index]);
			void *bufptr = place;

			vm->stack[vm->sp++] =
				fwrite(bufptr, size, nmemb, fp);
		} break;
		case 2 : {
			char *mode = (char *)stacum_stack_pop(vm);
			char *name = (char *)stacum_stack_pop(vm);
			vm->stack[vm->sp++] = (u64)fopen(name, mode);
		} break;
		case 3 : {
		        FILE *stream = (FILE *)stacum_stack_pop(vm);
			vm->stack[vm->sp++] = (u64)fclose(stream);
		} break;
		case 4 : {
			vm->stack[vm->sp++] = (u64)stdin;
		} break;
		case 5 : {
			vm->stack[vm->sp++] = (u64)stdout;
		} break;
		case 6 : {
			vm->stack[vm->sp++] = (u64)stderr;
		} break;

		case 255 : {
			printf("%ld\n", stacum_stack_pop(vm));
		} break;
		case 254 : {
			printf("%s\n", (char *)&vm->heap[stacum_stack_pop(vm)]);
		} break;

		case 22 : {
			int flags = (int)stacum_stack_pop(vm);
			char *filepath = (char *)stacum_stack_pop(vm);
			void *handle = dlopen(filepath, flags);
			stacum_stack_push(vm, (u64)handle);
		} break;
		case 23 : {
			char *symbol_name = (char *)stacum_stack_pop(vm);
			void *handle = (void *)stacum_stack_pop(vm);
			dynamic_function function =
				(dynamic_function)dlsym(handle, symbol_name);
			stacum_stack_push(vm, (u64)function);
		} break;
		case 24 : {
			dynamic_function function =
				(dynamic_function)stacum_stack_pop(vm);
			byte args = stacum_get_program_u16(vm);
			switch(args) {
			case 0: { stacum_stack_push(vm, function());
			} break;
			case 1: { stacum_stack_push(vm, 
						    function(stacum_stack_pop(vm)));
			} break;
			case 2: { stacum_stack_push(vm, 
						    function(stacum_stack_pop(vm),
							     stacum_stack_pop(vm)));
			} break;
			case 3: { stacum_stack_push(vm, 
						    function(stacum_stack_pop(vm),
							     stacum_stack_pop(vm),
							     stacum_stack_pop(vm)));
			} break;
			case 4: { stacum_stack_push(vm, 
						    function(stacum_stack_pop(vm),
							     stacum_stack_pop(vm),
							     stacum_stack_pop(vm),
							     stacum_stack_pop(vm)));
			} break;
			case 5: { stacum_stack_push(vm, 
						    function(stacum_stack_pop(vm),
							     stacum_stack_pop(vm),
							     stacum_stack_pop(vm),
							     stacum_stack_pop(vm),
							     stacum_stack_pop(vm)));
			} break;
			case 6: { stacum_stack_push(vm, 
						    function(stacum_stack_pop(vm),
							     stacum_stack_pop(vm),
							     stacum_stack_pop(vm),
							     stacum_stack_pop(vm),
							     stacum_stack_pop(vm),
							     stacum_stack_pop(vm)));
			} break;
			}
		} break;
		}
	} break;
	case JMP0 : {
		printf("JMP0\n");
		if(!stacum_stack_pop(vm)) {
			goto JMP_LABEL;
		} else {
			stacum_get_program_u64(vm);
		}
	} break;
	case CALL0 : {
		if(!stacum_stack_pop(vm)) {
			goto CALL_LABEL;
		} else {
			stacum_get_program_u64(vm);
		}
	} break;
	case JMP1 : {
		if(stacum_stack_pop(vm)) {
			goto JMP_LABEL;
		} else {
			stacum_get_program_u64(vm);
		}
	} break;
	case CALL1 : {
		if(stacum_stack_pop(vm)) {
			goto CALL_LABEL;
		} else {
			stacum_get_program_u64(vm);
		}
	} break;
	JMP_LABEL:
	case JMP : {
		u64 addr = stacum_get_program_u64(vm);
		vm->pc = addr;
	} break;
	CALL_LABEL:
	case CALL : {
		u64 addr = stacum_get_program_u64(vm);
		vm->pc = addr;
		vm->call_stack[vm->csp++] = addr;
	} break;
	case RET : {
		u64 addr = vm->call_stack[--vm->csp];
		vm->pc = addr;
	} break;
	case EQU : {
		u64 a = stacum_stack_pop(vm);
		u64 b = stacum_stack_pop(vm);
		vm->stack[vm->sp++] = b == a;
	} break;
	case NEQ : {
		u64 a = stacum_stack_pop(vm);
		u64 b = stacum_stack_pop(vm);
		vm->stack[vm->sp++] = b != a;
	} break;
	case LST : {
		u64 a = stacum_stack_pop(vm);
		u64 b = stacum_stack_pop(vm);
		vm->stack[vm->sp++] = b < a;
	} break;
	case GRT : {
		u64 a = stacum_stack_pop(vm);
		u64 b = stacum_stack_pop(vm);
		vm->stack[vm->sp++] = b > a;
	} break;
	case LEQ : {
		u64 a = stacum_stack_pop(vm);
		u64 b = stacum_stack_pop(vm);
		vm->stack[vm->sp++] = b <= a;
	} break;
	case GEQ : {
		u64 a = stacum_stack_pop(vm);
		u64 b = stacum_stack_pop(vm);
		vm->stack[vm->sp++] = b >= a;
	} break;
	}

	return 0;
}

void
stacum_run(Stacum_VM *vm) {
	if(!vm->program_loaded) {
		return;
	}

	while(stacum_step(vm) == 0) {
	};
	printf("[\n  Exited Gracefully\n]\n");
}

int
main(int argc, char **argv) {
	if(argc != 2) {
		printf("argc aint 2\n");
		return 1;
	}
	printf("argv[1] = %s\n", argv[1]);

	Stacum_VM vm;
	stacum_init(&vm);

	u8 *buf = NULL;
	int buf_size = 0;
	readwholefile(argv[1], &buf, &buf_size);
	assert(buf != NULL && buf_size != 0);

	sized_u8_buffer sbuf;
	init_sized_u8_buffer(&sbuf, buf, buf_size, 0);

	stacum_format format;
	stacum_format_read_from_buffer(&format, &sbuf);

	stacum_read_from_format(&vm, &format);

	stacum_run(&vm);

	return 0;
}
