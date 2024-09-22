#include <stdio.h>
#include <unistd.h>
#include <search.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <stdint.h>
#include <assert.h>
#include <dlfcn.h>

typedef uint8_t byte;
typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

#define PROGRAM_SIZE 32000
typedef enum {
	EXIT0,
	EXIT_GRACEFULLY,
	NOP,
	PUSH,
	ADD,
	SUB,
	MUL,
	DIV,
	MOD,
	AND,
	BWAND,
	OR,
	BWOR,
	NOT,
	BWNOT,
	XOR,
	BWXOR,
	NOR,
	BWNOR,
	XNOR,
	BWXNOR,
	STACKALLOC,
	ALLOC,
	FREE,
	MEMGET8,
	MEMSET8,
	MEMGET16,
	MEMSET16,
	MEMGET32,
	MEMSET32,
	MEMGET64,
	MEMSET64,
	SYSCALL,
	JMP0,
	CALL0,
	JMP1,
	CALL1,
	JMP,
	CALL,
	RET,
	EQU,
	NEQ,
	LST,
	GRT,
	LEQ,
	GEQ
} INST;

char *inst_string[] = {
	[EXIT0] = "EXIT0",
	[EXIT_GRACEFULLY] = "EXIT_GRACEFULLY",
	[NOP] = "NOP",
	[PUSH] = "PUSH",
	[ADD] = "ADD",
	[SUB] = "SUB",
	[MUL] = "MUL",
	[DIV] = "DIV",
	[MOD] = "MOD",
	[AND] = "AND",
	[BWAND] = "BWAND",
	[OR] = "OR",
	[BWOR] = "BWOR",
	[NOT] = "NOT",
	[BWNOT] = "BWNOT",
	[XOR] = "XOR",
	[BWXOR] = "BWXOR",
	[NOR] = "NOR",
	[BWNOR] = "BWNOR",
	[XNOR] = "XNOR",
	[BWXNOR] = "BWXNOR",
	[STACKALLOC] = "STACKALLOC",
	[ALLOC] = "ALLOC",
	[FREE] = "FREE",
	[MEMGET8] = "MEMGET8",
	[MEMSET8] = "MEMSET8",
	[MEMGET16] = "MEMGET16",
	[MEMSET16] = "MEMSET16",
	[MEMGET32] = "MEMGET32",
	[MEMSET32] = "MEMSET32",
	[MEMGET64] = "MEMGET64",
	[MEMSET64] = "MEMSET64",
	[SYSCALL] = "SYSCALL",
	[JMP0] = "JMP0",
	[CALL0] = "CALL0",
	[JMP1] = "JMP1",
	[CALL1] = "CALL1",
	[JMP] = "JMP",
	[CALL] = "CALL",
	[RET] = "RET",
	[EQU] = "EQU",
	[NEQ] = "NEQ",
	[LST] = "LST",
	[GRT] = "GRT",
	[LEQ] = "LEQ",
	[GEQ] = "GEQ"
};

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

INST
stacum_get_program_inst(Stacum_VM *vm) {
	return (INST)vm->program[vm->pc++];
}

byte
stacum_get_program_byte(Stacum_VM *vm) {
	return vm->program[vm->pc++];
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
	switch(stacum_get_program_inst(vm)) {
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
		byte which = stacum_get_program_byte(vm);
		if(which == 255) {
			printf("%ld\n", stacum_stack_pop(vm));
		} else if(which == 254) {
			printf("got here\n");
			printf("%s\n", (char *)&vm->heap[stacum_stack_pop(vm)]);
		} else if(which == 0) {
			//FILE *fp = (FILE *)stacum_stack_pop(vm);
			//u64 nbyte  = stacum_stack_pop(vm);
			//u64 bufptr = stacum_stack_pop(vm);
			//vm->stack[vm->sp++] =
			//	fwrite(&vm->heap[bufptr], nbyte, fp);
			//size_t fwrite(const void ptr[restrict .size * .nmemb],
			//	      size_t size, size_t nmemb,
			//	      FILE *restrict stream);
		} else if(which == 1) {
			//u64 nbyte  = stacum_stack_pop(vm);
			//u64 bufptr = stacum_stack_pop(vm);
			//u64 fildes = stacum_stack_pop(vm);
			//vm->stack[vm->sp++] =
			//	fread(fildes, &vm->heap[bufptr], nbyte);
			//size_t fread(void ptr[restrict .size * .nmemb],
			//	     size_t size, size_t nmemb,
			//	     FILE *restrict stream);
		} else if(which == 3) {
			int flags = (int)stacum_stack_pop(vm);
			char *filepath = (char *)stacum_stack_pop(vm);
			void *handle = dlopen(filepath, flags);
			stacum_stack_push(vm, (u64)handle);
		} else if(which == 4) {
			char *symbol_name = (char *)stacum_stack_pop(vm);
			void *handle = (void *)stacum_stack_pop(vm);
			dynamic_function function =
				(dynamic_function)dlsym(handle, symbol_name);
			stacum_stack_push(vm, (u64)function);
		} else if(which == 5) {
			dynamic_function function =
				(dynamic_function)stacum_stack_pop(vm);
			byte args = stacum_get_program_byte(vm);
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
		}
	} break;
	case JMP0 : {
		if(!stacum_stack_pop(vm)) {
			goto JMP_LABEL;
		}
	} break;
	case CALL0 : {
		if(!stacum_stack_pop(vm)) {
			goto CALL_LABEL;
		}
	} break;
	case JMP1 : {
		if(stacum_stack_pop(vm)) {
			goto JMP_LABEL;
		}
	} break;
	case CALL1 : {
		if(stacum_stack_pop(vm)) {
			goto CALL_LABEL;
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

	stacum_run(&vm);

	return 0;
}
