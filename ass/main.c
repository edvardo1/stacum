#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdint.h>

typedef uint8_t byte;
typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

int
readwholefile(char *filepath, char **output) {
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

typedef enum {
	NUMBER,
	OPERATOR,
	ID,
	STRING
} TokenType;
char *tokentype_as_string[] = {
	[NUMBER] = "NUMBER",
	[OPERATOR] = "OPERATOR",
	[ID] = "ID",
	[STRING] = "STRING"
};

typedef struct {
	char *str;
	int len;
	TokenType type;
} Token;
typedef struct {
	char *str;
	int len;
} TokenWithNoType;

typedef struct {
	char *str;

	int in_comment;
	int max;
	int pos;
} Lexer;

static inline TokenWithNoType token_with_no_type_of_token(Token *token) {
	TokenWithNoType twnt;
	twnt.str = malloc(token->len);
	assert(twnt.str != NULL);
	memcpy(twnt.str, token->str, token->len);
	twnt.len = token->len;
	return twnt;
}

void lexer_copy_string(Lexer *lexer, char *str) {
	lexer->max = strlen(str) + 1;
	lexer->str = malloc(lexer->max);
	assert(lexer->str != NULL);
	memcpy(lexer->str, str, lexer->max);
	lexer->pos = 0;
}

void lexer_init(Lexer *lexer) {
	lexer->pos = -1;
	lexer->max = -1;
	lexer->in_comment = 0;
	lexer->str = NULL;
}

char lexer_get_ch(Lexer *lexer, int index) {
	if(index < lexer->max) {
		return lexer->str[index];
	} else {
		return 0;
	}
}

// id =
//   alpha .. alphanumeric * | '_' *

int is_alpha(char c) {
	return (('a' <= c && c <= 'z') ||
		('A' <= c && c <= 'Z'));
}

int is_number(char c) {
	return ('0' <= c && c <= '9');
}

int is_alphanumeric(char c) {
	return is_alpha(c) || is_number(c);
}

char *strcpy_len(char *str, int len) {
	char *ret = malloc(len);
	assert(ret != NULL);
	memcpy(ret, str, len);
	return ret;
}

void token_set(Token *token, char *str, int len, TokenType type) {
	token->str = strcpy_len(str, len + 1);
	token->str[len] = '\0';
	token->len = len;
	token->type = type;
}

int find_number_len(char *str) {
	int len;
	for(len = 1; str[len] != '\0'; len++) {
		if(!is_number(str[len])) {
			return len;
		}
	}
	return len;
}

int find_string_len(char *str) {
	int len;
	for(len = 1; str[len] != '\0'; len++) {
		if(str[len] == '"') {
			return len + 1;
		}
	}
	return len;
}

int is_id_char(char c) {
	return (c != '\0' &&
		c != ' '  &&
		c != '\t' &&
		c != '\n' &&
		c != '"' &&
		1
		);
}

int find_id_len(char *str) {
	int len;
	for(len = 1; str[len] != '\0'; len++) {
		//if(!(is_alphanumeric(str[len]) || str[len] == '_')) {
		if(!is_id_char(str[len])) {
			return len;
		}
	}
	return len;
}

#define LEXER_CONTINUE 0 
#define LEXER_DONE 1
#define LEXER_ERROR 2

int lexer_get_token_copy(Lexer *lexer, Token *token) {
	if(lexer->str == NULL) {
		printf("GOT ERROR!\n");
		return LEXER_ERROR;
		token->len = 2;
	}
	assert(lexer->str != NULL);

	if(lexer->pos >= lexer->max) {
		return LEXER_DONE;
	}

	int len;

	for(int i = lexer->pos; i < lexer->max; i++) {
		if(lexer_get_ch(lexer, i) == '/' &&
		   lexer_get_ch(lexer, i+1) == '*') {
			lexer->in_comment += 1;
		}

		if(lexer_get_ch(lexer, i) == '*' &&
		   lexer_get_ch(lexer, i+1) == '/') {
			if(lexer->in_comment <= 0) {
				printf("GOT ERROR!\n");
				return LEXER_ERROR;
			}
			lexer->in_comment -= 1;

			i++;
			continue;
		}

		if(lexer->in_comment > 0) {
			continue;
		}

		if(lexer->str[i] == '"') {
			len = find_string_len(&lexer->str[i]);
			token_set(token, &lexer->str[i], len, STRING);
			lexer->pos = i + len;

			return LEXER_CONTINUE;
		}

		if(is_id_char(lexer->str[i])) {
			len = find_id_len(&lexer->str[i]);
			token_set(token, &lexer->str[i], len, ID);
			lexer->pos = i + len;

			return LEXER_CONTINUE;
		}
	}

	return LEXER_DONE;
}

int lexer_get_all_tokens(Lexer *lexer, Token **tokens, int *number) {
	int size = 16;

	*tokens = malloc(sizeof(Token) * size);
	if(*tokens == NULL) { goto error; }

	int token_index = 0;
	int running = 1;

	while(running) {
		if(token_index + 1 >= size) {
			size *= 2;

			*tokens = realloc(*tokens, sizeof(Token) * size);
			if(tokens == NULL) { goto error; }
		}

		int exit = lexer_get_token_copy(lexer, &((*tokens)[token_index]));
		if(exit == LEXER_ERROR) { goto error; }
		if(exit == LEXER_DONE) { running = 0; }
		if(exit == LEXER_CONTINUE) { token_index++; }
	}

	*tokens = realloc(*tokens, sizeof(Token) * token_index);
	if(*tokens == NULL) { goto error; }

	*number = token_index;

	return 0;
error:
	if(*tokens != NULL) {
		free(*tokens);
	}
	return 1;
}

typedef struct {
	char *str;
	int len;
	u64 pos;
} Label;
typedef struct {
	Token *tokens;
	int tokens_c;
	int tokens_n;

	Label labels[512];
	int labels_c;
} Assembler;

Token *
assembler_token_get(Assembler *ass) {
	if(ass->tokens_c >= ass->tokens_n) {
		return NULL;
	}
	return &ass->tokens[ass->tokens_c++];
}

void
assembler_token_reset(Assembler *ass) {
	ass->tokens_c = 0;
}

void
assembler_init(Assembler *ass, Token *tokens, int tokens_n) {
	ass->tokens = tokens;
	ass->tokens_n = tokens_n;

	ass->labels_c = 0;
	ass->tokens_c = 0;
}

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
	[EXIT0]           = "exit0",
	[EXIT_GRACEFULLY] = "exit_gracefully",
	[NOP]             = "nop",
	[PUSH]            = "push",
	[ADD]             = "add",
	[SUB]             = "sub",
	[MUL]             = "mul",
	[DIV]             = "div",
	[MOD]             = "mod",
        [AND]             = "and",
        [BWAND]           = "bwand",
        [OR]              = "or",
        [BWOR]            = "bwor",
        [NOT]             = "not",
        [BWNOT]           = "bwnot",
	[XOR]             = "xor",
	[BWXOR]           = "bwxor",
	[NOR]             = "nor",
	[BWNOR]           = "bwnor",
	[XNOR]            = "xnor",
	[BWXNOR]          = "bwxnor",
	[STACKALLOC     ] = "stackalloc",
	[ALLOC]           = "alloc",
	[FREE]            = "free",
	[MEMGET8]         = "memget8",
	[MEMSET8]         = "memset8",
	[MEMGET16]        = "memget16",
	[MEMSET16]        = "memset16",
	[MEMGET32]        = "memget32",
	[MEMSET32]        = "memset32",
	[MEMGET64]        = "memget64",
	[MEMSET64]        = "memset64",
	[SYSCALL]         = "syscall",
	[JMP0]            = "jmp0",
	[CALL0]           = "call0",
	[JMP1]            = "jmp1",
	[CALL1]           = "call1",
	[JMP]             = "jmp",
	[CALL]            = "call",
	[RET]             = "ret",
	[EQU]             = "equ",
	[NEQ]             = "neq",
	[LST]             = "lst",
	[GRT]             = "grt",
	[LEQ]             = "leq",
	[GEQ]             = "geq"
};

int
token_is_number(Token *token) {
	return 1;
}

u64
token_get_number(Token *token) {
	char *str = strndup(token->str, token->len);
	return strtol(str, NULL, 0);
}

int
string_equals(char *str1, char *str2, int len1, int len2) {
	if(len1 != len2) {
		return 0;
	}
	return strncmp(str1, str2, len1) == 0;
}

int
is_instruction(Token *token, u8 *n) {
	for(int i = 0; i < sizeof(inst_string) / sizeof(inst_string[0]); i++) {
		if(strncmp(token->str, inst_string[i], strlen(inst_string[i])) == 0) {
			if(n != NULL) {
				*n = i;
			}
			return 1;
		}
	}

	return 0;
}

int
assembler_assemble(Assembler *ass, u8 **output, int *output_size) {
	/* look for labels */
	int size_so_far = 0;
	Token *t;
	t = assembler_token_get(ass);
	while (t != NULL) {
		if(strncmp(":", t->str, t->len) == 0) {
			t = assembler_token_get(ass);
			assert(t != NULL);

			ass->labels[ass->labels_c].str =
				strndup(t->str, t->len);
			ass->labels[ass->labels_c].len = t->len;
			ass->labels[ass->labels_c].pos = size_so_far;
			ass->labels_c++;
		} else if(is_instruction(t, NULL)) {
			size_so_far += 1;
		} else if(token_is_number(t)) {
			size_so_far += 8;
		}

		t = assembler_token_get(ass);
	}

	u8 *buf = malloc(size_so_far);
	assert(buf != NULL);
	int buf_index = 0;

	struct {
		char *name;
	        int bytesize;
	} type_array[] = {
		{"I8", 1},
		{"I16", 2},
		{"I32", 4},
		{"I64", 8},
		{"U8", 1},
		{"U16", 2},
		{"U32", 4},
		{"U64", 8}
	};
	int next_size = 0;

	assembler_token_reset(ass);
	t = assembler_token_get(ass);
	while (t != NULL) {
		u8 inst = 0;
		if(strncmp(":", t->str, t->len) == 0) {
			t = assembler_token_get(ass);
			assert(t != NULL);
			t = assembler_token_get(ass);
			assert(t != NULL);
			continue;
		}

		for(int i = 0; i < ass->labels_c; i++) {
			if(strncmp(t->str, ass->labels[i].str, t->len) == 0) {
				//printf("%20s -> %016x: %lx\n",
				//       t->str, buf_index, ass->labels[i].pos);
				buf[buf_index++] = (ass->labels[i].pos >> 56) & 0xff;
				buf[buf_index++] = (ass->labels[i].pos >> 48) & 0xff;
				buf[buf_index++] = (ass->labels[i].pos >> 40) & 0xff;
				buf[buf_index++] = (ass->labels[i].pos >> 32) & 0xff;
				buf[buf_index++] = (ass->labels[i].pos >> 24) & 0xff;
				buf[buf_index++] = (ass->labels[i].pos >> 16) & 0xff;
				buf[buf_index++] = (ass->labels[i].pos >> 8)  & 0xff;
				buf[buf_index++] = (ass->labels[i].pos >> 0)  & 0xff;

				t = assembler_token_get(ass);
				continue;
			}
		}
		if(is_instruction(t, &inst)) {
			//printf("%20s -> %016x: %s\n", t->str, buf_index, inst_string[inst]);
			buf[buf_index++] = inst;
		} else if(t->str[0] == '&') {
			int i;
			for(i = 0; i < sizeof(type_array) / sizeof(type_array[0]); i++) {
				if(string_equals(&t->str[1], type_array[i].name,
						 t->len-1, strlen(type_array[i].name))) {
					goto skip_crash;
				}
			}
			printf("\"%s\" is not equal to any int type\n", t->str);
			assert(0);
		skip_crash:
			next_size = type_array[i].bytesize;
		} else if(token_is_number(t)) {
			u64 num = token_get_number(t);
			//printf("%20s -> %016x: %lx\n", t->str, buf_index, num);
			//buf[buf_index++] = (num >> 56) & 0xff;
			//buf[buf_index++] = (num >> 48) & 0xff;
			//buf[buf_index++] = (num >> 40) & 0xff;
			//buf[buf_index++] = (num >> 32) & 0xff;
			//buf[buf_index++] = (num >> 24) & 0xff;
			//buf[buf_index++] = (num >> 16) & 0xff;
			//buf[buf_index++] = (num >> 8)  & 0xff;
			//buf[buf_index++] = (num >> 0)  & 0xff;
			if(next_size == 0) {
				next_size = 8;
			}
			for(int i = next_size - 1; i >= 0; i--) {
				buf[buf_index++] = (num >> (next_size * i)) & 0xff;
			}

			next_size = 0;
		}

		t = assembler_token_get(ass);
	}
	int buf_size = buf_index;

	for(int i = 0; i < buf_size; i++) {
		//printf("%x\n", buf[i]);
	}

	*output = buf;
	*output_size = buf_size;
	//for(int i = 0; i < buf_size; i++) {
	//	printf("%x ", buf[i]);
	//}
	//fputc('\n', stdout);

	return 1;
}

int
main(int argc, char **argv) {
	if(argc != 3) {
		printf("argc aint 3\n");
		return 1;
	}
	printf("argv[1] = %s\n", argv[1]);
	printf("argv[2] = %s\n", argv[2]);
	Lexer lexer;

	lexer_init(&lexer);

	char *program;
	readwholefile(argv[1], &program);
	assert(program != NULL);

	lexer_copy_string(&lexer, program);

	Token *tokens;
	int tokens_n;

	if(lexer_get_all_tokens(&lexer, &tokens, &tokens_n)) {
		printf("ERROR\n");
		return 1;
	}

	Assembler ass;
	assembler_init(&ass, tokens, tokens_n);
	u8 *out = NULL;
	int out_size = 0;
	assembler_assemble(&ass, &out, &out_size);
	assert(out != NULL);

	FILE *fp = fopen(argv[2], "wb");
	assert(fp != NULL);
	assert(fwrite(out, 1, out_size, fp) == out_size);
	fclose(fp);

	return 0;
}
