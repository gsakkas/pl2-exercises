#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <time.h>

// op-codes
#define HALT 0x00
#define JUMP 0x01
#define JNZ 0x02
#define DUP 0x03
#define DROP 0x04
#define PUSH4 0x05
#define PUSH2 0x06
#define PUSH1 0x07
#define ADD 0x08
#define SUB 0x09
#define MUL 0x0a
#define DIV 0x0b
#define MOD 0x0c
#define EQ 0x0d
#define NE 0x0e
#define LT 0x0f
#define GT 0x10
#define LE 0x11
#define GE 0x12
#define NOT 0x13
#define AND 0x14
#define OR 0x15
#define INPUT 0x16
#define OUTPUT 0x17
#define CLOCK 0x2a

// Length of original label_tab
#define LEN 0x2b

// Debug
// #define __DEBUG__
// #define __DEBUG_STACK__
// #define __DEBUG_GET_BYTES__


// Helper functions
#define push(STACK, TOP, ELEM) (STACK[++TOP] = ELEM)

#define pop(STACK, TOP) (STACK[TOP--])

uint8_t get_1_ubyte(uint8_t *pc) {
	#ifdef __DEBUG_GET_BYTES__
		printf("1_byte: %c\n", pc[0]);
	#endif
	return pc[0];
}

uint16_t get_2_ubytes(uint8_t *pc) {
	#ifdef __DEBUG_GET_BYTES__
		printf("2_bytes: %hd\n", pc[0] + (pc[1] << 8));
	#endif
	return pc[0] + (pc[1] << 8);
}

int8_t get_1_byte(uint8_t *pc) {
	#ifdef __DEBUG_GET_BYTES__
		printf("1_byte: %c\n", pc[0]);
	#endif
	return pc[0];
}

int16_t get_2_bytes(uint8_t *pc) {
	#ifdef __DEBUG_GET_BYTES__
		printf("2_bytes: %hd\n", pc[0] + (pc[1] << 8));
	#endif
	return pc[0] + (pc[1] << 8);
}

int32_t get_4_bytes(uint8_t *pc) {
	#ifdef __DEBUG_GET_BYTES__
		printf("4_bytes: %d\n", pc[0] + (pc[1] << 8) + (pc[2] << 16) + (pc[3] << 24));
	#endif
	return pc[0] + (pc[1] << 8) + (pc[2] << 16) + (pc[3] << 24);
}


// Indirectly threaded interpreter's next instruction
#ifndef __DEBUG_STACK__
	#define NEXT_INSTR goto *(void *)(label_tab[(*pc) + (LEN * state)])
#else
	#define NEXT_INSTR goto print_stack_label
	#define NEXT_INSTR_ORIG goto *(void *)(label_tab[(*pc) + (LEN * state)])
#endif
#define EXIT goto exit_label


int main(int argc, char const *argv[]) {
	// Indirectly threaded interpreter's label table
	static void *label_tab[] = {
		&&halt_label_0,
		&&jump_label_0,
		&&jnz_label_0,
		&&dup_label_0,
		&&drop_label_0,
		&&push4_label_0,
		&&push2_label_0,
		&&push1_label_0,
		&&add_label_0,
		&&sub_label_0,
		&&mul_label_0,
		&&div_label_0,
		&&mod_label_0,
		&&eq_label_0,
		&&ne_label_0,
		&&lt_label_0,
		&&gt_label_0,
		&&le_label_0,
		&&ge_label_0,
		&&not_label_0,
		&&and_label_0,
		&&or_label_0,
		&&input_label_0,
		&&ouput_label_0,
		&&not_valid_label,
		&&not_valid_label,
		&&not_valid_label,
		&&not_valid_label,
		&&not_valid_label,
		&&not_valid_label,
		&&not_valid_label,
		&&not_valid_label,
		&&not_valid_label,
		&&not_valid_label,
		&&not_valid_label,
		&&not_valid_label,
		&&not_valid_label,
		&&not_valid_label,
		&&not_valid_label,
		&&not_valid_label,
		&&not_valid_label,
		&&not_valid_label,
		&&clock_label_0,
		&&halt_label_1,
		&&jump_label_1,
		&&jnz_label_1,
		&&dup_label_1,
		&&drop_label_1,
		&&push4_label_1,
		&&push2_label_1,
		&&push1_label_1,
		&&add_label_1,
		&&sub_label_1,
		&&mul_label_1,
		&&div_label_1,
		&&mod_label_1,
		&&eq_label_1,
		&&ne_label_1,
		&&lt_label_1,
		&&gt_label_1,
		&&le_label_1,
		&&ge_label_1,
		&&not_label_1,
		&&and_label_1,
		&&or_label_1,
		&&input_label_1,
		&&ouput_label_1,
		&&not_valid_label,
		&&not_valid_label,
		&&not_valid_label,
		&&not_valid_label,
		&&not_valid_label,
		&&not_valid_label,
		&&not_valid_label,
		&&not_valid_label,
		&&not_valid_label,
		&&not_valid_label,
		&&not_valid_label,
		&&not_valid_label,
		&&not_valid_label,
		&&not_valid_label,
		&&not_valid_label,
		&&not_valid_label,
		&&not_valid_label,
		&&not_valid_label,
		&&clock_label_1
	};

	// Take program from stdin
	FILE *fin = fopen(argv[1], "rb");

	// Initialize a stack to hold the program
	register int32_t top = -1;
	register int32_t top_of_stack;
	int32_t stack[1 << 16];
	uint8_t program[1 << 16];

	// Read the file that contains the program
	uint16_t length = 0;
	while (fscanf(fin, "%c", &program[length]) == 1) length++;
	fclose(fin);

	// The Bytecode Interpreter
	register uint8_t *pc = &program[0];
	register bool state = 0;
	register bool loop = true;
	clock_t start_time = clock();
	NEXT_INSTR;
	while (loop) {
		#ifdef __DEBUG_STACK__
		print_stack_label:
			if (top < 0) printf("stack[-1] = ...\n");
			else {
				for (int32_t j = top; j >= 0; j--){
					printf("stack[%d] = %d\n", j, stack[j]);
				}
			}
			NEXT_INSTR_ORIG;
		#endif
		halt_label_0:
		{
			#ifdef __DEBUG__
				printf("HALT\n");
			#endif
			EXIT;
		}
		jump_label_0:
		{
			#ifdef __DEBUG__
				printf("JUMP\n");
			#endif
			uint16_t jump_addr = get_2_ubytes(&pc[1]);
			pc = &program[jump_addr];
			NEXT_INSTR;
		}
		jnz_label_0:
		{
			#ifdef __DEBUG__
				printf("JNZ\n");
			#endif
			uint16_t jump_addr = get_2_ubytes(&pc[1]);
			int32_t stack_top = pop(stack, top);
			pc = (stack_top != 0) ? &program[jump_addr] : (pc + 3);
			NEXT_INSTR;
		}
		dup_label_0:
		{
			#ifdef __DEBUG__
				printf("DUP\n");
			#endif
			uint8_t i = get_1_ubyte(&pc[1]);
			pc += 2;
			state++;
			top_of_stack = stack[top - i];
			NEXT_INSTR;
		}
		drop_label_0:
		{
			#ifdef __DEBUG__
				printf("DROP\n");
			#endif
			pc += 1;
			top--;
			NEXT_INSTR;
		}
		push4_label_0:
		{
			#ifdef __DEBUG__
				printf("PUSH4\n");
			#endif
			state++;
			top_of_stack = get_4_bytes(&pc[1]);
			pc += 5;
			NEXT_INSTR;
		}
		push2_label_0:
		{
			#ifdef __DEBUG__
				printf("PUSH2\n");
			#endif
			state++;
			top_of_stack = get_2_bytes(&pc[1]);
			pc += 3;
			NEXT_INSTR;
		}
		push1_label_0:
		{
			#ifdef __DEBUG__
				printf("PUSH1\n");
			#endif
			state++;
			top_of_stack = get_1_byte(&pc[1]);
			pc += 2;
			NEXT_INSTR;
		}
		add_label_0:
		{
			#ifdef __DEBUG__
				printf("ADD\n");
			#endif
			pc += 1;
			state++;
			top_of_stack = stack[top - 1] + stack[top];
			top -= 2;
			NEXT_INSTR;
		}
		sub_label_0:
		{
			#ifdef __DEBUG__
				printf("SUB\n");
			#endif
			pc += 1;
			state++;
			top_of_stack = stack[top - 1] - stack[top];
			top -= 2;
			NEXT_INSTR;
		}
		mul_label_0:
		{
			#ifdef __DEBUG__
				printf("MUL\n");
			#endif
			pc += 1;
			state++;
			top_of_stack = stack[top - 1] * stack[top];
			top -= 2;
			NEXT_INSTR;
		}
		div_label_0:
		{
			#ifdef __DEBUG__
				printf("DIV\n");
			#endif
			pc += 1;
			state++;
			top_of_stack = stack[top - 1] / stack[top];
			top -= 2;
			NEXT_INSTR;
		}
		mod_label_0:
		{
			#ifdef __DEBUG__
				printf("MOD\n");
			#endif
			pc += 1;
			state++;
			top_of_stack = stack[top - 1] % stack[top];
			top -= 2;
			NEXT_INSTR;
		}
		eq_label_0:
		{
			#ifdef __DEBUG__
				printf("EQ\n");
			#endif
			pc += 1;
			state++;
			top_of_stack = stack[top - 1] == stack[top];
			top -= 2;
			NEXT_INSTR;
		}
		ne_label_0:
		{
			#ifdef __DEBUG__
				printf("NE\n");
			#endif
			pc += 1;
			state++;
			top_of_stack = stack[top - 1] != stack[top];
			top -= 2;
			NEXT_INSTR;
		}
		lt_label_0:
		{
			#ifdef __DEBUG__
				printf("LT\n");
			#endif
			pc += 1;
			state++;
			top_of_stack = stack[top - 1] < stack[top];
			top -= 2;
			NEXT_INSTR;
		}
		gt_label_0:
		{
			#ifdef __DEBUG__
				printf("GT\n");
			#endif
			pc += 1;
			state++;
			top_of_stack = stack[top - 1] > stack[top];
			top -= 2;
			NEXT_INSTR;
		}
		le_label_0:
		{
			#ifdef __DEBUG__
				printf("LE\n");
			#endif
			pc += 1;
			state++;
			top_of_stack = stack[top - 1] <= stack[top];
			top -= 2;
			NEXT_INSTR;
		}
		ge_label_0:
		{
			#ifdef __DEBUG__
				printf("GE\n");
			#endif
			pc += 1;
			state++;
			top_of_stack = stack[top - 1] >= stack[top];
			top -= 2;
			NEXT_INSTR;
		}
		not_label_0:
		{
			#ifdef __DEBUG__
				printf("NOT\n");
			#endif
			pc += 1;
			state++;
			top_of_stack = !stack[top];
			top--;
			NEXT_INSTR;
		}
		and_label_0:
		{
			#ifdef __DEBUG__
				printf("AND\n");
			#endif
			pc += 1;
			state++;
			top_of_stack = stack[top - 1] && stack[top];
			top -= 2;
			NEXT_INSTR;
		}
		or_label_0:
		{
			#ifdef __DEBUG__
				printf("OR\n");
			#endif
			pc += 1;
			state++;
			top_of_stack = stack[top - 1] || stack[top];
			top -= 2;
			NEXT_INSTR;
		}
		input_label_0:
		{
			#ifdef __DEBUG__
				printf("INPUT\n");
			#endif
			pc += 1;
			state++;
			char ch;
			if (scanf("%c", &ch) != 1) {
				printf("Error: Problem with input!\n");
				return -1;
			}
			top_of_stack = ch;
			NEXT_INSTR;
		}
		ouput_label_0:
		{
			#ifdef __DEBUG__
				printf("OUTPUT\n");
			#endif
			pc += 1;
			int32_t ch = pop(stack, top);
			printf("%c", (char)ch);
			#ifdef __DEBUG__
				printf("\n");
				printf("%d\n", ch);
			#endif
			NEXT_INSTR;
		}
		clock_label_0:
		{
			#ifdef __DEBUG__
				printf("CLOCK\n");
			#endif
			pc += 1;
			double time_spent = (double)(clock() - start_time) / CLOCKS_PER_SEC;
			printf("%0.6lf\n", time_spent);
			NEXT_INSTR;
		}
		halt_label_1:
		{
			#ifdef __DEBUG__
				printf("HALT\n");
			#endif
			EXIT;
		}
		jump_label_1:
		{
			#ifdef __DEBUG__
				printf("JUMP\n");
			#endif
			uint16_t jump_addr = get_2_ubytes(&pc[1]);
			pc = &program[jump_addr];
			NEXT_INSTR;
		}
		jnz_label_1:
		{
			#ifdef __DEBUG__
				printf("JNZ\n");
			#endif
			state--;
			uint16_t jump_addr = get_2_ubytes(&pc[1]);
			pc = (top_of_stack != 0) ? &program[jump_addr] : (pc + 3);
			NEXT_INSTR;
		}
		dup_label_1:
		{
			#ifdef __DEBUG__
				printf("DUP\n");
			#endif
			uint8_t i = get_1_ubyte(&pc[1]);
			pc += 2;
			push(stack, top, top_of_stack);
			if (i > 0) top_of_stack = stack[top - i];
			NEXT_INSTR;
		}
		drop_label_1:
		{
			#ifdef __DEBUG__
				printf("DROP\n");
			#endif
			pc += 1;
			state--;
			NEXT_INSTR;
		}
		push4_label_1:
		{
			#ifdef __DEBUG__
				printf("PUSH4\n");
			#endif
			push(stack, top, top_of_stack);
			top_of_stack = get_4_bytes(&pc[1]);
			pc += 5;
			NEXT_INSTR;
		}
		push2_label_1:
		{
			#ifdef __DEBUG__
				printf("PUSH2\n");
			#endif
			push(stack, top, top_of_stack);
			top_of_stack = get_2_bytes(&pc[1]);
			pc += 3;
			NEXT_INSTR;
		}
		push1_label_1:
		{
			#ifdef __DEBUG__
				printf("PUSH1\n");
			#endif
			push(stack, top, top_of_stack);
			top_of_stack = get_1_byte(&pc[1]);
			pc += 2;
			NEXT_INSTR;
		}
		add_label_1:
		{
			#ifdef __DEBUG__
				printf("ADD\n");
			#endif
			pc += 1;
			top_of_stack = stack[top] + top_of_stack;
			top--;
			NEXT_INSTR;
		}
		sub_label_1:
		{
			#ifdef __DEBUG__
				printf("SUB\n");
			#endif
			pc += 1;
			top_of_stack = stack[top] - top_of_stack;
			top--;
			NEXT_INSTR;
		}
		mul_label_1:
		{
			#ifdef __DEBUG__
				printf("MUL\n");
			#endif
			pc += 1;
			top_of_stack = stack[top] * top_of_stack;
			top--;
			NEXT_INSTR;
		}
		div_label_1:
		{
			#ifdef __DEBUG__
				printf("DIV\n");
			#endif
			pc += 1;
			top_of_stack = stack[top] / top_of_stack;
			top--;
			NEXT_INSTR;
		}
		mod_label_1:
		{
			#ifdef __DEBUG__
				printf("MOD\n");
			#endif
			pc += 1;
			top_of_stack = stack[top] / top_of_stack;
			top--;
			NEXT_INSTR;
		}
		eq_label_1:
		{
			#ifdef __DEBUG__
				printf("EQ\n");
			#endif
			pc += 1;
			top_of_stack = stack[top] == top_of_stack;
			top--;
			NEXT_INSTR;
		}
		ne_label_1:
		{
			#ifdef __DEBUG__
				printf("NE\n");
			#endif
			pc += 1;
			top_of_stack = stack[top] != top_of_stack;
			top--;
			NEXT_INSTR;
		}
		lt_label_1:
		{
			#ifdef __DEBUG__
				printf("LT\n");
			#endif
			pc += 1;
			top_of_stack = stack[top] < top_of_stack;
			top--;
			NEXT_INSTR;
		}
		gt_label_1:
		{
			#ifdef __DEBUG__
				printf("GT\n");
			#endif
			pc += 1;
			top_of_stack = stack[top] > top_of_stack;
			top--;
			NEXT_INSTR;
		}
		le_label_1:
		{
			#ifdef __DEBUG__
				printf("LE\n");
			#endif
			pc += 1;
			top_of_stack = stack[top] <= top_of_stack;
			top--;
			NEXT_INSTR;
		}
		ge_label_1:
		{
			#ifdef __DEBUG__
				printf("GE\n");
			#endif
			pc += 1;
			top_of_stack = stack[top] >= top_of_stack;
			top--;
			NEXT_INSTR;
		}
		not_label_1:
		{
			#ifdef __DEBUG__
				printf("NOT\n");
			#endif
			pc += 1;
			top_of_stack = !top_of_stack;
			NEXT_INSTR;
		}
		and_label_1:
		{
			#ifdef __DEBUG__
				printf("AND\n");
			#endif
			pc += 1;
			top_of_stack = stack[top] && top_of_stack;
			top--;
			NEXT_INSTR;
		}
		or_label_1:
		{
			#ifdef __DEBUG__
				printf("OR\n");
			#endif
			pc += 1;
			top_of_stack = stack[top] || top_of_stack;
			top--;
			NEXT_INSTR;
		}
		input_label_1:
		{
			#ifdef __DEBUG__
				printf("INPUT\n");
			#endif
			pc += 1;
			push(stack, top, top_of_stack);
			char ch;
			if (scanf("%c", &ch) != 1) {
				printf("Error: Problem with input!\n");
				return -1;
			}
			top_of_stack = ch;
			NEXT_INSTR;
		}
		ouput_label_1:
		{
			#ifdef __DEBUG__
				printf("OUTPUT\n");
			#endif
			pc += 1;
			state--;
			printf("%c", (char)top_of_stack);
			#ifdef __DEBUG__
				printf("\n");
				printf("%d\n", top_of_stack);
			#endif
			NEXT_INSTR;
		}
		clock_label_1:
		{	
			#ifdef __DEBUG__
				printf("CLOCK\n");
			#endif
			pc += 1;
			double time_spent = (double)(clock() - start_time) / CLOCKS_PER_SEC;
			printf("%0.6lf\n", time_spent);
			NEXT_INSTR;
		}
		not_valid_label:
		{
			#ifdef __DEBUG__
				printf("NOT_AN_OPCODE\n");
			#endif
			EXIT;
		}
	}
	exit_label:
	#ifdef __DEBUG_STACK__
		if (top < 0) printf("stack[-1] = ...\n");
		else {
			for (int32_t j = top; j >= 0; j--){
				printf("stack[%d] = %d\n", j, stack[j]);
			}
		}
	#endif
	return 0;
}