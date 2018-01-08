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

// Debug
// #define __DEBUG__
// #define __DEBUG_STACK__
// #define __DEBUG_GET_BYTES__


// Helper functions
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


int32_t main(int32_t argc, char const *argv[]) {
	// Take program from stdin
	FILE *fin = fopen(argv[1], "rb");

	// Initialize a stack to hold the program
	register int32_t top = -1;
	int32_t stack[1 << 16];
	register int32_t stack_top, stack_below_top;
	uint8_t program[1 << 16];

	// Read the file that contains the program
	uint16_t length = 0;
	while (fscanf(fin, "%c", &program[length]) == 1) length++;
	fclose(fin);
	uint8_t *last_opcode = &program[--length];

	// The Bytecode Interpreter
	register uint8_t *pc = &program[0];
	register uint8_t opcode;
	register bool loop = true;
	clock_t start_time = clock();
	while (loop) {
		#ifdef __DEBUG_STACK__
			if (top < 0) printf("stack[-1] = ...\n");
			else {
				for (int32_t j = top; j >= 0; j--){
					printf("stack[%d] = %d\n", j, stack[j]);
				}
			}
		#endif
		opcode = pc[0];
		switch (opcode) {
			case HALT:
			{
				#ifdef __DEBUG__
					printf("HALT\n");
				#endif
				loop = false;
				pc += 1;
				break;
			}
			case JUMP:
			{	
				#ifdef __DEBUG__
					printf("JUMP\n");
				#endif
				uint16_t jump_addr = get_2_ubytes(&pc[1]);
				pc = &program[jump_addr];
				break;
			}
			case JNZ:
			{
				#ifdef __DEBUG__
					printf("JNZ\n");
				#endif
				uint16_t jump_addr = get_2_ubytes(&pc[1]);
				pc = (stack_top != 0) ? &program[jump_addr] : (pc + 3);
				stack_top = stack_below_top;
				stack_below_top = stack[top--];
				break;
			}
			case DUP:
			{	
				#ifdef __DEBUG__
					printf("DUP\n");
				#endif
				uint8_t i = get_1_ubyte(&pc[1]);
				register int32_t dup;
				if (i == 0) dup = stack_top;
				else if (i == 1) dup = stack_below_top;
				else dup = stack[top - i + 2];
				stack[++top] = stack_below_top;
				stack_below_top = stack_top;
				stack_top = dup;
				pc += 2;
				break;
			}
			case DROP:
				#ifdef __DEBUG__
					printf("DROP\n");
				#endif
				stack_top = stack_below_top;
				stack_below_top = stack[top--];
				pc += 1;
				break;
			case PUSH4:
			{
				#ifdef __DEBUG__
					printf("PUSH4\n");
				#endif
				int32_t num = get_4_bytes(&pc[1]);
				stack[++top] = stack_below_top;
				stack_below_top = stack_top;
				stack_top = num;
				pc += 5;
				break;
			}
			case PUSH2:
			{
				#ifdef __DEBUG__
					printf("PUSH2\n");
				#endif
				int16_t num = get_2_bytes(&pc[1]);
				stack[++top] = stack_below_top;
				stack_below_top = stack_top;
				stack_top = num;
				pc += 3;
				break;
			}
			case PUSH1:
			{
				#ifdef __DEBUG__
					printf("PUSH1\n");
				#endif
				int8_t num = get_1_byte(&pc[1]);
				stack[++top] = stack_below_top;
				stack_below_top = stack_top;
				stack_top = num;
				pc += 2;
				break;
			}
			case ADD:
			{
				#ifdef __DEBUG__
					printf("ADD\n");
				#endif
				stack_top = stack_below_top + stack_top;
				stack_below_top = stack[top--];
				pc += 1;
				break;
			}
			case SUB:
			{
				#ifdef __DEBUG__
					printf("SUB\n");
				#endif
				stack_top = stack_below_top - stack_top;
				stack_below_top = stack[top--];
				pc += 1;
				break;
			}
			case MUL:
			{
				#ifdef __DEBUG__
					printf("MUL\n");
				#endif
				stack_top = stack_below_top * stack_top;
				stack_below_top = stack[top--];
				pc += 1;
				break;
			}
			case DIV:
			{
				#ifdef __DEBUG__
					printf("DIV\n");
				#endif
				stack_top = stack_below_top / stack_top;
				stack_below_top = stack[top--];
				pc += 1;
				break;
			}
			case MOD:
			{
				#ifdef __DEBUG__
					printf("MOD\n");
				#endif
				stack_top = stack_below_top % stack_top;
				stack_below_top = stack[top--];
				pc += 1;
				break;
			}
			case EQ:
			{
				#ifdef __DEBUG__
					printf("EQ\n");
				#endif
				stack_top = (stack_below_top == stack_top) ? 1 : 0;
				stack_below_top = stack[top--];
				pc += 1;
				break;
			}
			case NE:
			{
				#ifdef __DEBUG__
					printf("NE\n");
				#endif
				stack_top = (stack_below_top != stack_top) ? 1 : 0;
				stack_below_top = stack[top--];
				pc += 1;
				break;
			}
			case LT:
			{
				#ifdef __DEBUG__
					printf("LT\n");
				#endif
				stack_top = (stack_below_top < stack_top) ? 1 : 0;
				stack_below_top = stack[top--];
				pc += 1;
				break;
			}
			case GT:
			{
				#ifdef __DEBUG__
					printf("GT\n");
				#endif
				stack_top = (stack_below_top > stack_top) ? 1 : 0;
				stack_below_top = stack[top--];
				pc += 1;
				break;
			}
			case LE:
			{
				#ifdef __DEBUG__
					printf("LE\n");
				#endif
				stack_top = (stack_below_top <= stack_top) ? 1 : 0;
				stack_below_top = stack[top--];
				pc += 1;
				break;
			}
			case GE:
			{
				#ifdef __DEBUG__
					printf("GE\n");
				#endif
				stack_top = (stack_below_top >= stack_top) ? 1 : 0;
				stack_below_top = stack[top--];
				pc += 1;
				break;
			}
			case NOT:
			{
				#ifdef __DEBUG__
					printf("NOT\n");
				#endif
				stack_top = (stack_top == 0) ? 1 : 0;
				pc += 1;
				break;
			}
			case AND:
			{
				#ifdef __DEBUG__
					printf("AND\n");
				#endif
				stack_top = (stack_below_top && stack_top) ? 1 : 0;
				stack_below_top = stack[top--];
				pc += 1;
				break;
			}
			case OR:
			{
				#ifdef __DEBUG__
					printf("OR\n");
				#endif
				stack_top = (stack_below_top || stack_top) ? 1 : 0;
				stack_below_top = stack[top--];
				pc += 1;
				break;
			}
			case INPUT:
			{
				#ifdef __DEBUG__
					printf("INPUT\n");
				#endif
				char ch;
				if (scanf("%c", &ch) == 1);
				else {
					loop = false;
					printf("Error: Problem with input!\n");
					return -1;
				}
				stack[++top] = stack_below_top;
				stack_below_top = stack_top;
				stack_top = (int32_t)ch;
				pc += 1;
				break;
			}
			case OUTPUT:
			{
				#ifdef __DEBUG__
					printf("OUTPUT\n");
				#endif
				printf("%c", (char)stack_top);
				stack_top = stack_below_top;
				stack_below_top = stack[top--];
				#ifdef __DEBUG__
					printf("\n");
					printf("%d\n", ch);
				#endif
				pc += 1;
				break;
			}
			case CLOCK:
			{	
				#ifdef __DEBUG__
					printf("CLOCK\n");
				#endif
				double time_spent = (double)(clock() - start_time) / CLOCKS_PER_SEC;
				printf("%0.6lf\n", time_spent);
				pc += 1;
				break;
			}
			default:
				#ifdef __DEBUG__
					printf("NOT_AN_OPCODE\n");
				#endif
				loop = false;
				pc += 1;
		}
		if (pc == last_opcode) loop = false;
	}
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