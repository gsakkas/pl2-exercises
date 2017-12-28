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
// #define __DEBUG_GET_BYTES__


// Helper functions
#define push(STACK, TOP, ELEM) (STACK[++TOP] = ELEM)
#define pop(STACK, TOP) (STACK[TOP--])
uint8_t get_1_byte(uint8_t *pc) {
	#ifdef __DEBUG_GET_BYTES__
		printf("1_byte: %c\n", pc[0]);
	#endif
	return pc[0];
}
uint16_t get_2_bytes(uint8_t *pc) {
	#ifdef __DEBUG_GET_BYTES__
		printf("2_bytes: %hd\n", pc[0] + (pc[1] << 8));
	#endif
	return pc[0] + (pc[1] << 8);
}
uint32_t get_4_bytes(uint8_t *pc) {
	#ifdef __DEBUG_GET_BYTES__
		printf("4_bytes: %d\n", pc[0] + (pc[1] << 8) + (pc[2] << 16) + (pc[2] << 24));
	#endif
	return pc[0] + (pc[1] << 8) + (pc[2] << 16) + (pc[2] << 24);
}


int main(int argc, char const *argv[]) {
	// Take program from stdin
	FILE *fin = fopen(argv[1], "rb");

	// Initialize a stack to hold the program
	int top = 0;
	int32_t stack[1 << 16];
	uint8_t program[1 << 16];

	// Read the file that contains the program
	int length = 0;
	while (fscanf(fin, "%c", &program[length]) == 1) length++;
	fclose(fin);
	uint8_t *last_opcode = &program[--length];

	// The Bytecode Interpreter
	uint8_t *pc = &program[0];
	uint8_t opcode;
	bool loop = true;
	clock_t start_time = clock();
	while (loop) {
		opcode = pc[0];
		switch (opcode) {
			case HALT:
				#ifdef __DEBUG__
					printf("HALT\n");
				#endif
				loop = false;
				pc += 1;
				break;
			case JUMP:
			{	
				#ifdef __DEBUG__
					printf("JUMP\n");
				#endif
				uint16_t jump_addr = get_2_bytes(&pc[1]);
				pc = &program[jump_addr];
				break;
			}
			case JNZ:
			{
				#ifdef __DEBUG__
					printf("JNZ\n");
				#endif
				int stack_top = pop(stack, top);
				uint16_t jump_addr = get_2_bytes(&pc[1]);
				pc = (stack_top != 0) ? &program[jump_addr] : (pc + 3);
				break;
			}
			case DUP:
			{	
				#ifdef __DEBUG__
					printf("DUP\n");
				#endif
				uint8_t i = get_1_byte(&pc[1]);
				int elem = stack[top - i];
				push(stack, top, elem);
				pc += 2;
				break;
			}
			case DROP:
				#ifdef __DEBUG__
					printf("DROP\n");
				#endif
				pop(stack, top);
				pc += 1;
				break;
			case PUSH4:
			{
				#ifdef __DEBUG__
					printf("PUSH4\n");
				#endif
				int32_t num = get_4_bytes(&pc[1]);
				push(stack, top, num);
				pc += 5;
				break;
			}
			case PUSH2:
			{
				#ifdef __DEBUG__
					printf("PUSH2\n");
				#endif
				int16_t num = get_2_bytes(&pc[1]);
				push(stack, top, num);
				pc += 3;
				break;
			}
			case PUSH1:
			{
				#ifdef __DEBUG__
					printf("PUSH1\n");
				#endif
				int8_t num = get_1_byte(&pc[1]);
				push(stack, top, num);
				pc += 2;
				break;
			}
			case ADD:
			{
				#ifdef __DEBUG__
					printf("ADD\n");
				#endif
				int a = pop(stack, top);
				int b = pop(stack, top);
				push(stack, top, a + b);
				pc += 1;
				break;
			}
			case SUB:
			{
				#ifdef __DEBUG__
					printf("SUB\n");
				#endif
				int a = pop(stack, top);
				int b = pop(stack, top);
				push(stack, top, a - b);
				pc += 1;
				break;
			}
			case MUL:
			{
				#ifdef __DEBUG__
					printf("MUL\n");
				#endif
				int a = pop(stack, top);
				int b = pop(stack, top);
				push(stack, top, a * b);
				pc += 1;
				break;
			}
			case DIV:
			{
				#ifdef __DEBUG__
					printf("DIV\n");
				#endif
				int a = pop(stack, top);
				int b = pop(stack, top);
				push(stack, top, a / b);
				pc += 1;
				break;
			}
			case MOD:
			{
				#ifdef __DEBUG__
					printf("MOD\n");
				#endif
				int a = pop(stack, top);
				int b = pop(stack, top);
				push(stack, top, a % b);
				pc += 1;
				break;
			}
			case EQ:
			{
				#ifdef __DEBUG__
					printf("EQ\n");
				#endif
				int a = pop(stack, top);
				int b = pop(stack, top);
				int eq = (a == b) ? 1 : 0;
				push(stack, top, eq);
				pc += 1;
				break;
			}
			case NE:
			{
				#ifdef __DEBUG__
					printf("NE\n");
				#endif
				int a = pop(stack, top);
				int b = pop(stack, top);
				int eq = (a != b) ? 1 : 0;
				push(stack, top, eq);
				pc += 1;
				break;
			}
			case LT:
			{
				#ifdef __DEBUG__
					printf("LT\n");
				#endif
				int a = pop(stack, top);
				int b = pop(stack, top);
				int eq = (a < b) ? 1 : 0;
				push(stack, top, eq);
				pc += 1;
				break;
			}
			case GT:
			{
				#ifdef __DEBUG__
					printf("GT\n");
				#endif
				int a = pop(stack, top);
				int b = pop(stack, top);
				int eq = (a > b) ? 1 : 0;
				push(stack, top, eq);
				pc += 1;
				break;
			}
			case LE:
			{
				#ifdef __DEBUG__
					printf("LE\n");
				#endif
				int a = pop(stack, top);
				int b = pop(stack, top);
				int eq = (a <= b) ? 1 : 0;
				push(stack, top, eq);
				pc += 1;
				break;
			}
			case GE:
			{
				#ifdef __DEBUG__
					printf("GE\n");
				#endif
				int a = pop(stack, top);
				int b = pop(stack, top);
				int eq = (a >= b) ? 1 : 0;
				push(stack, top, eq);
				pc += 1;
				break;
			}
			case NOT:
			{
				#ifdef __DEBUG__
					printf("NOT\n");
				#endif
				int stack_top = pop(stack, top);
				int not = (stack_top == 0) ? 1 : 0;
				push(stack, top, not);
				pc += 1;
				break;
			}
			case AND:
			{
				#ifdef __DEBUG__
					printf("AND\n");
				#endif
				int a = pop(stack, top);
				int b = pop(stack, top);
				int and = (a && b) ? 1 : 0;
				push(stack, top, and);
				pc += 1;
				break;
			}
			case OR:
			{
				#ifdef __DEBUG__
					printf("OR\n");
				#endif
				int a = pop(stack, top);
				int b = pop(stack, top);
				int or = (a || b) ? 1 : 0;
				push(stack, top, or);
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
				push(stack, top, (int32_t)ch);
				pc += 1;
				break;
			}
			case OUTPUT:
			{
				#ifdef __DEBUG__
					printf("OUTPUT\n");
				#endif
				int32_t ch = pop(stack, top);
				printf("%c", (char)ch);
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
	return 0;
}