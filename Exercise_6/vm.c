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


// Helper functions
uint8_t get_1_byte(uint8_t *pc);
uint16_t get_2_bytes(uint8_t *pc);
uint32_t get_4_bytes(uint8_t *pc);
#define push(STACK, TOP, ELEM) (STACK[++TOP] = ELEM)
#define pop(STACK, TOP) (STACK[TOP--])


int main(int argc, char const *argv[]) {
	// Take program from stdin
	FILE *fin = fopen(argv[1], "r");

	// Initialize a stack to hold the program
	int top = 0;
	int32_t stack[1 << 16];
	uint8_t program[1 << 16];

	// Read the file that contains the program
	int length = 0;
	while (fscanf(fin, "%c", &program[length++]) == 1);
	fclose(fin);

	// The Bytecode Interpreter
	uint8_t *pc = &program[0];
	uint8_t opcode;
	bool loop = true;
	clock_t start_time = clock();
	while (loop) {
		opcode = pc[0];
		switch (opcode) {
			case HALT:
				loop = false;
				pc += 1;
				break;
			case JUMP:
			{
				uint16_t jump_addr = get_2_bytes(&pc[1]);
				pc = &program[jump_addr];
				break;
			}
			case JNZ:
			{
				int stack_top = pop(stack, top);
				uint16_t jump_addr = get_2_bytes(&pc[1]);
				pc = (stack_top != 0) ? &program[jump_addr] : (pc + 3);
				break;
			}
			case DUP:
			{
				uint8_t i = get_1_byte(&pc[1]);
				int elem = stack[top - i];
				push(stack, top, elem);
				pc += 2;
				break;
			}
			case DROP:
				pop(stack, top);
				pc += 1;
				break;
			case PUSH4:
			{
				int32_t num = get_4_bytes(&pc[1]);
				push(stack, top, num);
				pc += 5;
				break;
			}
			case PUSH2:
			{
				int16_t num = get_2_bytes(&pc[1]);
				push(stack, top, num);
				pc += 3;
				break;
			}
			case PUSH1:
			{
				int8_t num = get_1_byte(&pc[1]);
				push(stack, top, num);
				pc += 2;
				break;
			}
			case ADD:
			{
				int a = pop(stack, top);
				int b = pop(stack, top);
				push(stack, top, a + b);
				pc += 1;
				break;
			}
			case SUB:
			{
				int a = pop(stack, top);
				int b = pop(stack, top);
				push(stack, top, a - b);
				pc += 1;
				break;
			}
			case MUL:
			{
				int a = pop(stack, top);
				int b = pop(stack, top);
				push(stack, top, a * b);
				pc += 1;
				break;
			}
			case DIV:
			{
				int a = pop(stack, top);
				int b = pop(stack, top);
				push(stack, top, a / b);
				pc += 1;
				break;
			}
			case MOD:
			{
				int a = pop(stack, top);
				int b = pop(stack, top);
				push(stack, top, a % b);
				pc += 1;
				break;
			}
			case EQ:
			{
				int a = pop(stack, top);
				int b = pop(stack, top);
				int eq = (a == b) ? 1 : 0;
				push(stack, top, eq);
				pc += 1;
				break;
			}
			case NE:
			{
				int a = pop(stack, top);
				int b = pop(stack, top);
				int eq = (a != b) ? 1 : 0;
				push(stack, top, eq);
				pc += 1;
				break;
			}
			case LT:
			{
				int a = pop(stack, top);
				int b = pop(stack, top);
				int eq = (a < b) ? 1 : 0;
				push(stack, top, eq);
				pc += 1;
				break;
			}
			case GT:
			{
				int a = pop(stack, top);
				int b = pop(stack, top);
				int eq = (a > b) ? 1 : 0;
				push(stack, top, eq);
				pc += 1;
				break;
			}
			case LE:
			{
				int a = pop(stack, top);
				int b = pop(stack, top);
				int eq = (a <= b) ? 1 : 0;
				push(stack, top, eq);
				pc += 1;
				break;
			}
			case GE:
			{
				int a = pop(stack, top);
				int b = pop(stack, top);
				int eq = (a >= b) ? 1 : 0;
				push(stack, top, eq);
				pc += 1;
				break;
			}
			case NOT:
			{
				int stack_top = pop(stack, top);
				int not = (stack_top == 0) ? 1 : 0;
				push(stack, top, not);
				pc += 1;
				break;
			}
			case AND:
			{
				int a = pop(stack, top);
				int b = pop(stack, top);
				int and = (a && b) ? 1 : 0;
				push(stack, top, and);
				pc += 1;
				break;
			}
			case OR:
			{
				int a = pop(stack, top);
				int b = pop(stack, top);
				int or = (a || b) ? 1 : 0;
				push(stack, top, or);
				pc += 1;
				break;
			}
			case INPUT:
			{
				char ch;
				if (scanf("%c", &ch) == 1);
				push(stack, top, (int32_t)ch);
				pc += 1;
				break;
			}
			case OUTPUT:
			{
				int32_t ch = pop(stack, top);
				pc += 1;
				break;
			}
			case CLOCK:
			{	
				double time_spent = (double)(clock() - start_time) / CLOCKS_PER_SEC;
				printf("%0.6lf\n", time_spent);
				pc += 1;
				break;
			}
		}
	}
	return 0;
}