#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <time.h>
#include <string.h>

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
#define CONS 0x2b
#define HD 0x2c
#define TL 0x2d

// If program gives Segmentation Fault, try to cut in half this number
#define STARTING_MAX_ALLOCS (1<<24)
#define BLOCK_SIZE 64

// Debug
// #define __DEBUG__
// #define __DEBUG_STACK__
// #define __DEBUG_GET_BYTES__
// #define __DEBUG_GC__


// Helper functions
#define push(STACK, TOP, ELEM) (STACK[++TOP] = ELEM)
#define pop(STACK, TOP) (STACK[TOP--])
#define get_1_ubyte(PC) program[PC + 1]
#define get_2_ubytes(PC) program[PC + 1] + (program[PC + 2] << 8)
#define get_1_byte(PC) program[PC + 1]
#define get_2_bytes(PC) program[PC + 1] + (program[PC + 2] << 8)
#define get_4_bytes(PC) program[PC + 1] + (program[PC + 2] << 8) + (program[PC + 3] << 16) + (program[PC + 4] << 24)

// Directly threaded interpreter's next instruction
#ifndef __DEBUG_STACK__
	#define NEXT_INSTR goto *program[pc]
#else
	#define NEXT_INSTR goto print_stack_label
	#define NEXT_INSTR_ORIG goto *program[pc]
#endif
#define EXIT goto exit_label

// Heap
struct heap_node_t {
	int64_t hd;
	bool hd_type;
	int64_t tl;
	bool tl_type;
	uint8_t mark;
	uint8_t old;
	struct heap_node_t *next; 
};

typedef struct heap_node_t heap_node;

// Create Heap node
inline heap_node* new(int64_t hd, bool hd_type, int64_t tl, bool tl_type, heap_node** clean, heap_node** new_node) {
	heap_node* node;
	if (*clean) {
		node = (*clean);
		(*clean) = (*clean)->next;
	}
	else {
		node = (*new_node);
		(*new_node)++;
	}
	node->hd = hd;
	node->hd_type = hd_type;
	node->tl = tl;
	node->tl_type = tl_type;
	node->mark = 0;
	node->old = 0;
	return node;
};

// Garbage collection - Generational Mark-and-Sweep algorithm
void mark_cons(heap_node* node) {
	if (node->old) return;
	node->mark++;
	if (node->hd_type) {
		heap_node* hd = (heap_node*)node->hd;
		if (!hd->mark) mark_cons(hd);
	}
	if (node->tl_type) {
		heap_node* tl = (heap_node*)node->tl;
		if (!tl->mark) mark_cons(tl);
	}
	return;
}

void mark(int64_t *stack, bool *types, int32_t top) {
	for (int i = 0; i <= top; i++)
		if (types[i]) {
			heap_node* node = (heap_node*)stack[i];
			if (!node->mark) mark_cons(node);
		}
	return;
}

heap_node* sweep(heap_node* head, heap_node** last, heap_node** clean, uint32_t *num_of_cons) {
	heap_node* heap_head = head;
	heap_node* current = head, *previous = head;
	while (current) {
		if (current->mark) {
			current->mark--;
			current->old++;
			previous = current;
			current = current->next;
		}
		else {
			if (current == heap_head) {
				heap_node* temp = current;
				current = current->next;
				previous = current;
				heap_head = current;
				temp->next = (*clean);
				(*clean) = temp;
			}
			else {
				heap_node* temp = current;
				current = current->next;
				previous->next = current;
				temp->next = (*clean);
				(*clean) = temp;
			}
			(*num_of_cons)--;
		}
	}
	*last = previous;
	return heap_head;
}

#define gc() mark(&stack[0], &types[0], top); heap_head = sweep(heap_head, &last, &clean, &num_of_cons);
#define gc_old() mark(&stack[0], &types[0], top); old_heap_head = sweep(old_heap_head, &last, &clean, &num_of_old_cons);

int main(int argc, char const *argv[]) {
	// Indirectly threaded interpreter's label table
	static void *label_tab[] = {
		&&halt_label,
		&&jump_label,
		&&jnz_label,
		&&dup_label,
		&&drop_label,
		&&push4_label,
		&&push2_label,
		&&push1_label,
		&&add_label,
		&&sub_label,
		&&mul_label,
		&&div_label,
		&&mod_label,
		&&eq_label,
		&&ne_label,
		&&lt_label,
		&&gt_label,
		&&le_label,
		&&ge_label,
		&&not_label,
		&&and_label,
		&&or_label,
		&&input_label,
		&&ouput_label,
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
		&&clock_label,
		&&cons_label,
		&&hd_label,
		&&tl_label
	};

	static uint8_t bytes_to_skip[] = {
		0,	//HALT
		2,	//JUMP
		2,	//JNZ
		1,	//DUP
		0,	//DROP
		4,	//PUSH4
		2,	//PUSH2
		1,	//PUSH1
		0,	//ADD
		0,	//SUB
		0,	//MUL
		0,	//DIV
		0,	//MOD
		0,	//EQ
		0,	//NE
		0,	//LT
		0,	//GT
		0,	//LE
		0,	//GE
		0,	//NOT
		0,	//AND
		0,	//OR
		0,	//INPUT
		0,	//OUTPUT
		0,	//NOT_VALID
		0,	//NOT_VALID
		0,	//NOT_VALID
		0,	//NOT_VALID
		0,	//NOT_VALID
		0,	//NOT_VALID
		0,	//NOT_VALID
		0,	//NOT_VALID
		0,	//NOT_VALID
		0,	//NOT_VALID
		0,	//NOT_VALID
		0,	//NOT_VALID
		0,	//NOT_VALID
		0,	//NOT_VALID
		0,	//NOT_VALID
		0,	//NOT_VALID
		0,	//NOT_VALID
		0,	//NOT_VALID
		0,	//CLOCK
		0,	//CONS
		0,	//HD
		0	//TL
	};

	// Take program from stdin
	FILE *fin = fopen(argv[1], "rb");

	// Initialize a stack to hold the program
	register int32_t top = -1;
	int64_t stack[1 << 16];
	bool types[1 << 16];
	uint64_t program[1 << 16];
	uint8_t opcode_byte;

	// Initialize types to false (= int)
	memset(&types, 0, sizeof(types));

	// Read the file that contains the program
	uint16_t length = 0;
	while (fscanf(fin, "%c", &opcode_byte) == 1) {
		program[length++] = (uint64_t)label_tab[opcode_byte];
		for (uint8_t byte = 0; byte < bytes_to_skip[opcode_byte]; byte++) {
			uint8_t opcode_byte_temp;
			if (fscanf(fin, "%c", &opcode_byte_temp) != 1) {
				printf("Error: Program bytecode is wrong!\n");
				return -1;
			}
			else program[length++] = (uint64_t)opcode_byte_temp;
		}
	}
	fclose(fin);

	// The Bytecode Interpreter
	register uint32_t pc = 0;
	register heap_node* heap_head = NULL;
	register heap_node* old_heap_head = NULL;
	heap_node* clean = NULL;
	heap_node* last = NULL;
	uint32_t max_allocations = (STARTING_MAX_ALLOCS) >> 2;
	uint32_t num_of_cons = 0;
	uint32_t num_of_old_cons = 0;
	heap_node* new_nodes;
	register bool first_time = true;
	heap_node* last_addr = NULL;
	clock_t start_time = clock();
	NEXT_INSTR;
	#ifdef __DEBUG_STACK__
	print_stack_label:
		if (top < 0) printf("stack[-1] = ...\n");
		else {
			for (int32_t j = top; j >= 0; j--){
				if (types[j]) printf("stack[%d] = %p\n", j, (heap_node*)stack[j]);
				else printf("stack[%d] = %ld\n", j, stack[j]);
			}
		}
		NEXT_INSTR_ORIG;
	#endif
	halt_label:
	{
		#ifdef __DEBUG__
			printf("HALT\n");
		#endif
		EXIT;
	}
	jump_label:
	{	
		#ifdef __DEBUG__
			printf("JUMP\n");
		#endif
		pc = get_2_ubytes(pc);
		NEXT_INSTR;
	}
	jnz_label:
	{
		#ifdef __DEBUG__
			printf("JNZ\n");
		#endif
		int32_t stack_top = pop(stack, top);
		pc = (stack_top != 0) ? get_2_ubytes(pc) : (pc + 3);
		NEXT_INSTR;
	}
	dup_label:
	{	
		#ifdef __DEBUG__
			printf("DUP\n");
		#endif
		uint8_t i = get_1_ubyte(pc);
		pc += 2;
		int64_t elem = stack[top - i];
		bool type = types[top - i];
		push(stack, top, elem);
		types[top] = type;
		NEXT_INSTR;
	}
	drop_label:
	{
		#ifdef __DEBUG__
			printf("DROP\n");
		#endif
		pc += 1;
		top--;
		NEXT_INSTR;
	}
	push4_label:
	{
		#ifdef __DEBUG__
			printf("PUSH4\n");
		#endif
		int32_t num = get_4_bytes(pc);
		pc += 5;
		push(stack, top, num);
		NEXT_INSTR;
	}
	push2_label:
	{
		#ifdef __DEBUG__
			printf("PUSH2\n");
		#endif
		int16_t num = get_2_bytes(pc);
		pc += 3;
		push(stack, top, num);
		NEXT_INSTR;
	}
	push1_label:
	{
		#ifdef __DEBUG__
			printf("PUSH1\n");
		#endif
		int8_t num = get_1_byte(pc);
		pc += 2;
		push(stack, top, num);
		NEXT_INSTR;
	}
	add_label:
	{
		#ifdef __DEBUG__
			printf("ADD\n");
		#endif
		pc += 1;
		stack[top - 1] += stack[top];
		top--;
		NEXT_INSTR;
	}
	sub_label:
	{
		#ifdef __DEBUG__
			printf("SUB\n");
		#endif
		pc += 1;
		stack[top - 1] -= stack[top];
		top--;
		NEXT_INSTR;
	}
	mul_label:
	{
		#ifdef __DEBUG__
			printf("MUL\n");
		#endif
		pc += 1;
		stack[top - 1] *= stack[top];
		top--;
		NEXT_INSTR;
	}
	div_label:
	{
		#ifdef __DEBUG__
			printf("DIV\n");
		#endif
		pc += 1;
		stack[top - 1] /= stack[top];
		top--;
		NEXT_INSTR;
	}
	mod_label:
	{
		#ifdef __DEBUG__
			printf("MOD\n");
		#endif
		pc += 1;
		stack[top - 1] %= stack[top];
		top--;
		NEXT_INSTR;
	}
	eq_label:
	{
		#ifdef __DEBUG__
			printf("EQ\n");
		#endif
		pc += 1;
		stack[top - 1] = stack[top - 1] == stack[top];
		top--;
		NEXT_INSTR;
	}
	ne_label:
	{
		#ifdef __DEBUG__
			printf("NE\n");
		#endif
		pc += 1;
		stack[top - 1] = stack[top - 1] != stack[top];
		top--;
		NEXT_INSTR;
	}
	lt_label:
	{
		#ifdef __DEBUG__
			printf("LT\n");
		#endif
		pc += 1;
		stack[top - 1] = stack[top - 1] < stack[top];
		top--;
		NEXT_INSTR;
	}
	gt_label:
	{
		#ifdef __DEBUG__
			printf("GT\n");
		#endif
		pc += 1;
		stack[top - 1] = stack[top - 1] > stack[top];
		top--;
		NEXT_INSTR;
	}
	le_label:
	{
		#ifdef __DEBUG__
			printf("LE\n");
		#endif
		pc += 1;
		stack[top - 1] = stack[top - 1] <= stack[top];
		top--;
		NEXT_INSTR;
	}
	ge_label:
	{
		#ifdef __DEBUG__
			printf("GE\n");
		#endif
		pc += 1;
		stack[top - 1] = stack[top - 1] >= stack[top];
		top--;
		NEXT_INSTR;
	}
	not_label:
	{
		#ifdef __DEBUG__
			printf("NOT\n");
		#endif
		pc += 1;
		stack[top] = !stack[top];
		NEXT_INSTR;
	}
	and_label:
	{
		#ifdef __DEBUG__
			printf("AND\n");
		#endif
		pc += 1;
		stack[top - 1] = stack[top - 1] && stack[top];
		top--;
		NEXT_INSTR;
	}
	or_label:
	{
		#ifdef __DEBUG__
			printf("OR\n");
		#endif
		pc += 1;
		stack[top - 1] = stack[top - 1] || stack[top];
		top--;
		NEXT_INSTR;
	}
	input_label:
	{
		#ifdef __DEBUG__
			printf("INPUT\n");
		#endif
		pc += 1;
		char ch;
		if (scanf("%c", &ch) != 1) {
			printf("Error: Problem with input!\n");
			return -1;
		}
		push(stack, top, (int32_t)ch);
		NEXT_INSTR;
	}
	ouput_label:
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
	clock_label:
	{	
		#ifdef __DEBUG__
			printf("CLOCK\n");
		#endif
		pc += 1;
		double time_spent = (double)(clock() - start_time) / CLOCKS_PER_SEC;
		printf("%0.6lf\n", time_spent);
		NEXT_INSTR;
	}
	cons_label:
	{
		#ifdef __DEBUG__
			printf("CONS\n");
		#endif
		if (first_time) {
			new_nodes = malloc(sizeof(heap_node) * BLOCK_SIZE);
			first_time = false;
			last_addr = &new_nodes[BLOCK_SIZE - 1];
		}
		pc += 1;
		heap_node* node = new(stack[top - 1], types[top - 1], stack[top], types[top], &clean, &new_nodes);
		if (last_addr == new_nodes) first_time = true;
		num_of_cons++;
		top--;
		node->next = heap_head;
		heap_head = node;
		stack[top] = (intptr_t)node;
		types[top] = true;
		if (num_of_cons + max_allocations >= STARTING_MAX_ALLOCS){
			#ifdef __DEBUG_GC__
				printf("Garbage collection is executed!\n");
			#endif
			gc();
			last->next = old_heap_head;
			old_heap_head = heap_head;
			heap_head = NULL;
			num_of_old_cons += num_of_cons;
			num_of_cons = 0;
			if (num_of_old_cons >= max_allocations) {
				#ifdef __DEBUG_GC__
					printf("Old garbage collection is executed!\n");
				#endif
				gc_old();
			}
		}
		NEXT_INSTR;
	}
	hd_label:
	{
		#ifdef __DEBUG__
			printf("HD\n");
		#endif
		pc += 1;
		heap_node* node = (heap_node*)stack[top];
		stack[top] = node->hd;
		types[top] = node->hd_type;
		NEXT_INSTR;
	}
	tl_label:
	{
		#ifdef __DEBUG__
			printf("TL\n");
		#endif
		pc += 1;
		heap_node* node = (heap_node*)stack[top];
		stack[top] = node->tl;
		types[top] = node->tl_type;
		NEXT_INSTR;	
	}
	not_valid_label:
	{
		#ifdef __DEBUG__
			printf("NOT_AN_OPCODE\n");
		#endif
		EXIT;
	}
	exit_label:
	#ifdef __DEBUG_STACK__
		if (top < 0) printf("stack[-1] = ...\n");
		else {
			for (int32_t j = top; j >= 0; j--){
				if (types[j]) printf("stack[%d] = %p\n", j, (heap_node*)stack[j]);
				else printf("stack[%d] = %ld\n", j, stack[j]);
			}
		}
	#endif
	return 0;
}