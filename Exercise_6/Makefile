all: vm_switch vm_indirect vm_direct

vm_switch: vm_switch_based.c
	gcc -O3 $^ -o $@

vm_indirect: vm_indirectly_threaded.c
	gcc -O3 $^ -o $@

vm_direct: vm_directly_threaded.c
	gcc -O3 $^ -o $@

.PHONY: clean clean_switch clean_indirect clean_direct

clean:
	rm -f vm_switch vm_indirect vm_direct

clean_switch:
	rm -f vm_switch

clean_indirect:
	rm -f vm_indirect

clean_direct:
	rm -f vm_direct 