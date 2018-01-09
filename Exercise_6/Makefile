CFLAGS = -O2 -ffast-math

all: vm_switch vm_switch_opt vm_indirect vm_indirect_opt vm_direct vm_direct_opt

vm_switch: vm_switch_based.c
	gcc $(CFLAGS)  $^ -o $@

vm_switch_opt: vm_switch_based_optimized.c
	gcc $(CFLAGS) $^ -o $@

vm_indirect: vm_indirectly_threaded.c
	gcc $(CFLAGS) $^ -o $@

vm_indirect_opt: vm_indirectly_threaded_optimized.c
	gcc $(CFLAGS) $^ -o $@

vm_direct: vm_directly_threaded.c
	gcc $(CFLAGS) $^ -o $@

vm_direct_opt: vm_directly_threaded_optimized.c
	gcc $(CFLAGS) $^ -o $@

.PHONY: clean clean_switch clean_indirect clean_direct

clean:
	rm -f vm_switch vm_switch_opt vm_indirect vm_indirect_opt vm_direct vm_direct_opt

clean_switch:
	rm -f vm_switch vm_switch_opt

clean_indirect:
	rm -f vm_indirect vm_indirect_opt

clean_direct:
	rm -f vm_direct vm_direct_opt