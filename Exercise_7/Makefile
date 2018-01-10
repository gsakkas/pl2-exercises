CFLAGS = -O2 -ffast-math

all: vm_gc

vm_gc: vm_directly_threaded_with_gc.c
	gcc $(CFLAGS)  $^ -o $@

.PHONY: clean

clean:
	rm -f vm_gc