for i in 1 2 3 4 5;
	do /usr/bin/time --format="Test $i: %U user %S system %E elapsed" \
	   ./type-inference < testcases/typeinfer.o$i.in | diff - testcases/typeinfer.o$i.out;
done