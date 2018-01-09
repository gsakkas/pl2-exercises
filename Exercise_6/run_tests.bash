if [ $# -eq 0 ]; then
	for n in {1..3}
	do
		echo "/usr/bin/time --format=\"%E\" ./vm_switch big_loop.b > __test_out__";
		/usr/bin/time --format="%E" ./vm_switch big_loop.b > __test_out__;
	done
else
	for n in {1..3}
	do
		echo "/usr/bin/time --format=\"%E\" ./$1 big_loop.b > __test_out__";
		/usr/bin/time --format="%E" ./$1 big_loop.b > __test_out__;
	done
fi
echo "rm -f __test_out__"
rm -f __test_out__