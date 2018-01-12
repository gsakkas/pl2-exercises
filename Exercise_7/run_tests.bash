for n in {1..3}
do
	echo "/usr/bin/time --format=\"%E\" ./vm_gc pp.b > __test_out__";
	/usr/bin/time --format="%E" ./vm_gc pp.b > __test_out__;
done
echo "rm -f __test_out__"
rm -f __test_out__