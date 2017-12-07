all:
	ocaml pkg/pkg.ml build --tests true

clean:
	ocaml pkg/pkg.ml clean

test: all
	@ echo "# Generating a Python module for messages in test.proto"
	@ mkdir -p _build/proto-python
	@ protoc --python_out=_build/proto-python lib_test/test.proto

	@ echo "# Using the generated Python module to generate serialized messages:"
	@ echo "#    lib_test/*.python.serialized"
	@ PYTHONPATH=_build/proto-python/lib_test lib_test/test_gen.py

	@ echo "# Running the OCaml tests:"
	@ echo "#   - checking that pb can read the python messages"
	@ echo "#   - using pb to generate serialized messages:"
	@ echo "#       lib_test/*.ocaml.serialized"
	@ ocaml pkg/pkg.ml test

	@ echo "# Checking that Python can read the pb-generated messages"
	@ PYTHONPATH=_build/proto-python/lib_test lib_test/test_read.py
	@ echo OK

.PHONY: all clean test
