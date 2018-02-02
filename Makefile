all:
	jbuilder build

clean:
	jbuilder clean

test:
	jbuilder runtest

.PHONY: all clean test
