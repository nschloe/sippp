all: factor fixpoint prime prim_root big_int-test random-test

big_int-test:big_int.c big_int.h big_int-test.c
	gcc -o big_int-test big_int.c big_int-test.c

random-test: random.h random.c random_test.c
	gcc -o random-test -lm -std=c99 random.c random_test.c

factor:	factor.c
	gcc -o factor -lm -std=c99 factor.c

fixpoint:fixpoint.c big_int.c big_int.h
	gcc -o fixpoint fixpoint.c big_int.c

prime: prime.c
	gcc -o prime -lm -std=c99 prime.c

prim_root: primitive_root.c big_int.c big_int.h
	gcc -o prim_root -lm -std=c99 primitive_root.c big_int.c

clean:
	rm factor fixpoint prime prim_root big_int-test random-test
