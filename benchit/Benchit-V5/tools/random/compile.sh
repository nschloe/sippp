gcc -o factor -lm -std=c99 factor.c
gcc -o fixpoint fixpoint.c big_int.c
gcc -o prime -lm -std=c99 prime.c
gcc -o prim_root -lm -std=c99 primitive_root.c big_int.c
#tests
gcc -o big_int-test big_int.c big_int-test.c
gcc -o random-test -lm -std=c99 random.c random_test.c