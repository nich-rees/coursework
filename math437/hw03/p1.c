#include <stdio.h>
#include <math.h>

int main() {
	unsigned int a0 = 0;
	unsigned int a1 = 1;
	unsigned int a2 = 2;

	for (unsigned int i = 0; i < 100000; i++){
		unsigned int new = (((unsigned int) pow(5, i)) * a2 + (i*i)*a1 + 11*a0) % 2023;
		if (new == 0)
			printf("%d\n", i+3);
		a0 = a1;
		a1 = a2;
		a2 = new;
	}
	return 0;
}
