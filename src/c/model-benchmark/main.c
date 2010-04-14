#include <stdio.h>
#include <stdlib.h>
#include "model.h"


int main (int argc, const char * argv[]) {
	float att;
	float cost;
	
	float m[32] = {0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1};
	
	setupModel();
	
	model(&cost, &att, m);
	
	printf("Attainment: %.5f\tCost: %.5f\n", att, cost);
		
    return 0;
}
