#include <stdio.h>
#include "model.h"

int main (int argc, char **argv) {
  float *att_total;
  float *cost_total;

  setupModel();

  printf("ATTAINMENT: %f\n", &att_total);

  return 0;
}
