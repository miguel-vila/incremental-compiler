#include <stdio.h>
#include <stdlib.h>
#include "scheme_entry.h"

typedef unsigned int ptr;

char* toString(int value) {
  char* str = (char *) malloc(15);
  if((~value & 0b11) == 0b11) {
    sprintf(str, "%d", value >> 2);
  } else if(value == 0b00101111) {
    sprintf(str, "#f");
  } else if(value == 0b01101111) {
    sprintf(str, "#t");
  } else if(value == 0b00111111) {
    sprintf(str, "nil");
  } else if((value & 0b1111) == 0b1111) {
    sprintf(str, "#\\%c", value >> 8);
  } else {
    sprintf(str, "<unknown: %d>",value);
  }
  return str;
}

int main(int argc, char** argv){
  printf("%s\n", toString(scheme_entry()));
  printf("%s\n", toString(12));
  printf("%s\n", toString(47));
  printf("%s\n", toString(47+64));
  printf("%s\n", toString(16655));
  return 0;
}
