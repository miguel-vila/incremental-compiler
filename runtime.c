#include <stdio.h>
#include <stdlib.h>
#include "scheme_entry.h"
#include <sys/mman.h>
#include <unistd.h>
#include <err.h>
#include <sysexits.h>

static char* allocate_protected_space(int size){
  int page = getpagesize();
  int status;
  int aligned_size = ((size + page - 1) / page) * page;
  char* p = mmap(0, aligned_size + 2 * page,
                 PROT_READ | PROT_WRITE,
                 MAP_ANONYMOUS | MAP_PRIVATE,
                 0, 0);
  if (p == MAP_FAILED){
    err(EX_OSERR, "mmap");
  }
  status = mprotect(p, page, PROT_NONE);
  if(status != 0){
    err(EX_OSERR, "mprotect1");
  }
  status = mprotect(p + page + aligned_size, page, PROT_NONE);
  if(status != 0){
    err(EX_OSERR, "mprotect2");
  }
  return (p + page);
}

static void deallocate_protected_space(char* p, int size){
  int page = getpagesize();
  int status;
  int aligned_size = ((size + page - 1) / page) * page;
  status = munmap(p - page, aligned_size + 2 * page);
  if(status != 0){
    err(EX_OSERR, "munmap");
  }
}

typedef unsigned int ptr;

char* toString(ptr value) {
  char* str = (char *) malloc(15);
  if((value & 0b11) == 0) {
    sprintf(str, "%d", ((int)value) >> 2);
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

void print_ptr(ptr value) {
  printf("%s\n", toString(value));
}

int main(int argc, char** argv){
  int stack_size = 16 * 4096; /* holds 16K cells */
  char* stack_top = allocate_protected_space(stack_size);
  char* stack_base = stack_top + stack_size;
  print_ptr(scheme_entry(stack_base));
  deallocate_protected_space(stack_top, stack_size);
  /*
  printf("%s\n", toString(12));
  printf("%s\n", toString(47));
  printf("%s\n", toString(47+64));
  printf("%s\n", toString(16655));
  */
  return EX_OK;
}
