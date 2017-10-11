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

int isNil(ptr value) {
  return value == 0b00111111;
}

int isPair(ptr value) {
  return (value & 0b111) == 0b001;
}

void print_ptr(ptr value) {
  if((value & 0b11) == 0) {
    printf("%d", ((int)value) >> 2);
  } else if(value == 0b00101111) {
    printf("#f");
  } else if(value == 0b01101111) {
    printf("#t");
  } else if(isNil(value)) {
    printf("()");
  } else if((value & 0b1111) == 0b1111) {
    printf("#\\%c", value >> 8);
  } else if(isPair(value)) {
    ptr pair_ptr = value - 1;
    ptr car = *((ptr*)(pair_ptr-4));
    ptr cdr = *((ptr*)(pair_ptr-8));
    printf("(");
    print_ptr(car);
    while(1) {
      if(isNil(cdr))
        break;
      if(isPair(cdr)) {
        pair_ptr = cdr - 1;
        car = *((ptr*)(pair_ptr-4));
        cdr = *((ptr*)(pair_ptr-8));
        printf(" ");
        print_ptr(car);
      } else {
        printf(" . ");
        print_ptr(cdr);
        break;
      }
    }
    printf(")");
  } else {
    printf("<unknown: %d>",value);
  }
}

int main(int argc, char** argv){
  int stack_size = 16 * 4096; /* holds 16K cells */
  int heap_size  = 16 * 4096; /* holds 16K cells */
  char* stack_top = allocate_protected_space(stack_size);
  char* stack_base = stack_top + stack_size;
  char* heap_top = allocate_protected_space(heap_size);
  char* heap_base = heap_top + heap_size;
  context ctx;
  print_ptr(scheme_entry(&ctx, stack_base, heap_base));
  printf("\n");
  deallocate_protected_space(stack_top, stack_size);
  deallocate_protected_space(heap_top, heap_size);
  return EX_OK;
}
