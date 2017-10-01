#ifndef __SCHEME_ENTRY__
#define __SCHEME_ENTRY__

typedef struct {
  void* eax; /* 0  scratch  */
  void* ebx; /* 4  preserve */
  void* ecx; /* 8  scratch  */
  void* edx; /* 12 scratch  */
  void* esi; /* 16 preserve */
  void* edi; /* 20 preserve */
  void* ebp; /* 24 preserve */
  void* esp; /* 28 preserve */
} context;

int scheme_entry(context* ctx, char* stack_base, char* heap_base);

#endif // __SCHEME_ENTRY__
