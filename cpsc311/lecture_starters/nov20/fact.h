#include <setjmp.h>

struct _jmpbuf_int;
typedef struct _jmpbuf_int _jmpbuf_int;
struct _jmpbuf_int {
  jmp_buf *jmpbuf;
  int value;
};

struct kt;
typedef struct kt kt;
struct kt {
  enum {
    _empty_kt,
    _extend_kt
  } tag;
  union {
    struct { _jmpbuf_int* _dismount; } _empty;
    struct { int _n; kt* _k; } _extend;
  } u;
};

kt* kt_empty(_jmpbuf_int* dismount);
kt* kt_extend(int n, kt* k);

struct th;
typedef struct th th;
struct th {
  enum {
    _factorial_th,
    _apply_k_th
  } tag;
  union {
    struct { int _n; kt* _k; } _factorial;
    struct { kt* _k; int _v; } _apply_k;
  } u;
};

th* th_factorial(int n, kt* k);
th* th_apply_k(kt* k, int v);

th* factorial_cps(int n, kt* k);
th* apply_k(kt* k, int v);
th* apply_th(th* thunk);
int factorial(int n);
int main();
