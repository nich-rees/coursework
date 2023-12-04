#include <setjmp.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "fact.h"

kt *
kt_empty(_jmpbuf_int * dismount)
{
  kt *_data = (kt *) malloc(sizeof(kt));
  if (!_data)
    assert(!"Out of memory.");
  _data->tag = _empty_kt;
  _data->u._empty._dismount = dismount;
  return _data;
}

kt *
kt_extend(int n, kt * k)
{
  kt *_data = (kt *) malloc(sizeof(kt));
  if (!_data)
    assert(!"Out of memory.");
  _data->tag = _extend_kt;
  _data->u._extend._n = n;
  _data->u._extend._k = k;
  return _data;
}

th *
th_factorial(int n, kt * k)
{
  th *_data = (th *) malloc(sizeof(th));
  if (!_data)
    assert(!"Out of memory.");
  _data->tag = _factorial_th;
  _data->u._factorial._n = n;
  _data->u._factorial._k = k;
  return _data;
}

th *
th_apply_k(kt * k, int v)
{
  th *_data = (th *) malloc(sizeof(th));
  if (!_data)
    assert(!"Out of memory.");
  _data->tag = _apply_k_th;
  _data->u._apply_k._k = k;
  _data->u._apply_k._v = v;
  return _data;
}

th *
factorial_cps(int n, kt * k)
{
  if (n == 0) {
    return th_apply_k(k, 1);
  } else {
    return th_factorial(n, k);
  }
}

th *
apply_k(kt * k, int v)
{
  {
    kt *_c = (kt *) k;
    switch (_c->tag) {
    case _empty_kt:{
	_jmpbuf_int *dismount = _c->u._empty._dismount;
	{
	  dismount->value = v;
	  longjmp(*dismount->jmpbuf, 1);
	}
      }
    case _extend_kt:{
	int n = _c->u._extend._n;
	kt *k = _c->u._extend._k;
	return th_apply_k(k, (n * v));
      }
    }
  }
  return (NULL);
}

th *
apply_th(th * thunk)
{
  {
    th *_c = (th *) thunk;
    switch (_c->tag) {
    case _factorial_th:{
	int n = _c->u._factorial._n;
	kt *k = _c->u._factorial._k;
	return factorial_cps((n - 1), kt_extend(n, k));
      }
    case _apply_k_th:{
	kt *k = _c->u._apply_k._k;
	int v = _c->u._apply_k._v;
	return apply_k(k, v);
      }
    }
  }
  return (NULL);
}

int
factorial(int n)
{
  {
    jmp_buf _jb;
    _jmpbuf_int _trstr;
    _jmpbuf_int *dismount = &_trstr;
    th *_th;
    int _status = setjmp(_jb);
    _trstr.jmpbuf = &_jb;
    /* dismount = &_trstr; */
    if (!_status) {
      _th = factorial_cps(n, kt_empty(dismount));
      for (;;) {
	_th = apply_th(_th);
      }
    } else {
      return _trstr.value;
    }
  }
}

int
main()
{
  return printf("Factorial 5:  %d\n", factorial(5));
}
