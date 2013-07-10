#pragma once

typedef void (*fun)();

void print();
void add();

void* mk_app_aa(void*, void*);
void* mk_app_af(void*, fun);
void* mk_app_an(void*, int);
void* mk_app_fa(fun, void*);
void* mk_app_ff(fun, fun);
void* mk_app_fn(fun, int);
void* mk_app_na(int, void*);
void* mk_app_nf(int, fun);
void* mk_app_nn(int, int);

void* eval(void*);
void collect_garbage(void);

