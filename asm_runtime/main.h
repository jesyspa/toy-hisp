#pragma once

typedef unsigned char byte;
typedef unsigned short word;
typedef unsigned dword;
typedef unsigned long qword;

typedef struct {
    struct object* next;
    byte alive;
    byte type;
    byte size;
    byte filler[5];
} object;

extern object print;
extern object add;
extern object sub;
extern object le;
extern object comb_s;
extern object comb_k;
extern object comb_i;
extern object comb_l;
extern object comb_r;
extern object comb_y;

object* mk_app(object*, object*);
object* mk_num(int);

