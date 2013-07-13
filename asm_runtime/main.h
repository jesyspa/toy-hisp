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

object* mk_app(object*, object*);
object* mk_num(int);

