#pragma once

#include "window.h"

typedef struct Key Key;

Key** getKeys(Window*);
int keyIsPress(Key*);
int keyIsRepeat(Key*);
unsigned int keyCode(Key*);

unsigned int readCode(const char*);
const char* showCode(unsigned int);
