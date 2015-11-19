#pragma once

#include "window.h"

typedef struct Key Key;

Key* getKey(Window*);
int keyIsPress(Key*);
int keyIsRepeat(Key*);
unsigned int keyCode(Key*);
