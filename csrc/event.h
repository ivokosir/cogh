#pragma once

#include "window.h"

void pollEvents(Window*);

typedef struct Quit Quit;

Quit** getQuits(Window*);

typedef struct Size Size;

Size** getSizes(Window*);
unsigned int sizeW(Size*);
unsigned int sizeH(Size*);
