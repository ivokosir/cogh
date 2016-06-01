#pragma once

#include "window.h"

void pollEvents(Window*);

int getQuit(Window*);

typedef struct Size Size;

Size** getSizes(Window*);
unsigned int sizeW(Size*);
unsigned int sizeH(Size*);
