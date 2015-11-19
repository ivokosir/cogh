#pragma once

#include "window.h"

int pollEvent(Window*);

typedef struct Quit Quit;

Quit* getQuit(Window*);

typedef struct Resize Resize;

Resize* getResize(Window*);
unsigned int resizeW(Resize*);
unsigned int resizeH(Resize*);
