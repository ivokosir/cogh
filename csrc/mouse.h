#pragma once

#include "event.h"

typedef struct MouseButton MouseButton;

MouseButton** getMouseButtons(Window*);
int mouseButtonIsLeft(MouseButton*);
int mouseButtonIsMiddle(MouseButton*);
int mouseButtonIsRight(MouseButton*);
int mouseButtonIsPress(MouseButton*);
unsigned int mouseButtonCode(MouseButton*);

typedef struct MouseMotion MouseMotion;

MouseMotion** getMouseMotions(Window*);
int mouseMotionX(MouseMotion*);
int mouseMotionY(MouseMotion*);
int mouseMotionRelativeX(MouseMotion*);
int mouseMotionRelativeY(MouseMotion*);

typedef struct Scroll Scroll;

Scroll** getScrolls(Window*);
int scrollX(Scroll*);
int scrollY(Scroll*);
