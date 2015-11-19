#pragma once

#include "event.h"

typedef struct MouseButton MouseButton;

MouseButton* getMouseButton(Window*);
int mouseButtonIsLeft(MouseButton*);
int mouseButtonIsMiddle(MouseButton*);
int mouseButtonIsRight(MouseButton*);
int mouseButtonIsPress(MouseButton*);
unsigned int mouseButtonCode(MouseButton*);

typedef struct MousePosition MousePosition;

MousePosition* getMousePosition(Window*);
int mousePositionX(MousePosition*);
int mousePositionY(MousePosition*);

typedef struct Scroll Scroll;

Scroll* getScroll(Window*);
int scrollX(Scroll*);
int scrollY(Scroll*);
