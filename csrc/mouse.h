#pragma once

#include "event.h"

typedef struct MouseButton MouseButton;

MouseButton** getMouseButtons(Window*);
int mouseButtonIsLeft(MouseButton*);
int mouseButtonIsMiddle(MouseButton*);
int mouseButtonIsRight(MouseButton*);
int mouseButtonIsPress(MouseButton*);
unsigned int mouseButtonCode(MouseButton*);

typedef struct MousePosition MousePosition;

MousePosition** getMousePositions(Window*);
int mousePositionX(MousePosition*);
int mousePositionY(MousePosition*);

typedef struct Scroll Scroll;

Scroll** getScrolls(Window*);
int scrollX(Scroll*);
int scrollY(Scroll*);
