#include "window-private.h"

#include "mouse.h"

MouseButton** getMouseButtons(Window* w) { return w->mouseButtons; }
int mouseButtonIsPress(MouseButton* mb) { return mb->isPress; }
int mouseButtonIsLeft(MouseButton* mb) { return mb->code == SDL_BUTTON_LEFT; }
int mouseButtonIsMiddle(MouseButton* mb) { return mb->code == SDL_BUTTON_MIDDLE; }
int mouseButtonIsRight(MouseButton* mb) { return mb->code == SDL_BUTTON_RIGHT; }
unsigned int mouseButtonCode(MouseButton* mb) { return mb->code; }

MouseMotion** getMouseMotions(Window* w) { return w->mouseMotions; }
int mouseMotionPositionX(MouseMotion* mp) { return mp->positionX; }
int mouseMotionPositionY(MouseMotion* mp) { return mp->positionY; }
int mouseMotionMotionX(MouseMotion* mp) { return mp->motionX; }
int mouseMotionMotionY(MouseMotion* mp) { return mp->motionY; }

Scroll** getScrolls(Window* w) { return w->scrolls; }
int scrollX(Scroll* s) { return s->x; }
int scrollY(Scroll* s) { return s->y; }
