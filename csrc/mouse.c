#include "window-private.h"

#include "mouse.h"

MouseButton** getMouseButtons(Window* w) { return w->mouseButtons; }
int mouseButtonIsPress(MouseButton* mb) { return mb->isPress; }
int mouseButtonIsLeft(MouseButton* mb) { return mb->code == SDL_BUTTON_LEFT; }
int mouseButtonIsMiddle(MouseButton* mb) { return mb->code == SDL_BUTTON_MIDDLE; }
int mouseButtonIsRight(MouseButton* mb) { return mb->code == SDL_BUTTON_RIGHT; }
unsigned int mouseButtonCode(MouseButton* mb) { return mb->code; }

MousePosition** getMousePositions(Window* w) { return w->mousePositions; }
int mousePositionX(MousePosition* mp) { return mp->x; }
int mousePositionY(MousePosition* mp) { return mp->y; }

Scroll** getScrolls(Window* w) { return w->scrolls; }
int scrollX(Scroll* s) { return s->x; }
int scrollY(Scroll* s) { return s->y; }
