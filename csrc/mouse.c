#include "window-private.h"

#include "mouse.h"

#include <SDL.h>

MouseButton* getMouseButton(Window* w) {
	if (w->event.type == SDL_MOUSEBUTTONDOWN | w->event.type == SDL_MOUSEBUTTONUP) {
		w->mouseButton.isPress = w->event.button.type == SDL_MOUSEBUTTONDOWN;
		w->mouseButton.code = w->event.button.button;
		return &w->mouseButton;
	}
	return NULL;
}

int mouseButtonIsPress(MouseButton* mb) { return mb->isPress; }
int mouseButtonIsLeft(MouseButton* mb) { return mb->code == SDL_BUTTON_LEFT; }
int mouseButtonIsMiddle(MouseButton* mb) { return mb->code == SDL_BUTTON_MIDDLE; }
int mouseButtonIsRight(MouseButton* mb) { return mb->code == SDL_BUTTON_RIGHT; }
unsigned int mouseButtonCode(MouseButton* mb) { return mb->code; }


MousePosition* getMousePosition(Window* w) {
	if (w->event.type == SDL_MOUSEMOTION) {
		w->mousePosition.x = w->event.motion.x;
		w->mousePosition.y = w->event.motion.y;
		return &w->mousePosition;
	}
	return NULL;
}

int mousePositionX(MousePosition* mp) { return mp->x; }
int mousePositionY(MousePosition* mp) { return mp->y; }

Scroll* getScroll(Window* w) {
	if (w->event.type == SDL_MOUSEWHEEL) {
		w->scroll.x = w->event.wheel.x;
		w->scroll.y = w->event.wheel.y;
		return &w->scroll;
	}
	return NULL;
}

int scrollX(Scroll* s) { return s->x; }
int scrollY(Scroll* s) { return s->y; }
