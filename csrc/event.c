#include "window-private.h"

#include "event.h"

#include <SDL.h>

int pollEvent(Window* w) {
	return SDL_PollEvent(&w->event);
}

Quit* getQuit(Window* w) {
	if (w->event.type == SDL_QUIT) {
		return &w->quit;
	}
	return NULL;
}

Resize* getResize(Window* w) {
	if (w->event.type == SDL_WINDOWEVENT) {
		if (w->event.window.event == SDL_WINDOWEVENT_SIZE_CHANGED) {
			w->resize.w = w->event.window.data1;
			w->resize.h = w->event.window.data2;
			return &w->resize;
		}
	}
	return NULL;
}

unsigned int resizeW(Resize* r) { return r->w; }
unsigned int resizeH(Resize* r) { return r->h; }
