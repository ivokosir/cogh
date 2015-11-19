#include "window-private.h"

#include "key.h"

#include <SDL.h>

Key* getKey(Window* w) {
	if (w->event.type == SDL_KEYDOWN | w->event.type == SDL_KEYUP) {
		w->key.isPress = w->event.key.type == SDL_KEYDOWN;
		w->key.isRepeat = w->event.key.repeat;
		w->key.code = w->event.key.keysym.sym;
		return &w->key;
	}
	return NULL;
}

int keyIsPress(Key* k) { return k->isPress; }
int keyIsRepeat(Key* k) { return k->isRepeat; }
unsigned int keyCode(Key* k) { return k->code; }
