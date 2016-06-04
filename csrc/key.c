#include "window-private.h"

#include "key.h"

Key** getKeys(Window* w) { return w->keys; }
int keyIsPress(Key* k) { return k->isPress; }
int keyIsRepeat(Key* k) { return k->isRepeat; }
unsigned int keyCode(Key* k) { return k->code; }

unsigned int readCode(const char* name) {
    return SDL_GetKeyFromName(name);
}

const char* showCode(unsigned int code) {
    return SDL_GetKeyName(code);
}
