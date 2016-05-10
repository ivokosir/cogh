#include "window-private.h"

#include "event.h"

#include "key.h"

#include <SDL.h>

static void pollEvent(Window* w, SDL_Event event) {
	if (event.type == SDL_KEYDOWN || event.type == SDL_KEYUP) {
		Key* key = malloc(sizeof(Key));
		key->isPress = event.key.type == SDL_KEYDOWN;
		key->isRepeat = event.key.repeat;
		key->code = event.key.keysym.sym;
		addEvent((void**) w->keys, key);
	} else if (event.type == SDL_MOUSEBUTTONDOWN || event.type == SDL_MOUSEBUTTONUP) {
		MouseButton* mouseButton = malloc(sizeof(MouseButton));
		mouseButton->isPress = event.button.type == SDL_MOUSEBUTTONDOWN;
		mouseButton->code = event.button.button;
		addEvent((void**) w->mouseButtons, mouseButton);
	} else if (event.type == SDL_MOUSEMOTION) {
		MousePosition* mousePosition = malloc(sizeof(MousePosition));
		mousePosition->x = event.motion.x;
		mousePosition->y = event.motion.y;
		addEvent((void**) w->mousePositions, mousePosition);
	} else if (event.type == SDL_MOUSEWHEEL) {
		Scroll* scroll = malloc(sizeof(Scroll));
		scroll->x = event.wheel.x;
		scroll->y = event.wheel.y;
		addEvent((void**) w->scrolls, scroll);
	} else if (event.type == SDL_JOYBUTTONDOWN || event.type == SDL_JOYBUTTONUP) {
		JoystickButton* joystickButton = malloc(sizeof(JoystickButton));
		joystickButton->id = event.jbutton.which;
		joystickButton->isPress = event.jbutton.state == SDL_PRESSED;
		joystickButton->code = event.jbutton.button;
		addEvent((void**) w->joystickButtons, joystickButton);
	} else if (event.type == SDL_MOUSEWHEEL) {
		JoystickAxis* joystickAxis = malloc(sizeof(JoystickAxis));
		joystickAxis->id = event.jaxis.which;
		joystickAxis->axis = event.jaxis.axis;

		double value = event.jaxis.value / 32767.0;
		value = value < -1.0 ? -1.0 : value;
		joystickAxis->value = event.jaxis.value;

		addEvent((void**) w->joystickAxii, joystickAxis);
	} else if (event.type == SDL_WINDOWEVENT && event.window.event == SDL_WINDOWEVENT_SIZE_CHANGED) {
		Size* size = malloc(sizeof(Size));
		size->w = event.window.data1;
		size->h = event.window.data2;
		glViewport(0, 0, size->w, size->h);
		addEvent((void**) w->sizes, size);
	} else if (event.type == SDL_QUIT) {
		Quit* quit = malloc(sizeof(Quit));
		addEvent((void**) w->quits, quit);
	}
}

void pollEvents(Window* w) {
	clearAllEvents(w);

	SDL_Event event;
	while (SDL_PollEvent(&event)) {
		pollEvent(w, event);
	}
}

Quit** getQuits(Window* w) { return w->quits; }

Size** getSizes(Window* w) { return w->sizes; }
unsigned int sizeW(Size* r) { return r->w; }
unsigned int sizeH(Size* r) { return r->h; }
