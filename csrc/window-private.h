#pragma once

#include "event.h"
#include "key.h"
#include "mouse.h"
#include "joystick.h"

#include "event-private.h"

#include <SDL.h>

struct Window {
	SDL_Window* window;
	SDL_Renderer* renderer;
	SDL_Event event;
	Quit quit;
	Key key;
	Resize resize;
	MouseButton mouseButton;
	MousePosition mousePosition;
	Scroll scroll;
	JoystickButton joystickButton;
	JoystickAxis joystickAxis;
};
