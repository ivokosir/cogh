#pragma once

#include "event.h"
#include "key.h"
#include "mouse.h"
#include "joystick.h"

#include "event-private.h"

#define GLEW_STATIC
#include <GL/glew.h>

#include <SDL.h>

#define EVENTS_SIZE 50

struct Window {
	SDL_Window* window;

	SDL_GLContext glContext;
	GLint shaderMvp;
	GLint shaderMode;
	GLint shaderColor;
	GLint shaderTexture;

	Quit* quits[EVENTS_SIZE];
	Key* keys[EVENTS_SIZE];
	Size* sizes[EVENTS_SIZE];
	MouseButton* mouseButtons[EVENTS_SIZE];
	MousePosition* mousePositions[EVENTS_SIZE];
	Scroll* scrolls[EVENTS_SIZE];
	JoystickButton* joystickButtons[EVENTS_SIZE];
	JoystickAxis* joystickAxii[EVENTS_SIZE];
};

void clearAllEvents(Window* w);
void addEvent(void** events, void* event);
