#include "window-private.h"

#include "window.h"

#include "shader.h"

#include <SDL_ttf.h>
#include <SDL_image.h>

static Uint32 openWindows = 0;

static void initEvents(void** events) {
	for (int i = 0; i < EVENTS_SIZE; i++) {
		events[i] = NULL;
	}
}

static void initAllEvents(Window* w) {
	w->quit = 0;
	initEvents((void**) w->keys);
	initEvents((void**) w->sizes);
	initEvents((void**) w->mouseButtons);
	initEvents((void**) w->mousePositions);
	initEvents((void**) w->scrolls);
	initEvents((void**) w->joystickButtons);
	initEvents((void**) w->joystickAxii);
	initEvents((void**) w->joystickAddEvents);
	initEvents((void**) w->joystickRemoveEvents);
}

static void clearEvents(void** events) {
	for (int i = 0; i < EVENTS_SIZE; i++) {
		if (events[i] != NULL) {
			free(events[i]);
			events[i] = NULL;
		} else {
			return;
		}
	}
}

void clearAllEvents(Window* w) {
	w->quit = 0;
	clearEvents((void**) w->keys);
	clearEvents((void**) w->sizes);
	clearEvents((void**) w->mouseButtons);
	clearEvents((void**) w->mousePositions);
	clearEvents((void**) w->scrolls);
	clearEvents((void**) w->joystickButtons);
	clearEvents((void**) w->joystickAxii);
	clearEvents((void**) w->joystickAddEvents);
	clearEvents((void**) w->joystickRemoveEvents);
}

void addEvent(void** events, void* event) {
	for (int i = 0; i < EVENTS_SIZE; i++) {
		if (events[i] == NULL) {
			events[i] = event;
			return;
		}
	}

	clearEvents(events);
	events[0] = event;
}

Window* newWindow(const char* title) {
	if (openWindows == 0) {
		if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
			return NULL;
		}
		if (TTF_Init() != 0) {
			return NULL;
		}
		int imgFlags = IMG_INIT_JPG | IMG_INIT_PNG;
		if ((IMG_Init(imgFlags) & imgFlags) != imgFlags) {
			return NULL;
		}
	}
	else {
		// Implementation with SLD2 does not support
		// multiple windows, because event polling in
		// SLD2 is not done by single window
		return NULL;
	}
	openWindows++;

	SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);
	SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3);
	SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 2);

	Window* w = malloc(sizeof(Window));
	initAllEvents(w);

	w->window = SDL_CreateWindow(
			title,
			SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED,
			500, 500,
			SDL_WINDOW_OPENGL | SDL_WINDOW_RESIZABLE);
	if (w->window == NULL) {
		deleteWindow(w);
		return NULL;
	}

	w->glContext = SDL_GL_CreateContext(w->window);
	if (w->glContext == NULL) {
		deleteWindow(w);
		return NULL;
	}

	glewExperimental = GL_TRUE;
	GLenum glewError = glewInit();
	glGetError(); // temp fix
	if(glewError != GLEW_OK) {
		//printf("Error initializing GLEW! %s\n", glewGetErrorString(glewError));
		return NULL;
	}

	if(SDL_GL_SetSwapInterval(1) < 0) {
		//printf("Warning: Unable to set VSync! SDL Error: %s\n", SDL_GetError());
		return NULL;
	}

	glEnable(GL_BLEND);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

	if (!compileShader(w)) {
		return NULL;
	}

	return w;
}

void deleteWindow(Window* w) {
	if (w->glContext != NULL) {
		SDL_GL_DeleteContext(w->glContext);
		if (w->window != NULL) {
			SDL_DestroyWindow(w->window);
		}
	}

	clearAllEvents(w);
	free(w);

	openWindows--;
	if (openWindows == 0) {
		SDL_Quit();
		TTF_Quit();
		IMG_Quit();
	}
}

unsigned int time() {
	return SDL_GetTicks();
}
