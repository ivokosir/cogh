#include "window-private.h"

#include "window.h"

#include <SDL_ttf.h>
#include <SDL_image.h>

static Uint32 openWindows = 0;

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

	Window* w = malloc(sizeof(Window));

	w->window = SDL_CreateWindow(
			title,
			SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED,
			500, 500,
			SDL_WINDOW_RESIZABLE);
	if (w->window == NULL) {
		deleteWindow(w);
		return NULL;
	}

	w->renderer = SDL_CreateRenderer(
		w->window,
		-1,
		SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
	if (w->renderer == NULL) {
		deleteWindow(w);
		return NULL;
	}

	return w;
}

void deleteWindow(Window* w) {
	if (w->renderer != NULL) {
		SDL_DestroyRenderer(w->renderer);
		if (w->window != NULL) {
			SDL_DestroyWindow(w->window);
		}
	}

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
