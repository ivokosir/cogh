#include "window-private.h"

#include "render-private.h"

#include "image.h"

#include <SDL_image.h>


Texture* newTextureFromImage(Window* w, const char* file) {
	Texture* t = malloc(sizeof(Texture));

	SDL_Surface* surface =
		IMG_Load(file);
	t->sdl_texture =
		SDL_CreateTextureFromSurface(w->renderer, surface);
	SDL_FreeSurface(surface);

	return t;
}
