#include "window-private.h"

#include "render-private.h"

#include "render.h"


void clear(Window* w) {
	SDL_SetRenderDrawColor(w->renderer,
			0,
			0,
			0,
			255);
	SDL_RenderClear(w->renderer);
}

void swapBuffers(Window* w) {
	SDL_RenderPresent(w->renderer);
}

void drawRect(
		Window* window,
		int x, int y, int w, int h,
		unsigned int color) {
	SDL_Renderer* r = window->renderer;
	SDL_SetRenderDrawColor(r,
			(color >> (8*3)) & 0xFF,
			(color >> (8*2)) & 0xFF,
			(color >> (8*1)) & 0xFF,
			(color >> (8*0)) & 0xFF);
	SDL_Rect rect = {x, y, w, h};
	SDL_RenderFillRect(r, &rect);
}

void drawTexture(
		Window* window, Texture* t,
		int x, int y, int w, int h,
		double angle, int cx, int cy) {
	SDL_Rect rect = {x, y, w, h};
	SDL_Point center = {cx, cy};
	SDL_RenderCopyEx(
			window->renderer, t->sdl_texture,
			NULL, &rect,
			angle, &center, SDL_FLIP_NONE);
}

int textureWidth(Texture* t) {
	int w;
	SDL_QueryTexture(t->sdl_texture, NULL, NULL, &w, NULL);
	return w;
}

int textureHeight(Texture* t) {
	int h;
	SDL_QueryTexture(t->sdl_texture, NULL, NULL, NULL, &h);
	return h;
}

void deleteTexture(Texture* t) {
	SDL_DestroyTexture(t->sdl_texture);
	free(t);
}
