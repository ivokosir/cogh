#include "window-private.h"

#include "render-private.h"

#include "text.h"


Font* newFont(const char* file, int size) {
	Font* f = malloc(sizeof(Font));
	f->ttf_font =
		TTF_OpenFont("/usr/share/fonts/TTF/DejaVuSans.ttf", size);
	return f;
}

void deleteFont(Font* f) {
	TTF_CloseFont(f->ttf_font);
	free(f);
}


Texture* newTextureFromText(
		Window* w,
		Font* f, const char* text,
		unsigned int color) {
	Texture* t = malloc(sizeof(Texture));

	SDL_Color sdl_color;
	sdl_color.r = (color >> (8*3)) & 0xFF;
	sdl_color.g = (color >> (8*2)) & 0xFF;
	sdl_color.b = (color >> (8*1)) & 0xFF;
	sdl_color.a = (color >> (8*0)) & 0xFF;

	SDL_Surface* surface =
		TTF_RenderUTF8_Solid(f->ttf_font, text, sdl_color);
	t->sdl_texture =
		SDL_CreateTextureFromSurface(w->renderer, surface);
	SDL_FreeSurface(surface);

	return t;
}
