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
		float* color) {
	Texture* t = malloc(sizeof(Texture));

	SDL_Color sdl_color;
	sdl_color.r = color[0] * 0xFF;
	sdl_color.g = color[1] * 0xFF;
	sdl_color.b = color[2] * 0xFF;
	sdl_color.a = color[3] * 0xFF;

	return surfaceToTexture(
		TTF_RenderUTF8_Blended(f->ttf_font, text, sdl_color));
}
