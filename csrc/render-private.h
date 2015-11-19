#pragma once

#include <SDL.h>
#include <SDL_ttf.h>


struct Texture {
	SDL_Texture* sdl_texture;
};

struct Font {
	TTF_Font* ttf_font;
};
