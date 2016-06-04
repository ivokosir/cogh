#pragma once

#include "window-private.h"

#include <SDL.h>
#include <SDL_ttf.h>

struct Texture {
    GLuint id;
    int width;
    int height;
};

struct Font {
    TTF_Font* ttf_font;
};

struct Texture* surfaceToTexture(SDL_Surface* surface);
