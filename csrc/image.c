#include "window-private.h"

#include "render-private.h"

#include "image.h"

#include <SDL_image.h>


Texture* newTextureFromImage(Window* w, const char* file) {
    return surfaceToTexture(IMG_Load(file));
}
