#pragma once

#include "window.h"

#include "render.h"

typedef struct Font Font;

Font* newFont(const char* file, int size);

void deleteFont(Font* f);

Texture* newTextureFromText(
        Window* w,
        Font* f, const char* text,
        float* color);
