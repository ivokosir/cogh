#pragma once

#include "window.h"

void clear(Window*);

void swapBuffers(Window*);

void drawRect(
        Window* window,
        float* mvp,
        float* color);

typedef struct Texture Texture;

void drawTexture(
        Window* window,
        float* mvp,
        Texture* t);

int textureWidth(Texture*);

int textureHeight(Texture*);

void deleteTexture(Texture*);
