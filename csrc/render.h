#pragma once

#include "window.h"

void clear(Window*);

void swapBuffers(Window*);

void drawRect(Window*, int x, int y, int w, int h, unsigned int color);

typedef struct Texture Texture;

void drawTexture(
		Window* window, Texture* t,
		int x, int y, int w, int h,
		double angle, int cx, int cy);

int textureWidth(Texture*);

int textureHeight(Texture*);

void deleteTexture(Texture*);
