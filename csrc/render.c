#include "window-private.h"

#include "render-private.h"

#include "render.h"

void clear(Window* w) {
	glClearColor(0.0, 0.0, 0.0, 1.0);
	glClear(GL_COLOR_BUFFER_BIT);
}

void swapBuffers(Window* w) {
	SDL_GL_SwapWindow(w->window);
}

void drawRect(
		Window* window,
		float* mvp,
		float* color) {
	glUniformMatrix3fv(window->shaderMvp, 1, GL_FALSE, mvp);
	glUniform1i(window->shaderMode, 0);
	glUniform4fv(window->shaderColor, 1, color);

	glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_BYTE, 0);
}

void drawTexture(
		Window* window,
		float* mvp,
		Texture* t) {
	glUniformMatrix3fv(window->shaderMvp, 1, GL_FALSE, mvp);
	glUniform1i(window->shaderMode, 1);
	glBindTexture(GL_TEXTURE_2D, t->id);

	glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_BYTE, 0);
}

int textureWidth(Texture* t) {
	return t->width;
}

int textureHeight(Texture* t) {
	return t->height;
}

void deleteTexture(Texture* t) {
	glDeleteTextures(1, &t->id);
	free(t);
}

Texture* surfaceToTexture(SDL_Surface* surface) {
	Texture* t = malloc(sizeof(Texture));

	glGenTextures(1, &(t->id));
	glBindTexture(GL_TEXTURE_2D, t->id);

	GLenum mode = GL_RGB;

	if(surface->format->BytesPerPixel == 4) {
		mode = GL_RGBA;
	}

	glTexImage2D(GL_TEXTURE_2D,
			0, mode,
			surface->w, surface->h,
			0, mode, GL_UNSIGNED_BYTE,
			surface->pixels);

	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

	SDL_FreeSurface(surface);

	t->width = surface->w;
	t->height = surface->h;

	return t;
}
