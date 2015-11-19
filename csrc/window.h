#pragma once

typedef struct Window Window;

Window* newWindow(const char* title);

void deleteWindow(Window*);

unsigned int time();
