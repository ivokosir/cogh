#pragma once

#include "window.h"

typedef struct JoystickButton JoystickButton;

JoystickButton* getJoystickButton(Window*);
unsigned int joystickButtonId(JoystickButton*);
int joystickButtonIsPress(JoystickButton*);
unsigned int joystickButtonCode(JoystickButton*);

typedef struct JoystickAxis JoystickAxis;

JoystickAxis* getJoystickAxis(Window*);
unsigned int joystickAxisId(JoystickAxis*);
unsigned int joystickAxisAxis(JoystickAxis*);
double joystickAxisValue(JoystickAxis*);
