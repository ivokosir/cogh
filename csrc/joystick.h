#pragma once

#include "window.h"

typedef struct JoystickButton JoystickButton;

JoystickButton** getJoystickButtons(Window*);
unsigned int joystickButtonId(JoystickButton*);
int joystickButtonIsPress(JoystickButton*);
unsigned int joystickButtonCode(JoystickButton*);

typedef struct JoystickAxis JoystickAxis;

JoystickAxis** getJoystickAxii(Window*);
unsigned int joystickAxisId(JoystickAxis*);
unsigned int joystickAxisAxis(JoystickAxis*);
double joystickAxisValue(JoystickAxis*);
