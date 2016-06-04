#pragma once

#include "window.h"

typedef struct Joystick Joystick;

Joystick** getJoysticks(Window*);
unsigned int getJoystickId(Joystick*);
int getJoystickNumberOfAxii(Joystick*);
int getJoystickIsAdd(Joystick*);

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
