#include "window-private.h"

#include "joystick.h"

JoystickButton** getJoystickButtons(Window* w) { return w->joystickButtons; }
unsigned int joystickButtonId(JoystickButton* jb) { return jb->id; }
int joystickButtonIsPress(JoystickButton* jb) { return jb->isPress; }
unsigned int joystickButtonCode(JoystickButton* jb) { return jb->code; }


JoystickAxis** getJoystickAxii(Window* w) { return w->joystickAxii; }
unsigned int joystickAxisId(JoystickAxis* ja) { return ja->id; }
unsigned int joystickAxisAxis(JoystickAxis* ja) { return ja->axis; }
double joystickAxisValue(JoystickAxis* ja) { return ja->value; }
