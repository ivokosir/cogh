#include "window-private.h"

#include "joystick.h"

#include <SDL.h>

JoystickButton* getJoystickButton(Window* w) {
	if (w->event.type == SDL_JOYBUTTONDOWN | w->event.type == SDL_JOYBUTTONUP) {
		w->joystickButton.id = w->event.jbutton.which;
		w->joystickButton.isPress = w->event.jbutton.state == SDL_PRESSED;
		w->joystickButton.code = w->event.jbutton.button;
		return &w->joystickButton;
	}
	return NULL;
}

unsigned int joystickButtonId(JoystickButton* jb) { return jb->id; }
int joystickButtonIsPress(JoystickButton* jb) { return jb->isPress; }
unsigned int joystickButtonCode(JoystickButton* jb) { return jb->code; }


JoystickAxis* getJoystickAxis(Window* w) {
	if (w->event.type == SDL_MOUSEWHEEL) {
		w->joystickAxis.id = w->event.jaxis.which;
		w->joystickAxis.axis = w->event.jaxis.axis;

		double value = w->event.jaxis.value / 32767.0;
		value = value < -1.0 ? -1.0 : value;
		w->joystickAxis.value = w->event.jaxis.value;

		return &w->joystickAxis;
	}
	return NULL;
}

unsigned int joystickAxisId(JoystickAxis* ja) { return ja->id; }
unsigned int joystickAxisAxis(JoystickAxis* ja) { return ja->axis; }
double joystickAxisValue(JoystickAxis* ja) { return ja->value; }
