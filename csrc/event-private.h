#pragma once

struct Key {
	int isPress;
	int isRepeat;
	unsigned int code;
};

struct Size {
	int w;
	int h;
};

struct MouseButton {
	int isPress;
	unsigned int code;
};

struct MousePosition {
	int x;
	int y;
};

struct Scroll {
	int x;
	int y;
};

struct JoystickButton {
	unsigned int id;
	int isPress;
	unsigned int code;
};

struct JoystickAxis {
	unsigned int id;
	unsigned int axis;
	double value;
};
