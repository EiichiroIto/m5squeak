#include "M5Stack.h"
#include "Wire.h"
#include "msq.h"
#include "m5config.h"
#include "m5keyboard.h"

int modifierKeyState = 0;

#ifdef USE_SERIAL_KEYBOARD
#define SERIAL_KEYBOARD_BAUDRATE 115200
SerialKeyboard keyboard(SERIAL_KEYBOARD_BAUDRATE);

SerialKeyboard::SerialKeyboard(int _baud)
{
	baud = _baud;
	ch = -1;
	state = SK_NONE;
}

void SerialKeyboard::setup()
{
	Serial.begin(baud);
}

void SerialKeyboard::poll()
{
}

void SerialKeyboard::_setCharWithModkey(int c, int mod)
{
	modifierKeyState = mod;
	ch = (modifierKeyState << 8) | c;
}

void SerialKeyboard::_normal(int c)
{
	state = SK_NONE;
	_setCharWithModkey(c, (c < 0x20) ? CONTROLKEY : 0);
}

void SerialKeyboard::_shift(int c)
{
	state = SK_NONE;
	_setCharWithModkey(c, SHIFTKEY);
}

void SerialKeyboard::_alt(int c)
{
	Serial.print("<ALT-");
	Serial.print(c);
	Serial.print(">");
	state = SK_NONE;
	_setCharWithModkey(c, COMMANDKEY);
}

void SerialKeyboard::_transition(int c)
{
	switch (state) {
	case SK_NONE:
		if (c == ESC) {
			state = SK_ESC;
		} else if (c != 0x0A) {
			_normal(c);
		}
		break;
	case SK_ESC:
		if (c == 0x5B) {
			state = SK_CSI;
		} else if (c >= 'a' && c <= 'z') {
			_alt(c);
		} else {
			_normal(ESC);
		}
		break;
	case SK_CSI:
		if (c == 0x31) {
			state = SK_CSI1;
		} else if (c == 0x32) {
			state = SK_CSI2;
		} else if (c == 0x33) {
			state = SK_CSI3;
		} else if (c == 0x35) {
			state = SK_CSI5;
		} else if (c == 0x36) {
			state = SK_CSI6;
		} else if (c == 0x41) {
			_normal(UPKEY);
		} else if (c == 0x42) {
			_normal(DOWNKEY);
		} else if (c == 0x43) {
			_normal(RIGHTKEY);
		} else if (c == 0x44) {
			_normal(LEFTKEY);
		} else if (c == 0x46) {
			_normal(ENDKEY);
		} else if (c == 0x48) {
			_normal(HOMEKEY);
		} else {
			state = SK_NONE;
		}
		break;
	case SK_CSI1:
		if (c == 0x3B) {
			state = SK_CSI1a;
		} else {
			state = SK_NONE;
		}
		break;
	case SK_CSI1a:
		if (c == 0x32) {
			state = SK_CSI1b;
		} else {
			state = SK_NONE;
		}
		break;
	case SK_CSI1b:
		if (c == 0x41) {
			_shift(UPKEY);
		} else if (c == 0x42) {
			_shift(DOWNKEY);
		} else if (c == 0x43) {
			_shift(RIGHTKEY);
		} else if (c == 0x44) {
			_shift(LEFTKEY);
		} else {
			state = SK_NONE;
		}
		break;
	case SK_CSI2:
	case SK_CSI3:
	case SK_CSI5:
	case SK_CSI6:
		state = SK_NONE;
		if (c == 0x7E) {
			if (state == SK_CSI2) {
				_normal(INSKEY);
			} else if (state == SK_CSI3) {
				_normal(DELKEY);
			} else if (state == SK_CSI5) {
				_normal(PRIORKEY);
			} else if (state == SK_CSI6) {
				_normal(NEXTKEY);
			}
		}
		break;
	}
}

void SerialKeyboard::_poll()
{
	if (ch != -1) {
		return;
	}
	while (Serial.peek() != -1) {
		_transition(Serial.read());
		if (ch != -1) {
			break;
		}
		delay(10);
	}
}

int SerialKeyboard::peek()
{
	_poll();
	return ch;
}

int SerialKeyboard::read()
{
	_poll();
	int tmp = ch;
	ch = -1;
	return tmp;
}
#endif /* USE_SERIAL_KEYBOARD */

#ifdef USE_FACES_KEYBOARD
#define FACES_KEYBOARD_I2C_ADDR 0x08
FacesKeyboard keyboard(FACES_KEYBOARD_I2C_ADDR);

FacesKeyboard::FacesKeyboard(int _addr)
{
	addr = _addr;
	ch = -1;
}

void FacesKeyboard::setup()
{
	pinMode(5, INPUT);
	digitalWrite(5,HIGH);
}

void FacesKeyboard::poll()
{
	if (ch >= 0) {
		return;
	}
	if (digitalRead(5) == LOW) {
		Wire.requestFrom(addr, 1);
		if (Wire.available()) {
			ch = Wire.read();
		}
	}
}

int FacesKeyboard::peek()
{
	return ch;
}

int FacesKeyboard::read()
{
	int tmp = ch;
	ch = -1;
	return tmp;
}
#endif /* USE_FACES_KEYBOARD */

#ifdef USE_ONSCREEN_KEYBOARD
#include <M5OnScreenKeyboard.h>
#define USE_ONSCREEN_KEYBOARD_JOYSTICK

OnScreenKeyboard keyboard;

OnScreenKeyboard::OnScreenKeyboard()
{	
}

void OnScreenKeyboard::setup()
{
#ifdef USE_ONSCREEN_KEYBOARD_JOYSTICK
	m5osk.useJoyStick = true;
#endif /* USE_ONSCREEN_KEYBOARD_JOYSTICK */
	text = String("");
	pos = 0;
}

void OnScreenKeyboard::poll()
{
	if (peek() != -1) {
		return;
	}
	if (M5.BtnC.isPressed()) {
		start();
	}
}

void OnScreenKeyboard::start()
{
	m5osk.setup();
	while (m5osk.loop()) {
		delay(1);
	}
	text = m5osk.getString();
	pos = 0;
	m5osk.close();
}

int OnScreenKeyboard::peek()
{
	return pos >= text.length() ? -1 : text.charAt(pos);
}

int OnScreenKeyboard::read()
{
	return pos >= text.length() ? -1 : text.charAt(pos++);
}
#endif /* USE_ONSCREEN_KEYBOARD */
