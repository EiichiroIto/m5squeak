#include <M5Core2.h>
#include "msq.h"
#include "m5keyboard.h"

/* squeak keys */
#define ESC (0x1B)
#define LEFTKEY	(28)
#define UPKEY (30)
#define RIGHTKEY (29)
#define DOWNKEY (31)
#define INSKEY (5)
#define DELKEY (5)
#define PRIORKEY (11)
#define NEXTKEY (12)
#define HOMEKEY (1)
#define ENDKEY (127)

/* modifier keys */
#define SHIFTKEY (0x01)
#define CONTROLKEY (0x02)
#define ALTKEY (0x04)
#define COMMANDKEY (0x08)

static int modifierKeyState = 0;
static int ch = -1;
static bool i2cKeyboardAvailable = false;

static enum {
  SK_NONE = 0,
  SK_ESC,
  SK_CSI, /* 1B 5B */
  SK_CSI1, /* 1B 5B 31 */
  SK_CSI1a, /* 1B 5B 31 3B */
  SK_CSI1b, /* 1B 5B 31 3B 32 */
  SK_CSI2, /* 1B 5B 32 */
  SK_CSI3, /* 1B 5B 33 */
  SK_CSI5, /* 1B 5B 35 */
  SK_CSI6, /* 1B 5B 36 */
} state;

static void _setCharWithModkey(int c, int mod);
static void _normal(int c);
static void _shift(int c);
static void _alt(int c);
static void _transition(int c);
static void _uart_poll();
static void _cardkb_poll();

void keyboard_setup()
{
	ch = -1;
	state = SK_NONE;
  modifierKeyState = 0;
  delay(1000);
  Wire.beginTransmission(I2C_CARDKB_ADDR);
  delay(100);
  i2cKeyboardAvailable = Wire.endTransmission() == 0;
}

void keyboard_poll()
{
	if (ch != -1) {
		return;
	}
  _cardkb_poll();
	if (ch != -1) {
		return;
	}
  _uart_poll();
}

int keyboard_peek()
{
  keyboard_poll();
  return ch;
}

int keyboard_read()
{
	keyboard_poll();
	int tmp = ch;
	ch = -1;
	return tmp;
}

int keyboard_modifiers()
{
  return modifierKeyState;
}

static void _setCharWithModkey(int c, int mod)
{
	modifierKeyState = mod;
	ch = (modifierKeyState << 8) | c;
}

// For serial keyboard

static void _normal(int c)
{
	state = SK_NONE;
	_setCharWithModkey(c, (c < 0x20) ? CONTROLKEY : 0);
}

static void _shift(int c)
{
	state = SK_NONE;
	_setCharWithModkey(c, SHIFTKEY);
}

static void _alt(int c)
{
	state = SK_NONE;
	_setCharWithModkey(c, COMMANDKEY);
}

static void _transition(int c)
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

static void _uart_poll()
{
	while (Serial.peek() != -1) {
		_transition(UART.read());
		if (ch != -1) {
			break;
		}
		delay(10);
	}
}

// For I2C CardKB keyboard

struct {
  int key;
  int modifier;
} cardkb_table[] = {
  { ESC, 0 },   // 0x80
  { '1', SHIFTKEY },   // 0x81
  { '2', SHIFTKEY },   // 0x82
  { '3', SHIFTKEY },   // 0x83
  { '4', SHIFTKEY },   // 0x84
  { '5', SHIFTKEY },   // 0x85
  { '6', SHIFTKEY },   // 0x86
  { '7', SHIFTKEY },   // 0x87
  { '8', SHIFTKEY },   // 0x88
  { '9', SHIFTKEY },   // 0x89
  { '0', SHIFTKEY },   // 0x8A
  { 0x08, 0 },   // 0x8B
  { 0x09, 0 },   // 0x8C
  { 'q', COMMANDKEY },   // 0x8D
  { 'w', COMMANDKEY },   // 0x8E
  { 'e', COMMANDKEY },   // 0x8F
  { 'r', COMMANDKEY },   // 0x90
  { 't', COMMANDKEY },   // 0x91
  { 'y', COMMANDKEY },   // 0x92
  { 'u', COMMANDKEY },   // 0x93
  { 'i', COMMANDKEY },   // 0x94
  { 'o', COMMANDKEY },   // 0x95
  { 'p', COMMANDKEY },   // 0x96
  { 0, 0 },   // 0x97
  { HOMEKEY, SHIFTKEY },   // 0x98
  { PRIORKEY, SHIFTKEY },   // 0x99
  { 'a', COMMANDKEY },   // 0x9A
  { 's', COMMANDKEY },   // 0x9B
  { 'd', COMMANDKEY },   // 0x9C
  { 'f', COMMANDKEY },   // 0x9D
  { 'g', COMMANDKEY },   // 0x9E
  { 'h', COMMANDKEY },   // 0x9F
  { 'j', COMMANDKEY },   // 0xA0
  { 'k', COMMANDKEY },   // 0xA1
  { 'l', COMMANDKEY },   // 0xA2
  { 0x0D, 0 },   // 0xA3
  { NEXTKEY, SHIFTKEY },   // 0xA4
  { ENDKEY, SHIFTKEY },   // 0xA5
  { 'z', COMMANDKEY },   // 0xA6
  { 'x', COMMANDKEY },   // 0xA7
  { 'c', COMMANDKEY },   // 0xA8
  { 'v', COMMANDKEY },   // 0xA9
  { 'b', COMMANDKEY },   // 0xAA
  { 'n', COMMANDKEY },   // 0xAB
  { 'm', COMMANDKEY },   // 0xAC
  { ',', 0 },   // 0xAD
  { '.', 0 },   // 0xAE
  { ' ', 0 },   // 0xAF
  { 0, 0 },   // 0xB0
  { 0, 0 },   // 0xB1
  { 0, 0 },   // 0xB2
  { 0, 0 },   // 0xB3
  { LEFTKEY, 0 },   // 0xB4
  { UPKEY, 0 },   // 0xB5
  { DOWNKEY, 0 },   // 0xB6
  { RIGHTKEY, 0 },   // 0xB7
};

static void _cardkb_poll()
{
  if (!i2cKeyboardAvailable) {
    return;
  }
	if (ch != -1) {
		return;
	}
  Wire.requestFrom(I2C_CARDKB_ADDR, 1);
  if (Wire.available()) {
    ch = Wire.read();
    if (ch >= 0x80 && ch <= 0xB7) {
      _setCharWithModkey(cardkb_table[ch - 0x80].key, cardkb_table[ch - 0x80].modifier);
    } else if (ch >= 'A' && ch <= 'Z') {
      modifierKeyState = SHIFTKEY;
    }
    if (ch == 0) {
      ch = -1;
      modifierKeyState = 0;
    }
  }
}
