#include "M5Stack.h"
#include "Wire.h"
#include "msq.h"
#include "m5config.h"
#include "m5mouse.h"

#ifdef USE_JOYSTICK_MOUSE
#define JOYSTICK_ADDR 0x52
JoyMouse mouse(JOYSTICK_ADDR);

JoyMouse::JoyMouse(int _addr)
{
	addr = _addr;
}

void JoyMouse::setup(int w, int h, int _xc, int _yc)
{
	width = w;
	height = h;
	xcenter = _xc;
	ycenter = _yc;
	xpos = width / 2;
	ypos = height / 2;
	pushed = 0;
}

void JoyMouse::hide()
{
	M5.Lcd.drawPixel(xpos, ypos, 0x0000);
	M5.Lcd.drawPixel(xpos-1, ypos, 0x0000);
	M5.Lcd.drawPixel(xpos+1, ypos, 0x0000);
	M5.Lcd.drawPixel(xpos, ypos-1, 0x0000);
	M5.Lcd.drawPixel(xpos, ypos+1, 0x0000);
}

void JoyMouse::show()
{
	M5.Lcd.drawPixel(xpos, ypos, 0xFFFF);
	M5.Lcd.drawPixel(xpos-1, ypos, 0xFFFF);
	M5.Lcd.drawPixel(xpos+1, ypos, 0xFFFF);
	M5.Lcd.drawPixel(xpos, ypos-1, 0xFFFF);
	M5.Lcd.drawPixel(xpos, ypos+1, 0xFFFF);
}

void JoyMouse::poll()
{
	Wire.requestFrom(addr, 3);
	if (!Wire.available()) {
		return;
	}
	int dx = xcenter - Wire.read();
	int dy = ycenter - Wire.read();
	pushed = Wire.read();
	if (abs(dx) > 20) {
		dx = (dx < 0) ? (dx + 20) / 10 : (dx - 20) / 10;
	} else {
		dx = 0;
	}
	if (abs(dy) > 20) {
		dy = (dy < 0) ? (dy + 20) / 10 : (dy - 20) / 10;
	} else {
		dy = 0;
	}
	if ((xpos + dx < 0) || (xpos + dx > width)) {
		return;
	}
	if ((ypos - dy < 0) || (ypos - dy > height)) {
		return;
	}
	if (dx == 0 && dy == 0) {
		return;
	}
	hide();
	xpos += dx;
	ypos -= dy;
	show();
}

int JoyMouse::x()
{
	return xpos;
}

int JoyMouse::y()
{
	return ypos;
}

int JoyMouse::button()
{
	return pushed;
}
#endif /* USE_JOYSTICK_MOUSE */
