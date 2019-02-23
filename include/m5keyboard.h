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

extern int modifierKeyState;

#ifdef USE_SERIAL_KEYBOARD
class SerialKeyboard {
	private:
		int baud;
		int ch;
		enum {
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
		void _setCharWithModkey(int c, int mod);
		void _normal(int c);
		void _shift(int c);
		void _alt(int c);
		void _transition(int c);
		void _poll();

	public:
		SerialKeyboard(int _baud);
		void setup();
		void poll();
		int peek();
		int read();
};
extern SerialKeyboard keyboard;
#endif /* USE_SERIAL_KEYBOARD */

#ifdef USE_FACES_KEYBOARD
class FacesKeyboard {
	private:
		int addr;
		int ch;
	public:
		FacesKeyboard(int _addr);
		void setup();
		void poll();
		int peek();
		int read();
};
extern FacesKeyboard keyboard;
#endif /* USE_FACES_KEYBOARD */

#ifdef USE_ONSCREEN_KEYBOARD
class M5OnScreenKeyboard;

class OnScreenKeyboard {
	private:
		String text;
		int pos;
		M5OnScreenKeyboard m5osk;

	public:
		OnScreenKeyboard() {}
		void setup();
		void poll();
		void start();
		int peek();
		int read();
};
extern OnScreenKeyboard keyboard;
#endif /* USE_ONSCREEN_KEYBOARD */
