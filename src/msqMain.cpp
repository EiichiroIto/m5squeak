/* msqMain.cpp

	One purpose of this file is to provide an implementation roadmap when bootstrapping
	Squeak on a new platform. Once all the non-stubbed-out functions in this file have
	been implemented, you will have a working, usable Squeak virtual machine!

*** Implementation Notes ***

  File Naming

	The virtual machine keeps track of the full path name of the Squeak image
	file and the path to the directory containing the virtual machine. In this
	minimal implementation, the VM path is the empty string and the image name is
	hardwired to "msqueak.image". It is assumed that the image file, the changes
	file, the Squeak application, and the system sources file are all in the
	the same directory, and that that directory is the default working directory
	for file operations. The "shortImageName" is used to display the image file
	name (but not its full path) in the title bar of the Macintosh window.

  I/O Functions

	The following I/O functions are essential for graphical display and user interaction:
		ioScreenSize()
		ioShowDisplay()
		ioGetButtonState()
		ioGetKeystroke()
		ioMousePoint()
		ioPeekKeystroke()

	The following can be made no-ops:
		ioProcessEvents() 	-- poll for input events on some platforms
		ioSetCursor()		-- install a 16x16 black and white hardware cursor
		ioSetCursorWithMask() -- install a masked cursor
		ioBeep()			-- make a short beep through the speaker
		ioExit()			-- exit the VM: quit the application, reboot, power down, or
							-- some other behavior appropriate to this platform
							-- (if this is a noop you won't be able to quit from Squeak)
		ioRelinquishProcessorForMicroseconds()
							-- called when Squeak is idle to return time to the OS

  Time Functions

		ioMSecs(), ioMicroMSecs()
							-- both return a millisecond clock value, but historically
							-- ioMicroMSecs() used a higher resolution timer; the
							-- ideal implementation is an inexpensive clock with 1
							-- millisecond accuracy, but both functions can use a
							-- clock with much coarser accuracy (e.g., 50-100 mSecs)
							-- if necessary
		ioSeconds()			-- return the number of seconds since Jan 1, 1901
	   						-- may return 0, but then the current date and time
	   						-- will be wrong

*** Linking ***

	To build a Linux VM using this file, link together:

		interp.c		-- automatically generated interpreter file
		sqDirPrims.c	-- directory primitives (can be stubbed out)
		sqFilePrims.c	-- file primitives (can be stubbed out)
		sqLinuxMain.c	-- this file
		sqMiscPrims.c	-- automatically generated primitives (optional)

	plus the appropriate standard libraries.

*/

#include "M5Stack.h"
#include "msq.h"
#include "Wire.h"
#include "config.h"

/*** Variables -- Imported from Virtual Machine ***/
extern int fullScreenFlag;
extern int interruptCheckCounter;
extern int interruptKeycode;
extern int interruptPending;  /* set to true by recordKeystroke if interrupt key is pressed */
extern unsigned char *memory;
extern int savedWindowSize;   /* set from header when image file is loaded */

/*** Variables -- image and path names ***/
char imageName[] = "msqueak.image";
const char vmPath[] = "m5stackvm";
int ScreenWidth = 100;
int ScreenHeight = 100;

/* Mouse Emulation */

#ifdef USE_JOYSTICK_MOUSE
class JoyMouse {
	private:
		int xpos;
		int ypos;
		int pushed;
		int addr;
		int xcenter;
		int ycenter;
		int width;
		int height;

	public:
		JoyMouse(int _addr) {
			addr = _addr;
		}
		void setup(int w, int h, int _xc, int _yc) {
			width = w;
			height = h;
			xcenter = _xc;
			ycenter = _yc;
			xpos = width / 2;
			ypos = height / 2;
			pushed = 0;
		}
		void hide() {
			M5.Lcd.drawPixel(xpos, ypos, 0x0000);
			M5.Lcd.drawPixel(xpos-1, ypos, 0x0000);
			M5.Lcd.drawPixel(xpos+1, ypos, 0x0000);
			M5.Lcd.drawPixel(xpos, ypos-1, 0x0000);
			M5.Lcd.drawPixel(xpos, ypos+1, 0x0000);
		}
		void show() {
			M5.Lcd.drawPixel(xpos, ypos, 0xFFFF);
			M5.Lcd.drawPixel(xpos-1, ypos, 0xFFFF);
			M5.Lcd.drawPixel(xpos+1, ypos, 0xFFFF);
			M5.Lcd.drawPixel(xpos, ypos-1, 0xFFFF);
			M5.Lcd.drawPixel(xpos, ypos+1, 0xFFFF);
		}
		void poll() {
			Wire.requestFrom(addr, 3);
			if (!Wire.available()) {
				return;
			}
			int dx = Wire.read() - xcenter;
			int dy = Wire.read() - ycenter;
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
		int x() { return xpos; }
		int y() { return ypos; }
		int button() { return pushed; }
};
#define JOY_ADDR 0x52
JoyMouse mouse(JOY_ADDR);
#endif /* USE_JOYSTICK_MOUSE */

/* Keyboard Emulation */

#ifdef USE_SERIAL_KEYBOARD
class SerialKeyboard {
	private:
		int baud;
		int ch;
	public:
		SerialKeyboard(int _baud) {
			baud = _baud;
			ch = -1;
		}
		void setup() {
			Serial.begin(baud);
		}
		void poll() {
		}
		void _poll() {
			if (ch != -1) {
				return;
			}
			if (Serial.peek() == -1) {
				return;
			}
			ch = Serial.read();
		}
		int peek() {
			_poll();
			return ch;
		}
		int read() {
			_poll();
			int tmp = ch;
			ch = -1;
			return tmp;
		}
};
SerialKeyboard keyboard(115200);
#endif /* USE_SERIAL_KEYBOARD */

#ifdef USE_FACES_KEYBOARD
class FacesKeyboard {
	private:
		int addr;
		int ch;
	public:
		FacesKeyboard(int _addr) {
			addr = _addr;
			ch = -1;
		}
		void setup() {
			pinMode(5, INPUT);
			digitalWrite(5,HIGH);
		}
		void poll() {
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
		int peek() {
			return ch;
		}
		int read() {
			int tmp = ch;
			ch = -1;
			return tmp;
		}
};
#define FACES_KEYBOARD_I2C_ADDR 0x08
FacesKeyboard keyboard(FACES_KEYBOARD_I2C_ADDR);
#endif /* USE_FACES_KEYBOARD */

/*** Display Primitives ***/
void ioSetFullScreen(int fullScreen)
{
	/* noop */
}

int ioGetButtonState(void)
{
	/* left button: 0x04 */
	/* middle button: 0x02 */
	/* right button: 0x01 */
	int stButtons = 0;
	M5.update();
	stButtons |= (M5.BtnA.wasPressed() ? 0x04 : 0);
	stButtons |= mouse.button() ? 0x04 : 0;
	stButtons |= (M5.BtnB.wasPressed() ? 0x02 : 0);
	stButtons |= (M5.BtnC.wasPressed() ? 0x01 : 0);
	return stButtons;
}

int ioGetKeystroke(void)
{
	return keyboard.read();
}

int ioMousePoint(void)
{
	int x = mouse.x();
	int y = mouse.y();
	return (x << 16) | y;
}

int ioPeekKeystroke(void)
{
	return keyboard.peek();
}

void ioProcessEvents(void)
{
	mouse.poll();
	keyboard.poll();
}

void ioPutChar(int ch)
{
	M5.Lcd.printf("%c",ch);
}

int ioRelinquishProcessorForMicroseconds(int microSeconds)
{
  /* noop */
  return microSeconds;
}

int ioScreenSize(void)
{
	return (ScreenWidth << 16) | (ScreenHeight & 0xFFFF);  /* w is high 16 bits; h is low 16 bits */
}

void ioSetCursor(int cursorBitsIndex, int offsetX, int offsetY)
{
  /* noop */
}

void ioSetCursorWithMask(int cursorBitsIndex, int cursorMaskIndex, int offsetX, int offsetY)
{
  /* noop */
}

/* displaying */
unsigned int	 stColors[256];

void SetColorEntry(int index, int red, int green, int blue)
{
  if (index >= 256)
    return;

  stColors[index] = ((((unsigned int)red  >>(16-3))<<5) |
		     (((unsigned int)green>>(16-3))<<2) |
		     ((unsigned int)blue >>(16-2)));
}

void SetUpPixmap(void)
{
  int r, g, b;

  SetColorEntry( 0, 65535, 65535, 65535);	/* white or transparent */
  SetColorEntry( 1,     0,     0,     0);	/* black */
  SetColorEntry( 2, 65535, 65535, 65535);	/* opaque white */
  SetColorEntry( 3, 32768, 32768, 32768);	/* 1/2 gray */
  SetColorEntry( 4, 65535,     0,     0);	/* red */
  SetColorEntry( 5,     0, 65535,     0);	/* green */
  SetColorEntry( 6,     0,     0, 65535);	/* blue */
  SetColorEntry( 7,     0, 65535, 65535);	/* cyan */
  SetColorEntry( 8, 65535, 65535,     0);	/* yellow */
  SetColorEntry( 9, 65535,     0, 65535);	/* magenta */
  SetColorEntry(10,  8192,  8192,  8192);	/* 1/8 gray */
  SetColorEntry(11, 16384, 16384, 16384);	/* 2/8 gray */
  SetColorEntry(12, 24576, 24576, 24576);	/* 3/8 gray */
  SetColorEntry(13, 40959, 40959, 40959);	/* 5/8 gray */
  SetColorEntry(14, 49151, 49151, 49151);	/* 6/8 gray */
  SetColorEntry(15, 57343, 57343, 57343);	/* 7/8 gray */
  SetColorEntry(16,  2048,  2048,  2048);	/*  1/32 gray */
  SetColorEntry(17,  4096,  4096,  4096);	/*  2/32 gray */
  SetColorEntry(18,  6144,  6144,  6144);	/*  3/32 gray */
  SetColorEntry(19, 10240, 10240, 10240);	/*  5/32 gray */
  SetColorEntry(20, 12288, 12288, 12288);	/*  6/32 gray */
  SetColorEntry(21, 14336, 14336, 14336);	/*  7/32 gray */
  SetColorEntry(22, 18432, 18432, 18432);	/*  9/32 gray */
  SetColorEntry(23, 20480, 20480, 20480);	/* 10/32 gray */
  SetColorEntry(24, 22528, 22528, 22528);	/* 11/32 gray */
  SetColorEntry(25, 26624, 26624, 26624);	/* 13/32 gray */
  SetColorEntry(26, 28672, 28672, 28672);	/* 14/32 gray */
  SetColorEntry(27, 30720, 30720, 30720);	/* 15/32 gray */
  SetColorEntry(28, 34815, 34815, 34815);	/* 17/32 gray */
  SetColorEntry(29, 36863, 36863, 36863);	/* 18/32 gray */
  SetColorEntry(30, 38911, 38911, 38911);	/* 19/32 gray */
  SetColorEntry(31, 43007, 43007, 43007);	/* 21/32 gray */
  SetColorEntry(32, 45055, 45055, 45055);	/* 22/32 gray */
  SetColorEntry(33, 47103, 47103, 47103);	/* 23/32 gray */
  SetColorEntry(34, 51199, 51199, 51199);	/* 25/32 gray */
  SetColorEntry(35, 53247, 53247, 53247);	/* 26/32 gray */
  SetColorEntry(36, 55295, 55295, 55295);	/* 27/32 gray */
  SetColorEntry(37, 59391, 59391, 59391);	/* 29/32 gray */
  SetColorEntry(38, 61439, 61439, 61439);	/* 30/32 gray */
  SetColorEntry(39, 63487, 63487, 63487);	/* 31/32 gray */
  for (r= 0; r < 6; r++) {
    for (g= 0; g < 6; g++) {
      for (b= 0; b < 6; b++) {
	int i= 40 + ((36 * r) + (6 * b) + g);
	SetColorEntry(i, (r * 65535) / 5, (g * 65535) / 5, (b * 65535) / 5);
      }
    }
  }
}

#define r16(col) ((col >> 10) & 0x1f)
#define g16(col) ((col >> 5)  & 0x1f)
#define b16(col) (col & 0x1f)

int map16To16(int c)
{
  int r = r16(c);
  int g = g16(c);
  int b = b16(c);
  return (r << 11)|(g << 6)| b;
}

#define bytesPerLine(width, depth)	((((width)*(depth) + 31) >> 5) << 2)
#define bytesPerLineRD(width, depth)	((((width)*(depth)) >> 5) << 2)

#define BUFF_SIZE 64
#define SCANLINE_SIZE 2048

static uint16_t scanline[SCANLINE_SIZE];
static uint8_t dummyline[SCANLINE_SIZE];

void PushScanline(int size)
{
	uint16_t nb = size / BUFF_SIZE;
	uint16_t *ptr = scanline;
	for (int i = 0; i < nb; i++) {
		M5.Lcd.pushColors(ptr, BUFF_SIZE);
		ptr += BUFF_SIZE;
	}
	uint16_t np = size % BUFF_SIZE;
	if (np) {
		M5.Lcd.pushColors(ptr, np);
	}
}

void ioShowDisplay(int dispBitsIndex, int width, int height, int depth, int affectedL, int affectedR, int affectedT, int affectedB)
{
	int line;

	if (affectedR <= affectedL || affectedT >= affectedB) {
		return;
	}
	M5.Lcd.setWindow(affectedL, affectedT, affectedR-1, affectedB-1);
	if (depth == 1) {
		int scanLine1 = ((width + 31) / 32) * 4;
		int top = scanLine1 * affectedT;
		for (line = affectedT; line < affectedB; line++) {
			register int index = affectedL / 8;
			register uint8_t mask = 0x80 >> (affectedL % 8);
			register uint8_t *src = (uint8_t *)(dispBitsIndex+top);
			register uint8_t *dst = dummyline;
			int i;
			for (i = 0; i < scanLine1; i ++) {
				dst[0] = src[3];
				dst[1] = src[2];
				dst[2] = src[1];
				dst[3] = src[0];
				src += 4;
				dst += 4;
			}
			dst = &dummyline[index];
			uint16_t *ptr = scanline;
			for (i = 0; i < affectedR - affectedL; i ++) {
				*ptr++ = (*dst & mask) ? 0xFFFF : 0;
				mask >>= 1;
				if (mask == 0) {
					mask = 0x80;
					dst ++;
				}
			}
			PushScanline(affectedR - affectedL);
			top += scanLine1;
		}
	} else if (depth == 8) {
		int scanLine8 = bytesPerLine(width, 8);
		int firstWord8 = scanLine8 * affectedT + bytesPerLineRD(affectedL, 8);
		int lastWord8 = scanLine8 * affectedT + bytesPerLine(affectedR, 8);
		for (line = affectedT; line < affectedB; line ++) {
			register unsigned char *from = (unsigned char *)(dispBitsIndex + firstWord8);
			register unsigned char *limit = (unsigned char *)(dispBitsIndex + lastWord8);
			register int count = 0;
			while (from < limit) {
				scanline[count++] = stColors[from[3]];
				scanline[count++] = stColors[from[2]];
				scanline[count++] = stColors[from[1]];
				scanline[count++] = stColors[from[0]];
				from += 4;
			}
			PushScanline(affectedR - affectedL);
			firstWord8 += scanLine8;
			lastWord8 += scanLine8;
		}
	} else if (depth == 16) {
		int scanLine16 = bytesPerLine(width, 16);
		int firstWord16 = scanLine16*affectedT + bytesPerLineRD(affectedL, 16);
		int lastWord16 = scanLine16*affectedT + bytesPerLine(affectedR, 16);
		for (line = affectedT; line < affectedB; line++) {
			register unsigned short *from= (unsigned short *)(dispBitsIndex + firstWord16);
			register unsigned short *limit= (unsigned short *)(dispBitsIndex + lastWord16);
			register int count = 0;
			while (from < limit) {
				scanline[count++] = map16To16(from[1]);
				scanline[count++] = map16To16(from[0]);
				from += 2;
			}
			PushScanline(affectedR - affectedL);
			firstWord16 += scanLine16;
			lastWord16 += scanLine16;
		}
	} else {
		M5.Lcd.fillRect(affectedL, affectedT, affectedR - affectedL, affectedB - affectedT, *(unsigned char *) dispBitsIndex);
	}
}

/*** Timing Primitives ***/

int startUpTime;

void SetUpTimers(void)
{
	startUpTime = millis();
}

int ioMicroMSecs(void) {
	return millis() - startUpTime;
}

int ioMSecs(void) {
	return ioMicroMSecs();
}

int ioLowResMSecs(void) {
	return ioMicroMSecs();
}

int ioSeconds(void) {
	return ioMicroMSecs() / 1000;
}

/*** Image File Naming ***/

int imageNameSize(void)
{
	return strlen(imageName);
}

int imageNameGetLength(int sqImageNameIndex, int length)
{
	char *sqImageName = (char *) sqImageNameIndex;
	int count, i;

	count = strlen(imageName);
	count = (length < count) ? length : count;

	/* copy the file name into the Squeak string */
	for (i = 0; i < count; i++) {
		sqImageName[i] = imageName[i];
	}
	return count;
}

int imageNamePutLength(int sqImageNameIndex, int length)
{
	return length;
}

/*** System Attributes ***/

char * GetAttributeString(int id) {
	/* This is a hook for getting various status strings back from
	   the OS. In particular, it allows Squeak to be passed arguments
	   such as the name of a file to be processed. Command line options
	   are reported this way as well, on platforms that support them.
	*/

	// id #0 should return the full name of VM; for now it just returns its path
	if (id == 0) return (char*) vmPath;
	// id #1 should return imageName, but returns empty string in this release to
	// ease the transition (1.3x images otherwise try to read image as a document)
	if (id == 1) return (char*) "";  /* will be imageName */
	if (id == 2) return (char*) "";

	if (id == 1001) return (char*) "M5Stack";
	if (id == 1002) return (char*) "(none)";
	if (id == 1003) return (char*) "ESP32";

	/* attribute undefined by this platform */
	success(false);
	return (char*) "";
}

int attributeSize(int id) {
	return strlen(GetAttributeString(id));
}

int getAttributeIntoLength(int id, int byteArrayIndex, int length) {
	const char *srcPtr, *end;
	char *dstPtr;
	int charsToMove;

	srcPtr = GetAttributeString(id);
	charsToMove = strlen(srcPtr);
	if (charsToMove > length) {
		charsToMove = length;
	}

	dstPtr = (char *) byteArrayIndex;
	end = srcPtr + charsToMove;
	while (srcPtr < end) {
		*dstPtr++ = *srcPtr++;
	}
	return charsToMove;
}

/*** Clipboard Support ***/

int clipboardReadIntoAt(int count, int byteArrayIndex, int startIndex) {
	return 0;
}

int clipboardSize(void) {
	return 0;
}

int clipboardWriteFromAt(int count, int byteArrayIndex, int startIndex) {
	return 0;
}

/*** Misc Primitives ***/

void ioBeep(void) {
	/* do nothing on Linux */
}

int ioExit(void) {
	M5.Lcd.printf("ioExit");
	while(1)
		;
}

void ioError(const char *str) {
	M5.Lcd.printf("error: %s\n", str);
	ioExit();
}

void ioForceDisplayUpdate(void) {
	/* do nothing on Linux */
}

void ioFormPrint(int bitsAddr, int width, int height, int depth, double hScale, double vScale, int landscapeFlag) {
	/* experimental: print a form with the given bitmap, width, height, and depth at
	   the given horizontal and vertical scales in the given orientation */

	success(false);  /* stubbed out */
}

void InitM5Stack(void)
{
	delay(500);
	// Initialize the M5Stack object
	M5.begin();
	// Screen Size
	ScreenWidth = M5.Lcd.width();
	ScreenHeight = M5.Lcd.height();
	// LCD display
	M5.Lcd.println("Starting MicroSqueak System...");
	// Initialization
	SetUpTimers();
	SetUpPixmap();
	// Initialize Peripherals
	Wire.begin(21, 22, 400000);
	mouse.setup(ScreenWidth, ScreenHeight, 125, 116);
	keyboard.setup();
	//Serial.begin(115200);
}

/*** Main ***/

void setup(){
	int f;
	int availableMemory;

	/* check the interpreter's size assumptions for basic data types */
	if (sizeof(int) != 4) {
		ioError("This C compiler's integers are not 32 bits.");
	}
	if (sizeof(double) != 8) {
		ioError("This C compiler's floats are not 64 bits.");
	}
	if (sizeof(time_t) != 4) {
		ioError("This C compiler's time_t's are not 32 bits.");
	}
	InitM5Stack();
	sqFileInit();
	/* compute the desired memory allocation */
	availableMemory = heap_caps_get_largest_free_block(MALLOC_CAP_8BIT);
	M5.Lcd.printf("Available Memory=%d\n", availableMemory);
	/* read the image file and allocate memory for Squeak heap */
	f = sqImageFileOpen(imageName, (char*) "rb");
	readImageFromFileHeapSize((int)f, availableMemory);
	sqImageFileClose(f);
	ioSetFullScreen(fullScreenFlag);
	interpret();
}

void loop() {
}
