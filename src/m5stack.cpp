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
    ioProcessEvents()   -- poll for input events on some platforms
    ioSetCursor()   -- install a 16x16 black and white hardware cursor
    ioSetCursorWithMask() -- install a masked cursor
    ioBeep()      -- make a short beep through the speaker
    ioExit()      -- exit the VM: quit the application, reboot, power down, or
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
    ioSeconds()     -- return the number of seconds since Jan 1, 1901
                -- may return 0, but then the current date and time
                -- will be wrong

*** Linking ***

  To build a Linux VM using this file, link together:

    interp.c    -- automatically generated interpreter file
    sqDirPrims.c  -- directory primitives (can be stubbed out)
    sqFilePrims.c -- file primitives (can be stubbed out)
    sqLinuxMain.c -- this file
    sqMiscPrims.c -- automatically generated primitives (optional)

  plus the appropriate standard libraries.

*/

#include <M5Core2.h>
#include "msq.h"
#include "m5keyboard.h"

/*** Variables -- Imported from Virtual Machine ***/
extern int fullScreenFlag;
extern int interruptCheckCounter;
extern int interruptKeycode;
extern int interruptPending;  /* set to true by recordKeystroke if interrupt key is pressed */
extern unsigned char *memory;
extern int savedWindowSize;   /* set from header when image file is loaded */

/*** Variables -- image and path names ***/
char imageName[] = "/m5squeak.image";
const char vmPath[] = "m5stackvm";
int ScreenWidth = 100;
int ScreenHeight = 100;

bool BtnA = false;
bool BtnB = false;
bool BtnC = false;
int MouseX = 0;
int MouseY = 0;

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
  stButtons |= BtnA ? 0x04 : 0;
  stButtons |= BtnB ? 0x02 : 0;
  stButtons |= BtnC ? 0x01 : 0;
  return (keyboard_modifiers() << 3) | stButtons;
}

int ioGetKeystroke(void)
{
  return keyboard_read();
}

int ioMousePoint(void)
{
  return (MouseX << 16) | MouseY;
}

int ioPeekKeystroke(void)
{
  return keyboard_peek();
}

void ioProcessEvents(void)
{
  M5.update();
  BtnA = M5.BtnA.isPressed();
  BtnB = M5.BtnB.isPressed();
  BtnC = M5.BtnC.isPressed();
  if (M5.Touch.ispressed()) {
    Point p = M5.Touch.getPressPoint();
    if (p.y < ScreenHeight) {
      M5.Lcd.drawLine(MouseX-5, MouseY, MouseX+5, MouseY, ILI9341_WHITE);
      M5.Lcd.drawLine(MouseX, MouseY-5, MouseX, MouseY+5, ILI9341_WHITE);
      MouseX = p.x;
      MouseY = p.y;
      M5.Lcd.drawLine(MouseX-5, MouseY, MouseX+5, MouseY, ILI9341_BLACK);
      M5.Lcd.drawLine(MouseX, MouseY-5, MouseX, MouseY+5, ILI9341_BLACK);
    }
  }
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
#define r8(col) (((col) >> 5) & 0x07)
#define g8(col) (((col) >> 2)  & 0x07)
#define b8(col) ((col) & 0x03)

unsigned int   stColors[256];

void SetUpPixmap(void)
{
  int i;

  for (i = 0; i < 256; i ++) {
    int r = r8(i);
    int g = g8(i);
    int b = b8(i);
    uint16_t c = (r << (16-3)) | (g << (11-3)) | (b << (5-2));
    stColors[i] = c;
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

#define bytesPerLine(width, depth)  ((((width)*(depth) + 31) >> 5) << 2)
#define bytesPerLineRD(width, depth)  ((((width)*(depth)) >> 5) << 2)

#define BUFF_SIZE 64
#define SCANLINE_SIZE 2048

static uint16_t scanline[SCANLINE_SIZE];
static uint8_t dummyline[SCANLINE_SIZE];

void PushScanline(int offset, int size)
{
  uint16_t nb = size / BUFF_SIZE;
  uint16_t *ptr = &scanline[offset];
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
  //M5.Lcd.setWindow(affectedL, affectedT, affectedR-1, affectedB-1);
  M5.Lcd.setAddrWindow(affectedL, affectedT, affectedR-affectedL, affectedB-affectedT);
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
        *ptr++ = (*dst & mask) ? 0 : 0xFFFF;
        mask >>= 1;
        if (mask == 0) {
          mask = 0x80;
          dst ++;
        }
      }
      PushScanline(0, affectedR - affectedL);
      top += scanLine1;
    }
  } else if (depth == 8) {
    int scanLine8 = bytesPerLine(width, 8);
    int firstWord8 = scanLine8 * affectedT + bytesPerLineRD(affectedL, 8);
    int lastWord8 = scanLine8 * affectedT + bytesPerLine(affectedR, 8);
    for (line = affectedT; line < affectedB; line ++) {
      register unsigned char *from = (unsigned char *)(dispBitsIndex + firstWord8);
      register unsigned char *limit = (unsigned char *)(dispBitsIndex + lastWord8);
      uint16_t *ptr = scanline;
      while (from < limit) {
        *ptr++ = stColors[from[3]];
        *ptr++ = stColors[from[2]];
        *ptr++ = stColors[from[1]];
        *ptr++ = stColors[from[0]];
        from += 4;
      }
      PushScanline(affectedL % 4, affectedR - affectedL);
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
      uint16_t *ptr = scanline;
      while (from < limit) {
        *ptr++ = map16To16(from[1]);
        *ptr++ = map16To16(from[0]);
        from += 2;
      }
      PushScanline(0, affectedR - affectedL);
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
  M5.begin(true, true, false);
  // Screen Size
  ScreenWidth = M5.Lcd.width();
  ScreenHeight = M5.Lcd.height();
  // LCD display
  M5.Lcd.println("Starting M5Squeak System...");
  // Initialization
  SetUpTimers();
  SetUpPixmap();
  // Initialize Peripherals
  M5.Lcd.println("Initializing Peripherals.");
  Wire.begin(32, 33, 400000U);
  Serial.begin(115200);
  keyboard_setup();
  M5.Lcd.println("Initializing done.");
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
  if (!f) {
    ioError("Can't open image file.");
  }
  readImageFromFileHeapSize((int)f, availableMemory);
  sqImageFileClose(f);
  ioSetFullScreen(fullScreenFlag);
  interpret();
}

void loop() {
}
