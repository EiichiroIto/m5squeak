#define UART Serial
#define I2C Wire
#define I2C_CARDKB_ADDR 0x5F

extern void keyboard_setup();
extern int keyboard_peek();
extern int keyboard_read();
extern int keyboard_modifiers();
