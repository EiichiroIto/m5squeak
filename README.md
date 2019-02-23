# m5squeak
m5squeak is a squeak implementation of m5stack microcontroller.

# Prerequisites
* M5Stack FIRE (https://docs.m5stack.com/#/en/core/fire)
* Visual Studio Code (https://code.visualstudio.com/)
* Platform IO (vscode extension)

# Options
* M5Stack_OnScreenKeyboard for Screen Keyboard (https://github.com/lovyan03/M5Stack_OnScreenKeyboard)
* miniterm.py for Serial Keyboard (pySerial)
* JoyStick for mouse emulation (https://www.switch-science.com/catalog/4050/)

## Build
* `git clone https://github.com/EiichiroIto/m5squeak`
* Run Visual Studio Code and Open m5stack folder.
* Build and Upload.

## Run
* Turn m5stack power on then automatically start.
* `miniterm.py /dev/ttyUSB0 115200`

## Customize
modify include/m5config.h.

select a _KEYBOARD.
    #define USE_SERIAL_KEYBOARD
    #undef USE_ONSCREEN_KEYBOARD

select a _MOUSE. (currently have single choice only)
    #define USE_JOYSTICK_MOUSE

# Squeak VM and Images
## MicroSqueak VM
m5squeak vm is one from MicroSqueak. (http://web.media.mit.edu/~jmaloney/microsqueak/)
original license is here. (http://web.media.mit.edu/~jmaloney/microsqueak/license.txt)

## Mini Image
m5squeak image is one from MiniSqueak2.2. (http://files.squeak.org/SmallSqueaksForPDAs/MiniSqueak2.2/)
