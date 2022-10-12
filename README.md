# m5squeak
m5squeak is a squeak implementation of m5stack microcontroller.

![Screen](https://raw.githubusercontent.com/EiichiroIto/m5squeak/master/images/m5squeak.jpg)

![Video](https://www.youtube.com/watch?v=xtqu4CF1MqY)

# Prerequisites
* M5Stack CORE2 (https://shop.m5stack.com/products/m5stack-core2-esp32-iot-development-kit)
* Micro SD Card (I recommend 16GB card)
* Serial Communication Terminal Application to input (I recommend miniterm)

# Options
* M5Stack Official CardKB Mini Keyboard.

# Run
1. Write m5squeak.image in the images folder to the root of the micro SD card.
2. Insert the micro SD card into the CORE2.
3. Write m5squeak.bin in the firmware folder to CORE2.
4. Restart CORE2.

# Usage
## Keyboard Input
* Use Serial Communication Terminal on your PC.
* Or Use M5Stack Official CardKB Mini Keyboard.

## Mouse Input
* Touch screen to specify the position.
* Touch left red circle for red button (primary).
* Touch center red circle for yellow button (secondary).
* Touch right red circle for blue button (meta).

# Build
* `git clone https://github.com/EiichiroIto/m5squeak`
* Run Visual Studio Code and Open m5squeak folder.
* Build and Upload.

# Squeak VM and Image
## VM
m5squeak vm is from MicroSqueak. (http://web.media.mit.edu/~jmaloney/microsqueak/)
original license is here. (http://web.media.mit.edu/~jmaloney/microsqueak/license.txt)

## Image
m5squeak.image is from MiniSqueak2.2. (http://files.squeak.org/SmallSqueaksForPDAs/MiniSqueak2.2/)


