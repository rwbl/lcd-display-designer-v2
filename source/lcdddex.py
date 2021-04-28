#!/usr/bin/env python
# -*- coding: utf-8 -*-
## lcdex.py
## Tinkerforge LCD 20x4 Bricklet Text example created by the LCD Display Designer.
## Tested with Master Brick 2.1, LCD 20x4 Bricklet 1.2.
## IMPORTANT=Change #UID# to the UID of the LCD 20x4 Bricklet.
## Tabs are replaced by 4 spaces.

HOST = "localhost"
PORT = 4223
UID = "#UID#"

from tinkerforge.ip_connection import IPConnection
from tinkerforge.bricklet_lcd_20x4 import BrickletLCD20x4

if __name__ == "__main__":
    # Create IP connection
    ipcon = IPConnection()
    # Create device object
    lcd = BrickletLCD20x4(UID, ipcon)
    # Connect to brickd
    ipcon.connect(HOST, PORT)
    # Turn backlight on
    lcd.backlight_on()
    # Clear screen
    lcd.clear_display()
    # Define custom character name = [8 pixels pattern]
#LCDCUSTOMCHARSDEFINE#
    # Set the custom char at index 0 (write with "\x08") to 7 (\0xF)
#LCDCUSTOMCHARSCREATE#
    # Write text at pos row=0,col=0
    # lcd.write_line(0, 0, "Hello World")
#LCDTEXT#
    # Disconnect
    ipcon.disconnect()
    