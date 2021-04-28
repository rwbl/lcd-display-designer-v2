/*
	lcdex.pde
	Arduino LCD Text example created by the LCD Display Designer.
	Tested with Arduino UNO, MEGA and I2C LCD Display 20x4 (LCD2004).
	Additional Libraries:
	LiquidCrystal_I2C for DFRobot I2C LCD displays (see https://github.com/marcoschwartz/LiquidCrystal_I2C)
	Wiring:
	Arduino = LCD I2C: GND = GND,5v = VCC,SDA = SDA,SCL = SCL
*/

#include <Wire.h>
#include <LiquidCrystal_I2C.h>

// Declare the lcd object
LiquidCrystal_I2C lcd(0x27, 20, 4);

// Set printByte and define custom characters
#LCDCUSTOMCHARSDEFINE#

void setup() {
	lcd.init();
	// Create custom characters
#LCDCUSTOMCHARSCREATE#
	lcd.backlight();
#LCDTEXT#
}

void loop(){

}
