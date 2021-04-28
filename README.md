# LCD-Display-Designer-v2
Design layouts for 20x4 or 16x2 LCD Displays (with HD44780 controller) connected to Arduino, Raspberry Pi or other.

**Main Screen**

![lcddd-01](https://user-images.githubusercontent.com/47274144/116393860-73cb7480-a822-11eb-958f-e413a3b27f28.png)

**Code Viewer with CCP generated code from the LCD layout**

![lcddd-02](https://user-images.githubusercontent.com/47274144/116362727-a9606580-a802-11eb-9bf3-e4b679505d8e.png)

## Functionality
* Design LCD display layouts for 20x4 (20 columns, 4 rows) or 16x2 displays (16 columns, 2 rows).
* Preview the LCD display pixels.
* Insert characters from the HD44780 character table list - defined in an external file with 255 characters AscII code & pixel pattern.
* Save / open the display layout to / from a textfile located in the application folder.
* Generate simple code examples for B4R, CPP or Python Tinkerforge.
**Developed for personal use only under the GNU GENERAL PUBLIC LICENSE Version 3.**

## Development
* Application developed with [Lazarus](https://www.lazarus-ide.org/) v2.0.12, [Free Pascal Compiler](https://www.freepascal.org/) 3.2.0.
* Source included and well documented.
* Application executable not included. Use Lazarus to build.
* Developed and tested under both Windows 10 and Ubuntu 20.04.

## Code Examples & Library Base
The code examples use templates and external library references.
The templates are text files and can be changed.
CPP code for the [Arduino](https://www.arduino.cc/) - Arduino is an open-source electronics platform.
Library: LiquidCrystal_I2C.
The template to generate the code is lcdddex.pde.

B4R (with the B4X language) for [B4R](https://www.b4x.com/b4r.html) - Development tool for native Arduino and ESP programs by [Anywhere Software](https://www.b4x.com).
B4R library: rLiquidCrystalI2CEx. 
The template to generate the code is lcdddex.b4r.

Python for [Tinkerforge](https://www.tinkerforge.com/en) - System of building blocks.
See LCD 20x4 Bricklet Python API [Documentation](https://www.tinkerforge.com/en/doc/Software/Bricklets/LCD20x4_Bricklet_Python.html#lcd-20x4-bricklet-python-api).
The template to generate the code is lcdddex.py.

## Archive Folders & Files
* Displays: Some example LCD display definition files.
* Source: Lazarus project source..
* Libraries: rLiquidCrystal_I2C.zip, liquidcrystali2c.b4r

## Install
Unpack the ZIP archive to a folder of choice.
Load the Lazarus project file **lcddd.lpi** in the Lazarus IDE and build.

## LCD Table Characters
The LCD table characters are loaded from the LCD table character definition file **tablechars.def** (text format).
There are 255 characters defined: Index,Name,Pattern.
* Index - AscII code index. 
* Name -Character name.
* Pattern - 8 integers holding the 5 pixels for the 5x8 character definition.

Example Character A with Index 65:
```
65:A:14,17,17,17,31,17,17,0
```
The characters with name NN and pattern 0,0,0,0,0,0,0,0 are not loaded BUT needs to remain in the definition file.
The definition file can be changed with a text editor.
If making changes while the application is running, restart the application to load the new defintions.
Checkout the definition file and make changes as required.

The characters with index 0-7 are custom characters. Max 8 custom characters can be defined.

Example Battery with Index 0:
```
0:Battery:14,27,17,17,17,17,17,31
```

There are some custom characters defined but can be changed as required.
Hint: The Lazarus application [lcd-custom-char-maker-v2](https://github.com/rwbl/lcd-custom-char-maker-v2) is a handy tool to create & save custom characters.

The characters with index > 127 are extended table characters. Not all are loaded (named NN).

## LCD Layout Definitions
The designed LCD layouts can be saved to & loaded from a file with JSON structure.
lcdtype - index 0 - 4
rows - number of rows, i.e. 4 for a LCD2004
cols - number of cols, i.e. 20 for a LCD2004
customchars - max index for a custom chars, i.e. 7 (for 8 custom chars)
NN - Character definition:
NN - Index Row Col, i.e. row 1 col 19 is 119, row 4 col 20 is 420
row - LCD row 
col - LCD column
name - name as defined from the LCD table character definition file including custom character definition
index - AscII index (also the row index of the LCD table character definition file)
pattern - 8 integers definition the 5 pixels for each row.
"11":{"row":1,"col":1,"name":"H","index":72,"pattern":"17,17,17,31,17,17,17,0"},

Example:
```
{
"lcdtype":1,
"rows":4,
"cols":20,
"customchars":7,
"0":{"index":0,"name":"Battery","pattern":"14,27,17,17,17,17,17,31"},
"1":{"index":1,"name":"BatteryHalf","pattern":"14,27,17,17,31,31,31,31"},
"2":{"index":2,"name":"BatteryFull","pattern":"14,31,31,31,31,31,31,31"},
"3":{"index":3,"name":"ArrowDown","pattern":"0,4,4,4,31,14,4,0"},
"4":{"index":4,"name":"ArrowUp","pattern":"0,4,14,31,4,4,4,0"},
"5":{"index":5,"name":"Temperature","pattern":"4,10,10,14,14,31,31,14"},
"6":{"index":6,"name":"Section","pattern":"6,9,12,10,10,6,18,12"},
"7":{"index":7,"name":"Drop","pattern":"4,4,10,10,17,17,17,14"},
"11":{"row":1,"col":1,"name":"H","index":72,"pattern":"17,17,17,31,17,17,17,0"},
"12":{"row":1,"col":2,"name":"e","index":101,"pattern":"0,0,14,17,31,16,14,0"},
"13":{"row":1,"col":3,"name":"l","index":108,"pattern":"12,4,4,4,4,4,14,0"},
"14":{"row":1,"col":4,"name":"l","index":108,"pattern":"12,4,4,4,4,4,14,0"},
"15":{"row":1,"col":5,"name":"o","index":111,"pattern":"0,0,14,17,17,17,14,0"},
"16":{"row":1,"col":6,"name":"","index":-1,"pattern":"0,0,0,0,0,0,0,0"},
"17":{"row":1,"col":7,"name":"W","index":87,"pattern":"17,17,17,21,21,21,10,0"},
"18":{"row":1,"col":8,"name":"o","index":111,"pattern":"0,0,14,17,17,17,14,0"},
"19":{"row":1,"col":9,"name":"r","index":114,"pattern":"0,0,22,25,16,16,16,0"},
"110":{"row":1,"col":10,"name":"l","index":108,"pattern":"12,4,4,4,4,4,14,0"},
"111":{"row":1,"col":11,"name":"d","index":100,"pattern":"1,1,13,19,17,17,15,0"},
...
"420":{"row":4,"col":20,"name":"r","index":114,"pattern":"0,0,22,25,16,16,16,0"}
}
```

## Licence
This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by 
the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS for A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
You should have received a copy of the GNU General Public License along with the application.
If not, see [GNU Licenses](http://www.gnu.org/licenses/).
