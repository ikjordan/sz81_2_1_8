# sz81_2_1_8
sz81 is an excellent zx81 emulator, which supports multiple OS. Currently it is at version 2.3.12. The latest version can be found [here](https://github.com/SegHaxx/sz81)

This repository is based on an obsolete version of the sz81 ZX81 emulator (version 2.1.8). Version 2.1.8 was the first version to support Hires graphics, and the last version (excluding 2.1.9 which did not support hires) before sz81 was moved to use the Z80 core and pixel generation from [EightyOne](https://github.com/charlierobson/EightyOne)

The EightyOne core has delivered excellent Hires graphics support into sz81. However, this comes at the cost of higher CPU load. This is not an issue for most systems, the latest sz81 will run perfectly on a Raspberry Pi.

The core code in sz81 version 2.3.x is too CPU intensive to run at 100% emulation speed on a very low specification MCU, such as the ARM M0+ based Raspberry Pi Pico. It is also too processor intensive to execute at full speed with sound under the RiscOS operating system running on a Raspberry Pi 2 or 3.

The core in version 2.1.8 will run at 100% speed on an overclocked Pi Pico and on early Raspberry Pis running RiscOS. This repository reflects modifications and improvements to the code base, which were triggered by the generation of [picozx81](https://github.com/ikjordan/picozx81)

# Configuration changes from original 2.1.8
## Extra options in menus
### ZX80 4K and 8K
A separate, more accurate emulator for ZX80 hardware has been ported from EightyOne. It is possible to select to emulate either the 4K or 8K ROM

### M1NOT
Enables execution of code in 32kB to 48kB window

### WRX
Enables high resolution graphics. (WRX always enabled if RAM <= 2kB)

### UDG
Either:
1. Off
2. 64 character or QS user defined graphic characters (64)
3. CHR16 user defined graphic characters (128)

**Note:** CHR16 is needed for programs such as Zedragon

### Extra Memory size Options
Low Memory (i.e. memory in the 8 to 16kB area) is selected by choosing a memory size of 24, 40 or 56 kB

**Note**: Enabling UDG automatically enables low memory

## Extra command line options
### -n  Emulate NTSC ZX81
For programs originally written for a 60Hz TV display
### -b  full display (414 pixels by 313)
Displays any pixels generated by ZX81, useful for debugging display routine
### -p  576 line display (360 pixels by 288)
Emulates the display area of a typical 1980's UK TV
### -c  Do not centre screen in display window
By default the display is byte aligned and centred in the window, for both PAL and NTSC. This can result in some of the display being cropped when used with programs that extend the display (e.g. `maxtext.p`). This option removes the alignment. Does not apply when either a 576 line or full display is used. Does not apply when Chroma emulation is enabled
### -l  Emulate real time load speeds
Loads a p file in "real time". Will display a realistic load screen. If vsync sound is selected realistic load display and sounds will be generated. Note that the ZX80 did not produce load sounds
### -s  Emulate real time save speeds
Saves a p file in "real time". Will display a realistic save screen. If vsync sound is selected realistic save sounds will be generated

### -vTOL 
e.g. `-v100` for 100 line vertical sync tolerance.

Determines how fast the emulated TV will find vertical lock. The smaller the number, the longer to achieve lock

## Extra functionality
+ The ZX81 emulation supports the loading and saving of named blocks. The syntax follows that of [picozx81](https://github.com/ikjordan/picozx81) and [ZXpand](https://github.com/charlierobson/ZXpand-Vitamins/wiki/ZXpand---Online-Manual)
+ Chroma 80 and Chroma 81 video and sound support. 56 kB RAM must be selected to enable Chroma. Vsync sound emulates the sound generated by Chroma 8x. i.e. sound is only generated when the display is not synchronised
+ "Real time" load and save effects. Select VSYNC sound to hear load and save sounds

# Build and Run
## To run on Pi
sz81 uses SDL1.2. The latest Raspberry Pi OS (bookworm) defaults to Wayland for Pi4 and Pi5. When SDL1.2 is used with wayland the resize flag is not respected. This means that the sz81 window can be resized by the user, even though the underlying code does not support this. The following can prevent the resize:

1. Install sdl12-compat  
`sudo apt install libsdl1.2-compat`

2. Use sdl12-compat  
Modify the symlink `libSDL.so` at:
`/lib/aarch64-linux-gnu` to point to the `.so` at `/lib/aarch64-linux-gnu/sdl12-compat`

3. Configure SDL to use wayland and run sz81  
`export SDL_VIDEODRIVER=wayland;./sz81`

## To build on Risc OS
The script `mkrisc.sh` creates a directory tree under `riscroot` that can be copied to Risc OS to compile with the gcc available from !PackMan

The SDL 1.2 libraries and includes must be downloaded from !Packman and placed where they can be found from the Makefile, i.e. the `SDL` directory` should have the same parent as the `c`directory

### Notes
1. The compiler heap size needs to be increased to build the z80 emulator  
`*SetEval cc1$HeapMax 128`
2. Enter `gcc` in the comand window to enable detection of `Make`
3. The executable needs to be converted to aif format
`*elf2aif sz81`
4. The app types for the ! files should be changed back to obey (the type is lost when saving to github)
5. To create the packaged executable, create a new directory and copy `sz81`, `!Boot`, `!Run`, `!Sprites`, `data` and `games-etc` to that directory. Then rename the directory to `!sz81`
6. dot extensions result in files with slash. ie. saving `prog.p` results in a file named `prog/p`

## Pre-build RiscOS executable
An executable, zipped using RiscOS `!Infozip` and named `sz81c.zip`, can be found in the `RiscOS` directory
