# sz81_2_1_8
sz81 is an excellent zx81 emulator, which supports multiple OS. Currently it is at version 2.3.12. The latest version can be found [here](https://github.com/SegHaxx/sz81)

This repository is based on an obsolete version of the sz81 ZX81 emulator (version 2.1.8). Version 2.1.8 was the first version to support Hires graphics, and the last version (excluding 2.1.9 which did not support hires) before sz81 was moved to use the Z80 core and pixel generation from [EightyOne](https://github.com/charlierobson/EightyOne)

The EightyOne core has delivered excellent Hires graphics support into sz81. However, this comes at the cost of higher CPU load. This is not an issue for most systems, the lastest sz81 will run perfectly on a Raspberry Pi.

The core code in sz81 version 2.3.x is too CPU intensive to run at 100% emulation speed on a very low specification MCU, such as the ARM M0+ based Raspberry Pi Pico.

The core in version 2.1.8 will run at 100% speed on an overclocked Pi Pico. This repository relects modifications and improvements to the code base, which were triggered by the generation of [picozx81](https://github.com/ikjordan/picozx81)

# Configuration changes from original 2.1.8
## Extra options in menus
### M1NOT
Enables execution of code in 32kB to 48kB window

### WRX
Enables high resolution graphics. (WRX always enabled if RAM <= 2kB)

### UDG
Enables QS graphics

## Extra command line options
### -n  Emulate NTSC ZX81
For programs originally written for a 60Hz TV display
### -b  full display (414 pixels by 313)
Displays any pixels generated by ZX81, useful for debugging display routine
### -p  576 line display (360 pixels by 288)
Emulates the display area of a typical 1980's UK TV
### -c  Centre screen in display window
Centres display on screen, for both PAL and NTSC. When used with programs that extend the display (e.g. `maxtext.p`) will result in some of the display being lost. Does not apply to either 576 line or full display 
### -r  Enable CHR128 support
For programs such as zdragon
### -l  Enable RAM in 8kB to 16kB"
Needed for some WRX programs.

**Note:** RAM in 8kB to 16kB automatically enabled if either UDG or CHR128 enabled
### -vTOL 
e.g. `-r100` for 100 line vertical sync tolerance.

Determines how fast the emulated TV will find vertical lock. The smaller the number, the longer to achieve lock


## To run on Pi
sz81 uses SDL1.2. The latest Raspberry Pi OS (bookworm) default to Wayland for faster Pis. When SDL1.2 is used with wayland the resize flag is not respected. This means that the sz81 window can be resized by the user, even though the underlying code does not support this. The following can prevent the resize:

1. Install sdl12-compat  
`sudo apt install libsdl1.2-compat`

2. Use sdl12-compat  
Modify the symlink `libSDL.so` at:
`/lib/aarch64-linux-gnu` to point to the `.so` at `/lib/aarch64-linux-gnu/sdl12-compat`

3. Configure SDL to use wayland and run sz81  
`export SDL_VIDEODRIVER=wayland;./sz81`

## To build on Risc OS
The script `mkrisc.sh` creates a directory tree under `riscroot` that can be copied to Risc OS to compile with the gcc available from PackMan. 

The SDL 1.2 libraries and includes must be downloaded from Packman and placed where they can be found from the Makefile

### Notes
1. The heap size needs to be increased to build the z80 emualator  
`*SetEval cc1$HeapMax 128`

2. The executable needs to be converted to aif format  
`*elf2aif sz81`

3. The app types for the ! files should be changed back to obey (the type is lost when saving to github)

