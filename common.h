/* z81/xz81, Linux console and X ZX81/ZX80 emulators.
 * Copyright (C) 1994 Ian Collier. z81 changes (C) 1995-2001 Russell Marks.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *
 * common.h - prototypes etc. for common.c.
 */

/*
   Display constants
 */
#include <stdbool.h>
#include <stdint.h>

// Sizes for normal display
#define DISPLAY_N_WIDTH       320
#define DISPLAY_N_HEIGHT      240
#define DISPLAY_N_START_X     46
#define DISPLAY_N_START_Y     24
#define DISPLAY_N_PIXEL_OFF   4

// Size for 576 line display (as for PAL TV)
#define DISPLAY_P_WIDTH       360
#define DISPLAY_P_HEIGHT      288
#define DISPLAY_P_START_X     24
#define DISPLAY_P_START_Y     0
#define DISPLAY_P_PIXEL_OFF   4

// Sizes for full (debug) display
#define DISPLAY_F_WIDTH       416
#define DISPLAY_F_HEIGHT      314
#define DISPLAY_F_START_X     0
#define DISPLAY_F_START_Y     0
#define DISPLAY_F_PIXEL_OFF   6

// Size of Padding between lines (in Bytes)
#define DISPLAY_PADDING       1

// ZX80 ROM OFFSET
#define DISPLAY_ZX80_OFF      6

typedef struct
{
    uint16_t width;         // width of screen in pixels
    uint16_t height;        // Height of screen in pixels
    uint16_t length;        // Size of buffer, including padding
    uint16_t stride_bit;    // bits in one line including padding
    uint16_t stride_byte;   // bytes in one line including padding
    uint16_t start_x;       // X Offset in bits to first pixel to display, without centring
    uint16_t end_x;         // X Offset in bits to last pixel to display, without centring
    uint16_t start_y;       // Y Offset in lines to first line to display
    uint16_t end_y;         // Y Offset in lines to last line to display
    int16_t  adjust_x;      // Pixels to skip before start to display line
    int16_t  offset;        // Offset in bits to convert from raster to display coords
    uint16_t padding;       // Padding per line in bytes
} Display_T;

extern Display_T disp;

#ifdef LOAD_AND_SAVE

/* ROM Patching */
#define LOAD_START_4K       0x207       // POP DE           D1
#define SAVE_START_4K       0x1b7       // POP DE           D1
#define LOAD_SAVE_RET_4K    0x203       // POP HL           E1
#define LOAD_SAVE_RSTRT_4K  0x283

#define LOAD_START_8K       0x347       // RRC D            CB 10
#define SAVE_START_8K       0x2ff       // LD DE,$12CB      11
#define LOAD_SAVE_RET_8K    0x20A       // LD HL,$403B      21
#define LOAD_SAVE_RSTRT_8K  0x207

typedef struct
{
    uint16_t start;
    bool use_rom;
} RomPatch_T;

typedef struct
{
    RomPatch_T load;
    RomPatch_T save;
    uint16_t   retAddr;
    uint16_t   rstrtAddr;
} RomPatches_T;

extern RomPatches_T rom_patches;
#endif

/* AY board types */
#define AY_TYPE_NONE        0
#define AY_TYPE_QUICKSILVA  1
#define AY_TYPE_ZONX        2

#define HIRESDISABLED   0
#define HIRESWRX        1

#define CHRGENSINCLAIR  0
#define CHRGENQS        2
#define CHRGENCHR16     3

extern unsigned char mem[];
extern unsigned char *memptr[64];
extern int memattr[64];
extern unsigned char keyports[9];

extern unsigned long tstates,tsmax;
extern int help,sound,sound_vsync,sound_ay,sound_ay_type,vsync_visuals;
extern int invert_screen;

extern int interrupted;
extern int taguladisp;
extern int autoload;
extern int scrn_freq;
extern int fakedispx,fakedispy;

extern int refresh_screen;
extern int zx80;
extern int rom4k;
extern int ignore_esc;

/* Variables set from SDL menus */
extern bool m1not;
extern bool useWRX;
extern bool useQSUDG;
extern bool chr128;
extern bool UDGEnabled;
extern bool LowRAM;
extern bool useNTSC;

/* Variable set from command line options*/
extern bool centreScreen;
extern bool fullDisplay;
extern bool fiveSevenSix;
extern bool romLoad;
extern bool romSave;

extern int  vertTol;

/* Chroma variables */
extern int chromamode;
extern unsigned char bordercolour;
extern unsigned char bordercolournew;
extern unsigned char fullcolour;
extern unsigned char chroma_set;

extern void initmem();
extern void zxpopen(void);
extern void zxpclose(void);
extern unsigned int in(int h,int l);
extern unsigned int out(int h,int l,int a);
extern void do_interrupt();
extern void update_kybd();
extern void do_interrupt();
extern void frame_pause(void);
extern void common_reset(void);
extern void initdisplay(void);
