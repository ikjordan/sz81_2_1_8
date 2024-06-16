/* Emulation of the Z80 CPU with hooks into the other parts of z81.
 * Copyright (C) 1994 Ian Collier.
 * z81 changes (C) 1995-2001 Russell Marks.
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
 */


#include <string.h> /* for memset/memcpy */
#include <stdbool.h>
#include <stdio.h>
#include "common.h"
#include "sound.h"
#include "sdl_sound.h"
#include "z80.h"
#include "sdl.h"
#include "loadp.h"

#define parity(a) (partable[a])

unsigned char partable[256] = {
    4, 0, 0, 4, 0, 4, 4, 0, 0, 4, 4, 0, 4, 0, 0, 4,
    0, 4, 4, 0, 4, 0, 0, 4, 4, 0, 0, 4, 0, 4, 4, 0,
    0, 4, 4, 0, 4, 0, 0, 4, 4, 0, 0, 4, 0, 4, 4, 0,
    4, 0, 0, 4, 0, 4, 4, 0, 0, 4, 4, 0, 4, 0, 0, 4,
    0, 4, 4, 0, 4, 0, 0, 4, 4, 0, 0, 4, 0, 4, 4, 0,
    4, 0, 0, 4, 0, 4, 4, 0, 0, 4, 4, 0, 4, 0, 0, 4,
    4, 0, 0, 4, 0, 4, 4, 0, 0, 4, 4, 0, 4, 0, 0, 4,
    0, 4, 4, 0, 4, 0, 0, 4, 4, 0, 0, 4, 0, 4, 4, 0,
    0, 4, 4, 0, 4, 0, 0, 4, 4, 0, 0, 4, 0, 4, 4, 0,
    4, 0, 0, 4, 0, 4, 4, 0, 0, 4, 4, 0, 4, 0, 0, 4,
    4, 0, 0, 4, 0, 4, 4, 0, 0, 4, 4, 0, 4, 0, 0, 4,
    0, 4, 4, 0, 4, 0, 0, 4, 4, 0, 0, 4, 0, 4, 4, 0,
    4, 0, 0, 4, 0, 4, 4, 0, 0, 4, 4, 0, 4, 0, 0, 4,
    0, 4, 4, 0, 4, 0, 0, 4, 4, 0, 0, 4, 0, 4, 4, 0,
    0, 4, 4, 0, 4, 0, 0, 4, 4, 0, 0, 4, 0, 4, 4, 0,
    4, 0, 0, 4, 0, 4, 4, 0, 0, 4, 4, 0, 4, 0, 0, 4
   };

unsigned long tstates = 0, tsmax = 65000, frames = 0;

/* odd place to have this, but the display does work in an odd way :-) */
static unsigned char scrnbmp_new_base[((DISPLAY_F_WIDTH >> 3) + DISPLAY_F_PADDING) * DISPLAY_F_HEIGHT]; /* written */
static unsigned char scrnbmp_base[((DISPLAY_F_WIDTH >> 3) + DISPLAY_F_PADDING) * DISPLAY_F_HEIGHT];     /* displayed */

static unsigned char *const scrnbmp_new = scrnbmp_new_base + DISPLAY_F_PADDING;
unsigned char *const scrnbmp = scrnbmp_base + DISPLAY_F_PADDING;

/* chroma */
static unsigned char scrnbmpc_new_base[((DISPLAY_F_WIDTH >> 3) + DISPLAY_F_PADDING) * DISPLAY_F_HEIGHT];/* written */
static unsigned char scrnbmpc_base[((DISPLAY_F_WIDTH >> 3) + DISPLAY_F_PADDING) * DISPLAY_F_HEIGHT];	  /* displayed */

static unsigned char *const scrnbmpc_new = scrnbmpc_new_base + DISPLAY_F_PADDING;
unsigned char *const scrnbmpc = scrnbmpc_base + DISPLAY_F_PADDING;

int vsx = 0;
int vsy = 0;
int framewait = 0;
int ay_reg = 0;
static int LastInstruction;

#define RUN_ROM 2

#define LASTINSTNONE 0
#define LASTINSTINFE 1
#define LASTINSTOUTFE 2
#define LASTINSTOUTFD 3
#define LASTINSTOUTFF 4

// Horizontal line timings
#define HLENGTH       207 // TStates in horizontal scanline

// TV Emulation
#define SCAN50  310       // Number of scanline per frame at 50Hz
#define SCAN60  262       // Number of scanline per frame at 60Hz
#define HSCAN   (2*HLENGTH)
#define HTOL    30        // Tolerance in detection of horizontal sync
#define VTOL    10        // Tolerance in detection of VSYNC

#define VMIN    170
#define HMIN    8
#define HMAX    32

#define ZX80_HTOL 405
#define ZX80_VTOLMIN 290
#define ZX80_VTOLMAX 360
#define ZX80_HMIN 10
#define ZX80_VMIN 350

#define TVH 500

const int ZX80_HSYNC_TOLLERANCE=ZX80_HTOL;
const int ZX80_VSYNC_TOLLERANCEMIN=210;
const int ZX80_VSYNC_TOLLERANCEMAX=410;
const int ZX80_HSYNC_MINLEN=ZX80_HMIN;
const int ZX80_VSYNC_MINLEN=ZX80_VMIN;
const int VSYNC_TOLERANCEMAX_QS = 410;

static const int HSYNC_TOLERANCEMIN = HSCAN - HTOL;
static const int HSYNC_TOLERANCEMAX = HSCAN + HTOL;

static const int HSYNC_MINLEN = HMIN;
static const int HSYNC_MAXLEN = HMAX;
static const int VSYNC_MINLEN = VMIN;

static int VSYNC_TOLERANCEMIN = SCAN50 - VTOL;
static int VSYNC_TOLERANCEMAX = SCAN50 + VTOL;

static const int HSYNC_START = 16;
static const int HSYNC_END = 32;
static const int HLEN = HLENGTH;
static const int MAX_JMP = 8;

static int S_RasterX = 0;
static int S_RasterY = 0;
static int RasterX = 0;
static int RasterY = 0;
static bool frameNotSync = true;
static int dest;

static int int_pending, nmi_pending, hsync_pending;
static int NMI_generator;
static int VSYNC_state, HSYNC_state, SYNC_signal;
static int psync, sync_len;
static int rowcounter = 0;
static int hsync_counter = 0;
static bool rowcounter_hold = false;

static int videoFlipFlop1Q;
static int videoFlipFlop2Q;
static int videoFlipFlop3Q;
static int videoFlipFlop3Clear;
static int prevVideoFlipFlop3Q;

static inline void checkhsync(int tolchk);
static inline void checkvsync(int tolchk);
static inline void checksync(int inc);
static void anyout(void);
static void vsync_raise(void);
static void vsync_lower(void);
static inline int z80_interrupt(void);
static inline int nmi_interrupt(void);
static void setEmulatedTV(bool fiftyHz, uint16_t vtol);
static void zx80_loop(void);
static void zx81_loop(void);
extern int printer_inout(int is_out, int val);

static void setEmulatedTV(bool fiftyHz, uint16_t vtol)
{
  // This can look confusing as we have an emulated display, and a real
  // display, both can be at either 50 or 60 Hz
  if (fiftyHz)
  {
    VSYNC_TOLERANCEMIN = SCAN50 - vtol;
    VSYNC_TOLERANCEMAX = SCAN50 + vtol;
  }
  else
  {
    VSYNC_TOLERANCEMIN = SCAN60 - vtol;
    VSYNC_TOLERANCEMAX = SCAN60 + vtol;
  }
}

static void vsync_raise(void)
{
  static int lastRaiseX = 0;

  /* save current pos - in screen coords*/
  vsx = RasterX - (disp.start_x - adjustStartX);
  vsy = RasterY - (disp.start_y - adjustStartY);

  // move to next valid pixel
  if (vsx >= disp.width)
  {
      vsx = 0;
      vsy++;
  }
  else if (vsx < 0)
  {
    vsx = 0;
  }

  if ((vsy < 0) || (vsy >= disp.height))
  {
    vsx = 0;
    vsy = 0;
  }

  if (((RasterY < VSYNC_TOLERANCEMIN) || (RasterY > VSYNC_TOLERANCEMAX)) && (RasterX != lastRaiseX))
  {
    frameNotSync = true;
    lastRaiseX = RasterX;
  }
}

/* for vsync on -> off */
static void vsync_lower(void)
{
  static int lastLowerX = 0;

  int ny = RasterY - (disp.start_y - adjustStartY);
  int nx = RasterX - (disp.start_x - adjustStartX);

  // Move to the next valid pixel
  if (nx >= disp.width)
  {
      nx = 0;
      ny++;
  }
  else if (nx < 0)
  {
    nx = 0;
  }

  if ((ny < 0) || (ny >= disp.height))
  {
    nx = 0;
    ny = 0;
  }

  if (((RasterY < VSYNC_TOLERANCEMIN) || (RasterY > VSYNC_TOLERANCEMAX)) && (RasterX != lastLowerX))
  {
    frameNotSync = true;
    lastLowerX = RasterX;
  }

  // leave if start and end are same pixel
  if ((nx == vsx) && (ny == vsy)) return;

  // Determine if there is a frame wrap
  if((ny < vsy) || ((ny == vsy) && (nx < vsx)))
  {
    // wrapping around frame, so display bottom
    uint8_t* start = scrnbmp_new+vsy*disp.stride_byte+(vsx>>3)-1;
    *start++ = (0xff >> (vsx & 0x7));
    memset(start, 0xff, disp.stride_byte*(disp.height-vsy)-(vsx>>3)-1);

    // check for case where wrap ends at bottom
    if ((nx == 0) && (ny == 0)) return;

    // Fall through to display top half
    vsx = 0;
    vsy = 0;
  }

  uint8_t* start = scrnbmp_new+vsy*disp.stride_byte+(vsx>>3)-1;
  uint8_t* end = scrnbmp_new+ny*disp.stride_byte+(nx>>3);
  *start++ = (0xff >> (vsx & 0x7));

  // end bits?
  if (nx & 0x7)
  {
    *end = (0xff << (nx & 0x7));
  }

  // Note: End equalling start is not unusual after adjusting positions to be on screen
  // espcially when displaying the loading screen
  if (end > start)
  {
    memset(start, 0xff, end-start);
  }
}

unsigned char a, f, b, c, d, e, h, l;
unsigned char r, a1, f1, b1, c1, d1, e1, h1, l1, i, iff1, iff2, im;
unsigned short pc;
unsigned short m1cycles;
unsigned short ix, iy, sp;
unsigned char radjust;
unsigned char ixoriy, new_ixoriy;
unsigned char intsample = 0;
unsigned char op;

void mainloop()
{
  intsample = 0;
  framewait = 0;

  a = f = b = c = d = e = h = l = a1 = f1 = b1 = c1 = d1 = e1 = h1 = l1 = i = iff1 = iff2 = im = r = 0;
  ixoriy = new_ixoriy = 0;
  ix = iy = sp = pc = 0;
  tstates = radjust = 0;

  RasterX = 0;
  RasterY = 0;
  dest = disp.offset + (adjustStartY * disp.stride_bit) + adjustStartX;
  psync = 1;
  sync_len = 0;

  /* ULA */

  NMI_generator = 0;
  nmi_pending = 0;
  int_pending = 0;
  rowcounter = 0;
  hsync_pending = 0;
  VSYNC_state = HSYNC_state = 0;
  frames = 0;
  hsync_counter = 0;
  setEmulatedTV(!useNTSC, vertTol);

  if (sdl_emulator.autoload)
  {
    sdl_emulator.autoload = 0;
    /* This could be an initial autoload or a later forcedload */
    if (!sdl_load_file(0, LOAD_FILE_METHOD_DETECT))
      /* wait for a real frame, to avoid an annoying frame `jump'. */
      framewait = 1;
  }

  if (zx80 && !rom4k) zx80 = 2;

  zx80 ? zx80_loop() : zx81_loop();
}

void zx81_loop(void)
{
  bool videodata;
  unsigned long ts;
  unsigned long tstore;
  unsigned char v = 0;
  unsigned char colour;

  while (1)
  {
#if 0
    /* Currently this is for development but it would be useful to
     * make it a feature temp temp
     * ZX80 load hook @ 0206:    ed fc|c3 83 02 =          LOAD|JP 0283
     * ZX81 load hook @ 0347: eb|ed fc|c3 07 02 = EX DE,HL|LOAD|JP 0207 */
    if ((zx80 && pc == 0x283) || (!zx80 && pc == 0x207))
    {
      if (!zx80)
      {
        printf("ZX81 System Variables\n");
        printf("mem[0x%04x] = 0x%02x; /* ERR_NR */\n", 0x4000, mem[0x4000]);
        printf("mem[0x%04x] = 0x%02x; /* FLAGS */\n", 0x4001, mem[0x4001]);
        printf("mem[0x%04x] = 0x%02x; /* ERR_SP lo */\n", 0x4002, mem[0x4002]);
        printf("mem[0x%04x] = 0x%02x; /* ERR_SP hi */\n", 0x4003, mem[0x4003]);
        printf("mem[0x%04x] = 0x%02x; /* RAMTOP lo */\n", 0x4004, mem[0x4004]);
        printf("mem[0x%04x] = 0x%02x; /* RAMTOP hi */\n", 0x4005, mem[0x4005]);
        printf("mem[0x%04x] = 0x%02x; /* MODE */\n", 0x4006, mem[0x4006]);
        printf("mem[0x%04x] = 0x%02x; /* PPC lo */\n", 0x4007, mem[0x4007]);
        printf("mem[0x%04x] = 0x%02x; /* PPC hi */\n", 0x4008, mem[0x4008]);
      }
      printf("Registers\n");
      printf("a = 0x%02x; f = 0x%02x; b = 0x%02x; c = 0x%02x;\n", a, f, b, c);
      printf("d = 0x%02x; e = 0x%02x; h = 0x%02x; l = 0x%02x;\n", d, e, h, l);
      printf("sp = 0x%04x; pc = 0x%04x;\n", sp, pc);
      printf("ix = 0x%04x; iy = 0x%04x; i = 0x%02x; r = 0x%02x;\n", ix, iy, i, r);
      printf("a1 = 0x%02x; f1 = 0x%02x; b1 = 0x%02x; c1 = 0x%02x;\n", a1, f1, b1, c1);
      printf("d1 = 0x%02x; e1 = 0x%02x; h1 = 0x%02x; l1 = 0x%02x;\n", d1, e1, h1, l1);
      printf("iff1 = 0x%02x; iff2 = 0x%02x; im = 0x%02x;\n", iff1, iff2, im);
      printf("radjust = 0x%02x;\n", radjust);
      printf("Machine/GOSUB Stack\n");
      printf("mem[0x%04x] = 0x%02x;\n", sp + 0, mem[sp + 0]);
      printf("mem[0x%04x] = 0x%02x;\n", sp + 1, mem[sp + 1]);
      printf("mem[0x%04x] = 0x%02x;\n", sp + 2, mem[sp + 2]);
      printf("mem[0x%04x] = 0x%02x;\n", sp + 3, mem[sp + 3]);
      printf("\n");
    }
#endif
    v = 0;
    ts = 0;
    LastInstruction = LASTINSTNONE;
    colour = (bordercolour << 4) + bordercolour;

    if (intsample && !((radjust - 1) & 64) && iff1)
      int_pending = 1;

    if (nmi_pending)
    {
      ts = nmi_interrupt();
    }
    else if (int_pending)
    {
      ts = z80_interrupt();
      hsync_counter = -2;             /* INT ACK after two tstates */
      hsync_pending = 1;              /* a HSYNC may be started */
    }
    else
    {
      // Get the next op, calculate the next byte to display and execute the op
      op = fetchm(pc);

      if (m1not && pc<0xC000)
      {
        videodata = false;
      }
      else
      {
        videodata = (pc&0x8000) ? true: false;
      }

      if(videodata && !(op&64))
      {
        v=0xff;

        if ((i<0x20) || (i<0x40 && LowRAM && (!useWRX)))
        {
          int addr;
          if (chr128 && i>0x20 && i&1)
            addr = ((i&0xfe)<<8)|((((op&128)>>1)|(op&63))<<3)|rowcounter;
          else
            addr = ((i&0xfe)<<8)|((op&63)<<3)|rowcounter;

          if (UDGEnabled && addr>=0x1E00 && addr<0x2000)
          {
            v = mem[addr + ((op&128) ? 0x6800 : 0x6600)];
          }
          else
          {
            v = mem[addr];
          }
        }
        else
        {
          int addr = (i<<8)|(r&0x80)|(radjust&0x7f);
          if (useWRX)
          {
            v = mem[addr];
          }
        }
        v = (op&128)?~v:v;

        if (chromamode)
        {
            if (chromamode & 0x10)
                colour = fetch(pc);
            else
                colour = fetch(0xc000 | ((((op & 0x80) >> 1) | (op & 0x3f)) << 3) | rowcounter);
        }
        op=0; /* the CPU sees a nop */
      }
      else
      {
        if (pc == rom_patches.load.start) // load
        {
          int run_rom = sdl_load_file(de, (de < 0x800) ? LOAD_FILE_METHOD_NAMEDLOAD : LOAD_FILE_METHOD_SELECTLOAD);
          if ((!rom_patches.load.use_rom) || (run_rom != RUN_ROM))
          {
            pc = rom_patches.load.retAddr;
            op = fetchm(pc);
          }
        }
        else if (pc == rom_patches.save.start) // save
        {
          int run_rom = sdl_save_file(hl, SAVE_FILE_METHOD_NAMEDSAVE);
          if ((!rom_patches.save.use_rom) || (run_rom != RUN_ROM))
          {
            pc = rom_patches.save.retAddr;
            op = fetchm(pc);
          }
        }
      }
      tstore = tstates;

      do
      {
        pc++;
        radjust++;
        intsample = 1;

        switch (op)
        {
#include "z80ops.c"
        }
        ixoriy = 0;

        // Complete ix and iy instructions
        if (new_ixoriy)
        {
          ixoriy = new_ixoriy;
          new_ixoriy = 0;
          op = fetchm(pc);
        }
      } while (ixoriy);

      ts = tstates - tstore;
      tstates = tstore;
    }

    nmi_pending = int_pending = 0;
    tstates += ts;

    switch (LastInstruction)
    {
      case LASTINSTOUTFD:
        NMI_generator = nmi_pending = 0;
        anyout();
      break;
      case LASTINSTOUTFE:
        NMI_generator = 1;
        anyout();
      break;
      case LASTINSTINFE:
        if (!NMI_generator)
        {
          if (VSYNC_state == 0)
          {
            VSYNC_state = 1;
            vsync_raise();
          }
        }
#ifdef OSS_SOUND_SUPPORT
        if ((sdl_sound.device == DEVICE_VSYNC) && frameNotSync)
        {
            sound_beeper(1);
        }
#endif

      break;
      case LASTINSTOUTFF:
        anyout();
      break;
    }

    // Plot data in shift register
    // Note subtract 6 as this leaves the smallest positive number
    // of bits to carry to next byte (2)
    if ((v || chromamode) &&
        (RasterX >= (disp.start_x - adjustStartX - 6)) &&
        (RasterX < (disp.end_x - adjustStartX)) &&
        (RasterY >= (disp.start_y - adjustStartY)) &&
        (RasterY < (disp.end_y - adjustStartY)))
    {
      if (chromamode)
      {
        int k = (dest + RasterX) >> 3;
        scrnbmpc_new[k] = colour;
        scrnbmp_new[k] = v;
      }
      else
      {
        if (v)
        {
          int k = dest + RasterX;
          int kh = k >> 3;
          int kl = k & 7;

          if (kl)
          {
            scrnbmp_new[kh++] |= (v >> kl);
            scrnbmp_new[kh] = (v << (8 - kl));
          }
          else
          {
            scrnbmp_new[kh] = v;
          }
        }
      }
    }

    int tstate_inc;
    int states_remaining = ts;
    int since_hstart = 0;
    int tswait = 0;

    do
    {
      tstate_inc = states_remaining > MAX_JMP ? MAX_JMP : states_remaining;
      states_remaining -= tstate_inc;

      hsync_counter += tstate_inc;
      RasterX += (tstate_inc << 1);

      if (hsync_counter >= HLEN)
      {
        hsync_counter -= HLEN;
        hsync_pending = 1;
      }

      // Start of HSYNC, and NMI if enabled
      if (hsync_pending && hsync_counter >= HSYNC_START)
      {
        if (NMI_generator)
        {
          nmi_pending = 1;
          if (ts == 4)
          {
            tswait = 14 + (3 - states_remaining - (hsync_counter - HSYNC_START));
          }
          else
          {
            tswait = 14;
          }
          states_remaining += tswait;
          ts += tswait;
          tstates += tswait;
        }

        HSYNC_state = 1;
        hsync_pending = 0;

        since_hstart = hsync_counter - HSYNC_START + 1;

        if (VSYNC_state || rowcounter_hold)
        {
          rowcounter = 0;
          rowcounter_hold = false;
        }
        else
        {
          rowcounter++;
          rowcounter &= 7;
        }
      }

      // end of HSYNC
      if (HSYNC_state && hsync_counter >= HSYNC_END)
      {
        HSYNC_state = 0;
        hsync_pending = 0;
      }

      // NOR the vertical and horizontal SYNC states to create the SYNC signal
      SYNC_signal = (VSYNC_state || HSYNC_state) ? 0 : 1;
      checksync(since_hstart ? since_hstart : MAX_JMP);
      since_hstart = 0;
    }
    while (states_remaining);

    if (tstates >= tsmax)
    {
      tstates -= tsmax;

      frames++;
      frame_pause();
    }

    /* this isn't used for any sort of Z80 interrupts,
      * purely for the emulator's UI.
      */
    if (interrupted)
    {
      if (interrupted == 1)
      {
        do_interrupt(); /* also zeroes it */
      }
      /* I've added these new interrupt types to support a thorough
        * emulator reset and to do a proper exit i.e. back to main */
      else if (interrupted == INTERRUPT_EMULATOR_RESET ||
                interrupted == INTERRUPT_EMULATOR_EXIT)
      {
        return;
      }
      else /* must be 2 */
      {
        /* a kludge to let us do a reset */
        interrupted = 0;
        a = f = b = c = d = e = h = l = a1 = f1 = b1 = c1 = d1 = e1 = h1 = l1 = i = iff1 = iff2 = im = r = 0;
        ixoriy = new_ixoriy = 0;
        ix = iy = sp = pc = 0;
        tstates = radjust = 0;
        RasterX = 0;
        RasterY = 0;
        dest = disp.offset + (adjustStartY * disp.stride_bit) + adjustStartX;
        psync = 1;
        sync_len = 0;

        /* ULA */
        NMI_generator = 0;
        int_pending = 0;
        hsync_pending = 0;
        VSYNC_state = HSYNC_state = 0;
      }
    }
  }
}

#define SYNCNONE        0
#define SYNCTYPEH       1
#define SYNCTYPEV       2

const int scanlinePixelLength = (HLENGTH << 1);
const int ZX80HSyncDuration = 20;

const int ZX80HSyncDurationPixels = ZX80HSyncDuration * 2;
const int ZX80HSyncAcceptanceDuration = (3 * ZX80HSyncDuration) / 2;
const int ZX80HSyncAcceptanceDurationPixels = ZX80HSyncAcceptanceDuration * 2;
const int ZX80MaximumSupportedScanlineOverhang = ZX80HSyncDuration * 2;
const int ZX80MaximumSupportedScanlineOverhangPixels = ZX80MaximumSupportedScanlineOverhang * 2;
const int InterruptAcknowledgementDuration = 3; // The acknowledgement spans 3 clock cycles

const int PortActiveDuration = 3;
const int PortActiveDurationPixels = PortActiveDuration * 2;
const int ZX80HSyncAcceptancePixelPosition = scanlinePixelLength - ZX80HSyncAcceptanceDurationPixels;
const int scanlineThresholdPixelLength = scanlinePixelLength + ZX80MaximumSupportedScanlineOverhangPixels;

void zx80_loop(void)
{
  bool videodata;
  unsigned long ts;
  unsigned long tstore;
  bool frameSynchronised = false;
  bool vsyncFound = false;
  int scanlineCounter = 0;

  unsigned char v = 0;
  unsigned char colour;
  bool allowSoundOutput = false;
  int lineClockCarryCounter = 0;

  videoFlipFlop1Q = 1;
  videoFlipFlop2Q = 0;
  videoFlipFlop3Q = 0;
  videoFlipFlop3Clear = 0;
  prevVideoFlipFlop3Q = 0;

  int scanline_len = 0;
  int sync_type = SYNCNONE;
  int sync_len = 0;

  dest = disp.offset + (disp.stride_bit * adjustStartY) + adjustStartX;
  S_RasterX = 0;
  S_RasterY = 0;

  while (1)
  {
    // New scan line
    RasterX = S_RasterX;
    RasterY = S_RasterY;
    dest = disp.offset + (disp.stride_bit * (adjustStartY + RasterY)) + adjustStartX;

    // Handle line carry over here
    scanline_len = 0;

    if (sync_type != SYNCNONE)
    {
      sync_type = SYNCNONE;
      sync_len = 0;
    }

    if (lineClockCarryCounter > 0)
    {
      int lineClockCarryCounterPixels = lineClockCarryCounter * 2;
      //memcpy(CurScanLine->scanline, carryOverScanlineBuffer, lineClockCarryCounterPixels);
      scanline_len = lineClockCarryCounterPixels;
      RasterX += scanline_len;
      lineClockCarryCounter = 0;
    }

    do // instruction loop
    {
      v = 0;
      ts = 0;
      LastInstruction = LASTINSTNONE;
      colour = (bordercolour << 4) + bordercolour;

      // Get the next op, calculate the next byte to display and execute the op
      op = fetchm(pc);

      if (m1not && pc<0xC000)
      {
        videodata = false;
      }
      else
      {
        videodata = (pc&0x8000) ? true: false;
      }

      if(videodata && !(op&64))
      {
        v=0xff;

        if ((i<0x20) || (i<0x40 && LowRAM && (!useWRX)))
        {
          int addr;
          if (chr128 && i>0x20 && i&1)
            addr = ((i&0xfe)<<8)|((((op&128)>>1)|(op&63))<<3)|rowcounter;
          else
            addr = ((i&0xfe)<<8)|((op&63)<<3)|rowcounter;

          if (UDGEnabled && addr>=0x1E00 && addr<0x2000)
          {
            v = mem[addr + ((op&128) ? 0x6800 : 0x6600)];
          }
          else
          {
            v = mem[addr];
          }
        }
        else
        {
          int addr = (i<<8)|(r&0x80)|(radjust&0x7f);
          if (useWRX)
          {
            v = mem[addr];
          }
        }
        v = (op&128)?~v:v;

        if (chromamode)
        {
            if (chromamode & 0x10)
                colour = fetch(pc);
            else
                colour = fetch(0xc000 | ((((op & 0x80) >> 1) | (op & 0x3f)) << 3) | rowcounter);
        }
        op=0; /* the CPU sees a nop */
      }
      else
      {
        if (pc == rom_patches.load.start) // load
        {
          int run_rom;
          if(!rom4k && de < 0x8000)
          {
            run_rom = sdl_load_file(de, LOAD_FILE_METHOD_NAMEDLOAD);
          }
          else /* if((!rom4k && de >= 0x8000) || rom4k) */
          {
            run_rom = sdl_load_file(rom4k ? hl : de, LOAD_FILE_METHOD_SELECTLOAD);
          }
          if ((!rom_patches.load.use_rom) || (run_rom != RUN_ROM))
          {
            pc = rom_patches.load.retAddr;
            op = fetchm(pc);
          }
        }
        else if (pc == rom_patches.save.start) // save
        {
          int run_rom = sdl_save_file(hl, rom4k ? SAVE_FILE_METHOD_UNNAMEDSAVE : SAVE_FILE_METHOD_NAMEDSAVE);
          if ((!rom_patches.save.use_rom) || (run_rom != RUN_ROM))
          {
            pc = rom_patches.save.retAddr;
            op = fetchm(pc);
          }
        }
      }
      tstore = tstates;

      // execute the operation
      do
      {
        pc++;
        radjust++;
        intsample = 1;
        m1cycles=1;

        switch (op)
        {
#include "z80ops.c"
        }
        ixoriy = 0;

        // Complete ix and iy instructions
        if (new_ixoriy)
        {
          ixoriy = new_ixoriy;
          new_ixoriy = 0;
          op = fetchm(pc);
        }
      } while (ixoriy);

      ts = tstates - tstore;
      tstates = tstore;

      // Update the flip flop
      prevVideoFlipFlop3Q = videoFlipFlop3Q;

      for (int i = 0; i < m1cycles; i++)
      {
        if (videoFlipFlop3Clear)
        {
          videoFlipFlop3Q = videoFlipFlop2Q;
        }

        videoFlipFlop2Q = !videoFlipFlop1Q;
      }

      if (!videoFlipFlop3Q)
      {
        videoFlipFlop1Q = 0;

        if (prevVideoFlipFlop3Q)
        {
          rowcounter = (rowcounter + 1) & 7;
        }
      }

      //if (!((radjust) & 64)) int_pending = 1;
      if (intsample && !((radjust - 1) & 64) && iff1)
        int_pending = 1;

      // execute an interrupt
      if (int_pending)
      {
        int ti = z80_interrupt();

        if (ti > 0)
        {
          if (videoFlipFlop3Clear)
          {
            videoFlipFlop3Q = videoFlipFlop2Q;
          }

          videoFlipFlop2Q = !videoFlipFlop1Q;
          videoFlipFlop1Q = 1;

          ts += ti;
        }
        int_pending = 0;
      }

      // Plot data in shift register
      // Note subtract 6 as this leaves the smallest positive number
      // of bits to carry to next byte (2)
      if ((v || chromamode) &&
          (RasterX >= (disp.start_x - adjustStartX - 6)) &&
          (RasterX < (disp.end_x - adjustStartX)) &&
          (RasterY >= (disp.start_y - adjustStartY)) &&
          (RasterY < (disp.end_y - adjustStartY)))
      {
        if (chromamode)
        {
          int k = (dest + RasterX) >> 3;
          scrnbmpc_new[k] = colour;
          scrnbmp_new[k] = v;
        }
        else
        {
          if (v)
          {
            int k = dest + RasterX;
            int kh = k >> 3;
            int kl = k & 7;

            if (kl)
            {
              scrnbmp_new[kh++] |= (v >> kl);
              scrnbmp_new[kh] = (v << (8 - kl));
            }
            else
            {
              scrnbmp_new[kh] = v;
            }
          }
        }
      }

      RasterX += (ts << 1);
      scanline_len += (ts << 1);

      if (RasterX >= scanlinePixelLength)
      {
        RasterY++;
        dest += disp.stride_bit;
        RasterX -= scanlinePixelLength;
      }

      //if (!((radjust) & 64)) int_pending = 1;
      //if (!(r & 0x40)) int_pending = 1;
      tstates += ts;

      switch (LastInstruction)
      {
        case LASTINSTOUTFD:
        case LASTINSTOUTFE:
        case LASTINSTOUTFF:     // VSync end
          videoFlipFlop1Q = 0;
          videoFlipFlop2Q = 1;
          videoFlipFlop3Clear = 1;
          if (!videoFlipFlop3Q)
          {
            sync_len += ts;

            if (sync_len > ZX80HSyncAcceptanceDuration)
            {
              videoFlipFlop3Q = 1;
            }
          }
        break;

        case LASTINSTINFE:      // VSync start
          if (videoFlipFlop3Q)
          {
            sync_len = PortActiveDuration;
          }
          else
          {
            sync_len += ts;
          }

          videoFlipFlop1Q = 1;
          videoFlipFlop3Clear = 0;
          videoFlipFlop3Q = 0;
          rowcounter = 0;
        break;

        default:
          if (!videoFlipFlop3Q)
          {
            sync_len += ts;
          }
        break;
      }


      if (prevVideoFlipFlop3Q != videoFlipFlop3Q)
      {
        videoFlipFlop3Q ? vsync_lower() : vsync_raise();
        if (allowSoundOutput) sound_beeper(videoFlipFlop3Q);
      }

      if (videoFlipFlop3Q && (sync_len > 0))
      {
        // The line is now complete
        if (sync_len <= ZX80HSyncAcceptanceDuration)
        {
          sync_type = SYNCTYPEH;
        }
        else
        {
          sync_type = SYNCTYPEV;
        }

        if (sync_type == SYNCTYPEV)
        {
          int overhangPixels = scanline_len - scanlinePixelLength;

          if (overhangPixels < 0)
          {
            if (scanline_len >= ZX80HSyncAcceptancePixelPosition)
            {
              //memset(scanline + CurScanLine->scanline_len, 0xFF, -overhangPixels);

              lineClockCarryCounter = 0;
              scanline_len = scanlinePixelLength;
            }
            else
            {
              //memcpy(carryOverScanlineBuffer, scanline, CurScanLine->scanline_len);
              //memset(scanline, BLANKCOLOUR, scanlinePixelLength);

              lineClockCarryCounter = scanline_len / 2;
              scanline_len = scanlinePixelLength;

              //scanlinesPerFrame--;
            }
          }
          else if (overhangPixels > 0)
          {
            //memcpy(carryOverScanlineBuffer, CurScanLine->scanline + scanlinePixelLength, overhangPixels);

            lineClockCarryCounter = overhangPixels / 2;
            scanline_len = scanlinePixelLength;
          }
        }
        else if (scanline_len >= ZX80HSyncAcceptancePixelPosition)
        {
          int actualScanlineLength = scanline_len - ts;
          //memcpy(carryOverScanlineBuffer, CurScanLine->scanline + actualScanlineLength, instructionPixels);

          int pixelsToBlank = (scanlinePixelLength - actualScanlineLength);
          if (pixelsToBlank > 0)
          {
            //memset(scanline + actualScanlineLength, BLANKCOLOUR, pixelsToBlank);
          }

          lineClockCarryCounter = ts;
          scanline_len = scanlinePixelLength;
        }
      }
      // Check for end of frame
      if (tstates >= tsmax)
      {
        tstates -= tsmax;

        frames++;
        frame_pause();
      }

      /* this isn't used for any sort of Z80 interrupts,
        * purely for the emulator's UI.
        */
      if (interrupted)
      {
        //restart = true;

        if (interrupted == 1)
        {
          do_interrupt(); /* also zeroes it */
        }
        /* I've added these new interrupt types to support a thorough
          * emulator reset and to do a proper exit i.e. back to main */
        else if (interrupted == INTERRUPT_EMULATOR_RESET ||
                  interrupted == INTERRUPT_EMULATOR_EXIT)
        {
          return;
        }
        else /* must be 2 */
        {
          /* a kludge to let us do a reset */
          interrupted = 0;
          a = f = b = c = d = e = h = l = a1 = f1 = b1 = c1 = d1 = e1 = h1 = l1 = i = iff1 = iff2 = im = r = 0;
          ixoriy = new_ixoriy = 0;
          ix = iy = sp = pc = 0;
          tstates = radjust = 0;
          S_RasterX = 0;
          S_RasterY = 0;
          dest = disp.offset + (adjustStartY * disp.stride_bit) + adjustStartX;
          psync = 1;
          sync_len = 0;

          /* ULA */
          NMI_generator = 0;
          int_pending = 0;
          hsync_pending = 0;
          VSYNC_state = HSYNC_state = 0;

          videoFlipFlop1Q = 1;
          videoFlipFlop2Q = 0;
          videoFlipFlop3Q = 0;
          videoFlipFlop3Clear = 0;
          prevVideoFlipFlop3Q = 0;

          frameSynchronised = false;
          vsyncFound = false;
          scanlineCounter = 0;
        }
      }
    }
    while ((scanline_len < scanlineThresholdPixelLength) && (sync_type == SYNCNONE));

    // Process end of line and possibly end of frame
    if (sync_type == SYNCTYPEV)
    {
      if (vsyncFound)
      {
        frameSynchronised = (RasterY >= ZX80_VSYNC_TOLLERANCEMIN) && (RasterY <= VSYNC_TOLERANCEMAX_QS) &&
                            (scanlineCounter >= ZX80_VSYNC_TOLLERANCEMIN) && (scanlineCounter <= VSYNC_TOLERANCEMAX_QS);
        vsyncFound = frameSynchronised;
      }
      else
      {
        vsyncFound = (scanlineCounter >= ZX80_VSYNC_TOLLERANCEMIN) && (scanlineCounter <= VSYNC_TOLERANCEMAX_QS);
      }
      scanlineCounter = 0;

      //allowSoundOutput = sound_vsync && (!chromamode || (chromamode && !frameSynchronised));
      allowSoundOutput = (sound_vsync && !frameSynchronised);

      if (!vsyncFound)
      {
        sync_type = SYNCNONE;
        sync_len = 0;
      }
    }
    else
    {
      if (scanlineCounter < VSYNC_TOLERANCEMAX_QS)
      {
        if (sync_type == SYNCTYPEH)
        {
          scanlineCounter++;
        }
      }

      if (((sync_type == SYNCNONE) && videoFlipFlop3Q) || (scanlineCounter == VSYNC_TOLERANCEMAX_QS))
      {
              frameSynchronised = false;
              vsyncFound = false;
              allowSoundOutput = sound_vsync;
      }

      if (sync_type == SYNCNONE)
      {
        int overhangPixels = scanline_len - scanlinePixelLength;
        if (overhangPixels > 0)
        {
          // memcpy(carryOverScanlineBuffer, CurScanLine->scanline + scanlinePixelLength, overhangPixels);

          lineClockCarryCounter = overhangPixels / 2;
          scanline_len = scanlinePixelLength;
        }
      }
    }

    // Synchronise the TV position
    S_RasterX += scanline_len;
    if (S_RasterX >= scanlinePixelLength)
    {
      S_RasterX -= scanlinePixelLength;
      S_RasterY++;

      if (S_RasterY >= TVH)
      {
        S_RasterX = 0;
        sync_type=SYNCTYPEV;
        if (sync_len < HSYNC_MINLEN)
          sync_len=HSYNC_MINLEN;
      }
    }

    if (sync_len<HSYNC_MINLEN) sync_type=0;

    if (sync_type)
    {
      if (S_RasterX > ZX80_HSYNC_TOLLERANCE)
      {
        S_RasterX=0;
        S_RasterY++;
      }

      if (S_RasterY>=TVH || S_RasterY>=ZX80_VSYNC_TOLLERANCEMAX ||
          (sync_len>VSYNC_MINLEN && S_RasterY>ZX80_VSYNC_TOLLERANCEMIN))
      {
        // Display the frame
        memcpy(scrnbmp, scrnbmp_new, disp.length);
        if (chromamode) memcpy(scrnbmpc, scrnbmpc_new, disp.length);
        memset(scrnbmp_new, 0x00, disp.length);
        if (chromamode && (bordercolournew != bordercolour)) bordercolour = bordercolournew;
        if (chromamode) memset(scrnbmpc_new, (bordercolour << 4) + bordercolour, disp.length);

        S_RasterX = 0;
        S_RasterY = 0;
      }
    }
  }
}

void z80_reset(void)
{
  /* Reinitialise variables at the top of z80.c */
  tstates = 0;
  frames = 0;
  vsy = 0;
  ay_reg = 0;

  if (chromamode)
  {
    chromamode = 0;
    adjustChroma(false);
  }
}

static inline int z80_interrupt(void)
{
  int tinc = 0;

  if (iff1)
  {
    if (fetchm(pc) == 0x76)
    {
      pc++;
    }
    iff1 = iff2 = 0;
    push2(pc);
    radjust++;

    switch (im)
    {
    case 0: /* IM 0 */
    case 2: /* IM 1 */
      pc = 0x38;
      tinc = 13;
      break;
    case 3: /* IM 2 */
    {
      int addr = fetch2((i << 8) | 0xff);
      pc = addr;
      tinc = 19;
    }
    break;

    default:
      tinc = 12;
      break;
    }
  }
  return tinc;
}

static inline int nmi_interrupt(void)
{
  iff1 = 0;
  if (fetchm(pc) == 0x76)
  {
    pc++;
  }
  push2(pc);
  radjust++;
  m1cycles++;
  pc = 0x66;

  return 11;
}

/* Normally, these sync checks are done by the TV :-) */
static inline void checkhsync(int tolchk)
{
  if ((!tolchk && sync_len >= HSYNC_MINLEN && sync_len <= (HSYNC_MAXLEN + MAX_JMP) && RasterX >= HSYNC_TOLERANCEMIN) ||
      (tolchk && RasterX >= HSYNC_TOLERANCEMAX))
  {
    if (zx80)
      RasterX = (hsync_counter - HSYNC_END) << 1;
    else
      RasterX = ((hsync_counter - HSYNC_END) < MAX_JMP) ? ((hsync_counter - HSYNC_END) << 1) : 0;
    RasterY++;
    dest += disp.stride_bit;
  }
}

static inline void checkvsync(int tolchk)
{
  if ((!tolchk && sync_len >= VSYNC_MINLEN && RasterY >= VSYNC_TOLERANCEMIN) ||
      (tolchk && RasterY >= VSYNC_TOLERANCEMAX))
  {
    if (sync_len>(int)tsmax)
    {
      // If there has been no sync for an entire frame then blank the screen to black
      memset(scrnbmp, 0xff, disp.length);
      if (chromamode) memset(scrnbmpc, 0x0, disp.length);
      sync_len = 0;
      frameNotSync = true;
    }
    else
    {
      memcpy(scrnbmp, scrnbmp_new, disp.length);
      if (chromamode) memcpy(scrnbmpc, scrnbmpc_new, disp.length);
      frameNotSync = (RasterY >= VSYNC_TOLERANCEMAX);
    }
    memset(scrnbmp_new, 0x00, disp.length);
    if (chromamode && (bordercolournew != bordercolour)) bordercolour = bordercolournew;
    if (chromamode) memset(scrnbmpc_new, (bordercolour << 4) + bordercolour, disp.length);
    RasterY = 0;
    dest = disp.offset + (disp.stride_bit * adjustStartY) + adjustStartX;
  }
}

static void checksync(int inc)
{
  if (!SYNC_signal)
  {
    if (psync == 1)
      sync_len = 0;
    sync_len += inc;
    checkhsync(1);
    checkvsync(1);
  }
  else
  {
    if (!psync)
    {
      checkhsync(0);
      checkvsync(0);
    }
  }
  psync = SYNC_signal;
}

/* The rowcounter is a 7493; as long as both reset inputs are high, the counter is at zero
    and cannot count. Any out sets it free. */

static void anyout(void)
{
  if (VSYNC_state)
  {
    if (zx80)
      VSYNC_state = 2; // will be reset by HSYNC circuitry
    else
    {
      VSYNC_state = 0;
      // A fairly arbitrary value selected after comparing with a "real" ZX81
      if ((hsync_counter < HSYNC_START) || (hsync_counter >= 178))
      {
        rowcounter_hold = true;
      }
      vsync_lower();
    }
  }
}

unsigned int in(int h, int l)
{
  int data = 0x80;

  if (h == 0x7f && l == 0xef)
  {
    if (sdl_emulator.ramsize < 56)
    {
#ifdef DEBUG_CHROMA
      fprintf(stderr, "Insufficient RAM Size for Chroma!\n");
#endif
      return rom4k ? 0x40 : 0xFF;
    }
    return rom4k ? 0x02 : 0x02; /* Chroma available */
  }

  if (!(l & 1))
  {
    LastInstruction = LASTINSTINFE;

    if (l == 0x7e)
      return 0; // for Lambda

    if (rom_patches.load.use_rom && ((pc == rom_patches.in.val1) ||
                                     (pc == rom_patches.in.val2) ||
                                     (pc == rom_patches.in.val3) ))
    {
        data = useNTSC ? 0x40 : 0;
        data |= loadPGetBit() ? 0x0 : 0x80; // Reversed as use xor below
    }

    switch (h)
    {
      case 0xfe:
        return (keyports[0] ^ data);
      case 0xfd:
        return (keyports[1] ^ data);
      case 0xfb:
        return (keyports[2] ^ data);
      case 0xf7:
        return (keyports[3] ^ data);
      case 0xef:
        return (keyports[4] ^ data);
      case 0xdf:
        return (keyports[5] ^ data);
      case 0xbf:
        return (keyports[6] ^ data);
      case 0x7f:
        return (keyports[7] ^ data);

      default:
      {
        int i, mask, retval = 0xff;
        /* some games (e.g. ZX Galaxians) do smart-arse things
         * like zero more than one bit. What we have to do to
         * support this is AND together any for which the corresponding
         * bit is zero.
        */
        for (i = 0, mask = 1; i < 8; i++, mask <<= 1)
          if (!(h & mask))
            retval &= keyports[i];
        return (retval ^ data);
      }
    }
  }

  switch (l)
  {
  case 0xfb:
    return (printer_inout(0, 0));

  default:
    break;
  }
  return 0;
}

unsigned int out(int h, int l, int a)
{
  if (h==0x7f && l==0xef)
  {	/* chroma 80 and Chroma 81*/
#ifdef DEBUG_CHROMA
    fprintf(stderr, "0x7fef 0x%x.\n",a);
#endif
    chromamode = a&0x30;
    if (chromamode)
    {
      if (sdl_emulator.ramsize < 56)
      {
        chromamode = 0;
        printf("Insufficient RAM Size for Chroma!\n");
      }
      else
      {
        adjustChroma(true);
        bordercolournew = a & 0x0f;
      }
    } else {
#ifdef DEBUG_CHROMA
      fprintf(stderr, "Selecting B/W mode.\n");
#endif
      adjustChroma(false);
    }
    LastInstruction = LASTINSTOUTFF;
    return 0;
  }

#ifdef OSS_SOUND_SUPPORT
  if ((sdl_sound.device == DEVICE_VSYNC) && frameNotSync)
  {
    sound_beeper(0);
  }
#endif

  switch (l)
  {
  case 0x0f:
  case 0x1f:
    if (sound_ay && sound_ay_type == AY_TYPE_ZONX)
      sound_ay_write(ay_reg, a);
    break;

  case 0xbf:
  case 0xcf:
  case 0xdf:
    if (sound_ay && sound_ay_type == AY_TYPE_ZONX)
      ay_reg = (a & 15);
    break;

  case 0xfb:
    return (printer_inout(1, a));
    break;

  case 0xfd:
    if (zx80)
      break;
    LastInstruction = LASTINSTOUTFD;
    break;

  case 0xfe:
    if (zx80)
      break;
    LastInstruction = LASTINSTOUTFE;
    break;

  case 0xff: // default out handled below
    break;

  default:
    break;
  }
  if (LastInstruction == LASTINSTNONE)
    LastInstruction = LASTINSTOUTFF;

  return 0; // No additional tstates
}
