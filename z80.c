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
static unsigned char scrnbmp_new_base[((DISPLAY_F_WIDTH >> 3) + DISPLAY_PADDING) * DISPLAY_F_HEIGHT]; /* written */
static unsigned char scrnbmp_base[((DISPLAY_F_WIDTH >> 3) + DISPLAY_PADDING) * DISPLAY_F_HEIGHT];     /* displayed */

static unsigned char *const scrnbmp_new = scrnbmp_new_base + DISPLAY_PADDING;
unsigned char *const scrnbmp = scrnbmp_base + DISPLAY_PADDING;

/* chroma */
static unsigned char scrnbmpc_new_base[((DISPLAY_F_WIDTH >> 3) + DISPLAY_PADDING) * DISPLAY_F_HEIGHT];/* written */
static unsigned char scrnbmpc_base[((DISPLAY_F_WIDTH >> 3) + DISPLAY_PADDING) * DISPLAY_F_HEIGHT];	  /* displayed */

static unsigned char *const scrnbmpc_new = scrnbmpc_new_base + DISPLAY_PADDING;
unsigned char *const scrnbmpc = scrnbmpc_base + DISPLAY_PADDING;

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

static const int HSYNC_TOLERANCEMIN = HSCAN - HTOL;
static const int HSYNC_TOLERANCEMAX = HSCAN + HTOL;

static const int HSYNC_MINLEN = HMIN;
static const int HSYNC_MAXLEN = HMAX;
static const int VSYNC_MINLEN = VMIN;

static int VSYNC_TOLERANCEMIN = SCAN50 - VTOL;
static int VSYNC_TOLERANCEMAX = SCAN50 + VTOL;

static int FRAME_SCAN = SCAN50;

static const int HSYNC_START = 16;
static const int HSYNC_END = 32;
static const int HLEN = HLENGTH;
static const int MAX_JMP = 8;

static int S_RasterX = 0;
static int S_RasterY = 0;
static int RasterX = 0;
static int RasterY = 0;

static int adjustStartX=0;
static int adjustStartY=0;
static int startX = 0;
static int startY = 0;
static int endX = 0;
static int endY = 0;

static bool frameNotSync = true;
static int dest;

static int int_pending, nmi_pending, hsync_pending;
static int NMI_generator;
static int VSYNC_state, HSYNC_state, SYNC_signal;
static int psync, sync_len;
static int rowcounter = 0;
static int hsync_counter = 0;
static bool rowcounter_hold = false;
bool running_rom = false;

extern int printer_inout(int is_out, int val);

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
static void setRemainingDisplayBoundaries(void);
static void adjustChroma(bool start);
static unsigned long z80_op(void);

#ifdef LOAD_AND_SAVE
static void loadAndSaveROM(void);
#endif

static void setRemainingDisplayBoundaries(void)
{
  startX = disp.start_x - adjustStartX - 6;
  startY = disp.start_y - adjustStartY;
  endX = disp.end_x - adjustStartX;
  endY = disp.end_y - adjustStartY;
}

/* Ensure that chroma and pixels are byte aligned */
static void adjustChroma(bool start)
{
  if (start)
  {
    if (!adjustStartX)
    {
      if (fullDisplay)
      {
        adjustStartX = DISPLAY_F_PIXEL_OFF;
      }
      else if (fiveSevenSix)
      {
        adjustStartX = DISPLAY_P_PIXEL_OFF;
      }
      else
      {
        adjustStartX = DISPLAY_N_PIXEL_OFF;
      }
    }
    setRemainingDisplayBoundaries();
  }
  else
  {
    setDisplayBoundaries();
  }
}

void setDisplayBoundaries(void)
{
  adjustStartX = (zx80 && (!fullDisplay)) ? DISPLAY_ZX80_OFF : 0;
  if (centreScreen)
  {
    if (!(fullDisplay || fiveSevenSix))
    {
      adjustStartX = DISPLAY_N_PIXEL_OFF + (zx80 ? DISPLAY_ZX80_OFF : 0);
      adjustStartY = (useNTSC) ? (DISPLAY_N_START_Y >> 1) : -(DISPLAY_N_START_Y >> 1);
    }
  }

  setRemainingDisplayBoundaries();
}

static void setEmulatedTV(bool fiftyHz, uint16_t vtol)
{
  // This can look confusing as we have an emulated display, and a real
  // display, both can be at either 50 or 60 Hz
  if (fiftyHz)
  {
    FRAME_SCAN = SCAN50;
    VSYNC_TOLERANCEMIN = SCAN50 - vtol;
    VSYNC_TOLERANCEMAX = SCAN50 + vtol;
  }
  else
  {
    FRAME_SCAN = SCAN60;
    VSYNC_TOLERANCEMIN = SCAN60 - vtol;
    VSYNC_TOLERANCEMAX = SCAN60 + vtol;
  }
}

static void vsync_raise(void)
{
  /* save current pos - in screen coords*/
  vsx = RasterX - (disp.start_x - adjustStartX);
  vsy = RasterY - startY;

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
}

/* for vsync on -> off */
static void vsync_lower(void)
{
  int nx = RasterX - (disp.start_x - adjustStartX);
  int ny = RasterY - startY;

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
  // especially when displaying the loading screen
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

#define SYNCNONE        0
#define SYNCTYPEH       1
#define SYNCTYPEV       2

static int scanlineCounter = 0;

static int videoFlipFlop1Q = 1;
static int videoFlipFlop2Q = 0;
static int videoFlipFlop3Q = 0;
static int videoFlipFlop3Clear = 0;
static int prevVideoFlipFlop3Q = 0;

static int lineClockCarryCounter = 0;

static int scanline_len = 0;
static int sync_type = SYNCNONE;
static bool vsyncFound = false;

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

int nosync_lines = 0;

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

  zx80 ? zx80_loop() : zx81_loop();
}

void zx81_loop(void)
{
  unsigned long ts;
  unsigned char v;
  bool videodata;

  int addr;
  int k;
  int kh;
  int kl;

  unsigned char colour = 0;

  while (1)
  {
    LastInstruction = LASTINSTNONE;

    if(intsample && !((radjust-1)&64) && iff1)
      int_pending=1;

    if(nmi_pending)
    {
      ts = nmi_interrupt();
      tstates += ts;
    }
    else if (int_pending)
    {
      ts = z80_interrupt();
      hsync_counter = -2;             /* INT ACK after two tstates */
      hsync_pending = 1;              /* a HSYNC may be started */
      tstates += ts;
    }
    else
    {
      // Get the next op, calculate the next byte to display and execute the op
      op = fetchm(pc);

      intsample = 1;
      m1cycles = 1;

      if (m1not && pc<0xC000)
      {
        videodata = false;
      }
      else
      {
        videodata = (pc&0x8000) ? true: false;
      }

      if(videodata && !(op & 0x40))
      {
        if ((i < 0x20) || (i < 0x40 && LowRAM && (!useWRX)))
        {
          if (chr128 && i > 0x20 && i & 1)
            addr = ((i & 0xfe) << 8) | ((((op & 0x80) >> 1) | (op & 0x3f)) << 3)|rowcounter;
          else
            addr = ((i & 0xfe) << 8) | ((op & 0x3f) << 3) | rowcounter;

          if (UDGEnabled && (addr >= 0x1E00) && (addr < 0x2000))
          {
            v = mem[addr + ((op & 0x80) ? 0x6800 : 0x6600)];
          }
          else
          {
            v = mem[addr];
          }
        }
        else if (useWRX)
        {
          v = mem[(i << 8) | (r & 0x80) | (radjust & 0x7f)];
        }
        else
        {
          v = 0xff;
        }
        v = (op & 0x80) ? ~v : v;

        if (chromamode)
        {
          colour = (chromamode & 0x10) ? fetch(pc) : fetch(0xc000 | ((((op & 0x80) >> 1) | (op & 0x3f)) << 3) | rowcounter);
          chroma_set = (colour ^ fullcolour) & 0xf0;
        }
        /* The CPU sees a nop - so skip the Z80 emulation loop */
        pc++;
        radjust++;

        ts = 4;
        tstates += 4;
      // Plot data in shift register
      // Note subtract 6 as this leaves the smallest positive number
      // of bits to carry to next byte (2)
        if ((v || chroma_set) &&
            (RasterX >= startX) &&
            (RasterX < endX) &&
            (RasterY >= startY) &&
            (RasterY < endY))
        {
          if (chromamode)
          {
            k = (dest + RasterX) >> 3;
            scrnbmpc_new[k] = colour;
            scrnbmp_new[k] = v;
            chroma_set = 0;
          }
          else
          {
            k = dest + RasterX;
            kh = k >> 3;
            kl = k & 7;

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
      else
      {
        ts = z80_op();
      }
    }

    nmi_pending = int_pending = 0;

    switch(LastInstruction)
    {
      case LASTINSTOUTFD:
        NMI_generator = 0;
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
      break;

      case LASTINSTOUTFF:
        anyout();
      break;
    }

    int states_remaining = ts;
    int since_hstart = 0;
    int tswait = 0;
    int tstate_inc;

    do
    {
      tstate_inc = states_remaining > MAX_JMP ? MAX_JMP: states_remaining;
      states_remaining -= tstate_inc;

      hsync_counter+=tstate_inc;
      RasterX += (tstate_inc<<1);

      if (hsync_counter >= HLEN)
      {
          hsync_counter -= HLEN;
          hsync_pending = 1;
      }

      // Start of HSYNC, and NMI if enabled
      if (hsync_pending==1 && hsync_counter>=HSYNC_START)
      {
        if (NMI_generator)
        {
          nmi_pending = 1;
          if (ts==4)
          {
          tswait = 14 + (3-states_remaining - (hsync_counter - HSYNC_START));
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
        hsync_pending = 2;
      }

      // end of HSYNC
      if ((hsync_pending == 2) && (hsync_counter >= HSYNC_END))
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

void zx80_loop(void)
{
  unsigned long ts;
  unsigned long tstore;
  unsigned char v;
  bool videodata;

  int addr;
  int k;
  int kh;
  int kl;

  unsigned char colour = 0;

  while (1)
  {
    LastInstruction = LASTINSTNONE;

    // Get the next op, calculate the next byte to display and execute the op
    op = fetchm(pc);

    intsample = 1;
    m1cycles = 1;

    if (m1not && pc<0xC000)
    {
      videodata = false;
    }
    else
    {
      videodata = (pc&0x8000) ? true: false;
    }

    if(videodata && !(op & 0x40))
    {
      if ((i < 0x20) || (i < 0x40 && LowRAM && (!useWRX)))
      {
        if (chr128 && i > 0x20 && i & 1)
          addr = ((i & 0xfe) << 8) | ((((op & 0x80) >> 1) | (op & 0x3f)) << 3)|rowcounter;
        else
          addr = ((i & 0xfe) << 8) | ((op & 0x3f) << 3) | rowcounter;

        if (UDGEnabled && (addr >= 0x1E00) && (addr < 0x2000))
        {
          v = mem[addr + ((op & 0x80) ? 0x6800 : 0x6600)];
        }
        else
        {
          v = mem[addr];
        }
      }
      else if (useWRX)
      {
        v = mem[(i << 8) | (r & 0x80) | (radjust & 0x7f)];
      }
      else
      {
        v = 0xff;
      }
      v = (op & 0x80) ? ~v : v;

      if (chromamode)
      {
        colour = (chromamode & 0x10) ? fetch(pc) : fetch(0xc000 | ((((op & 0x80) >> 1) | (op & 0x3f)) << 3) | rowcounter);
        chroma_set = (colour ^ fullcolour) & 0xf0;
      }
      /* The CPU sees a nop - so skip the Z80 emulation loop */
      pc++;
      radjust++;

      ts = 4;
      tstates += 4;
      // Plot data in shift register
      // Note subtract 6 as this leaves the smallest positive number
      // of bits to carry to next byte (2)
      if ((v || chroma_set) &&
          (RasterX >= startX) &&
          (RasterX < endX) &&
          (RasterY >= startY) &&
          (RasterY < endY))
      {
        if (chromamode)
        {
          k = (dest + RasterX) >> 3;
          scrnbmpc_new[k] = colour;
          scrnbmp_new[k] = v;
          chroma_set = 0;
        }
        else
        {
          k = dest + RasterX;
          kh = k >> 3;
          kl = k & 7;

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
    else
    {
      ts = z80_op();
    }

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

    // execute an interrupt
    if (intsample && !((radjust - 1) & 0x40) && iff1)
    {
      tstore = z80_interrupt();
      tstates += tstore;
      ts += tstore;

      // single m1Cycle
      if (videoFlipFlop3Clear)
      {
        videoFlipFlop3Q = videoFlipFlop2Q;
      }

      videoFlipFlop2Q = !videoFlipFlop1Q;
      videoFlipFlop1Q = 1;
    }

    RasterX += (ts << 1);
    scanline_len += (ts << 1);
    if (RasterX >= scanlinePixelLength)
    {
      RasterX -= scanlinePixelLength;
      RasterY++;
    }

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
      // ZX80 HSYNC sound - excluded if Chroma
#ifdef OSS_SOUND_SUPPORT
      if ((sdl_sound.device == DEVICE_VSYNC) && frameNotSync)
      {
          sound_beeper(videoFlipFlop3Q);
      }
#endif

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
            lineClockCarryCounter = 0;
          }
          else
          {
            lineClockCarryCounter = scanline_len > 1;
          }
          scanline_len = scanlinePixelLength;
        }
        else if (overhangPixels > 0)
        {
          lineClockCarryCounter = overhangPixels > 1;
          scanline_len = scanlinePixelLength;
        }
      }
      else if (scanline_len >= ZX80HSyncAcceptancePixelPosition)
      {
        lineClockCarryCounter = ts;
        scanline_len = scanlinePixelLength;
      }
    }

    // If we are at the end of a zx80 line then process it
    if (!((scanline_len < scanlineThresholdPixelLength) && (sync_type == SYNCNONE)))
    {
      if (sync_type == SYNCTYPEV)
      {
#ifdef DEBUG_SYNC
        static bool found = false;
        static int count = 0;
#endif

        // Frames synchonised after second vsyncs in range
        if (vsyncFound)
        {
#ifdef DEBUG_SYNC
          if (!found)
          {
            printf("T %i\n", ++count);
            found = true;
          }
#endif
          frameNotSync = !((RasterY >= VSYNC_TOLERANCEMIN) && (RasterY <= VSYNC_TOLERANCEMAX) &&
                          (scanlineCounter >= VSYNC_TOLERANCEMIN) && (scanlineCounter <= VSYNC_TOLERANCEMAX));
          vsyncFound = !frameNotSync;
        }
        else
        {
#ifdef DEBUG_SYNC
          if (found)
          {
            printf("F %i\n", count);
            found = false;
          }
#endif
          vsyncFound = (scanlineCounter >= VSYNC_TOLERANCEMIN) && (scanlineCounter <= VSYNC_TOLERANCEMAX);
        }
        scanlineCounter = 0;

        if (!vsyncFound)
        {
          sync_type = SYNCNONE;
          sync_len = 0;
        }
        nosync_lines = 0;
      }
      else
      {
        if (scanlineCounter < VSYNC_TOLERANCEMAX)
        {
          if (sync_type == SYNCTYPEH)
          {
            scanlineCounter++;
          }
        }

        if (((sync_type == SYNCNONE) && videoFlipFlop3Q) || (scanlineCounter == VSYNC_TOLERANCEMAX))
        {
          frameNotSync = true;
          vsyncFound = false;
        }

        if (sync_type == SYNCNONE)
        {
          int overhangPixels = scanline_len - scanlinePixelLength;
          if (overhangPixels > 0)
          {
            lineClockCarryCounter = (overhangPixels >> 1);
            scanline_len = scanlinePixelLength;
          }
          nosync_lines++;
        }
        else
        {
          nosync_lines = 0;
        }
      }

      // Synchronise the TV position
      S_RasterX += scanline_len;
      if (S_RasterX >= scanlinePixelLength)
      {
        S_RasterX -= scanlinePixelLength;
        S_RasterY++;

        if (S_RasterY >= VSYNC_TOLERANCEMAX)
        {
          S_RasterX = 0;
          sync_type=SYNCTYPEV;
          if (sync_len < HSYNC_MINLEN)
          {
            sync_len=HSYNC_MINLEN;
          }
        }
      }

      if (sync_len<HSYNC_MINLEN) sync_type=0;

      if (sync_type)
      {
        if (S_RasterX > HSYNC_TOLERANCEMAX)
        {
          S_RasterX=0;
          S_RasterY++;
        }

        if (S_RasterY>=VSYNC_TOLERANCEMAX ||
            (sync_len>VSYNC_MINLEN && S_RasterY>VSYNC_TOLERANCEMIN))
        {
          if (nosync_lines >= FRAME_SCAN)
          {
            // Whole frame with no sync, so blank the display
            memset(scrnbmp, 0xff, disp.length);
            if (chromamode) memset(scrnbmpc, 0x0, disp.length);
            nosync_lines -= FRAME_SCAN;
          }
          else
          {
            memcpy(scrnbmp, scrnbmp_new, disp.length);
            if (chromamode) memcpy(scrnbmpc, scrnbmpc_new, disp.length);
          }

          // Display the frame
          memset(scrnbmp_new, 0x00, disp.length);
          if (chromamode && (bordercolournew != bordercolour))
          {
            bordercolour = bordercolournew;
            fullcolour = (bordercolour << 4) + bordercolour;
          }

          if (chromamode) memset(scrnbmpc_new, fullcolour, disp.length);

          S_RasterX = 0;
          S_RasterY = 0;
        }
      }

      // Update data for new ZX80 scanline
      RasterX = S_RasterX;
      RasterY = S_RasterY;
      dest = disp.offset + (disp.stride_bit * (adjustStartY + RasterY)) + adjustStartX;

      if (sync_type != SYNCNONE)
      {
        sync_type = SYNCNONE;
        sync_len = 0;
      }

      // Handle line carry over here
      if (lineClockCarryCounter > 0)
      {
        scanline_len = lineClockCarryCounter << 1;
        RasterX += scanline_len;
        lineClockCarryCounter = 0;
      }
      else
      {
        scanline_len = 0;
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

        /* ZX80 Hardware */
        videoFlipFlop1Q = 1;
        videoFlipFlop2Q = 0;
        videoFlipFlop3Q = 0;
        videoFlipFlop3Clear = 0;
        prevVideoFlipFlop3Q = 0;

        frameNotSync = true;
        vsyncFound = false;
        scanlineCounter = 0;
      }
    }
  }
}

static unsigned long z80_op(void)
{
  unsigned long tstore = tstates;

  do
  {
    pc++;
    radjust++;

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

  return tstates - tstore;
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

#ifdef LOAD_AND_SAVE
static void loadAndSaveROM(void)
{
  if (!running_rom)
  {
    if (pc == rom_patches.load.start) // load
    {
      int run_rom = sdl_load_file(de, (de < 0x8000) ? LOAD_FILE_METHOD_NAMEDLOAD : LOAD_FILE_METHOD_SELECTLOAD);
      if ((!rom_patches.load.use_rom) || (run_rom != RUN_ROM))
      {
        pc = rom_patches.rstrtAddr;
        op = fetchm(pc);
      }
    }
    else if (pc == rom_patches.save.start) // save
    {
      int run_rom = sdl_save_file(hl, SAVE_FILE_METHOD_NAMEDSAVE);
      if ((!rom_patches.save.use_rom) || (run_rom != RUN_ROM))
      {
        pc = rom_patches.rstrtAddr;
        op = fetchm(pc);
      }
    }
  }
  else
  {
    if (pc == rom_patches.retAddr)
    {
      running_rom = false;
    }
  }
}
#endif

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
#ifdef DEBUG_SYNC
    static bool found = false;
    static int count = 0;
#endif

    if (sync_len>(int)tsmax)
    {
      // If there has been no sync for an entire frame then blank the screen to black
      memset(scrnbmp, 0xff, disp.length);
      if (chromamode) memset(scrnbmpc, 0x0, disp.length);
      sync_len = 0;
      frameNotSync = true;
      vsyncFound = false;
    }
    else
    {
      memcpy(scrnbmp, scrnbmp_new, disp.length);
      if (chromamode) memcpy(scrnbmpc, scrnbmpc_new, disp.length);
      if (vsyncFound)
      {
#ifdef DEBUG_SYNC
        if (!found)
        {
          printf("T %i\n", ++count);
          found = true;
        }
#endif
        frameNotSync = (RasterY >= VSYNC_TOLERANCEMAX);
      }
      else
      {
#ifdef DEBUG_SYNC
        if (found)
        {
          printf("F %i\n", count);
          found = false;
        }
#endif
        frameNotSync = true;
        vsyncFound = (RasterY < VSYNC_TOLERANCEMAX);
      }
    }
    memset(scrnbmp_new, 0x00, disp.length);
    if (chromamode && (bordercolournew != bordercolour))
    {
      bordercolour = bordercolournew;
      fullcolour = (bordercolour << 4) + bordercolour;
    }

    if (chromamode) memset(scrnbmpc_new, fullcolour, disp.length);
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
    VSYNC_state = 0;
    // A fairly arbitrary value selected after comparing with a "real" ZX81
    if ((hsync_counter < HSYNC_START) || (hsync_counter >= 178) )
    {
      rowcounter_hold = true;
    }
    vsync_lower();
    if ((sync_len > HSYNC_MAXLEN) && (RasterY < VSYNC_TOLERANCEMIN))
    {
        vsyncFound = false;
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
      return zx80 ? 0x40 : 0xFF;
    }
    return zx80 ? 0x02 : 0x02; /* Chroma available */
  }

  if (!(l & 1))
  {
#ifdef OSS_SOUND_SUPPORT
    if ((sdl_sound.device == DEVICE_VSYNC) && frameNotSync)
    {
      sound_beeper(0);
    }
#endif
    LastInstruction = LASTINSTINFE;

    if (l == 0x7e)
      return 0; // for Lambda

#ifdef LOAD_AND_SAVE
    if (running_rom)
    {
      data = useNTSC ? 0x40 : 0;
      data |= loadPGetBit() ? 0x0 : 0x80; // Reversed as use xor below
    }
#endif

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
#ifdef OSS_SOUND_SUPPORT
  if ((sdl_sound.device == DEVICE_VSYNC) && frameNotSync)
  {
    sound_beeper(1);
  }
#endif

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
