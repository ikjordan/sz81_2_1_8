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
#include "common.h"
#include "sound.h"
#include "z80.h"
#ifdef SZ81 /* Added by Thunor */
#include "sdl.h"
#endif

// New definitions
#define LASTINSTNONE  0
#define LASTINSTINFE  1
#define LASTINSTOUTFE 2
#define LASTINSTOUTFD 3
#define LASTINSTOUTFF 4

#define HTOLMIN 414-30
#define HTOLMAX 414+30
#define VTOLMIN 310-100
#define VTOLMAX 310+100
#define HMIN 8
#define HMAX 32
#define VMIN 170

const static int HSYNC_TOLERANCEMIN = HTOLMIN;
const static int HSYNC_TOLERANCEMAX = HTOLMAX;
const static int VSYNC_TOLERANCEMIN = VTOLMIN;
const static int VSYNC_TOLERANCEMAX = VTOLMAX;
const static int HSYNC_MINLEN = HMIN;
const static int HSYNC_MAXLEN = HMAX;
const static int VSYNC_MINLEN = VMIN;

const static int HSYNC_START = 16;
const static int HSYNC_END = 32;
const static int HLEN = 207;
const static int tstate_jump = 8; // Step up to 8 tstates at a time

static int RasterX = 0;
static int RasterY = 0;
static int dest = -(DISPLAY_WIDTH * DISPLAY_START_Y);

static int int_pending, nmi_pending, hsync_pending;

static int LastInstruction;
static int NMI_generator;
static int VSYNC_state, HSYNC_state, SYNC_signal;
static int psync, sync_len;
static int rowcounter=0;
static int hsync_counter=0;

void checkhsync(int tolchk);
void checkvsync(int tolchk);
void checksync(int inc);
void anyout();
void vsync_raise(void);
void vsync_lower(void);

extern int printer_inout(int is_out,int val);


// end new definitions

#define parity(a) (partable[a])

unsigned char partable[256]={
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


//unsigned long tstates=0,tsmax=64998,frames=0;
unsigned long tstates=0,tsmax=65000,frames=0;

/* odd place to have this, but the display does work in an odd way :-) */
unsigned char scrnbmp_new[(DISPLAY_WIDTH>>3)*DISPLAY_HEIGHT];  /* written */
unsigned char scrnbmp[(DISPLAY_WIDTH>>3)*DISPLAY_HEIGHT];      /* displayed */
unsigned char scrnbmp_old[(DISPLAY_WIDTH>>3)*DISPLAY_HEIGHT];
        /* checked against for diffs */

#ifdef SZ81 /* Added by Thunor. I need these to be visible to sdl_loadsave.c */
int vsx=0;
int vsy=0;
#else
static int liney=0, lineyi=0;
static int vsy=0;
static unsigned long linestart=0;
static int vsync_toggle=0,vsync_lasttoggle=0;
#endif

int ay_reg=0;

static inline int z80_interrupt(void);
static inline int nmi_interrupt(void);

#ifndef SZ81  /* Added by Thunor. I need these to be visible to sdl_loadsave.c */
void mainloop()
{
#endif
unsigned char a, f, b, c, d, e, h, l;
unsigned char r, a1, f1, b1, c1, d1, e1, h1, l1, i, iff1, iff2, im;
unsigned short pc;
unsigned short ix, iy, sp;
unsigned char radjust;
unsigned char ixoriy, new_ixoriy;
unsigned char intsample=0;
unsigned char op;
//int nmipend=0,intpend=0,vsyncpend=0,vsynclen=0;
int framewait=0;

#ifdef SZ81 /* Added by Thunor */
void mainloop()
{
  intsample=0;
  framewait=0;
#endif
  bool videodata;

  a=f=b=c=d=e=h=l=a1=f1=b1=c1=d1=e1=h1=l1=i=iff1=iff2=im=r=0;
  ixoriy=new_ixoriy=0;
  ix=iy=sp=pc=0;
  tstates=radjust=0;

  RasterX = 0;
  RasterY = 0;
  dest = -(DISPLAY_WIDTH * DISPLAY_START_Y);
  psync = 1;
  sync_len = 0;

/* ULA */

  NMI_generator=0;
  nmi_pending=0;
  int_pending=0;
  rowcounter=0;
  hsync_pending=0;
  VSYNC_state=HSYNC_state=0;
  frames=0;
  hsync_counter=0;

#ifdef SZ81 /* Added by Thunor */
  if(sdl_emulator.autoload)
  {
    sdl_emulator.autoload=0;
    /* This could be an initial autoload or a later forcedload */
    if(!sdl_load_file(0,LOAD_FILE_METHOD_DETECT))
      /* wait for a real frame, to avoid an annoying frame `jump'. */
      framewait=1;
  }
#else
  if(autoload)
  {
    /* we load a snapshot, in effect. The memory was done by
    * common.c, this does the registers.
    */
    static unsigned char bit1[9]={0xFF,0x80,0xFC,0x7F,0x00,0x80,0x00,0xFE,0xFF};
    static unsigned char bit2[4]={0x76,0x06,0x00,0x3e};

    /* memory will already be zeroed at this point */
    memcpy(mem+0x4000,bit1,9);
    memcpy(mem+0x7ffc,bit2,4);
    a=0x0B; f=0x85; b=0x00; c=0xFF;
    d=0x43; e=0x99; h=0xC3; l=0x99;
    a1=0xE2; f1=0xA1; b1=0x81; c1=0x02;
    d1=0x00; e1=0x2B; h1=0x00; l1=0x00;
    i=0x1E; iff1=iff2=0;
    im=2;
    r=0xDD; radjust=0xCA;
    ix=0x281; iy=0x4000;
    sp=0x7FFC;
    pc=0x207;

    /* finally, load. It'll reset (via reset81) if it fails. */
    load_p(32768);

    /* wait for a real frame, to avoid an annoying frame `jump'. */
    framewait=1;
  }
#endif
  unsigned long ts;
  unsigned long tstore;
  unsigned char v=0;
  while(1)
  {
#ifdef SZ81 /* Added by Thunor */
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
#endif
    v=0;
    ts = 0;
    LastInstruction = LASTINSTNONE;

    if(intsample && !((radjust-1)&64) && iff1)
      int_pending=1;

    if(nmi_pending)
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
      } else {
        videodata = (pc&0x8000) ? true: false;
      }

      if(videodata && !(op&64))
      {
        v=0xff;

        if ((i<0x20) || (i<0x40 && LowRAM && (!useWRX)))
        {
          int addr = ((i&0xfe)<<8)|((op&63)<<3)|rowcounter;
          if (UDGEnabled && addr>=0x1E00 && addr<0x2000)
          {
            v = font[addr-((op&128)?0x1C00:0x1E00)];
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
        op=0; /* the CPU sees a nop */
      }

      tstore = tstates;

      do
      {
        pc++;
        radjust++;
        intsample=1;
        new_ixoriy=0;

        switch(op)
        {
#include "z80ops.c"
        }

        // Complete ix and iy instructions
        if (new_ixoriy)
        {
          ixoriy=new_ixoriy;
          op = fetchm(pc);
        }
      }  while (new_ixoriy);

      ixoriy = 0;

      ts = tstates - tstore;
      tstates = tstore;
    }

    nmi_pending = int_pending = 0;
    tstates += ts;

    switch(LastInstruction)
    {
      case LASTINSTOUTFD:
        NMI_generator = nmi_pending = 0;
        anyout();
      break;
      case LASTINSTOUTFE:
        if (!zx80)
        {
          NMI_generator=1;
        }
        anyout();
      break;
      case LASTINSTINFE:
        if (!NMI_generator)
        {
          if (VSYNC_state==0)
          {
            VSYNC_state = 1;
            vsync_raise();
          }
        }
      break;
      case LASTINSTOUTFF:
        anyout();
        if (zx80) hsync_pending=1;
      break;
      default:
      break;
    }

    /* Plot data in shift register */
    if (v &&
          (RasterX >= (DISPLAY_START_X - DISPLAY_PIXEL_OFF)) &&
          (RasterX < (DISPLAY_END_X - DISPLAY_PIXEL_OFF)) &&
          (RasterY >= DISPLAY_START_Y) &&
          (RasterY < DISPLAY_END_Y))
    {
      int k = dest + RasterX - (DISPLAY_START_X - DISPLAY_PIXEL_OFF); // DISPLAY_PIXEL_OFF to align lhs to byte boundary
      {
        int kh = k >> 3;
        int kl = k & 7;

        if (kl)
        {
          scrnbmp_new[kh++]|=(v>>kl);
          scrnbmp_new[kh]=(v<<(8-kl));
        }
        else
        {
          scrnbmp_new[kh]=v;
        }
      }
    }

    int tstate_inc;
    int states_remaining = ts;
    int since_hstart = 0;
    int tswait = 0;

    do
    {
      tstate_inc = states_remaining > tstate_jump ? tstate_jump: states_remaining;
      states_remaining -= tstate_inc;

      hsync_counter+=tstate_inc;
      RasterX += (tstate_inc<<1);

      if (hsync_counter >= HLEN)
      {
        hsync_counter -= HLEN;
        if (!zx80) hsync_pending = 1;
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

        if (VSYNC_state)
        {
          rowcounter = 0;
        } else
        {
          rowcounter++;
          rowcounter &= 7;
        }
        hsync_pending = 2;
      }

      // end of HSYNC
      if (hsync_pending==2 && hsync_counter>=HSYNC_END)
      {
        if (VSYNC_state==2)
        {
          VSYNC_state = 0;
          vsync_lower();
        }
        HSYNC_state = 0;
        hsync_pending = 0;
      }

      // NOR the vertical and horizontal SYNC states to create the SYNC signal
      SYNC_signal = (VSYNC_state || HSYNC_state) ? 0 : 1;
      checksync(since_hstart ? since_hstart : tstate_jump);
      since_hstart = 0;
    }
    while (states_remaining);

    if(tstates>=tsmax)
    {
      tstates-=tsmax;
      frames++;
      frame_pause();
    }

    /* this isn't used for any sort of Z80 interrupts,
    * purely for the emulator's UI.
    */
    if(interrupted)
    {
      if(interrupted==1)
      {
        do_interrupt(); /* also zeroes it */
      }
#ifdef SZ81 /* Added by Thunor */
        /* I've added these new interrupt types to support a thorough
        * emulator reset and to do a proper exit i.e. back to main */
      else if(interrupted==INTERRUPT_EMULATOR_RESET ||
              interrupted==INTERRUPT_EMULATOR_EXIT)
      {
        return;
      }
#endif
      else  /* must be 2 */
      {
        /* a kludge to let us do a reset */
        interrupted=0;
        a=f=b=c=d=e=h=l=a1=f1=b1=c1=d1=e1=h1=l1=i=iff1=iff2=im=r=0;
        ixoriy=new_ixoriy=0;
        ix=iy=sp=pc=0;
        tstates=radjust=0;
        RasterX = 0;
        RasterY = adjustStartY;
        dest = -(DISPLAY_WIDTH * DISPLAY_START_Y);
        psync = 1;
        sync_len = 0;

/* ULA */

        NMI_generator=0;
        int_pending=0;
        hsync_pending=0;
        VSYNC_state=HSYNC_state=0;
      }
    }
#ifdef  SZ81
  }
#endif
}

#ifdef SZ81 /* Added by Thunor */
void z80_reset(void)
{
  /* Reinitialise variables at the top of z80.c */
  tstates=0;
  frames=0;
  vsy=0;
  ay_reg=0;
}
#endif

static inline int z80_interrupt(void)
{
  int tinc=0;

  if(iff1)
  {
    if(fetchm(pc)==0x76)
    {
      pc++;
    }
    iff1=iff2=0;
    push2(pc);
    radjust++;

    switch(im)
    {
      case 0: /* IM 0 */
      case 2: /* IM 1 */
        pc=0x38;
        tinc=13;
      break;
      case 3: /* IM 2 */
      {
        int addr=fetch2((i<<8)|0xff);
        pc=addr;
        tinc=19;
      }
      break;

      default:
        tinc=12;
      break;
    }
  }
  return tinc;
}

static inline int nmi_interrupt(void)
{
  iff1=0;
  if(fetchm(pc)==0x76)
  {
    pc++;
  }
  push2(pc);
  radjust++;
  pc=0x66;

  return 11;
}

/* Normally, these sync checks are done by the TV :-) */
void checkhsync(int tolchk)
{
  if ( ( !tolchk && sync_len >= HSYNC_MINLEN && sync_len <= HSYNC_MAXLEN && RasterX>=HSYNC_TOLERANCEMIN ) ||
        (  tolchk &&                                                         RasterX>=HSYNC_TOLERANCEMAX ) )
  {
    if (zx80)
      RasterX = 0;
    else
      RasterX = (hsync_counter - HSYNC_END) < tstate_jump ? ((hsync_counter - HSYNC_END) << 1) : 0;
    RasterY++;
    dest += DISPLAY_WIDTH;
  }
}

void checkvsync(int tolchk)
{
  if ( ( !tolchk && sync_len >= VSYNC_MINLEN && RasterY>=VSYNC_TOLERANCEMIN ) ||
        (  tolchk &&                             RasterY>=VSYNC_TOLERANCEMAX ) )
  {
    RasterY = adjustStartY;
    dest =  -(DISPLAY_WIDTH * (DISPLAY_START_Y-adjustStartY));

    if (sync_len>tsmax)
    {
      // If there has been no sync for an entire frame then blank the screen
      memset(scrnbmp, 0xff, DISPLAY_HEIGHT * (DISPLAY_WIDTH  >> 3));
      sync_len = 0;
    }
    else
    {
      memcpy(scrnbmp,scrnbmp_new,sizeof(scrnbmp));
    }
    memset(scrnbmp_new, 0x00, DISPLAY_HEIGHT * (DISPLAY_WIDTH >> 3));
  }
}

void checksync(int inc)
{
  if (!SYNC_signal)
  {
    if (psync==1)
      sync_len = 0;
    sync_len += inc;
    checkhsync(1);
    checkvsync(1);
  } else
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

void anyout()
{
  if (VSYNC_state) {
    if (zx80)
    {
			VSYNC_state = 2; // will be reset by HSYNC circuitry
    }
    else
    {
      VSYNC_state = 0;
      vsync_lower();
    }
  }
}

unsigned int in(int h, int l)
{
  int ts=0;               /* additional cycles*256 */
  static int tapemask=0;
  int data=0;             /* = 0x80 if no tape noise (?) */

  tapemask++;
  data |= (tapemask & 0x0100) ? 0x80 : 0;
  if (useNTSC) data|=64;

  if (!(l&1))
  {
    LastInstruction=LASTINSTINFE;

    if (l==0x7e) return 0; // for Lambda

    switch(h)
    {
      case 0xfe:        return(ts|(keyports[0]^data));
      case 0xfd:        return(ts|(keyports[1]^data));
      case 0xfb:        return(ts|(keyports[2]^data));
      case 0xf7:        return(ts|(keyports[3]^data));
      case 0xef:        return(ts|(keyports[4]^data));
      case 0xdf:        return(ts|(keyports[5]^data));
      case 0xbf:        return(ts|(keyports[6]^data));
      case 0x7f:        return(ts|(keyports[7]^data));

      default:
      {
        int i,mask,retval=0xff;
        /* some games (e.g. ZX Galaxians) do smart-arse things
          * like zero more than one bit. What we have to do to
          * support this is AND together any for which the corresponding
          * bit is zero.
          */
        for(i=0,mask=1;i<8;i++,mask<<=1)
          if(!(h&mask))
            retval&=keyports[i];
        return(ts|(retval^data));
      }
    }
  }

  switch(l)
  {
    case 0xfb:
      return (printer_inout(0,0));

    default:
    break;
  }
  return(255);
}

unsigned int out(int h, int l,int a)
{
  switch(l)
  {
    case 0x0f:
      if(sound_ay && sound_ay_type==AY_TYPE_ZONX)
        sound_ay_write(ay_reg,a);
    break;

    case 0xbf:
    case 0xcf:
    case 0xdf:
      if(sound_ay && sound_ay_type==AY_TYPE_ZONX)
        ay_reg=(a&15);
    break;

    case 0xfb:
      return (printer_inout(1, a));
    break;

    case 0xfd:
      if (zx80) break;
      LastInstruction = LASTINSTOUTFD;
    break;

    case 0xfe:
      if (zx80) break;
      LastInstruction = LASTINSTOUTFE;
    break;

    case 0xff: // default out handled below
    break;

    default:
    break;
  }
  if (LastInstruction == LASTINSTNONE)
    LastInstruction=LASTINSTOUTFF;

  return 0; // No additional tstates
}

void vsync_raise(void)
{
  /* save current pos */
  vsx=RasterX;
  vsy=RasterY;
}

/* for vsync on -> off */
void vsync_lower(void)
{
//  if (!vsync_visuals)
//    return;
  if ((RasterY < DISPLAY_END_Y) || (vsy < DISPLAY_END_Y))
  {
    int nx=RasterX;
    int ny=RasterY;

    if (vsy < DISPLAY_START_Y)
      vsy = DISPLAY_START_Y;
    else if (vsy >= DISPLAY_END_Y)
      vsy = DISPLAY_END_Y - 1;

    if (ny < DISPLAY_START_Y)
      ny = DISPLAY_START_Y;
    else if (ny >= DISPLAY_END_Y)
      ny = DISPLAY_END_Y - 1;

    vsy -= DISPLAY_START_Y;
    ny -= DISPLAY_START_Y;

    if (vsy != ny)
    {
      if (vsx < DISPLAY_START_PIXEL)
        vsx = DISPLAY_START_PIXEL;
      else if (vsx >= DISPLAY_END_PIXEL)
        vsx = DISPLAY_END_PIXEL - 1;

      if (nx < DISPLAY_START_PIXEL)
        nx = DISPLAY_START_PIXEL;
      else if (nx >= DISPLAY_END_PIXEL)
        nx = DISPLAY_END_PIXEL - 1;

      vsx -= DISPLAY_START_Y;
      nx -= DISPLAY_START_Y;

      if(ny<vsy)
      {
        /* must be wrapping around a frame edge; do bottom half */
        memset(scrnbmp_new+vsy*(DISPLAY_WIDTH>>3)+(vsx>>3), 0xff, (DISPLAY_WIDTH>>3)*(DISPLAY_HEIGHT-vsy)-(vsx>>3));
        vsy=0;
        vsx=0;
      }
      memset(scrnbmp_new+vsy*(DISPLAY_WIDTH>>3)+(vsx>>3),0xff,(DISPLAY_WIDTH>>3)*(ny-vsy)+((nx-vsx)>>3));
    }
  }
}
