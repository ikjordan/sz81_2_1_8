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

#include <string.h>	/* for memset/memcpy */
#include "common.h"
#include "sound.h"
#include "z80.h"
#ifdef SZ81	/* Added by Thunor */
#include "sdl.h"
#endif

//// Start of 2.3.12
#define LASTINSTNONE  0
#define LASTINSTINFE  1
#define LASTINSTOUTFE 2
#define LASTINSTOUTFD 3
#define LASTINSTOUTFF 4

//#define DEBUG_PRINT

// #define VRCNTR

static int RasterX = 0;
static int RasterY = 0;
static int TVP;
static int dest;

/* TV specifications */

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
typedef unsigned char BYTE;

int int_pending, nmi_pending, hsync_pending;

unsigned long tstates=0;
unsigned long tsmax=0;
unsigned long frames=0;

int NMI_generator;
int VSYNC_state, HSYNC_state, SYNC_signal;
int psync, sync_len;
int LastInstruction;
BYTE shift_register;
int rowcounter=0;
int hsync_counter=0;
BYTE z80halted;

int z80_interrupt(int ts);
int z80_nmi(int ts);
void z80_init(void);
void z80_reset(void);

//// End of 2.3.12

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


//unsigned long tstates=0,tsmax=65000,frames=0;

/* odd place to have this, but the display does work in an odd way :-) */
unsigned char scrnbmp_new[ZX_VID_FULLWIDTH*ZX_VID_FULLHEIGHT>>3]; /* written */
unsigned char scrnbmp[ZX_VID_FULLWIDTH*ZX_VID_FULLHEIGHT>>3];	/* displayed */
unsigned char scrnbmp_old[ZX_VID_FULLWIDTH*ZX_VID_FULLHEIGHT>>3];
						/* checked against for diffs */

#ifdef SZ81	/* Added by Thunor. I need these to be visible to sdl_loadsave.c */
int liney=0;
int vsy=0;
unsigned long linestart=0;
int vsync_toggle=0,vsync_lasttoggle=0;
#else
static int liney=0, lineyi=0;
static int vsy=0;
static unsigned long linestart=0;
static int vsync_toggle=0,vsync_lasttoggle=0;
#endif

int ay_reg=0;

static int linestate=0, linex=0, nrmvideo=1;

/* for vsync off -> on */
void vsync_raise(void)
{
  /* save current pos */
  vsy=RasterY;
}

/* for vsync on -> off */
void vsync_lower(void)
{
  int ny=RasterY;
  int y;

  vsync_toggle++;

  /* we don't emulate this stuff by default; if nothing else,
  * it can be fscking annoying when you're typing in a program.
  */
  if(!vsync_visuals)
    return;

  /* even when we do emulate it, we don't bother with x timing,
  * just the y. It gives reasonable results without being too
  * complicated, I think.
  */
  if(vsy<0) vsy=0;
  if(vsy>=ZX_VID_FULLHEIGHT) vsy=ZX_VID_FULLHEIGHT-1;
  if(ny<0) ny=0;
  if(ny>=ZX_VID_FULLHEIGHT) ny=ZX_VID_FULLHEIGHT-1;

  /* XXX both of these could/should be made into single memset calls */
  if(ny<vsy)
  {
    /* must be wrapping around a frame edge; do bottom half */
    for(y=vsy;y<ZX_VID_FULLHEIGHT;y++)
      memset(scrnbmp_new+y*(ZX_VID_FULLWIDTH>>3),0xff,ZX_VID_FULLWIDTH>>3);
    vsy=0;
  }

  for(y=vsy;y<ny;y++)
    memset(scrnbmp_new+y*(ZX_VID_FULLWIDTH>>3),0xff,ZX_VID_FULLWIDTH>>3);
}

#ifndef SZ81	/* Added by Thunor. I need these to be visible to sdl_loadsave.c */
void mainloop()
{
#endif
unsigned char a, f, b, c, d, e, h, l;
unsigned char r, a1, f1, b1, c1, d1, e1, h1, l1, i, iff1, iff2, im;
unsigned short pc;
unsigned short ix, iy, sp;
unsigned char radjust;
unsigned long nextlinetime=0,linegap=208,lastvsyncpend=0;
unsigned char ixoriy, new_ixoriy;
unsigned char intsample=0;
unsigned char op;
int ulacharline=0;
int nmipend=0,intpend=0,vsyncpend=0,vsynclen=0;
int hsyncskip=0;
int framewait=0;
int minx = ZX_VID_FULLHEIGHT;
int maxx = 0;
bool videodata = false;
unsigned char z80halted;

#ifdef SZ81	/* Added by Thunor */
void mainloop1()
{
  nextlinetime=0; linegap=208; lastvsyncpend=0;
  intsample=0;
  ulacharline=0;
  nmipend=0; intpend=0; vsyncpend=0; vsynclen=0;
  hsyncskip=0;
  framewait=0;
#endif

  a=f=b=c=d=e=h=l=a1=f1=b1=c1=d1=e1=h1=l1=i=iff1=iff2=im=r=0;
  ixoriy=new_ixoriy=0;
  ix=iy=sp=pc=0;
  tstates=radjust=0;
  nextlinetime=linegap;

#ifdef SZ81	/* Added by Thunor */
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

  while(1)
  {
#ifdef SZ81	/* Added by Thunor */
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
        printf("mem[0x%04x] = 0x%02x;	/* ERR_NR */\n", 0x4000, mem[0x4000]);
        printf("mem[0x%04x] = 0x%02x;	/* FLAGS */\n", 0x4001, mem[0x4001]);
        printf("mem[0x%04x] = 0x%02x;	/* ERR_SP lo */\n", 0x4002, mem[0x4002]);
        printf("mem[0x%04x] = 0x%02x;	/* ERR_SP hi */\n", 0x4003, mem[0x4003]);
        printf("mem[0x%04x] = 0x%02x;	/* RAMTOP lo */\n", 0x4004, mem[0x4004]);
        printf("mem[0x%04x] = 0x%02x;	/* RAMTOP hi */\n", 0x4005, mem[0x4005]);
        printf("mem[0x%04x] = 0x%02x;	/* MODE */\n", 0x4006, mem[0x4006]);
        printf("mem[0x%04x] = 0x%02x;	/* PPC lo */\n", 0x4007, mem[0x4007]);
        printf("mem[0x%04x] = 0x%02x;	/* PPC hi */\n", 0x4008, mem[0x4008]);
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
    /* this *has* to be checked before radjust is incr'd */
    if(intsample && !(radjust&64))
      intpend=1;

    ixoriy=new_ixoriy;
    new_ixoriy=0;
    intsample=1;
    op = fetchm(pc);

    if (m1not && pc<0xC000)
    {
      videodata = false;
    } else {
      videodata = ((pc&0x8000) != 0);
    }

    if (videodata && (!(op&64) && linestate==0))
    {
      nrmvideo = i<0x20 || radjust==0xdf;
      linestate = 1;
      linex = 200 - (nextlinetime - tstates);
      linex = linex>0 ? linex : 0;

      if (liney<ZX_VID_MARGIN) liney=ZX_VID_MARGIN;
    }
    else if (linestate>=1)
    {
      if (op&64)
      {
        linestate = 0;
        linex = ZX_VID_FULLWIDTH/2;
      } else
      {
        linestate++;
        linex+=4;
      }
    }

    if (!nrmvideo) ulacharline = 0;

    if(videodata && !(op&64))
    {
/*    printf("ULA %3d,%3d = %02X\n",x,y,op);*/
      if(liney>=0 && liney<ZX_VID_FULLHEIGHT &&
        linex>=0 && linex<(ZX_VID_FULLWIDTH>>1))
      {
        unsigned char v=0;

        maxx = (maxx>linex) ? maxx : linex;
        minx = (minx<linex) ? minx : linex;

        if (nrmvideo)
        {
          int addr = ((i&0xfe)<<8)|((op&63)<<3)|ulacharline;
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
          if ((addr < 0x4000) || SRAM)
          {
            /* If WRX memory not present then will read ROM shadow image instead */
            v = mem[addr];
          }
        }

        int p = linex>>2;
        int b = (linex&0x03)<<1;
        v = (op&128)?~v:v;

        if (b)
        {
          scrnbmp_new[liney*(ZX_VID_FULLWIDTH>>3)+p++]|=(v>>b);
          scrnbmp_new[liney*(ZX_VID_FULLWIDTH>>3)+p]=(v<<(8-b));
        }
        else
        {
          scrnbmp_new[liney*(ZX_VID_FULLWIDTH>>3)+p]=v;
        }
      }
      op=0;	/* the CPU sees a nop */
    }

    pc++;
    radjust++;

    switch(op)
    {
#include "z80ops.c"
    }

    if(tstates>=tsmax)
    {
      tstates-=tsmax;
      linestart-=tsmax;
      nextlinetime-=tsmax;
      lastvsyncpend-=tsmax;
      vsync_lasttoggle=vsync_toggle;
      vsync_toggle=0;

      frames++;
      frame_pause();
    }

    /* the vsync length test is pretty arbitrary, because
    * the timing isn't very accurate (more or less an instruction
    * count) - but it's good enough in practice.
    *
    * the vsync_toggle test is fairly arbitrary too;
    * there has to have been `not too many' for a TV to get
    * confused. In practice, more than one would screw it up,
    * but since we could be looking at over a frames' worth
    * given where vsync_toggle is zeroed, we play it safe.
    * also, we use a copy of the previous chunk's worth,
    * since we need a full frame(-plus) to decide this.
    */
    if(vsynclen && !vsync)
    {
      if(vsynclen>=10)
      {
        if(vsync_lasttoggle<=2)
        {
          vsyncpend=1;	/* start of frame */
          /* FAST mode screws up without this, but it's a bit
          * unpleasant... :-/
          */
          //tstates=nextlinetime - 7; // Galaxians
          tstates=nextlinetime;
        }
      }
      else
      {
        /* independent timing for this would be awkward, so
        * anywhere on the line is good enough. Also,
        * don't count it as a toggle.
        */
        vsync_toggle--;
        hsyncskip=1;
      }
    }

    /* should do this all the time vsync is set */
    if(vsync)
    {
      ulacharline=0;
      vsynclen++;
    }
    else
      vsynclen=0;

    if(tstates>=nextlinetime)	/* new line */
    {
      /* generate fake sync if we haven't had one for a while;
      * but if we just loaded/saved, wait for the first real frame instead
      * to avoid jumpiness.
      */
      if(!vsync && tstates-lastvsyncpend>=tsmax*2 && !framewait)
        vsyncpend=1;

      /* but that won't ever happen if we always have vsync on -
      * i.e., if we're grinding away in FAST mode. So for that
      * case, we check for vsync being held for a full frame.
      */
      if(vsync_visuals && vsynclen>=tsmax)
      {
        vsyncpend=1;
        vsynclen=1;

        memset(scrnbmp,0xff,sizeof(scrnbmp));	/* blank the screen */
        goto postcopy;				/* skip the usual copying */
      }

      if(!vsyncpend)
      {
        liney++;

        if(hsyncgen && !hsyncskip)
        {
  /*        printf("hsync %d\n",tstates);*/
          ulacharline++;
          ulacharline&=7;
          tstates++;
        }
      }
      else
      {
        memcpy(scrnbmp,scrnbmp_new,sizeof(scrnbmp));
        postcopy:
        memset(scrnbmp_new,0,sizeof(scrnbmp_new));
        lastvsyncpend=tstates;
        vsyncpend=0;
        framewait=0;
        liney=-1;		/* XXX might be something up here */
        minx = ZX_VID_FULLHEIGHT;
        maxx = 0;

  /*      printf("FRAME START - %s\n",(mem[16443]&128)?"slow":"fast");*/
      }

      if(nmigen)
        nmipend=1;

      hsyncskip=0;
      linestart=nextlinetime;
      nextlinetime+=linegap;
    }

    if(intsample && nmipend)
    {
      nmipend=0;

      if(nmigen)
      {
  /*      printf("NMI line %d tst %d\n",liney,tstates);*/
        iff2=iff1;
        iff1=0;
        /* hardware syncs tstates to falling of NMI pulse (?),
        * so a slight kludge here...
        */
        if(fetch(pc&0x7fff)==0x76)
        {
          pc++;
          tstates=linestart;
        }
        else
        {
          /* this seems curiously long, but equally, seems
          * to be just about right. :-)
          */
          tstates+=26;
          if (!(frames&0x7))
              tstates--;
        }
        push2(pc);
        pc=0x66;
      }
    }

    if(intsample && intpend)
    {
      intpend=0;

      if(iff1)
      {
  /*      printf("int line %d tst %d\n",liney,tstates);*/
        if(fetch(pc&0x7fff)==0x76)pc++;
        iff1=iff2=0;
        tstates+=5; /* accompanied by an input from the data bus */
        switch(im)
        {
          case 0: /* IM 0 */
          case 1: /* undocumented */
          case 2: /* IM 1 */
            /* there is little to distinguish between these cases */
            tstates+=8; /* perhaps */
            push2(pc);
            pc=0x38;
          break;
          case 3: /* IM 2 */
            /* Used by some hires drivers */
            tstates+=11; /* perhaps */
            int addr=fetch2((i<<8)|0xff);
            push2(pc);
            pc=addr;
        }
      }
    }

    /* this isn't used for any sort of Z80 interrupts,
    * purely for the emulator's UI.
    */
    if(interrupted)
    {
      if(interrupted==1)
      {
        do_interrupt();	/* also zeroes it */
      }
#ifdef SZ81	/* Added by Thunor */
        /* I've added these new interrupt types to support a thorough
        * emulator reset and to do a proper exit i.e. back to main */
      else if(interrupted==INTERRUPT_EMULATOR_RESET ||
              interrupted==INTERRUPT_EMULATOR_EXIT)
      {
        return;
      }
#endif
      else	/* must be 2 */
      {
        /* a kludge to let us do a reset */
        interrupted=0;
        a=f=b=c=d=e=h=l=a1=f1=b1=c1=d1=e1=h1=l1=i=iff1=iff2=im=r=0;
        ixoriy=new_ixoriy=0;
        ix=iy=sp=pc=0;
        tstates=radjust=0;
        nextlinetime=linegap;
        vsyncpend=vsynclen=0;
        hsyncskip=0;
      }
    }
#ifdef  SZ81
  }
#endif
}

#ifdef SZ81	/* Added by Thunor */
void z80_resetx(void)
{
  /* Reinitialise variables at the top of z80.c */
  tstates=0;
  frames=0;
  liney=0;
  vsy=0;
  linestart=0;
  vsync_toggle=0;
  vsync_lasttoggle=0;
  ay_reg=0;
}
#endif

////// 2.3.12 start
BYTE zx81_opcode_fetch_org(int Address)
{
	int inv;
	int opcode, bit6, update=0;
	BYTE data;

	if (Address<32768)
	{
		// This is not video related, so just return the opcode
		data = fetch(Address);
		return(data);
	}

	// We can only execute code below M1NOT.  If an opcode fetch occurs
	// above M1NOT, we actually fetch (address&32767).  This is important
	// because it makes it impossible to place the display file in the
	// 48-64k region if a 64k RAM Pack is used.  How does the real
	// Hardware work?

	//data = mem[(Address>=0xc000)?Address&0x7fff:Address];
  data = fetchm(Address);
	opcode=data;
	bit6=opcode&64;

	// Since we got here, we're generating video (ouch!)
	// Bit six of the opcode is important.  If set, the opcode
	// gets executed and nothing appears onscreen.  If unset
	// the Z80 executes a NOP and the code is used to somehow
	// generate the TV picture (exactly how depends on which
	// display method is used)

	if (!bit6) opcode=0;
	inv = data&128;

	// First check for WRX graphics.  This is easy, we just create a
	// 16 bit Address from the IR Register pair and fetch that byte
	// loading it into the video shift register.
	if (i>=0x20 && !bit6)
	{
		data=mem[(i<<8) | (r & 128) | ((radjust) & 127)];
		update=1;
	}
	else if (!bit6)
	{
		// If we get here, we're generating normal Characters
		// (or pseudo Hi-Res), but we still need to figure out
		// where to get the bitmap for the character from

		// First try to figure out which character set we're going
		// to use if CHR$x16 is in use.  Else, standard ZX81
		// character sets are only 64 characters in size.

		if (UDGEnabled)
			data = ((data&128)>>1)|(data&63);
		else
			data = data&63;

		// If I points to ROM, OR I points to the 8-16k region for
		// CHR$x16, we'll fetch the bitmap from there.
		// Lambda and the QS Character board have external memory
		// where the character set is stored, so if one of those
		// is enabled we better fetch it from the dedicated
		// external memory.
		// Otherwise, we can't get a bitmap from anywhere, so
		// display 11111111 (??What does a real ZX81 do?).

		if (i<64)
		{
			if (UDGEnabled)
				data=font[(data<<3) + rowcounter];
			else
				data=mem[(((i&254)<<8) + (data<<3)) + rowcounter];
		}
		else
		{
			data=255;
		}
		update=1;
    }

	if (update)
	{
		// Update gets set to true if we managed to fetch a bitmap from
		// somewhere.  The only time this doesn't happen is if we encountered
		// an opcode with bit 6 set above M1NOT.

		// Finally load the bitmap we retrieved into the video shift
		// register

		shift_register = inv ? ~data: data;
		return(0);
	}
	else
	{
		// This is the fallthrough for when we found an opcode with
		// bit 6 set in the display file.  We actually execute these
		// opcodes
		return(opcode);
	}
}

//int zx81_writeport(int Address, int Data)
unsigned int out(int h,int l,int a)
{
	switch(l)
	{
    case 0x0f:
      if(sound_ay && sound_ay_type==AY_TYPE_ZONX)
        sound_ay_write(ay_reg, a);
		break;

    case 0x1f:
      // Only one sound chip
		break;

    case 0xbf:
    case 0xcf:
    case 0xdf:
    if(sound_ay && sound_ay_type==AY_TYPE_ZONX)
      ay_reg=(a&15);
		break;

    case 0xfb:
	        //Data = printer_inout(1,Data);
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
		//		printf("Unhandled port write: %d\n", Address);
    break;
	}
	if (LastInstruction == LASTINSTNONE) LastInstruction=LASTINSTOUTFF;
  return 0;
}


//BYTE zx81_readport(int Address)
unsigned int in(int h, int l)
{
	int ts=0;               /* additional cycles*256 */
	static int tapemask=0;
	int data=0;             /* = 0x80 if no tape noise (?) */
	//int h, l;

	tapemask++;
	data |= (tapemask & 0x0100) ? 0x80 : 0;

	//h = Address >> 8;
	//l = Address & 0xff;

	//if (Address==0x7fef)
	//{
	//	return 255;	// no chroma
	//}
		
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
	return(255);
}

/* Normally, these sync checks are done by the TV :-) */
void checkhsync(int tolchk)
{
	if ( ( !tolchk && sync_len >= HSYNC_MINLEN && sync_len <= HSYNC_MAXLEN && RasterX>=HSYNC_TOLERANCEMIN ) ||
	     (  tolchk &&                                                         RasterX>=HSYNC_TOLERANCEMAX ) )
	{
		RasterX = (hsync_counter - HSYNC_END) << 1;
		RasterY++;
		dest += TVP;
	}
}

void checkvsync(int tolchk)
{
	if ( ( !tolchk && sync_len >= VSYNC_MINLEN && RasterY>=VSYNC_TOLERANCEMIN ) ||
	     (  tolchk &&                             RasterY>=VSYNC_TOLERANCEMAX ) )
	{
		RasterY = 0;
		dest = 0;

		if (sync_len>tsmax)
		{
			// If there has been no sync for an entire frame then blank the screen
			memset(scrnbmp, 0xff, ZX_VID_FULLHEIGHT * ZX_VID_FULLWIDTH / 8);
			sync_len = 0;
		}
		else
		{
			memcpy(scrnbmp,scrnbmp_new,sizeof(scrnbmp));
		}
		memset(scrnbmp_new, 0x00, ZX_VID_FULLHEIGHT * ZX_VID_FULLWIDTH / 8);
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
			VSYNC_state = 2; // will be reset by HSYNC circuitry
		else
			VSYNC_state = 0;
		vsync_lower();
	}
}

/* Rewritten zx81_do_scanlines() and AccurateDraw()  */
int zx81_do_scanlines(int tstotal)
{
    int ts;
	int tswait;

	do
	{
		/* at this point, z80.pc points to the instruction to be executed;
		so if nmi or int is pending, the RST instruction with the right number of tstates
		is emulated */

		ts = 0;

    if(intsample && !(radjust&64))
      int_pending=1;

    ixoriy=new_ixoriy;
    new_ixoriy=0;
    intsample=1;
    //op = fetchm(pc);
    op = zx81_opcode_fetch_org(pc);

		if (int_pending && !nmi_pending)
		{
			ts = z80_interrupt(0);
			hsync_counter = -2;             /* INT ACK after two tstates */
			hsync_pending = 1;              /* a HSYNC may be started */
		}

		if (nmi_pending)
		{
			ts = z80_nmi(0);
		}

		LastInstruction = LASTINSTNONE;
		if (!nmi_pending && !int_pending)
		{
			//z80.pc.w = PatchTest(z80.pc.w);
      pc++;
      radjust++;

      int save_ts = tstates;
      switch(op)
      {
#include "z80ops.c"
      }
      ts = tstates - save_ts;
			//ts = z80_do_opcode();
		}
		nmi_pending = int_pending = 0;

		//tstates += ts;

		/* check iff1 even though it is checked in z80_interrupt() */
		if (!((r-1) & 64) && iff1)
		{
			//int_pending = 1;
		}

		switch(LastInstruction)
		{
			case LASTINSTOUTFD:
				NMI_generator = nmi_pending = 0;
				anyout();
			break;
			case LASTINSTOUTFE:
				if (zx80)
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

		/* do what happened during the last instruction */

		/* Plot data in shift register */
		if (SYNC_signal)
		{
			int k = TVP + dest + RasterX;

			if (shift_register &&
			    (RasterX < ZX_VID_FULLWIDTH) &&
				(k < ZX_VID_FULLWIDTH*ZX_VID_FULLHEIGHT))
			{
				int kh = k >> 3;
				int kl = k & 7;

				if (kl)
				{
					scrnbmp_new[kh++]|=(shift_register>>kl);
					scrnbmp_new[kh]=(shift_register<<(8-kl));
				}
				else
				{
					scrnbmp_new[kh]=shift_register;
				}
			}
		}
		shift_register = 0;

		const static int tstate_jump = 8; // Step up to 8 tstates at a time
		int tstate_inc;
		int states_remaining = ts;
		int since_hstart = 0;

#ifdef DEBUG_PRINT
		static int print_debug = 0;
		static int dump_debug = 0;
		static int clear_debug = 0;
		tswait = 0;
#endif
		do
		{
			tstate_inc = states_remaining > tstate_jump ? tstate_jump: states_remaining;
			states_remaining -= tstate_inc;

			hsync_counter+=tstate_inc;
			RasterX += (tstate_inc<<1);

			if (hsync_counter >= 207)
			{
				hsync_counter -= 207;
				if (zx80) hsync_pending = 1;
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
					tstates += ts;
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
					VSYNC_state = 0;
				HSYNC_state = 0;
				hsync_pending = 0;
			}

			// NOR the vertical and horizontal SYNC states to create the SYNC signal
			SYNC_signal = (VSYNC_state || HSYNC_state) ? 0 : 1;
			checksync(since_hstart ? since_hstart : tstate_jump);
			since_hstart = 0;
#ifdef DEBUG_PRINT
			if (RasterY == 0)
			{
				if (dump_debug)
				{
					print_debug = 1;
					dump_debug = 0;
				}
				else if (clear_debug ==1 )
				{
					clear_debug = 0;
					print_debug = 0;
				}
			}

			if (print_debug && RasterX ==0)
			{
				printf("Y = %i, ts = %i, remaining %i, wait %i, hscount %i, sync %c\n",
				       RasterY, ts, states_remaining, tswait, hsync_counter, SYNC_signal ? 'Y' : 'N');
				tswait = 0;
				if (RasterY > 0)
					clear_debug = 1;
			}
#endif			
		}
		while (states_remaining);
		tstotal -= ts;

		if (tstates >= tsmax)
		{
			frame_pause();
			frames++;
			tstates -= tsmax;
		}

	} while (tstotal>0);

    return(tstotal);
}

/* (Modified) EightyOne code ends here */

void zx81_initialise(void)
{
/* Just to avoid changing the variable name in the EightyOne code;
   note that memattr[] is not used (perhaps TODO) */

/* Configuration variables used in EightyOne code */


	tsmax = 65000; //machine.tperframe;

/* Initialise Accurate Drawing */

	RasterX = 0;
	RasterY = 0;
	dest = 0;
	psync = 1;
	sync_len = 0;
	TVP = ZX_VID_X_WIDTH;

/* ULA */

	NMI_generator=0;
	int_pending=0;
	hsync_pending=0;
	VSYNC_state=HSYNC_state=0;
	
	z80_init();
	z80_reset();
}

void mainloop()
{
#ifdef SZ81	/* Added by Thunor */
	if(sdl_emulator.autoload)
	{
  		sdl_emulator.autoload=0;
  		/* This could be an initial autoload or a later forcedload */
  		if(!sdl_load_file(0,LOAD_FILE_METHOD_DETECT))
    	/* wait for a real frame, to avoid an annoying frame `jump'. */
	  		;	  // perhaps TODO    framewait=1;
  	}
#endif

	tstates = 0;

	while (1)
	{
		zx81_do_scanlines(64153);

		/* this isn't used for any sort of Z80 interrupts,
		* purely for the emulator's UI.
		*/
		if(interrupted)
		{
			if(interrupted==1)
			{
				do_interrupt();	/* also zeroes it */
			}
#ifdef SZ81	/* Added by Thunor */
		/* I've added these new interrupt types to support a thorough
		* emulator reset and to do a proper exit i.e. back to main */
			else if(interrupted==INTERRUPT_EMULATOR_RESET ||
					interrupted==INTERRUPT_EMULATOR_EXIT)
			{
				return;
			}
#else
			else	/* must be 2 */
			{
				/* a kludge to let us do a reset */
			}
#endif
		}
	}
}

/* Process a z80 maskable interrupt */
int z80_interrupt( int ts )
{
  int_pending = 0;
  /* Process if IFF1 set */
  if(iff1)
  {
    if( z80halted )
    {
      pc++;
      z80halted = 0;
    }

    iff1=iff2=0;
    push2(pc);

    //writebyte( --SP, PCH ); writebyte( --SP, PCL );
    r++;

    switch(im)
    {
      case 0: pc = 0x0038; return(13);
      case 1: pc = 0x0038; return(13);
      case 2: pc = 0x0038; return(13);
      case 3:
      {
        int addr=fetch2((i<<8)|0xff);
        pc=addr;

        //WORD inttemp=(0x100*i)+0xff;
        //PCL = readbyte(inttemp++); PCH = readbyte(inttemp);
        return(19);
      }
      default: 
        return(12);
    }
  }
  return 0;
}

/* Process a z80 non-maskable interrupt */
int z80_nmi( int ts )
{
  iff1 = 0;

  if (z80halted)
  {
    z80halted=0;
    pc++;
  }
  push2(pc);
  pc=0x66;
  //writebyte( --SP, PCH ); writebyte( --SP, PCL );
  r++;
  //PC = 0x0066;
  return(11);
}

void z80_init(void)
{
  // empty
}

void z80_reset(void)
{
  a=f=b=c=d=e=h=l=a1=f1=b1=c1=d1=e1=h1=l1=i=iff1=iff2=im=r=0;
  ixoriy=new_ixoriy=0;
  ix=iy=sp=pc=0;
  tstates=radjust=0;
  nextlinetime=linegap;
  frames=0;
  liney=0;
  vsy=0;
  linestart=0;
  vsync_toggle=0;
  vsync_lasttoggle=0;
  ay_reg=0;
  z80halted=0;
}
