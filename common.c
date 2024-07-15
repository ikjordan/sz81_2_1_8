/* z81/xz81, Linux console and X ZX81/ZX80 emulators.
 * Copyright (C) 1994 Ian Collier. z81 changes (C) 1995-2004 Russell Marks.
 * sz81 Copyright (C) 2007-2011 Thunor <thunorsif@hotmail.com>
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
 * common.c - various routines/vars common to z81/xz81/sz81.
 */

#define Z81_VER		"2.1"

#include <string.h>

#include "sdl.h"

#include "common.h"
#include "sound.h"
#include "z80.h"
#include "allmain.h"

unsigned char mem[65536],*helpscrn;
unsigned char keyports[9]={0xff,0xff,0xff,0xff, 0xff,0xff,0xff,0xff, 0xff};

/* this two work on a per-k basis, so we can support 1k etc. properly */
unsigned char *memptr[64];
int memattr[64];

int help=0;
int sound=0;
int sound_vsync=0,sound_ay=0,sound_ay_type=AY_TYPE_NONE;
int vsync_visuals=1;
int invert_screen=0;

/* Variables set from SDL menus */
bool m1not = false;
bool useWRX = true;
bool UDGEnabled = false;
bool useQSUDG = false;
bool LowRAM = true;
bool chr128 = false;

/* Variable set from command line options*/
bool useNTSC = false;
bool centreScreen = true;
bool fullDisplay = false;
bool fiveSevenSix = false;
bool romLoad = false;
bool romSave = false;
int vertTol = 30;

int signal_int_flag=0;
volatile int exit_program_flag=0;
int interrupted=0;
int scrn_freq=2;

/* for the printer */
/* This solves an issue for us with zxpopen. Originally the printer file
 * (if requested via -p on the CLI) is created right at the start and
 * it could also remain empty if nothing was printed to it, but we want
 * to create a uniquely named file only when it is printed to. The issue
 * with the default behaviour is that zxpopen initialises some variables
 * that are used within printer_inout which is always called regardless
 * of an open file, so they need to be initialised here from the outset.
 * zxpopen is now called from within zxpout (for sz81) and therefore
 * printer_inout gets called first.
 *
 * I don't know why static is being used here as it is protecting the
 * variables from being modified from outside this file, but they're
 * not being accessed from outside this file anyway. I think this code
 * may have been copied from xz80. Also zxpframes and zxpcycles aren't
 * being initialised so I'm setting them to zero too */
static int zxpframes=0,zxpcycles=0,zxpspeed=0,zxpnewspeed=0;
static int zxpheight=0,zxppixel=-1,zxpstylus=0;
static FILE *zxpfile=NULL;
char *zxpfilename=NULL;
static unsigned char zxpline[256];

/* This is actually redundant as z81's load selector has been entirely
 * replaced, but a load selector component still exists within sz81 that
 * is currently always off and so I'll leave this here for the moment
 * and mark it temp temp */
int load_selector_state = 0;

int refresh_screen=1;

/* > 0 if emulating ZX80 hardware rather than ZX81 */
int zx80=0;
int rom4k=0;

int ignore_esc=0;

int autolist=0;
int autoload=0;
char autoload_filename[1024];
int chromamode=0;
unsigned char bordercolour=0x0f;
unsigned char bordercolournew=0x0f;
unsigned char fullcolour=0xff;
unsigned char chroma_set=0;


/* not too many prototypes needed... :-) */





void loadrom(void)
{
/* sz81 has already preloaded the ROMs so now this function
 * is simply copying the data into the ROM area afresh */
if(rom4k && zx80)
  {
  memcpy(mem,sdl_zx80rom.data,4096);
  }
else
  {
  memcpy(mem,sdl_zx81rom.data,8192);
  }
}

RomPatches_T rom_patches;

#ifdef LOAD_AND_SAVE
RomPatches_T rom_patches;
#endif

void rom8kPatches()
{
#ifdef LOAD_AND_SAVE
  rom_patches.save.start = SAVE_START_8K;
  rom_patches.save.use_rom = romSave;
  rom_patches.load.start = LOAD_START_8K;
  rom_patches.load.use_rom = romLoad;
  rom_patches.retAddr = LOAD_SAVE_RET_8K;
  rom_patches.rstrtAddr = LOAD_SAVE_RSTRT_8K;
#else
  /* patch save routine */
  mem[0x2fc]=0xed; mem[0x2fd]=0xfd;
  mem[0x2fe]=0xc3; mem[0x2ff]=0x07; mem[0x300]=0x02;

  /* patch load routine */
  mem[0x347]=0xeb;
  mem[0x348]=0xed; mem[0x349]=0xfc;
  mem[0x34a]=0xc3; mem[0x34b]=0x07; mem[0x34c]=0x02;
#endif
}

void rom4kPatches()
{
#ifdef LOAD_AND_SAVE
  rom_patches.save.start = SAVE_START_4K;
  rom_patches.save.use_rom = romSave;
  rom_patches.load.start = LOAD_START_4K;
  rom_patches.load.use_rom = romLoad;
  rom_patches.retAddr = LOAD_SAVE_RET_4K;
  rom_patches.rstrtAddr = LOAD_SAVE_RSTRT_4K;
#else
  /* patch save routine */
  mem[0x1b6]=0xed; mem[0x1b7]=0xfd;
  mem[0x1b8]=0xc3; mem[0x1b9]=0x83; mem[0x1ba]=0x02;

  /* patch load routine */
  mem[0x206]=0xed; mem[0x207]=0xfc;
  mem[0x208]=0xc3; mem[0x209]=0x83; mem[0x20a]=0x02;
#endif
}

Display_T disp;

void initdisplay(void)
{
  if (fullDisplay)
  {
    disp.width = DISPLAY_F_WIDTH;
    disp.height = DISPLAY_F_HEIGHT;
    disp.start_x = DISPLAY_F_START_X;
    disp.start_y = DISPLAY_F_START_Y;
    disp.adjust_x = DISPLAY_F_PIXEL_OFF;
  }
  else if (fiveSevenSix)
  {
    disp.width = DISPLAY_P_WIDTH;
    disp.height = DISPLAY_P_HEIGHT;
    disp.start_x = DISPLAY_P_START_X;
    disp.start_y = DISPLAY_P_START_Y;
    disp.adjust_x = DISPLAY_P_PIXEL_OFF;
  }
  else
  {
    disp.width = DISPLAY_N_WIDTH;
    disp.height = DISPLAY_N_HEIGHT;
    disp.start_x = DISPLAY_N_START_X;
    disp.start_y = DISPLAY_N_START_Y;
    disp.adjust_x = DISPLAY_N_PIXEL_OFF;
  }

  disp.padding = DISPLAY_PADDING;
  disp.stride_bit = (disp.padding << 3) + disp.width;
  disp.stride_byte = disp.stride_bit >> 3;
  disp.length = disp.stride_byte * disp.height;
  disp.end_x = disp.start_x + disp.width;
  disp.end_y = disp.height + disp.start_y;
  disp.offset = -(disp.stride_bit * disp.start_y) - disp.start_x;
}

void initmem()
{
int f;
int ramsize;
int count;
int gap = 0;  // For 3K total RAM

// Set ROM type
rom4k = (zx80 == 1) ? 1 : 0;

loadrom();
if(rom4k && zx80)
  {
  memset(mem+0x1000,0,0xf000);
  }
else
  {
  memset(mem+0x2000,0,0xe000);
  }

/* ROM setup */
count=0;
for(f=0;f<16;f++)
  {
  memattr[f]=memattr[32+f]=0;
  memptr[f]=memptr[32+f]=mem+1024*count;
  count++;
  if(count>=((rom4k && zx80)?4:8)) count=0;
  }

/* RAM setup */
ramsize=sdl_emulator.ramsize;

useWRX = (sdl_emulator.wrx != HIRESDISABLED);
useWRX = useWRX || (ramsize < 3);

if ((ramsize == 56) || (ramsize == 40) || (ramsize == 24))
{
  ramsize -= 8;
  LowRAM = true;
}
else
{
  LowRAM = chr128 || useQSUDG;
}
count=0;
if (ramsize==3)
{
  ramsize=4;
  gap = 1;
}
for(f=16;f<32;f++)
  {
  memattr[f]=memattr[32+f]=1;
  memptr[f]=memptr[32+f]=mem+1024*(16+((gap && count==3) ? 2 : count));
  count++;
  if(count>=ramsize) count=0;
  }

/* z81's ROM and RAM initialisation code is OK for <= 16K RAM but beyond
 * that it requires a little tweaking.
 *
 * The following diagram shows the ZX81 + 8K ROM. The ZX80 version is
 * the same except that each 8K ROM region will contain two copies of
 * the 4K ROM.
 *
 * RAM less than 16K is mirrored throughout the 16K region.
 *
 * The ROM will only detect up to 8000h when setting RAMTOP, therefore
 * having more than 16K RAM will require RAMTOP to be set by the user
 * (or user program) to either 49152 for 32K or 65535 for 48/56K.
 *
 *           1K to 16K       32K           48K           56K      Extra Info.
 *
 *  65535  +----------+  +----------+  +----------+  +----------+
 * (FFFFh) | 16K RAM  |  | 16K RAM  |  | 16K RAM  |  | 16K RAM  | DFILE can be
 *         | mirrored |  | mirrored |  |          |  |          | wholly here.
 *         |          |  |          |  |          |  |          |
 *         |          |  |          |  |          |  |          | BASIC variables
 *         |          |  |          |  |          |  |          | can go here.
 *  49152  +----------+  +----------+  +----------+  +----------+
 * (C000h) | 8K ROM   |  | 16K RAM  |  | 16K RAM  |  | 16K RAM  | BASIC program
 *         | mirrored |  |          |  |          |  |          | is restricted
 *  40960  +----------+  |          |  |          |  |          | to here.
 * (A000h) | 8K ROM   |  |          |  |          |  |          |
 *         | mirrored |  |          |  |          |  |          |
 *  32768  +----------+  +----------+  +----------+  +----------+
 * (8000h) | 16K RAM  |  | 16K RAM  |  | 16K RAM  |  | 16K RAM  | No machine code
 *         |          |  |          |  |          |  |          | beyond here.
 *         |          |  |          |  |          |  |          |
 *         |          |  |          |  |          |  |          | DFILE can be
 *         |          |  |          |  |          |  |          | wholly here.
 *  16384  +----------+  +----------+  +----------+  +----------+
 * (4000h) | 8K ROM   |  | 8K ROM   |  | 8K ROM   |  | 8K RAM   |
 *         | mirrored |  | mirrored |  | mirrored |  |          |
 *   8192  +----------+  +----------+  +----------+  +----------+
 * (2000h) | 8K ROM   |  | 8K ROM   |  | 8K ROM   |  | 8K ROM   |
 *         |          |  |          |  |          |  |          |
 *      0  +----------+  +----------+  +----------+  +----------+
 */

switch(ramsize)
  {
  case 48:
    for(f=48;f<64;f++)
      {
      memattr[f]=1;
      memptr[f]=mem+1024*f;
      }
  case 32:
    for(f=32;f<48;f++)
      {
      memattr[f]=1;
      memptr[f]=mem+1024*f;
      }
    break;
  }

  if (LowRAM)
  {
      for(f=8;f<16;f++)
      {
        memattr[f]=1;         /* It's now writable */
        memptr[f]=mem+1024*f;
      }
  }
  useQSUDG = (sdl_emulator.chrgen == CHRGENQS);
  chr128 = (sdl_emulator.chrgen == CHRGENCHR16);
  m1not = (sdl_emulator.m1not != 0);
  UDGEnabled = false;

if(rom4k && zx80)
  rom4kPatches();
else
  rom8kPatches();
}


void zxpopen(void)
{
static int failcnt = 0;

zxpstylus=zxpspeed=zxpheight=zxpnewspeed=0;
zxppixel=-1;

if(!zxpfilename)
  return;

if((zxpfile=fopen(zxpfilename,"wb"))==NULL)
  {
  if (failcnt++ < 10)
    fprintf(stderr,"z81: couldn't open printer file, printing disabled\n");
  return;
  }

/* we reserve 10 chars for height */
fprintf(zxpfile,"P4\n256 %10d\n",0);
}


void zxpupdateheader(void)
{
long pos;

if(!zxpfile || !zxpheight) return;

pos=ftell(zxpfile);

/* seek back to write the image height */
if(fseek(zxpfile,strlen("P4\n256 "),SEEK_SET)!=0)
  fprintf(stderr,"z81: warning: couldn't seek to write image height\n");
else
  {
  /* I originally had spaces after the image height, but that actually
   * breaks the format as defined in pbm(5) (not to mention breaking
   * when read by zgv :-)). So they're now before the height.
   */
  fprintf(zxpfile,"%10d",zxpheight);
  }

if(fseek(zxpfile,pos,SEEK_SET)!=0)
  {
  fprintf(stderr,"z81: error: printer file re-seek failed, printout disabled!\n");
  fclose(zxpfile);
  zxpfile=NULL;
  }
}


void zxpclose(void)
{
unsigned long tmp;
int f;

/* stop the printer */
tmp=tstates;
tstates=tsmax;
out(0,0xfb,4);
tstates=tmp;

if(!zxpfile)
  return;

/* a blank line if we haven't actually printed anything :-) */
if(!zxpheight)
  {
  zxpheight++;
  for(f=0;f<32;f++)
    fputc(0,zxpfile);
  }

/* write header */
zxpupdateheader();

fclose(zxpfile);
zxpfile=NULL;
}


void zxpout(void)
{
int i,j,d;

if(!zxpfile)
	zxpopen();
    if(!zxpfile) return;

zxpheight++;
for(i=0;i<32;i++)
  {
  for(d=j=0;j<8;j++)
    d=(d<<1)+(zxpline[i*8+j]?1:0);
  fputc(d,zxpfile);
  }
}



/* ZX Printer support, transliterated from IMC's xz80 by a bear of
 * very little brain. :-) Or at least, I don't grok it that well.
 * It works wonderfully though.
 */
int printer_inout(int is_out,int val)
{
/* Note that we go through the motions even if printer support isn't
 * enabled, as the alternative would seem to be to crash... :-)
 */
if(!is_out)
  {
  /* input */

  if(!zxpspeed)
    return 0x3e;
  else
    {
    int frame=frames-zxpframes;
    int cycles=tstates-zxpcycles;
    int pix=zxppixel;
    int sp=zxpnewspeed;
    int x,ans;
    int cpp=440/zxpspeed;

    if(frame>400)
      frame=400;
    cycles+=frame*tsmax;
    x=cycles/cpp-64;        /* x-coordinate reached */

    while(x>320)
      {           /* if we are on another line, */
      pix=-1;              /* find out where we are */
      x-=384;
      if(sp)
        {
        x=(x+64)*cpp;
        cpp=440/sp;
        x=x/cpp-64;
        sp=0;
        }
      }
    if((x>-10 && x<0) | zxpstylus)
      ans=0xbe;
    else
      ans=0x3e;
    if(x>pix)
      ans|=1;
    return ans;
    }
  }


/* output */

if(!zxpspeed)
  {
  if(!(val&4))
    {
    zxpspeed=(val&2)?1:2;
    zxpframes=frames;
    zxpcycles=tstates;
    zxpstylus=val&128;
    zxppixel=-1;
    }
  }
else
  {
  int frame=frames-zxpframes;
  int cycles=tstates-zxpcycles;
  int i,x;
  int cpp=440/zxpspeed;

  if(frame>400)
    frame=400; /* limit height of blank paper */
  cycles+=frame*tsmax;
  x=cycles/cpp-64;        /* x-coordinate reached */
  for(i=zxppixel;i<x && i<256;i++)
    if(i>=0)		/* should be, but just in case */
      zxpline[i]=zxpstylus;
  if(x>=256 && zxppixel<256)
    zxpout();

  while(x>=320)
    {          /* move to next line */
    zxpcycles+=cpp*384;
    if(zxpcycles>=tsmax)
      zxpcycles-=tsmax,zxpframes++;
    x-=384;
    if(zxpnewspeed)
      {
      zxpspeed=zxpnewspeed;
      zxpnewspeed=0;
      x=(x+64)*cpp;
      cpp=440/zxpspeed;
      x=x/cpp-64;
      }
    for(i=0;i<x && i<256;i++)
      zxpline[i]=zxpstylus;
    if(x>=256)
      zxpout();
    }
  if(x<0)
    x=-1;
  if(val&4)
    {
    if(x>=0 && x<256)
      {
      for(i=x;i<256;i++)
        zxpline[i]=zxpstylus;
      zxpout();
      }
    zxpspeed=zxpstylus=0;

    /* this is pretty frequent (on a per-char-line basis!),
     * but it's the only time we can really do it automagically.
     */
    zxpupdateheader();
    }
  else
    {
    zxppixel=x;
    zxpstylus=val&128;
    if(x<0)
      zxpspeed=(val&2)?1:2;
    else
      {
      zxpnewspeed=(val&2)?1:2;
      if(zxpnewspeed==zxpspeed)
        zxpnewspeed=0;
      }
    }
  }

return(0);
}

void do_interrupt()
{
static int count=0;

/* being careful here not to screw up any pending reset... */
if(interrupted==1)
  interrupted=0;

/* only do screen update every 1/Nth */
count++;
if(count>sdl_emulator.frameskip)
  {
  count=0;
  update_scrn();
  }
check_events();	/* on X checks events, on VGA scans kybd */
}


void frame_pause(void)
{
#ifdef OSS_SOUND_SUPPORT
if(sound_enabled)
  {
  /* we block on the sound instead. It's a bit unpleasant,
   * but it's the best way.
   */
  sound_frame();

  }
#endif

while(!signal_int_flag)
  SDL_Delay(10);

signal_int_flag=0;

if(interrupted<2)
  interrupted=1;
}

void common_reset(void)
{
int count;

/* Reinitialise variables at the top of common.c that aren't unused
 * options, being managed by sz81 or in init functions elsewhere */
for(count=0;count<9;count++)
  keyports[count]=0xff;
signal_int_flag=0;
zxpframes=zxpcycles=zxpspeed=zxpnewspeed=0;
zxpheight=0;
zxppixel=-1;
zxpstylus=0;
refresh_screen=1;
}

