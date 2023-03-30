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

#define TSTATESPERFRAME 64998
#define TSTATESPERLINE    207
#define TSTATESPERTAPE    147

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


#define ZX_VID_WIDTH    ZX_VID_FULLWIDTH
#define ZX_VID_HEIGHT    ZX_VID_FULLHEIGHT

unsigned long tstates=0,tsmax=TSTATESPERFRAME,frames=0;
unsigned int lastDisplayPos;

/* odd place to have this, but the display does work in an odd way :-) */
unsigned char scrnbmp_new[ZX_VID_FULLWIDTH*ZX_VID_FULLHEIGHT/8]; /* written */
unsigned char scrnbmp[ZX_VID_FULLWIDTH*ZX_VID_FULLHEIGHT/8];	/* displayed */
unsigned char scrnbmp_old[ZX_VID_FULLWIDTH*ZX_VID_FULLHEIGHT/8];
						/* checked against for diffs */

#ifdef SZ81	/* Added by Thunor. I need these to be visible to sdl_loadsave.c */
int liney=0, lineyi=0;
int vsy=0;
unsigned long linestart=0;
int vsync_toggle=0,vsync_lasttoggle=0;
int vsyncstart=0;
#else
static int liney=0, lineyi=0;
static int vsy=0;
static unsigned long linestart=0;
static int vsync_toggle=0,vsync_lasttoggle=0;
#endif

int ay_reg=0;

//static int linestate=0, linex=0, nrmvideo=1;
void runOneTick();

/* for vsync off -> on */
void vsync_raise(void)
{
  return;
/* save current pos */
vsy=liney;
}


/* for vsync on -> off */
void vsync_lower(void)
{
  return;
int ny=liney,y;

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
    memset(scrnbmp_new+y*(ZX_VID_FULLWIDTH/8),0xff,ZX_VID_FULLWIDTH/8);
  vsy=0;
  }

for(y=vsy;y<ny;y++)
  memset(scrnbmp_new+y*(ZX_VID_FULLWIDTH/8),0xff,ZX_VID_FULLWIDTH/8);
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
unsigned long nextlinetime=0,linegap=TSTATESPERFRAME,lastvsyncpend=0;
unsigned char ixoriy, new_ixoriy;
unsigned char intsample=0;
unsigned char op;
int ulacharline=0;
int nmipend=0,intpend=0,vsyncpend=0,vsynclen=0;
int hsyncskip=0;
int framewait=0;

#ifdef SZ81	/* Added by Thunor */
void mainloop()
{
  nextlinetime=0; linegap=TSTATESPERFRAME; lastvsyncpend=0;
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
  nextlinetime=TSTATESPERLINE;

#ifdef SZ81	/* Added by Thunor */
  if(sdl_emulator.autoload)
  {
    sdl_emulator.autoload=0;
    /* This could be an initial autoload or a later forcedload */
    if(!sdl_load_file(0,LOAD_FILE_METHOD_DETECT))
      /* wait for a real frame, to avoid an annoying frame `jump'. */
      framewait=1;
  }

  while(1)
  {
    runOneTick();
    memcpy(scrnbmp,scrnbmp_new,sizeof(scrnbmp));
    memset(scrnbmp_new,0,sizeof(scrnbmp_new));
    frame_pause();

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
  }
}

#ifdef SZ81    /* Added by Thunor */
void z80_reset(void)
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

void runOneTick()
{
  int lastvsync=0;
  int prevvsync=0, curvsync=0;
  while(1)
  {
    // End of a real vsync
    // if(intsample && prevvsync>TSTATESPERFRAME-20*TSTATESPERLINE && lastvsync>400)
    if(intsample && tstates-prevvsync>50*TSTATESPERLINE && lastvsync>400)
    {
      //tapeSignal.origin-=tstates;
      nextlinetime=TSTATESPERLINE;
      vsyncstart=0;
      tstates=0;
      lastDisplayPos=0;
      return;
    }
    // Generate fake sync if we've been here too long
    if(intsample && tstates>=TSTATESPERFRAME)
    {
      if(vsync) vsyncstart=tstates;
      nextlinetime-=tstates;
      vsyncstart-=tstates;
      //tapeSignal.origin-=tstates;
      tstates=0;
      lastDisplayPos=0;
      return;
    }

    /* this *has* to be checked before radjust is incr'd */
    if(intsample && !(radjust&64))
      intpend=1;

    ixoriy=new_ixoriy;
    new_ixoriy=0;
    intsample=1;
    op=fetch(pc&0x7fff);

    if((pc&0x8000) && !(op&64))
    {
      int pos=tstates<<1;
      if(pos<ZX_VID_HEIGHT*ZX_VID_WIDTH)
      {
        unsigned char c=0;
        if(i<0x40)
        {
          c=memptr[i>>2][((i&3)<<8)|((op&63)<<3)|ulacharline];
        }
        else
        {
          c=memptr[i>>2][((i&3)<<8)|radjust];
        }

        if(op&0x80) c=~c;

        int p = pos>>3;
        int b = pos&0x7;
        if (b)
        {
          scrnbmp_new[p++]|=(c>>b);
          scrnbmp_new[p]=(c<<(8-b));
        }
        else
        {
          scrnbmp_new[p]=c;
        }
      }
      op=0;	/* the CPU sees a nop */
    }

    pc++;
    radjust++;

    int pvsync=vsync;

    switch(op)
    {
#include "z80ops.c"
    }

    /* should do this all the time vsync is set */
    if(vsync)
    {
      ulacharline=0;
    }
    else
    {
      if(vsync!=pvsync)
      {
        lastvsync=tstates-vsyncstart;
        prevvsync=curvsync;
        curvsync=tstates;
      }
      vsyncstart=tstates;
    }

    if(tstates>=nextlinetime)    /* new line */
    {
      nextlinetime+=TSTATESPERLINE;

      if(hsyncgen)
      {
/*        printf("hsync %d\n",tstates);*/
        ulacharline++;
        ulacharline&=7;
      }

      if(nmigen)
        nmipend=1;
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
          tstates-=(tstates-nextlinetime+TSTATESPERLINE);
        }
        else
        {
          /* this seems curiously long, but equally, seems
          * to be just about right. :-)
          */
          tstates+=26;
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
        if(fetch(pc&0x7fff)==0x76)
          pc++;
        iff1=iff2=0;

        //tstates+=5; /* accompanied by an input from the data bus */
        tstates+=4; // Well, using 5 here shifted everything, so 4 it is...

        switch(im)
        {
          case 0: /* IM 0 */
          case 1: /* undocumented */
          case 2: /* IM 1 */
            /* there is little to distinguish between these cases */
            tstates+=9; /* perhaps */
            push2(pc);
            pc=0x38;
          break;
          case 3: /* IM 2 */
            /* (seems unlikely it'd ever be used on the '81, but...) */
            tstates+=13; /* perhaps */
            int addr=fetch2((i<<8)|0xff);
            push2(pc);
            pc=addr;
          break;
        }
      }
    }
  }
}
#endif
