# Comment/uncomment these to choose an installation destination
# System wide installation
#PREFIX?=/usr/local
#BINDIR?=$(PREFIX)/bin
#DOCDIR?=$(PREFIX)/share/doc/$(TARGET)
#PACKAGE_DATA_DIR?=$(PREFIX)/share/$(TARGET)

# Local installation within your home folder
#PREFIX?=$(HOME)/Games/$(TARGET)
#BINDIR?=$(PREFIX)
#DOCDIR?=$(PREFIX)/doc
#PACKAGE_DATA_DIR?=$(PREFIX)/data

# Run from current folder i.e. no installation
PREFIX?=.
BINDIR?=$(PREFIX)
DOCDIR?=$(PREFIX)
PACKAGE_DATA_DIR?=$(PREFIX)/data

# For sz81 OSS_SOUND_SUPPORT is now synonymous with SDL_SOUND_SUPPORT.
# Comment this out if you don't want sound support.
SOUNDDEF=-DOSS_SOUND_SUPPORT

# You won't need to alter these
TARGET=sz81
SOURCES=sdl_main.c common.c sound.c z80.c sdl_engine.c sdl_hotspots.c \
	sdl_input.c sdl_loadsave.c sdl_resources.c sdl_sound.c sdl_video.c
OBJECT1=sdl_hotspots.o \
	sdl_input.o sdl_loadsave.o sdl_resources.o
OBJECT3=sound.o z80.o sdl_sound.o sdl_video.o common.o sdl_main.o sdl_engine.o
OBJECTS=$(OBJECT1) $(OBJECT3)

VERSION=2_1_8a
CC=gcc

# These should be ok for most.
# For debugging/profiling uncomment the following two lines:
#CFLAGS=-O0 -g -pg
#LDFLAGS=-pg
#CFLAGS=-O0 -g
CFLAGS+=-Wall -Wno-unused-result -ISDL -DVERSION=\"$(VERSION)\" -DENABLE_EMULATION_SPEED_ADJUST \
	-DPLATFORM_RISCOS -DPACKAGE_DATA_DIR=\"$(PACKAGE_DATA_DIR)\" $(SOUNDDEF) -DSZ81 -D_DZ80_EXCLUDE_SCRIPT
#CFLAGS+=-O1

LINK=$(CC)
LDFLAGS=
LIBS= -LSDL -lSDL

# You won't need to alter anything below
all: $(SOURCES) $(TARGET)

$(TARGET): $(OBJECTS)
	$(LINK) $(LDFLAGS) $(OBJECTS) $(LIBS) -o $@

$(OBJECT1): CGFLAGS = $(CFLAGS) -O1
$(OBJECT3): CGFLAGS = $(CFLAGS) -O3

$(OBJECTS): %.o: %.c
	$(CC) $(CGFLAGS) -c $< -o $@

.PHONY: all clean install

open%:
	-@if [ -n "`which pasmo 2> /dev/null`" ]; then \
		pasmo -v open8x/$@.asm open8x/$@.rom; \
		if [ -f open8x/$@.rom -a ! -e data/zx$*.rom ]; then \
			cp open8x/$@.rom data/zx$*.rom; \
		fi \
	else \
		echo "The Pasmo cross-assembler was not found: skipping $@"; \
	fi

clean:
	rm -f *.o *~

