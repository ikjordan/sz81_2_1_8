# May need to increase cc heap
# *SetEval cc1$HeapMax 128

# To convert binary to aif format use
# *elf2aif sz81

# SDL libs and includes assumed to be in SDL under install dir
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
SOURCES=sdl_main.c common.c sound.c z80.c loadp.c sdl_engine.c sdl_hotspots.c \
	sdl_input.c sdl_loadsave.c sdl_resources.c sdl_sound.c sdl_video.c
OBJECT1=sdl_hotspots.o \
	sdl_input.o sdl_loadsave.o sdl_resources.o
OBJECT3=sound.o z80.o loadp.o sdl_sound.o sdl_video.o common.o sdl_main.o sdl_engine.o
OBJECTS=$(OBJECT1) $(OBJECT3)

VERSION=2_1_8a
CC=gcc -static

# These should be ok for most.
CFLAGS+=-Wall -Wno-unused-result -ISDL -DVERSION=\"$(VERSION)\" -DENABLE_EMULATION_SPEED_ADJUST \
	-DPLATFORM_RISCOS -DPACKAGE_DATA_DIR=\"$(PACKAGE_DATA_DIR)\" $(SOUNDDEF) -DSZ81 -D_DZ80_EXCLUDE_SCRIPT -DLOAD_AND_SAVE

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

clean:
	${RM} $(OBJECT1) $(OBJECT3)  sz81

