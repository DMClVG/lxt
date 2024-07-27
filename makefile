P=lxt

SRC=src
BUILD=build

RESOURCES=resources/

SOURCES=core/renderer.c \
	core/renwindow.c \
	core/rencache.c \
	core/api/api.c \
	core/api/renderer.c \
	core/api/system.c \
	core/main.c

HEADERS=core/renderer.h \
	core/renwindow.h \
	core/unidata.h \
	core/utfconv.h \
	core/rencache.h \
	core/api.h

OBJECTS=$(patsubst %.c,$(BUILD)/%.o,$(SOURCES))

CC=gcc
CFLAGS=$(shell pkg-config --cflags sdl2 freetype2 libpcre2-8 guile-3.0)
LDFLAGS=$(shell pkg-config --libs sdl2 freetype2 libpcre2-8 guile-3.0) -lm

.PHONY : all

all : $(P)

$(P) : $(OBJECTS)
	$(CC) -o $@ $(OBJECTS) $(LDFLAGS)

$(BUILD)/%.o : $(SRC)/%.c
	mkdir -p $(basename $@)
	$(CC) -p $(CFLAGS) -I $(RESOURCES) -I src/core -c $< -o $@ $(LDFLAGS)

clean:
	rm -rf $(BUILD) $(P)
