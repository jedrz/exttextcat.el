CC := gcc
CFLAGS = -Wall -Wextra -pedantic
LIBS = -lexttextcat-2.0
TARGETFILE = exttextcat
SRCFILE = $(TARGETFILE).c
DESTDIR = $(PWD)
TARGET = $(DESTDIR)/$(TARGETFILE)

all: $(TARGET)

$(TARGET): $(SRCFILE)
	$(CC) -o $(TARGET) $(SRCFILE) $(LIBS) $(CLAGS)

clean:
	rm -f $(TARGET)

PHONY: clean
