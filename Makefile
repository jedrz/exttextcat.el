CC := gcc
CFLAGS = -Wall -Wextra -pedantic
LIBS = -lexttextcat-2.0
TARGET = exttextcat
SRCFILE = $(TARGET).c

all: $(TARGET)

$(TARGET): $(SRCFILE)
	$(CC) -o $(TARGET) $(SRCFILE) $(LIBS) $(CLAGS)

clean:
	rm -f $(TARGET)

PHONY: clean
