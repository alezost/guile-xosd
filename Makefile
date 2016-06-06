CC = gcc

CFLAGS = -Wall -ansi -pedantic

TARGET = libguile-xosd.so

SOURCES = xosd_wrap.c

all: $(TARGET)

$(TARGET): $(SOURCES)
	$(CC) $(CFLAGS) -shared -o $(TARGET) -lxosd xosd_wrap.c

clean:
	$(RM) $(TARGET)