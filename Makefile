CC = gcc

CFLAGS = -Wall -ansi -pedantic

TARGET = libguile-xosd.so

SOURCE = src/libguile-xosd.c

all: $(TARGET)

$(TARGET): $(SOURCE)
	$(CC) $(CFLAGS) -shared -o $(TARGET) -lxosd $(SOURCE)

clean:
	$(RM) $(TARGET)
