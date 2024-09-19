CC=gcc
CFLAGS= -Wall -Wextra -pedantic -O1 #`pkg-config --cflags --libs sdl2 SDL2_gfx`
DEPS= 
OBJ = stacum.o

%.o: %.c $(DEPS)
	$(CC) $(CFLAGS) -c -o $@ $<

stacum: $(OBJ)
	$(CC) $(CFLAGS) -o $@ $^ 
