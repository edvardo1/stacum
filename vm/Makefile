CC=gcc
CFLAGS= -Wall -Wextra -O1
OBJ = stacum.o util.o

stacum: $(OBJ)
	$(CC) $(CFLAGS) -o $@ $^ 

%.o: %.c $(DEPS)
	$(CC) $(CFLAGS) -c -o $@ $<

util.o: ../common/util.c
	$(CC) $(CFLAGS) -c -o $@ $<
