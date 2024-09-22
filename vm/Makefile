CC=gcc
CFLAGS= -Wall -Wextra -O1
OBJ = stacum.o util.o stacum_format.o

stacum: $(OBJ)
	$(CC) $(CFLAGS) -o $@ $^ 

%.o: %.c $(DEPS)
	$(CC) $(CFLAGS) -c -o $@ $<

util.o: ../common/util.c
	$(CC) $(CFLAGS) -c -o $@ $<

stacum_format.o: ../common/stacum_format.c
	$(CC) $(CFLAGS) -c -o $@ $<
