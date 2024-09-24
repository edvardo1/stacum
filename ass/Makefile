CC=gcc
CFLAGS=-Wall -O1
DEPS = 
OBJ = main.o util.o

stcass: $(OBJ)
	$(CC) $(CFLAGS) -o $@ $^ 

%.o: %.c $(DEPS)
	$(CC) $(CFLAGS) -c -o $@ $<

util.o: ../common/util.c $(DEPS)
	$(CC) $(CFLAGS) -c -o $@ $<

