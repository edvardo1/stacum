CC=gcc
CFLAGS=-Wall -O1
DEPS = 
OBJ = main.o

%.o: %.c $(DEPS)
	$(CC) $(CFLAGS) -c -o $@ $<


stcass: $(OBJ)
	$(CC) $(CFLAGS) -o $@ $^ 
