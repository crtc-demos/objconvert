CC = gcc 
LIBS = -lm
CFILES = stripe.c local.c newpolve.c options.c output.c outputex.c queue.c sgi_triang.c sgi_triangex.c struct.c structex.c ties.c add.c common.c free.c init.c util.c partial.c

CFLAGS =  -O0 -g

OFILES = stripe.o local.o newpolve.o options.o output.o outputex.o queue.o sgi_triang.o sgi_triangex.o struct.o structex.o ties.o add.o common.o free.o init.o util.o partial.o


strips:  $(OFILES) 
	$(CC) $(CFLAGS) $(OFILES) -o stripe $(LIBS)

lint: $(CFILES)
	lint $(CFILES) -o stripe $(LIBS)

clean:
	rm -rf *.o stripe

