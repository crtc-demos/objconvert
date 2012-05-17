	CFLAGS = -g -O0 -Wall -fno-strict-aliasing
	LDFLAGS = -g

	SRC = Adjacency.cpp RevisitedRadix.cpp Striper.cpp CustomArray.cpp \
	      StdAfx.cpp CamlStrips.cpp

	OBJS = Adjacency.o RevisitedRadix.o Striper.o CustomArray.o \
	       StdAfx.o CamlStrips.o

	TARGET = libstrips.so

	LIB = strips.cma
	LIBNAT = strips.cmxa

all:	$(LIB) $(LIBNAT)

$(TARGET): $(OBJS)
	g++ $(LDFLAGS) -shared -fPIC -DPIC $(OBJS) -o $@

$(LIB):	$(TARGET) strips.cmo
	ocamlc -g -a -o $@ strips.cmo -dllib -lstrips

$(LIBNAT):	$(TARGET) strips.o
	ocamlmklib -o $(basename $(LIBNAT)) strips.cmx -L. -lstrips -lstdc++

%.o:	%.cpp
	g++ $(CFLAGS) -shared -fPIC -DPIC -c $< -o $@

%.cmo:	%.ml
	ocamlc -g -c $< -o $@

%.o:	%.ml
	ocamlopt -c $< -o $@

.PHONY: clean
clean:
	rm -f *.o $(TARGET) $(LIB) $(LIBNAT)
