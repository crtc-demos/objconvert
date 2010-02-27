	SRC = Adjacency.cpp RevisitedRadix.cpp Striper.cpp CustomArray.cpp \
	      StdAfx.cpp CamlStrips.cpp

	OBJS = Adjacency.o RevisitedRadix.o Striper.o CustomArray.o \
	       StdAfx.o CamlStrips.o

	TARGET = dllstrips.so

	LIB = strips.cma

all:	$(LIB)

$(TARGET): $(OBJS)
	g++ -shared -fPIC -DPIC $(OBJS) -o $@

$(LIB):	$(TARGET) strips.cmo
	ocamlc -a -o $(LIB) strips.cmo -dllib -lstrips

%.o:	%.cpp
	g++ -shared -fPIC -DPIC -O2 -c $< -o $@

%.cmo:	%.ml
	ocamlc -c $< -o $@

.PHONY: clean
clean:
	rm -f *.o $(TARGET)
