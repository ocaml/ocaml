FILES=AreaBelow.c  Curves.c      Error.c   Main.c    Reorder.c  TopTwenty.c \
  AuxFile.c    Deviation.c   HpFile.c  Marks.c   Scale.c    TraceElement.c  \
  Axes.c       Dimensions.c  Key.c     PsFile.c  Shade.c    Utilities.c

CFLAGS=-Wall
EXE=../bin/hp2ps

../../hp2ps: $(FILES:.c=.o)
	gcc -o $(EXE)  -lm $(FILES:.c=.o)

clean:
	rm -f *.o $(EXE)

