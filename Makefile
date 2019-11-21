FC := gfortran
FFLAGS := -O3 -march=native
# FFLAGS := -g -fcheck=all

f08sdl2 := ./f08sdl2
INCLUDE := -I $(f08sdl2) $(shell pkg-config --cflags sdl2)
LDLIBS := -L $(f08sdl2) -lf08sdl2 $(shell pkg-config --libs sdl2)

PROGRAMS := sdltest gameoflife

all: $(PROGRAMS)

$(PROGRAMS): %: %.f90 | f08sdl2
	$(FC) $(INCLUDE) $(FFLAGS) $^ $(LDFLAGS) $(LDLIBS) -o $@

f08sdl2:
	$(MAKE) -C $(f08sdl2) FC="$(FC)" FFLAGS="$(FFLAGS) -fpic"
	$(AR) rcs libf08sdl2.a $(f08sdl2)/sdl2.o

clean:
	$(RM) *.mod *.smod *.o *.a $(PROGRAMS)
	$(MAKE) -C f08sdl2 clean

.PHONY: all f08sdl2 clean
