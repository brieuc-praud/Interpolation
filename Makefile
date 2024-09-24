ROBUST_PREDICATES ?= 0
DEBUG             ?= 0

F90 := gfortran
CC  := gcc
PYT := python3

EXE       	:= main

ERROR_FILE  := error.dat
TIMES_FILE  := times.dat

OBJ 		  := parameters.o predicatesf90.o miscellaneous.o mesh.o polygon_tools.o interpolation.o $(EXE).o
SOURCE_MESH   := source.mesh
TARGET_MESH   := target.mesh
GENMESHES     := genmeshes.py
PLOTMESHES    := plotmeshes.py
PLOTERROR     := ploterror.py
PLOTTIMES     := plottimes.py

ifeq ($(DEBUG),0)
	F90FLAGS := -O3 -march=native
else
	F90FLAGS := -Og -g -ffpe-trap=invalid,zero,overflow,underflow -fcheck=all -Wall -Wextra -pedantic -fbacktrace
endif

F90FLAGS += -cpp
CFLAGS    = -O3 -march=native

ifneq ($(ROBUST_PREDICATES),0)
	OBJ       := predicates.o $(OBJ) 
	OBJ_TESTS := predicates.o $(OBJ_TESTS) 
	F90FLAGS  += -DROBUST_PREDICATES
endif

all:
	make $(EXE)

$(EXE): $(OBJ)
	$(F90) -o $@ $(VAR) $(F90FLAGS) $^

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

%.o: %.c
	$(CC) -c $<

$(SOURCE_MESH): $(GENMESHES)
	./$(GENMESHES) 100 5 $(SOURCE_MESH) $(TARGET_MESH)
$(TARGET_MESH): $(GENMESHES)
	./$(GENMESHES) 100 5 $(SOURCE_MESH) $(TARGET_MESH)

run: $(EXE) $(SOURCE_MESH) $(TARGET_MESH) $(PLOTMESHES)
		./$(EXE) $(SOURCE_MESH) $(TARGET_MESH)
		./$(PLOTMESHES) $(SOURCE_MESH) $(TARGET_MESH)

run_tests: $(EXE)
	rm -f $(ERROR_FILE) $(TIMES_FILE)
	for i in 16 64 256 1024 4096 ; do \
		echo "-> n=$$i" ; \
		./$(GENMESHES) $$i 5 $(SOURCE_MESH) $(TARGET_MESH) ; \
		echo -n "$$i " >>$(ERROR_FILE) ; \
		echo -n "$$i " >>$(TIMES_FILE) ; \
		bash -c "./$^ $(SOURCE_MESH) $(TARGET_MESH) \
		| tee >(grep 'self-interpolation error' | cut -d':' -f2 | xargs | tr '\n' ' ' >>$(ERROR_FILE)) \
		| tee >(grep 'cross-interpolation error' | cut -d':' -f2 | xargs >>$(ERROR_FILE)) \
		| tee >(grep 'self-interpolation computation time' | cut -d':' -f2 | xargs | tr '\n' ' ' >>$(TIMES_FILE)) \
		| tee >(grep 'cross-interpolation computation time' | cut -d':' -f2 | xargs >>$(TIMES_FILE))" ; \
	done
	./$(PLOTMESHES) $(SOURCE_MESH) $(TARGET_MESH)
	./$(PLOTERROR)  $(ERROR_FILE)
	./$(PLOTTIMES)  $(TIMES_FILE)

clean:
	rm *.o *.mod *.mesh $(EXE) $(ERROR_FILE) $(TIMES_FILE) 2>/dev/null | true
