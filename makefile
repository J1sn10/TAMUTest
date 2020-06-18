# ===============================
# GFORTRAN OS X
 FC = gfortran
 FFLAGS = -O3 -fdefault-real-8 -g 
 FFLAGS += -fbounds-check
 MODFLAG = -J
# TARGET_ARCH =
 LDFLAGS = -g
LIBS = 
# ===============================
# ===============================
# GFORTRAN OS X
#  FC = f90
#  FFLAGS = -O3 -g -N113
# # FFLAGS += -Rb
#  MODFLAG = -I
# # TARGET_ARCH =
#  LDFLAGS = -g
#  LIBS = -lU77
# ===============================
EXE   = meshgen
VPATH = mod

.SUFFIXES:
.SUFFIXES: .f90 .o

vpath % main

SRCMOD =	constants.f90 \
            coord.f90 \
            geom.f90

SRCMAIN = 					\
		alloc.f90			\
		main.f90			\
		grdgen.f90          \
		initia.f90          \
		space1.f90          \
		ugrid2.f90          \
		upbndry.f90

OBJECT_DIR = objects

OBJMAIN = ${SRCMAIN:.f90=.o}

OBJMOD = ${SRCMOD:.f90=.o}

OBJ = $(OBJMOD) $(OBJMAIN)

# Add the object directory to the objects and module objects
#  -- 
OBJMAIN2 = ${OBJMAIN:%=$(OBJECT_DIR)/%}
OBJMOD2 = ${OBJMOD:%=$(OBJECT_DIR)/%}
OBJ2 = $(OBJMOD2) $(OBJMAIN2)

$(EXE): $(OBJ2)
	$(FC) $(LDFLAGS) $(OBJ2) $(LIBS) -o $(EXE) $(MODFLAG) $(OBJECT_DIR)

# Sets an order-only dependency on the directory for all objects
#  -- The result of this is that the directory is created before any of the 
#  -- objects are compiled, but the timestamp of the directory is not checked
$(OBJ2) : | $(OBJECT_DIR) 

# Move .mod files to OBJECT_DIR	for any compilers that won't put them there
# automatically (Absoft)
MODFILES: | $(OBJMOD2)
#	$(foreach modfile, $(wildcard *.mod), mv $(modfile) $(OBJECT_DIR);)

# Pattern rule for all object files within OBJECT_DIR
#  -- Compilation creates an object file in OBJECT_DIR, and checks for .mod files 
#  -- within OBJECT_DIR
$(OBJECT_DIR)/%.o  : %.f90
	$(FC) $(FFLAGS) -c $< -o $@ $(MODFLAG)$(OBJECT_DIR) 

# Rule to create OBJECT_DIR - only occurs if OBJECT_DIR does not exist
$(OBJECT_DIR):
	mkdir $(OBJECT_DIR)

# Define dependencies for modules
$(OBJMAIN2): $(OBJMOD2) | MODFILES

clean: 
	rm -f *.mod *~ core
	rm -f *.o
	rm -rf $(OBJECT_DIR)

library: $(OBJ2)
	ar -rs splitstruc.lib $(OBJ2)