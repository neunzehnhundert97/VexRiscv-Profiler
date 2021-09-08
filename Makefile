REGRESSION_PATH?=./
VEXRISCV_FILE?=VexRiscv.v
IBUS?=CACHED
IBUS_DATA_WIDTH?=32
DBUS?=CACHED
DBUS_DATA_WIDTH?=32
THREAD_COUNT?=$(shell nproc)
MEASUREMENTS?=N
DEP_HASH?=$(VARIANT)

include MyMakefile.mk

ADDCFLAGS += -CFLAGS -DREGRESSION_PATH='\"$(REGRESSION_PATH)/\"'
ADDCFLAGS += -CFLAGS -DIBUS_${IBUS}
ADDCFLAGS += -CFLAGS -DIBUS_DATA_WIDTH=${IBUS_DATA_WIDTH}
ADDCFLAGS += -CFLAGS -DDBUS_DATA_WIDTH=${DBUS_DATA_WIDTH}

ADDCFLAGS += -CFLAGS -DDBUS_${DBUS}
ADDCFLAGS += -CFLAGS -DREDO=${REDO}
ADDCFLAGS += -CFLAGS -pthread
ADDCFLAGS += -CFLAGS -Wno-unused-result

ADDCFLAGS += -CFLAGS -DTHREAD_COUNT=${THREAD_COUNT}
ADDCFLAGS += -CFLAGS -DSTALL=1
ADDCFLAGS += -CFLAGS -O3

ifneq ($(SHOWPC),)
ADDCFLAGS += -CFLAGS -DSHOWPC=1
endif

ifeq ($(DEP_HASH)$(PREFLIGHT),)
DIR:=obj_dir
TARGET_CORE:=VexRiscv.v
else
DIR:=obj_dir_$(DEP_HASH)
TARGET_CORE:=cores/$(DEP_HASH)/VexRiscv.v
endif

ifeq ($(PREFLIGHT),)
ADD_INCLUDE :=
else
ADD_INCLUDE :=../../Profiling/cores/$(DEP_HASH)
endif


all: verilator/$(DIR)/VVexRiscv | folder

folder:
	mkdir -p cores/$(DEP_HASH)
	mkdir -p verilator
	touch cores/$(DEP_HASH)/instructions_c.h
	touch cores/$(DEP_HASH)/instructions.py

verilator/$(DIR)/VVexRiscv.cpp: ${TARGET_CORE} profile.cpp
	cp ${TARGET_CORE}*.bin . | true
	verilator -cc -Mdir verilator/$(DIR) ${TARGET_CORE} --prefix VVexRiscv -O3 -CFLAGS -std=c++11 -LDFLAGS -pthread ${ADDCFLAGS} --gdbbt ${VERILATOR_ARGS} -Wno-UNOPTFLAT -Wno-WIDTH  --exe ../profile.cpp --x-assign unique

verilator/$(DIR)/VVexRiscv: verilator/$(DIR)/VVexRiscv.cpp
	$(MAKE)  -j${THREAD_COUNT} -C verilator/$(DIR)/ -f VVexRiscv.mk VVexRiscv

clean:
	-rm -rf cores/$(DEP_HASH)
	-rm -rf verilator/$(DIR)
	-rm -f *.debugTrace
	-rm -f *.logTrace
