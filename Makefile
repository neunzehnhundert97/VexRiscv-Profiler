REGRESSION_PATH?=./
VEXRISCV_FILE?=VexRiscv.v
IBUS?=CACHED
IBUS_DATA_WIDTH?=32
DBUS?=CACHED
DBUS_DATA_WIDTH?=32
THREAD_COUNT?=$(shell nproc)

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

all: obj_dir/VVexRiscv

obj_dir/VVexRiscv.cpp: ${VEXRISCV_FILE}
	cp ${VEXRISCV_FILE}*.bin . | true
	verilator -cc  ${VEXRISCV_FILE}  -O3 -CFLAGS -std=c++11 -LDFLAGS -pthread  ${ADDCFLAGS}  --gdbbt ${VERILATOR_ARGS} -Wno-UNOPTFLAT -Wno-WIDTH --x-assign unique --exe profile.cpp

obj_dir/VVexRiscv: obj_dir/VVexRiscv.cpp
	$(MAKE)  -j${THREAD_COUNT} -C obj_dir/ -f VVexRiscv.mk VVexRiscv

.PHONY: dummies
dummies:
	cd ../dummies/AES-128 && $(MAKE) clean all PROFILE=$(PROFILE) CACHE_OFFSET=$(CACHE_OFFSET)

.PHONY: hqc
hqc:
	cd ../hqc/hqc-128 && $(MAKE) clean all PROFILE=$(PROFILE) VERBOSE=$(VERBOSE) CACHE_OFFSET=$(CACHE_OFFSET)
	cd ../hqc/hqc-192 && $(MAKE) clean all PROFILE=$(PROFILE) VERBOSE=$(VERBOSE) CACHE_OFFSET=$(CACHE_OFFSET)
	cd ../hqc/hqc-256 && $(MAKE) clean all PROFILE=$(PROFILE) VERBOSE=$(VERBOSE) CACHE_OFFSET=$(CACHE_OFFSET)

.PHONY: mceliece
mceliece:
	cd ../McEliece/mceliece348864   && $(MAKE) clean all PROFILE=$(PROFILE) VERBOSE=$(VERBOSE) CACHE_OFFSET=$(CACHE_OFFSET)
	cd ../McEliece/mceliece348864f  && $(MAKE) clean all PROFILE=$(PROFILE) VERBOSE=$(VERBOSE) CACHE_OFFSET=$(CACHE_OFFSET)
	cd ../McEliece/mceliece460896   && $(MAKE) clean all PROFILE=$(PROFILE) VERBOSE=$(VERBOSE) CACHE_OFFSET=$(CACHE_OFFSET)
	cd ../McEliece/mceliece460896f  && $(MAKE) clean all PROFILE=$(PROFILE) VERBOSE=$(VERBOSE) CACHE_OFFSET=$(CACHE_OFFSET)
	#cd ../McEliece/mceliece6688128  && $(MAKE) clean all PROFILE=$(PROFILE) VERBOSE=$(VERBOSE) CACHE_OFFSET=$(CACHE_OFFSET)
	#cd ../McEliece/mceliece6688128f && $(MAKE) clean all PROFILE=$(PROFILE) VERBOSE=$(VERBOSE) CACHE_OFFSET=$(CACHE_OFFSET)
	#cd ../McEliece/mceliece6960119  && $(MAKE) clean all PROFILE=$(PROFILE) VERBOSE=$(VERBOSE) CACHE_OFFSET=$(CACHE_OFFSET)
	#cd ../McEliece/mceliece6960119f && $(MAKE) clean all PROFILE=$(PROFILE) VERBOSE=$(VERBOSE) CACHE_OFFSET=$(CACHE_OFFSET)
	#cd ../McEliece/mceliece8192128  && $(MAKE) clean all PROFILE=$(PROFILE) VERBOSE=$(VERBOSE) CACHE_OFFSET=$(CACHE_OFFSET)
	#cd ../McEliece/mceliece8192128f && $(MAKE) clean all PROFILE=$(PROFILE) VERBOSE=$(VERBOSE) CACHE_OFFSET=$(CACHE_OFFSET)

.PHONY: bike
bike:
	cd ../BIKE-Additional/BIKE-1 && $(MAKE) clean all
	cd ../BIKE-Additional/BIKE-3 && $(MAKE) clean all

clean:
	rm -rf obj_dir
	rm -f *.debugTrace
	rm -f *.logTrace
