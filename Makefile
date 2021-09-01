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
ADDCFLAGS += -CFLAGS -O3

ifeq ($(VARIANT),)
DIR:=obj_dir
TARGET_CORE:=VexRiscv.v
else
DIR:=obj_dir_$(VARIANT)
TARGET_CORE:=VexRiscv_$(VARIANT).v
endif


all: $(DIR)/VVexRiscv

$(DIR)/VVexRiscv.cpp: ${TARGET_CORE} profile.cpp
	cp ${TARGET_CORE}*.bin . | true
	verilator -cc -Mdir $(DIR) ${TARGET_CORE} --prefix VVexRiscv -O3 -CFLAGS -std=c++11 -LDFLAGS -pthread ${ADDCFLAGS} --gdbbt ${VERILATOR_ARGS} -Wno-UNOPTFLAT -Wno-WIDTH  --exe profile.cpp --x-assign unique

$(DIR)/VVexRiscv: $(DIR)/VVexRiscv.cpp
	$(MAKE)  -j${THREAD_COUNT} -C $(DIR)/ -f VVexRiscv.mk VVexRiscv

$(TARGET_CORE): VexRiscv.v
	cp $^ $@

clean:
	rm -rf $(DIR)
	rm -f *.debugTrace
	rm -f *.logTrace
