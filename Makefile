# Makefile for hardware implementation on Xilinx FPGAs and ASICs
# Author: Andreas Ehliar <ehliar@isy.liu.se>

XILINX_INIT = source /sw/xilinx/ise_12.4i/ISE_DS/settings32.sh;
PART=xc6slx16-3-csg324


hesthagen.%: S=bussen.vhd cpu/CPU.vhd 7seg/ssdCtrl.vhd 7seg/Binary_To_BCD.vhd joystick/spiCtrl.vhd joystick/spiMode0.vhd joystick/ClkDiv_5Hz.vhd joystick/ClkDiv_66_67kHz.vhd joystick/PmodJSTK.vhd gpu/GPU.vhd gpu/board.vhd
hesthagen.%: U=hesthagen.ucf

include build/util.mk
include build/vsim.mk
include build/xst.mk
include build/xilinx-par.mk
include build/digilentprog.mk



