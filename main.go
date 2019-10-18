package main

import (
	"gones/pkg/cpu"
	"gones/pkg/bus"
)

// Disassemble is not required for emulation.
// It is merely a convenience function to turn the binary instruction code into
// human readable form. Its included as part of the emulator because it can take
// advantage of many of the CPUs internal operations to do this.
func Disassemble(cpu CPU, nStart uint16, nStop uint16) map[uint16]string {

	var addr uint32 = uint32(nStart)
	var value, lo, hi uint8
	var mapLines map[uint16]string
	var lineAddr uint16

	// A convenient utility to convert variables into hex strings 
	hex := func(n uint32, d uint8)string {
		return fmt.Sprintf("%X", n)
	}

	// Starting at the specified address we read an instruction
	// byte, which in turn yields information from the lookup table
	// as to how many additional bytes we need to read and what the
	// addressing mode is. I need this info to assemble human readable
	// syntax, which is different depending upon the addressing mode

	// As the instruction is decoded, a std::string is assembled
	// with the readable output
	for addr <= uint32(nStop) {
		lineAddr = addr

		// Prefix line with instruction address
		sInst := "$" + hex(addr, 4) + ": ";

		// Read instruction, and get its readable name
		opcode uint8 = cpu.bus.read(addr, true); 
		addr++;
		sInst += the6502.lookup[opcode].name + " ";

		// Get oprands from desired locations, and form the
		// instruction based upon its addressing mode. These
		// routines mimmick the actual fetch routine of the
		// 6502 in order to get accurate data as part of the
		// instruction
		if (the6502.lookup[opcode].addrmode == the6502.IMP) {
			sInst += " {IMP}";
		} else if (lookup[opcode].addrmode == &olc6502::IMM) {
			value = cpu.bus.read(addr, true); addr++;
			sInst += "#$" + hex(value, 2) + " {IMM}";
		}
		else if (lookup[opcode].addrmode == &olc6502::ZP0)
		{
			lo = cpu.bus.read(addr, true); addr++;
			hi = 0x00;												
			sInst += "$" + hex(lo, 2) + " {ZP0}";
		}
		else if (lookup[opcode].addrmode == &olc6502::ZPX)
		{
			lo = cpu.bus.read(addr, true); addr++;
			hi = 0x00;														
			sInst += "$" + hex(lo, 2) + ", X {ZPX}";
		}
		else if (lookup[opcode].addrmode == &olc6502::ZPY)
		{
			lo = cpu.bus.read(addr, true); addr++;
			hi = 0x00;														
			sInst += "$" + hex(lo, 2) + ", Y {ZPY}";
		}
		else if (lookup[opcode].addrmode == &olc6502::IZX)
		{
			lo = cpu.bus.read(addr, true); addr++;
			hi = 0x00;								
			sInst += "($" + hex(lo, 2) + ", X) {IZX}";
		}
		else if (lookup[opcode].addrmode == &olc6502::IZY)
		{
			lo = cpu.bus.read(addr, true); addr++;
			hi = 0x00;								
			sInst += "($" + hex(lo, 2) + "), Y {IZY}";
		}
		else if (lookup[opcode].addrmode == &olc6502::ABS)
		{
			lo = cpu.bus.read(addr, true); addr++;
			hi = cpu.bus.read(addr, true); addr++;
			sInst += "$" + hex((uint16_t)(hi << 8) | lo, 4) + " {ABS}";
		}
		else if (lookup[opcode].addrmode == &olc6502::ABX)
		{
			lo = cpu.bus.read(addr, true); addr++;
			hi = cpu.bus.read(addr, true); addr++;
			sInst += "$" + hex((uint16_t)(hi << 8) | lo, 4) + ", X {ABX}";
		}
		else if (lookup[opcode].addrmode == &olc6502::ABY)
		{
			lo = cpu.bus.read(addr, true); addr++;
			hi = cpu.bus.read(addr, true); addr++;
			sInst += "$" + hex((uint16_t)(hi << 8) | lo, 4) + ", Y {ABY}";
		}
		else if (lookup[opcode].addrmode == &olc6502::IND)
		{
			lo = cpu.bus.read(addr, true); addr++;
			hi = cpu.bus.read(addr, true); addr++;
			sInst += "($" + hex((uint16_t)(hi << 8) | lo, 4) + ") {IND}";
		}
		else if (lookup[opcode].addrmode == &olc6502::REL)
		{
			value = cpu.bus.read(addr, true); addr++;
			sInst += "$" + hex(value, 2) + " [$" + hex(addr + value, 4) + "] {REL}";
		}

		// Add the formed string to a std::map, using the instruction's
		// address as the key. This makes it convenient to look for later
		// as the instructions are variable in length, so a straight up
		// incremental index is not sufficient.
		mapLines[line_addr] = sInst;
	}

	return mapLines;
}

func main() {
	the6502 := The6502{}

	the6502.CreateInstructions()

	the6502.instructions[0].Addrmode()
	the6502.instructions[0].Operate()

	the6502.reset()
	the6502.irq()
	the6502.nmi()
	the6502.clock()
}
