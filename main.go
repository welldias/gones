package main

import (
	"fmt"

	"github.com/welldias/gones/cpu"
)

// disassemble is not required for emulation.
// It is merely a convenience function to turn the binary instruction code into
// human readable form. Its included as part of the emulator because it can take
// advantage of many of the CPUs internal operations to do this.
func disassemble(olc6502 cpu.CPU, nStart uint16, nStop uint16) map[uint16]string {

	var addr uint16 = nStart
	var value, lo, hi uint8
	var lineAddr uint16

	var mapLines = make(map[uint16]string)

	// Starting at the specified address we read an instruction
	// byte, which in turn yields information from the lookup table
	// as to how many additional bytes we need to read and what the
	// addressing mode is. I need this info to assemble human readable
	// syntax, which is different depending upon the addressing mode

	// As the instruction is decoded, a std::string is assembled
	// with the readable output
	for addr <= nStop {
		lineAddr = addr

		// Prefix line with instruction address
		sInst := "$" + fmt.Sprintf("%4X", addr) + ": "

		// Read instruction, and get its readable name
		var opcode uint8 = olc6502.Read(addr)
		addr++
		sInst += fmt.Sprintf("%s ", olc6502.GetOperateType(opcode).OperType)

		// Get oprands from desired locations, and form the
		// instruction based upon its addressing mode. These
		// routines mimmick the actual fetch routine of the
		// 6502 in order to get accurate data as part of the
		// instruction
		switch olc6502.GetOperateType(opcode).AddrMdType {
		case cpu.AddrModeTypeImp:
			sInst += " {IMP}"
		case cpu.AddrModeTypeImm:
			value = olc6502.Read(addr)
			addr++
			sInst += fmt.Sprintf("#$%2X {IMM}", value)
		case cpu.AddrModeTypeZp0:
			lo = olc6502.Read(addr)
			addr++
			hi = 0x00
			sInst += fmt.Sprintf("$%2X {ZP0}", lo)
		case cpu.AddrModeTypeZpx:
			lo = olc6502.Read(addr)
			addr++
			hi = 0x00
			sInst += fmt.Sprintf("$%2X, X {ZPX}", lo)
		case cpu.AddrModeTypeZpy:
			lo = olc6502.Read(addr)
			addr++
			hi = 0x00
			sInst += fmt.Sprintf("$%2X, Y {ZPY}", uint32(lo))
		case cpu.AddrModeTypeIzx:
			lo = olc6502.Read(addr)
			addr++
			hi = 0x00
			sInst += fmt.Sprintf("($%2X, X) {IZX}", uint32(lo))
		case cpu.AddrModeTypeIzy:
			lo = olc6502.Read(addr)
			addr++
			hi = 0x00
			sInst += fmt.Sprintf("($%2X), Y {IZY}", uint32(lo))
		case cpu.AddrModeTypeAbs:
			lo = olc6502.Read(addr)
			addr++
			hi = olc6502.Read(addr)
			addr++
			sInst += fmt.Sprintf("$%4X {ABS}", ((uint16(hi) << 8) | uint16(lo)))
		case cpu.AddrModeTypeAbx:
			lo = olc6502.Read(addr)
			addr++
			hi = olc6502.Read(addr)
			addr++
			sInst += fmt.Sprintf("$%4X {ABX}", ((uint16(hi) << 8) | uint16(lo)))
		case cpu.AddrModeTypeAby:
			lo = olc6502.Read(addr)
			addr++
			hi = olc6502.Read(addr)
			addr++
			sInst += fmt.Sprintf("$%4X {ABY}", ((uint16(hi) << 8) | uint16(lo)))
		case cpu.AddrModeTypeInd:
			lo = olc6502.Read(addr)
			addr++
			hi = olc6502.Read(addr)
			addr++
			sInst += fmt.Sprintf("($%4X) {IND}", ((uint16(hi) << 8) | uint16(lo)))
		case cpu.AddrModeTypeRel:
			value = olc6502.Read(addr)
			addr++
			sInst += fmt.Sprintf("$%4X {REL}", value)
		}

		fmt.Println("lineAddr:", lineAddr)
		fmt.Println("sInst   :", sInst)

		// Add the formed string to a std::map, using the instruction's
		// address as the key. This makes it convenient to look for later
		// as the instructions are variable in length, so a straight up
		// incremental index is not sufficient.
		mapLines[lineAddr] = sInst
	}

	return mapLines
}

func main() {
	olc6502 := cpu.CPU{}

	olc6502.Config()
	disassemble(olc6502, 0, 10)
}
