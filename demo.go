package main

import (
	"fmt"

	rl "github.com/gen2brain/raylib-go/raylib"
	"github.com/welldias/gones/cpu"
)

// Olc6502CpuDemo is the demo to test cpu
type Olc6502CpuDemo struct {
	name   string
	mapAsm map[uint16]string
	nes    cpu.CPU
}

func (demo *Olc6502CpuDemo) init() {
	demo.nes.Config()
}

// DrawRAM is the method that show Ram info
func (demo *Olc6502CpuDemo) DrawRAM(x int32, y int32, nAddr uint16, nRows int, nColumns int) {

	nRAMX := x
	nRAMY := y

	for row := 0; row < nRows; row++ {
		sOffset := fmt.Sprintf("$%4X:", nAddr)
		for col := 0; col < nColumns; col++ {
			sOffset += fmt.Sprintf(" %2X", demo.nes.Read(nAddr))
			nAddr++
		}
		rl.DrawText(sOffset, nRAMX, nRAMY, 10, rl.Green)
		nRAMY += 10
	}
}

// DrawCPU is
func (demo *Olc6502CpuDemo) DrawCPU(x int32, y int32) {

	redOrGreen := func(flag cpu.Flag) rl.Color {
		if demo.nes.GetFlag(flag) == 1 {
			return rl.Green
		}
		return rl.Red
	}

	rl.DrawText("STATUS:", x, y, 10, rl.Green)
	rl.DrawText(fmt.Sprintf("N"), x+64, y, 10, redOrGreen(cpu.FlagN))
	rl.DrawText(fmt.Sprintf("V"), x+80, y, 10, redOrGreen(cpu.FlagV))
	rl.DrawText(fmt.Sprintf("-"), x+96, y, 10, redOrGreen(cpu.FlagU))
	rl.DrawText(fmt.Sprintf("B"), x+112, y, 10, redOrGreen(cpu.FlagB))
	rl.DrawText(fmt.Sprintf("D"), x+128, y, 10, redOrGreen(cpu.FlagD))
	rl.DrawText(fmt.Sprintf("I"), x+144, y, 10, redOrGreen(cpu.FlagI))
	rl.DrawText(fmt.Sprintf("Z"), x+160, y, 10, redOrGreen(cpu.FlagZ))
	rl.DrawText(fmt.Sprintf("C"), x+178, y, 10, redOrGreen(cpu.FlagC))

	rl.DrawText(fmt.Sprintf("PC: $%4X", demo.nes.GetProgramCounter()), x, y+10, 10, rl.Green)
	rl.DrawText(fmt.Sprintf("A: $%2X [%d]", demo.nes.GetRegisterA(), demo.nes.GetRegisterA()), x, y+20, 10, rl.Green)
	rl.DrawText(fmt.Sprintf("X: $%2X [%d]", demo.nes.GetRegisterX(), demo.nes.GetRegisterX()), x, y+30, 10, rl.Green)
	rl.DrawText(fmt.Sprintf("Y: $%2X [%d]", demo.nes.GetRegisterY(), demo.nes.GetRegisterY()), x, y+40, 10, rl.Green)
	rl.DrawText(fmt.Sprintf("Stack P: $%4X", demo.nes.GetStackPtr()), x, y+50, 10, rl.Green)
}

func runDemo() {

	demo := new(Olc6502CpuDemo)
	demo.init()

	// Load Program (assembled at https://www.masswerk.at/6502/assembler.html)
	/*
		*=$8000
		LDX #10
		STX $0000
		LDX #3
		STX $0001
		LDY $0000
		LDA #0
		CLC
		loop
		ADC $0001
		DEY
		BNE loop
		STA $0002
		NOP
		NOP
		NOP
	*/
	assCode := [28]uint8{0xa2, 0x0a, 0x8e, 0x00, 0x00, 0xa2, 0x03, 0x8e, 0x01, 0x00, 0xac, 0x00, 0x00, 0xa9, 0x00, 0x18, 0x6d, 0x01, 0x00, 0x88, 0xd0, 0xfa, 0x8d, 0x02, 0x00, 0xea, 0xea, 0xea}
	var nOffset uint16 = 0x8000

	for i, code := range assCode {
		demo.nes.Write(nOffset+uint16(i), code)
	}

	// Set Reset Vector
	demo.nes.Write(0xfffc, 0x00)
	demo.nes.Write(0xfffd, 0x80)

	demo.mapAsm = demo.disassemble(0x0000, 0xFFFF)

	screenWidth := int32(800)
	screenHeight := int32(480)

	rl.InitWindow(screenWidth, screenHeight, "raylib [text] example - text formatting")

	rl.SetTargetFPS(60)

	for !rl.WindowShouldClose() {

		if rl.IsKeyPressed(rl.KeySpace) {
			//fmt.Println("Space pressed")
			for ok := true; ok; ok = !demo.nes.Complete() {
				demo.nes.Clock()
			}
		}

		if rl.IsKeyPressed(rl.KeyR) {
			//fmt.Println("R pressed")
			demo.nes.Reset()
		}

		if rl.IsKeyPressed(rl.KeyI) {
			//fmt.Println("I pressed")
			demo.nes.Irq()
		}

		if rl.IsKeyPressed(rl.KeyN) {
			//fmt.Println("N pressed")
			demo.nes.Nmi()
		}

		rl.BeginDrawing()

		rl.ClearBackground(rl.Black)

		demo.DrawRAM(2, 2, 0x0000, 16, 16)
		demo.DrawRAM(2, 182, 0x8000, 16, 16)
		demo.DrawCPU(448, 2)
		demo.DrawCode(448, 72, 26)
		rl.DrawText("SPACE = Step Instruction    R = RESET    I = IRQ    N = NMI", 10, 370, 10, rl.Green)

		rl.EndDrawing()
	}

	rl.CloseWindow()
}

// disassemble is not required for emulation.
// It is merely a convenience function to turn the binary instruction code into
// human readable form. Its included as part of the emulator because it can take
// advantage of many of the CPUs internal operations to do this.
func (demo *Olc6502CpuDemo) disassemble(nStart uint16, nStop uint16) map[uint16]string {

	var addr uint32 = uint32(nStart)
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
	for addr <= uint32(nStop) {
		lineAddr = uint16(addr)

		// Prefix line with instruction address
		sInst := fmt.Sprintf("$%4X: ", addr)

		// Read instruction, and get its readable name
		var opcode uint8 = demo.nes.Read(uint16(addr))
		addr++
		sInst += fmt.Sprintf("%s ", demo.nes.GetOperateType(opcode).OperType)

		// Get oprands from desired locations, and form the
		// instruction based upon its addressing mode. These
		// routines mimmick the actual fetch routine of the
		// 6502 in order to get accurate data as part of the
		// instruction
		switch demo.nes.GetOperateType(opcode).AddrMdType {
		case cpu.AddrModeTypeImp:
			sInst += " {IMP}"
		case cpu.AddrModeTypeImm:
			value = demo.nes.Read(uint16(addr))
			addr++
			sInst += fmt.Sprintf("#$%2X {IMM}", value)
		case cpu.AddrModeTypeZp0:
			lo = demo.nes.Read(uint16(addr))
			addr++
			hi = 0x00
			sInst += fmt.Sprintf("$%2X {ZP0}", lo)
		case cpu.AddrModeTypeZpx:
			lo = demo.nes.Read(uint16(addr))
			addr++
			hi = 0x00
			sInst += fmt.Sprintf("$%2X, X {ZPX}", lo)
		case cpu.AddrModeTypeZpy:
			lo = demo.nes.Read(uint16(addr))
			addr++
			hi = 0x00
			sInst += fmt.Sprintf("$%2X, Y {ZPY}", uint32(lo))
		case cpu.AddrModeTypeIzx:
			lo = demo.nes.Read(uint16(addr))
			addr++
			hi = 0x00
			sInst += fmt.Sprintf("($%2X, X) {IZX}", uint32(lo))
		case cpu.AddrModeTypeIzy:
			lo = demo.nes.Read(uint16(addr))
			addr++
			hi = 0x00
			sInst += fmt.Sprintf("($%2X), Y {IZY}", uint32(lo))
		case cpu.AddrModeTypeAbs:
			lo = demo.nes.Read(uint16(addr))
			addr++
			hi = demo.nes.Read(uint16(addr))
			addr++
			sInst += fmt.Sprintf("$%4X {ABS}", ((uint16(hi) << 8) | uint16(lo)))
		case cpu.AddrModeTypeAbx:
			lo = demo.nes.Read(uint16(addr))
			addr++
			hi = demo.nes.Read(uint16(addr))
			addr++
			sInst += fmt.Sprintf("$%4X {ABX}", ((uint16(hi) << 8) | uint16(lo)))
		case cpu.AddrModeTypeAby:
			lo = demo.nes.Read(uint16(addr))
			addr++
			hi = demo.nes.Read(uint16(addr))
			addr++
			sInst += fmt.Sprintf("$%4X {ABY}", ((uint16(hi) << 8) | uint16(lo)))
		case cpu.AddrModeTypeInd:
			lo = demo.nes.Read(uint16(addr))
			addr++
			hi = demo.nes.Read(uint16(addr))
			addr++
			sInst += fmt.Sprintf("($%4X) {IND}", ((uint16(hi) << 8) | uint16(lo)))
		case cpu.AddrModeTypeRel:
			value = demo.nes.Read(uint16(addr))
			addr++
			sInst += fmt.Sprintf("$%4X {REL}", value)
		}

		//fmt.Print(",", addr)
		//fmt.Println("lineAddr:", lineAddr)
		//fmt.Println("sInst   :", sInst)

		// Add the formed string to a std::map, using the instruction's
		// address as the key. This makes it convenient to look for later
		// as the instructions are variable in length, so a straight up
		// incremental index is not sufficient.
		mapLines[lineAddr] = sInst
	}

	return mapLines
}

// DrawCode is ...
func (demo *Olc6502CpuDemo) DrawCode(x int32, y int32, nLines int32) {

	keyValue := demo.nes.GetProgramCounter()
	var nLineY int32 = (nLines>>1)*10 + y

	if int(keyValue) != len(demo.mapAsm)-1 {

		rl.DrawText(demo.mapAsm[keyValue], x, nLineY, 10, rl.Red)

		for ; nLineY < (nLines*10)+y; nLineY += 10 {
			if int(keyValue+1) != len(demo.mapAsm)-1 {
				keyValue = keyValue + 1
				rl.DrawText(fmt.Sprintf("(%d) %s", keyValue, demo.mapAsm[keyValue]), x, nLineY, 10, rl.Red)
			}
		}
	}

	/*
		keyValue = demo.nes.GetProgramCounter()
		nLineY = (nLines>>1)*7 + y

		if int(keyValue) != len(demo.mapAsm)-1 {
			for ; nLineY > y; nLineY -= 7 {
				if int(keyValue-1) != len(demo.mapAsm)-1 {
					keyValue = keyValue - 1
					rl.DrawText(demo.mapAsm[keyValue], x, nLineY, 10, rl.Red)

				}
			}
		}
	*/
}
