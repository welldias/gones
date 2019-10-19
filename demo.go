package main

import (
	"fmt"

	"github.com/welldias/gones/cpu"
)

// Olc6502CpuDemo is the demo to test cpu
type Olc6502CpuDemo struct {
	name   string
	mapAsm map[uint16]string
	nes    cpu.CPU
}

// DrawRRAM is the method that show Ram info
func (demo Olc6502CpuDemo) DrawRRAM(x int, y int, nAddr uint16, nRows int, nColumns int) {

	nRAMX := x
	nRAMY := y

	for row := 0; row < nRows; row++ {
		sOffset := fmt.Sprintf("$%4X:", nAddr)
		for col := 0; col < nColumns; col++ {
			sOffset += fmt.Sprintf(" %2X", demo.nes.Read(nAddr))
			nAddr++
		}
		demo.DrawString(nRAMX, nRAMY, sOffset)
		nRAMY += 10
	}
}

// DrawCPU is
func (demo Olc6502CpuDemo) DrawCPU(x int, y int) {

	demo.DrawString(x, y, "STATUS:")
	/*
		demo.DrawString(x  + 64, y, fmt.Sprintf("N%s", (nes.cpu.status & cpu.FlagN) != 0  );
		demo.DrawString(x  + 80, y , fmt.Sprintf("V%s", nes.cpu.status);
		demo.DrawString(x  + 96, y , fmt.Sprintf("U%s", nes.cpu.status);
		demo.DrawString(x  + 112, y , fmt.Sprintf("B%s", nes.cpu.status);
		demo.DrawString(x  + 128, y , fmt.Sprintf("D%s", nes.cpu.status);
		demo.DrawString(x  + 144, y , fmt.Sprintf("I%s", nes.cpu.status);
		demo.DrawString(x  + 160, y , fmt.Sprintf("Z%s", nes.cpu.status);
		demo.DrawString(x  + 178, y , fmt.Sprintf("C%s", nes.cpu.status);
	*/

	demo.DrawString(x, y+10, fmt.Sprintf("PC: $%4X", demo.nes.GetProgramCounter()))
	demo.DrawString(x, y+20, fmt.Sprintf("A : $%2X [%d]", demo.nes.GetRegisterA(), demo.nes.GetRegisterA()))
	demo.DrawString(x, y+30, fmt.Sprintf("X: $%2X [%d]", demo.nes.GetRegisterX(), demo.nes.GetRegisterX()))
	demo.DrawString(x, y+40, fmt.Sprintf("Y: $%2X [%d]", demo.nes.GetRegisterY(), demo.nes.GetRegisterY()))
	demo.DrawString(x, y+50, fmt.Sprintf("Stack P: $%4X", demo.nes.GetStackPtr()))
}

//DrawString is
func (demo Olc6502CpuDemo) DrawString(x int, y int, str string) {
}

func (demo Olc6502CpuDemo)  DrawCode(x int, y int, nLines int) {
	
	itA := demo.mapAsm[demo.nes.GetProgramCounter()]

	var nLineY int = (nLines >> 1) * 10 + y

		if itA != mapAsm.end() {
			
			DrawString(x, nLineY, (*it_a).second)

			while (nLineY < (nLines * 10) + y)
			{
				nLineY += 10;
				if (++it_a != mapAsm.end())
				{
					DrawString(x, nLineY, (*it_a).second);
				}
			}
		}

		it_a = mapAsm.find(nes.cpu.pc);
		nLineY = (nLines >> 1) * 10 + y;
		if (it_a != mapAsm.end())
		{
			while (nLineY > y)
			{
				nLineY -= 10;
				if (--it_a != mapAsm.end())
				{
					DrawString(x, nLineY, (*it_a).second);
				}
			}
		}
	}

//= "olc6502 CPU Demonstration"
