package main

import (
	"fmt"
	"log"
	"runtime"
	"strings"

	"github.com/go-gl/gl/v2.1/gl"
	"github.com/go-gl/glfw/v3.1/glfw"
)

const (
	width  = 500
	height = 500

	vertexShaderSource = `
		#version 410
		in vec3 vp;
		void main() {
			gl_Position = vec4(vp, 1.0);
		}
	` + "\x00"

	fragmentShaderSource = `
		#version 410
		out vec4 frag_colour;
		void main() {
			frag_colour = vec4(1, 1, 1, 1.0);
		}
	` + "\x00"
)

var (
	triangle = []float32{
		0, 0.5, 0,
		-0.5, -0.5, 0,
		0.5, -0.5, 0,
	}
)

func runDemo() {
	runtime.LockOSThread()

	window := initGlfw()
	defer glfw.Terminate()
	program := initOpenGL()

	vao := makeVao(triangle)
	for !window.ShouldClose() {
		draw(vao, window, program)
	}
}

func draw(vao uint32, window *glfw.Window, program uint32) {
	gl.Clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT)
	gl.UseProgram(program)

	gl.BindVertexArray(vao)
	gl.DrawArrays(gl.TRIANGLES, 0, int32(len(triangle)/3))

	glfw.PollEvents()
	window.SwapBuffers()
}

// initGlfw initializes glfw and returns a Window to use.
func initGlfw() *glfw.Window {
	if err := glfw.Init(); err != nil {
		panic(err)
	}
	glfw.WindowHint(glfw.Resizable, glfw.False)
	glfw.WindowHint(glfw.ContextVersionMajor, 4)
	glfw.WindowHint(glfw.ContextVersionMinor, 1)
	glfw.WindowHint(glfw.OpenGLProfile, glfw.OpenGLCoreProfile)
	glfw.WindowHint(glfw.OpenGLForwardCompatible, glfw.True)

	window, err := glfw.CreateWindow(width, height, "Conway's Game of Life", nil, nil)
	if err != nil {
		panic(err)
	}
	window.MakeContextCurrent()

	return window
}

// initOpenGL initializes OpenGL and returns an intiialized program.
func initOpenGL() uint32 {
	if err := gl.Init(); err != nil {
		panic(err)
	}
	version := gl.GoStr(gl.GetString(gl.VERSION))
	log.Println("OpenGL version", version)

	vertexShader, err := compileShader(vertexShaderSource, gl.VERTEX_SHADER)
	if err != nil {
		panic(err)
	}

	fragmentShader, err := compileShader(fragmentShaderSource, gl.FRAGMENT_SHADER)
	if err != nil {
		panic(err)
	}

	prog := gl.CreateProgram()
	gl.AttachShader(prog, vertexShader)
	gl.AttachShader(prog, fragmentShader)
	gl.LinkProgram(prog)
	return prog
}

// makeVao initializes and returns a vertex array from the points provided.
func makeVao(points []float32) uint32 {
	var vbo uint32
	gl.GenBuffers(1, &vbo)
	gl.BindBuffer(gl.ARRAY_BUFFER, vbo)
	gl.BufferData(gl.ARRAY_BUFFER, 4*len(points), gl.Ptr(points), gl.STATIC_DRAW)

	var vao uint32
	gl.GenVertexArrays(1, &vao)
	gl.BindVertexArray(vao)
	gl.EnableVertexAttribArray(0)
	gl.BindBuffer(gl.ARRAY_BUFFER, vbo)
	gl.VertexAttribPointer(0, 3, gl.FLOAT, false, 0, nil)

	return vao
}

func compileShader(source string, shaderType uint32) (uint32, error) {
	shader := gl.CreateShader(shaderType)

	csources, free := gl.Strs(source)
	gl.ShaderSource(shader, 1, csources, nil)
	free()
	gl.CompileShader(shader)

	var status int32
	gl.GetShaderiv(shader, gl.COMPILE_STATUS, &status)
	if status == gl.FALSE {
		var logLength int32
		gl.GetShaderiv(shader, gl.INFO_LOG_LENGTH, &logLength)

		log := strings.Repeat("\x00", int(logLength+1))
		gl.GetShaderInfoLog(shader, logLength, nil, gl.Str(log))

		return 0, fmt.Errorf("failed to compile %v: %v", source, log)
	}

	return shader, nil
}

/*
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
	//	demo.DrawString(x  + 64, y, fmt.Sprintf("N%s", (nes.cpu.status & cpu.FlagN) != 0  );
	//	demo.DrawString(x  + 80, y , fmt.Sprintf("V%s", nes.cpu.status);
	//	demo.DrawString(x  + 96, y , fmt.Sprintf("U%s", nes.cpu.status);
	//	demo.DrawString(x  + 112, y , fmt.Sprintf("B%s", nes.cpu.status);
	//	demo.DrawString(x  + 128, y , fmt.Sprintf("D%s", nes.cpu.status);
	//	demo.DrawString(x  + 144, y , fmt.Sprintf("I%s", nes.cpu.status);
	//	demo.DrawString(x  + 160, y , fmt.Sprintf("Z%s", nes.cpu.status);
	//	demo.DrawString(x  + 178, y , fmt.Sprintf("C%s", nes.cpu.status);

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

*/
