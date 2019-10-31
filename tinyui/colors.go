package tinyui

// Color is
type Color uint32

// Some default usefull colors
const (
	White           Color = 0xffffffff
	Grey            Color = 0xc0c0c0ff
	DarkGrey        Color = 0x808080ff
	VeryDarkGrey    Color = 0x404040ff
	Red             Color = 0xff0000ff
	DarkRed         Color = 0x800000ff
	VeryDarkRed     Color = 0x400000ff
	Yellow          Color = 0xffff00ff
	DarkYellow      Color = 0x808000ff
	VeryDarkYellow  Color = 0x404000ff
	Green           Color = 0x00ff00ff
	DarkGreen       Color = 0x008000ff
	VeryDarkGreen   Color = 0x004000ff
	Cyan            Color = 0x00ffffff
	DarkCyan        Color = 0x008080ff
	VeryDarkCyan    Color = 0x004040ff
	Blue            Color = 0x0000ffff
	DarkBlue        Color = 0x000080ff
	VeryDarkBlue    Color = 0x000040ff
	Magenta         Color = 0xff00ffff
	DarkMagenta     Color = 0x800080ff
	VeryDarkMagenta Color = 0x400040ff
	Black           Color = 0x000000ff
	Blank           Color = 0x00000000
)
