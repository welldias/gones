package tinyui

// Pixel is color of display pixel
type Pixel struct {
	mode  PixelMode
	value Color
}

// NewPixel is ...
func NewPixel(value Color) *Pixel {
	p := new(Pixel)
	p.value = value
	return p
}
