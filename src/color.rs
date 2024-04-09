use sdl2::pixels::Color as SdlColor;


#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct Color
{
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub a: u8
}

impl From<Color> for SdlColor
{
    fn from(x: Color) -> Self
    {
        SdlColor::RGBA(x.r, x.g, x.b, x.a)
    }
}

impl From<Hsv> for Color
{
    fn from(hsv: Hsv) -> Self
    {
        let t = |v: f32|
        {
            (v * 255.0) as u8
        };

        let f = |n: f32| -> u8
        {
            let k = (n + hsv.h / 60.0) % 6.0;

            t(hsv.v - hsv.v * hsv.s * (k.min(4.0 - k)).clamp(0.0, 1.0))
        };

        Color{r: f(5.0), g: f(3.0), b: f(1.0), a: 255}
    }
}

pub struct Hsv
{
    pub h: f32,
    pub s: f32,
    pub v: f32
}

impl From<Color> for Hsv
{
    fn from(rgb: Color) -> Self
    {
        let t = |v: u8|
        {
            v as f32 / 255.0
        };

        let r = t(rgb.r);
        let g = t(rgb.g);
        let b = t(rgb.b);

        let v = r.max(g.max(b));

        let min = r.min(g.min(b));

        let c = v - min;

        let h = if c == 0.0
        {
            0.0
        } else if v == r
        {
            ((g - b) / c) % 6.0
        } else if v == g
        {
            (b - r) / c + 2.0
        } else if v == b
        {
            (r - g) / c + 4.0
        } else
        {
            unreachable!()
        };

        let h = 60.0 * h;

        let s = if v == 0.0
        {
            0.0
        } else
        {
            c / v
        };

        Self{
            h,
            s,
            v
        }
    }
}
