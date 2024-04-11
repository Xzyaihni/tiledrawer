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

impl PartialEq for Color
{
    fn eq(&self, other: &Self) -> bool
    {
        if self.a == 0 && other.a == 0
        {
            true
        } else
        {
            self.r == other.r
                && self.g == other.g
                && self.b == other.b
                && self.a == other.a
        }
    }
}

impl From<Color> for SdlColor
{
    fn from(x: Color) -> Self
    {
        SdlColor::RGBA(x.r, x.g, x.b, x.a)
    }
}

impl From<Hsva> for Color
{
    fn from(hsva: Hsva) -> Self
    {
        let t = |v: f32|
        {
            (v * 255.0) as u8
        };

        let f = |n: f32| -> u8
        {
            let k = (n + hsva.h / 60.0) % 6.0;

            t(hsva.v - hsva.v * hsva.s * (k.min(4.0 - k)).clamp(0.0, 1.0))
        };

        Color{r: f(5.0), g: f(3.0), b: f(1.0), a: t(hsva.a)}
    }
}

impl From<Hsv> for Color
{
    fn from(hsv: Hsv) -> Self
    {
        Hsva::from(hsv).into()
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Hsva
{
    pub h: f32,
    pub s: f32,
    pub v: f32,
    pub a: f32
}

impl From<Hsv> for Hsva
{
    fn from(hsv: Hsv) -> Self
    {
        Self{
            h: hsv.h,
            s: hsv.s,
            v: hsv.v,
            a: 1.0
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
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
