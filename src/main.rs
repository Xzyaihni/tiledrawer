use std::{
    env,
    mem,
    thread,
    process,
    rc::Rc,
    cell::{RefMut, RefCell},
    fmt::Display,
    path::Path,
    time::Duration,
    ops::{Index, IndexMut}
};

use sdl2::{
    EventPump,
    mouse::MouseButton,
    keyboard::Keycode,
    render::{Canvas, Texture, TextureCreator, BlendMode},
    pixels::{PixelFormatEnum, Color},
    event::Event,
    video::{Window, WindowContext}
};

use image::{ImageError, RgbaImage, Rgba};
use ui::{Ui, ElementId, UiElement, UiElementType};
pub use point::Point2;

use config::Config;

mod point;
mod animator;
mod config;
mod ui;


pub fn complain(message: impl Display) -> !
{
    println!("{message}");

    process::exit(1)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TextureId(usize);

struct HeldTexture
{
    size: Point2<usize>,
    texture: Texture<'static>
}

pub struct Assets
{
    creator: TextureCreator<WindowContext>,
    // i despise the lifetime on the texture, this sdl wrapper is absolute CANCER
    textures: Vec<HeldTexture>
}

impl Assets
{
    pub fn new(creator: TextureCreator<WindowContext>) -> Self
    {
        Self{
            creator,
            textures: Vec::new()
        }
    }

    pub fn add_texture(&mut self, image: &Image) -> TextureId
    {
        let id = self.textures.len();

        let texture = unsafe{ self.texture_from_image(image) };
        self.textures.push(texture);

        TextureId(id)
    }

    pub fn update_texture(&mut self, id: TextureId, image: &Image)
    {
        if image.size() != self.textures[id.0].size
        {
            self.textures[id.0] = unsafe{ self.texture_from_image(image) };

            return;
        }

        let data = image.data_bytes();
        let row = image.bytes_row();

        self.texture_mut(id).update(None, &data, row).unwrap();
    }

    unsafe fn texture_from_image(&self, image: &Image) -> HeldTexture
    {
        let size = image.size();
        let mut texture = self.creator.create_texture_static(
            PixelFormatEnum::RGBA32,
            size.x as u32,
            size.y as u32
        ).unwrap();

        texture.set_blend_mode(BlendMode::Blend);

        let data = image.data_bytes();

        texture.update(None, &data, image.bytes_row()).unwrap();

        HeldTexture{
            size,
            texture: Self::make_texture_static(texture)
        }
    }

    unsafe fn make_texture_static(texture: Texture<'_>) -> Texture<'static>
    {
        mem::transmute(texture)
    }

    pub fn texture<'a>(&'a self, id: TextureId) -> &'a Texture<'static>
    {
        &self.textures[id.0].texture
    }

    pub fn texture_mut<'a>(&'a mut self, id: TextureId) -> &'a mut Texture<'static>
    {
        &mut self.textures[id.0].texture
    }
}

pub struct WindowWrapper
{
    pub canvas: Canvas<Window>
}

impl WindowWrapper
{
    pub fn new(window: Window) -> Self
    {
        Self{
            canvas: window.into_canvas().present_vsync().build().unwrap()
        }
    }

    pub fn window_size(&self) -> Point2<u32>
    {
        let (width, height) = self.canvas.output_size().unwrap();

        Point2{x: width, y: height}
    }
}

struct UiGroup
{
    pub main_texture: TextureId,
    pub main_screen: ElementId
}

struct DrawerWindow
{
    events: EventPump,
    window: Rc<RefCell<WindowWrapper>>,
    assets: Rc<RefCell<Assets>>,
    image: Image,
    ui: Ui,
    ui_group: UiGroup,
    scale: f32,
    billinear: bool,
    mouse_position: Point2<i32>,
    draw_held: bool,
    minus_held: bool,
    plus_held: bool
}

impl DrawerWindow
{
    pub fn new(image: Image, billinear: bool) -> Self
    {
        let ctx = sdl2::init().unwrap();

        let video = ctx.video().unwrap();

        let scale: f32 = 0.25;
        let scale_r = scale.recip().round() as u32;

        let width = image.width as u32 * 7 * scale_r;
        let height = image.height as u32 * 5 * scale_r;

        let window = video
            .window("imagedisplay thingy!", width, height)
            .resizable()
            .build()
            .unwrap();

        let window = Rc::new(RefCell::new(WindowWrapper::new(window)));

        let assets = {
            let window = window.borrow();

            Rc::new(RefCell::new(Assets::new(window.canvas.texture_creator())))
        };

        let main_texture = assets.borrow_mut().add_texture(&image);

        let events = ctx.event_pump().unwrap();

        let mut ui = Ui::new(window.clone(), assets.clone());

        let element = UiElement{
            kind: UiElementType::Panel,
            pos: Point2{x: 0.5, y: 0.5},
            size: Point2{x: 0.4, y: 0.4},
            texture: main_texture
        };

        let main_screen = ui.push(element);

        let ui_group = UiGroup{
            main_texture,
            main_screen
        };

        Self{
            events,
            window,
            assets,
            image,
            ui,
            ui_group,
            scale,
            billinear,
            mouse_position: Point2{x: 0, y: 0},
            draw_held: false,
            minus_held: false,
            plus_held: false
        }
    }

    pub fn canvas_mut(&mut self) -> RefMut<Canvas<Window>>
    {
        RefMut::map(self.window.borrow_mut(), |w| &mut w.canvas)
    }

    fn draw_main_image(&self, output: &mut Image)
    {
        for y in 0..output.height
        {
            for x in 0..output.width
            {
                let pos = Point2{x, y}.map(|a|
                {
                    a as f32 * self.scale
                });

                output[Point2{x, y}] = if self.billinear
                {
                    let pos = pos.zip(self.image.size()).map(|(a, b)| a % b as f32);

                    self.image.get_billinear_overflowing(pos)
                } else
                {
                    self.image[pos.zip(self.image.size()).map(|(a, b)| a.round() as usize % b)]
                };
            }
        }
    }

    pub fn draw(&mut self)
    {
        let size = self.ui.pixels_size(&self.ui_group.main_screen).map(|x| x as usize);
        let mut main_surface = Image::repeat(size.x, size.y, Color{r: 0, g: 0, b: 0, a: 0});

        self.draw_main_image(&mut main_surface);

        self.assets.borrow_mut().update_texture(self.ui_group.main_texture, &main_surface);

        self.ui.draw();
    }

    pub fn image(&self) -> &Image
    {
        &self.image
    }

    pub fn update(&mut self, dt: f32)
    {
        let speed = 1.0;
        if self.plus_held
        {
            self.scale *= 1.0 - speed * dt;
        } else if self.minus_held
        {
            self.scale *= 1.0 + speed * dt;
        }

        if self.draw_held
        {
            if let Some(position) = self.mouse_image()
            {
                self.image[position] = Color::RGB(255, 0, 0);
            }
        }
    }

    fn mouse_image(&self) -> Option<Point2<usize>>
    {
        let window_size = self.window.borrow().window_size().map(|x| x as f32);
        let mouse_position = self.mouse_position.map(|x| x as f32) / window_size;

        let mouse_position = Point2{
            y: 1.0 - mouse_position.y,
            ..mouse_position
        };

        let mouse_inside = self.ui.get(&self.ui_group.main_screen)
            .borrow().element()
            .inside_position(mouse_position);

        let size = self.ui.pixels_size(&self.ui_group.main_screen).map(|x| x as f32);
        mouse_inside.map(|pos|
        {
            let flipped_pos = Point2{
                y: 1.0 - pos.y,
                ..pos
            };

            (flipped_pos * size).zip(self.image.size())
                .map(|(mouse, size)|
                {
                    (mouse as f32 * self.scale).round() as usize % size
                })
        })
    }

    pub fn wait_exit(&mut self)
    {
        let fps = 60;
        let dt = 1.0 / fps as f32;

        loop
        {
            for event in self.events.poll_iter()
            {
                match event
                {
                    Event::Quit{..} => return,
                    Event::KeyDown{keycode: Some(Keycode::Minus), ..} =>
                    {
                        self.minus_held = true;
                    },
                    Event::KeyUp{keycode: Some(Keycode::Minus), ..} =>
                    {
                        self.minus_held = false;
                    },
                    Event::KeyDown{keycode: Some(Keycode::Equals), ..} =>
                    {
                        self.plus_held = true;
                    },
                    Event::KeyUp{keycode: Some(Keycode::Equals), ..} =>
                    {
                        self.plus_held = false;
                    },
                    Event::MouseButtonDown{mouse_btn: MouseButton::Left, ..} =>
                    {
                        self.draw_held = true;
                    },
                    Event::MouseButtonUp{mouse_btn: MouseButton::Left, ..} =>
                    {
                        self.draw_held = false;
                    },
                    Event::MouseMotion{x, y, ..} =>
                    {
                        self.mouse_position = Point2{x, y};
                    },
                    _ => ()
                }
            }

            self.update(dt);

            self.canvas_mut().clear();

            self.draw();

            self.canvas_mut().present();

            thread::sleep(Duration::from_millis(1000 / fps));
        }
    }
}

// values per pixel
const VPP: usize = 4;
// bytes per value
const BPV: usize = 1;
// bytes per pixel
const BPP: usize = VPP * BPV;

pub struct Image
{
    data: Vec<Color>,
    width: usize,
    height: usize
}

impl Image
{
    pub fn repeat(width: usize, height: usize, c: Color) -> Self
    {
        Self{
            data: vec![c; width * height],
            width,
            height
        }
    }

    pub fn save(&self, path: impl AsRef<Path>) -> Result<(), ImageError>
    {
        RgbaImage::from_fn(self.width as u32, self.height as u32, |x, y|
        {
            let rgba = self[Point2{x, y}.map(|x| x as usize)];

            Rgba([rgba.r, rgba.g, rgba.b, rgba.a])
        }).save(path)
    }

    pub fn get_billinear_saturating(&self, point: Point2<f32>) -> Color
    {
        self.get_billinear_with(|p| self.get_saturating(p), point)
    }

    pub fn get_billinear_overflowing(&self, point: Point2<f32>) -> Color
    {
        self.get_billinear_with(|p| self.get_overflowing(p), point)
    }

    fn get_billinear_with<G>(&self, mut g: G, point: Point2<f32>) -> Color
    where
        G: FnMut(Point2<i32>) -> Color
    {
        let low = point.map(|x| x.floor() as i32);
        let high = point.map(|x| x.ceil() as i32);

        let x_lerp = point.x.fract();
        let top = Self::lerp(
            g(Point2{x: low.x, y: high.y}),
            g(Point2{x: high.x, y: high.y}),
            x_lerp
        );

        let bottom = Self::lerp(
            g(Point2{x: low.x, y: low.y}),
            g(Point2{x: high.x, y: low.y}),
            x_lerp
        );

        Self::lerp(bottom, top, point.y.fract())
    }

    fn lerp(a: Color, b: Color, t: f32) -> Color
    {
        let mix = |a: u8, b: u8, t: f32|
        {
            (a as f32 * (1.0 - t) + b as f32 * t).round() as u8
        };

        Color{
            r: mix(a.r, b.r, t),
            g: mix(a.g, b.g, t),
            b: mix(a.b, b.b, t),
            a: mix(a.a, b.a, t),
        }
    }

    pub fn data_bytes(&self) -> Vec<u8>
    {
        self.data.iter().flat_map(|p|
        {
            [p.r, p.g, p.b, p.a]
        }).collect()
    }

    pub fn bytes_row(&self) -> usize
    {
        self.width * BPP
    }

    pub fn size(&self) -> Point2<usize>
    {
        Point2{
            x: self.width,
            y: self.height
        }
    }

    pub fn get_saturating(&self, pos: Point2<i32>) -> Color
    {
        let pos = pos.zip(self.size()).map(|(x, size)| x.clamp(0, size as i32 - 1) as usize);

        self[pos]
    }

    pub fn get_overflowing(&self, pos: Point2<i32>) -> Color
    {
        let pos = pos.zip(self.size()).map(|(x, size)|
        {
            let x = x % size as i32;

            (if x < 0
            {
                size as i32 + x
            } else
            {
                x
            }) as usize
        });

        self[pos]
    }

    #[allow(dead_code)]
    pub fn pixels(&self) -> impl Iterator<Item=(Point2<usize>, &Color)> + '_
    {
        self.data.iter().enumerate().map(|(index, c)|
        {
            (self.index_to_pos(index), c)
        })
    }

    pub fn to_index(&self, pos: Point2<usize>) -> usize
    {
        Self::to_index_assoc(self.width, pos)
    }

    pub fn to_index_assoc(width: usize, pos: Point2<usize>) -> usize
    {
        pos.y * width + pos.x
    }

    #[allow(dead_code)]
    pub fn index_to_pos(&self, index: usize) -> Point2<usize>
    {
        Self::index_to_pos_assoc(self.width, index)
    }

    #[allow(dead_code)]
    pub fn index_to_pos_assoc(width: usize, index: usize) -> Point2<usize>
    {
        Point2{
            x: index % width,
            y: index / width
        }
    }
}

impl From<RgbaImage> for Image
{
    fn from(value: RgbaImage) -> Self
    {
        Self{
            width: value.width() as usize,
            height: value.height() as usize,
            data: value.pixels().copied().map(|Rgba([r, g, b, a])|
            {
                Color{r, g, b, a}
            }).collect()
        }
    }
}

impl Index<Point2<usize>> for Image
{
    type Output = Color;

    fn index(&self, index: Point2<usize>) -> &Self::Output
    {
        &self.data[self.to_index(index)]
    }
}

impl IndexMut<Point2<usize>> for Image
{
    fn index_mut(&mut self, index: Point2<usize>) -> &mut Self::Output
    {
        let index = self.to_index(index);

        &mut self.data[index]
    }
}

fn main()
{
    let config = Config::parse(env::args().skip(1));

    let image = if let Some(input) = config.input
    {
        image::open(input)
            .expect("image must be openable, silly")
            .into_rgba8()
    } else
    {
        RgbaImage::from_pixel(config.width as u32, config.height as u32, Rgba([0, 0, 0, 255]))
    };

    let image = Image::from(image);

    let mut window = DrawerWindow::new(image, config.billinear);

    window.wait_exit();

    window.image().save(config.output).expect("saving must work");
}
